import Foundation

protocol Node {
    func interpret() throws -> Float
}


struct VariableDeclaration: Node {
    let name: String
    let value: Node
    
    func interpret() throws -> Float {
        let val = try value.interpret()
        identifiers[name] = .variable(value: val)
        
        return val
    }
}

enum Operator: String {
    case times = "*"
    case divideBy = "/"
    case plus = "+"
    case minus = "-"
    
    var precedence: Int {
        switch self {
        case .minus, .plus:
            return 10
        case .times, .divideBy:
            return 20
        }
    }
}

struct Block: Node {
    let nodes: [Node]
    
    func interpret() throws -> Float {
        for line in nodes[0..<(nodes.endIndex - 1)] {
            try line.interpret()
        }
        
        guard let last = nodes.last else {
            throw Parser.Error.expectedExpression
        }
        return try last.interpret()
    }
}

enum Token {
    typealias Generator = (String) -> Token?
    
    case op(Operator)
    case number(Float)
    case identifier(String)
    case parensOpen
    case parensClose
    case `var`
    case equals
    case function
    case curlyOpen
    case curlyClose
    case comma
    case `if`
    case `else`
    
    static var generators: [String: Generator] {
        return [
            "\\*|\\/|\\+|\\-": { .op(Operator(rawValue: $0)!) },
            "\\-?([0-9]*\\.[0-9]+|[0-9]+)": { .number(Float($0)!) },
            "[a-zA-Z_$][a-zA-Z_$0-9]*": {
                guard $0 != "var" else {
                    return .var
                }
                guard $0 != "function" else {
                    return .function
                }
                guard $0 != "if" else {
                    return .if
                }
                guard $0 != "else" else {
                    return .else
                }
                return .identifier($0)
            },
            "\\(": { _ in .parensOpen },
            "\\)": { _ in .parensClose },
            "\\=": { _ in .equals },
            "\\{": { _ in .curlyOpen },
            "\\}": { _ in .curlyClose },
            "\\,": { _ in .comma }
        ]
    }
}

struct FunctionCall: Node {
    let identifier: String
    let parameters: [Node]
    
    func interpret() throws -> Float {
        guard let definition = identifiers[identifier],
            case let .function(function) = definition else {
                throw Parser.Error.notDefined(identifier)
        }
        
        guard function.parameters.count == parameters.count else {
            throw Parser.Error.invalidParameters(toFunction: identifier)
        }
        
        let paramsAndValues = zip(function.parameters, parameters)
        
        // Temporarily add parameters to global index
        try paramsAndValues.forEach { (name, node) in
            guard identifiers[name] == nil else {
                throw Parser.Error.alreadyDefined(name)
            }
            identifiers[name] = .variable(value: try node.interpret())
        }
        print(paramsAndValues)
        
        let returnValue = try function.block.interpret()
        
        // Remove parameter values from global index after use
        paramsAndValues.forEach { (name, _) in
            identifiers.removeValue(forKey: name)
        }
        
        return returnValue
    }
}

struct InfixOperation: Node {
    let op: Operator
    let lhs: Node
    let rhs: Node
    
    func interpret() throws -> Float {
        let left = try lhs.interpret()
        let right = try rhs.interpret()
        switch op {
        case .divideBy:
            return left / right
        case .times:
            return left * right
        case .minus:
            return left - right
        case .plus:
            return left + right
        }
    }
}

class Parser {
    
    enum Error: Swift.Error {
        case expectedNumber
        case expectedIdentifier
        case expectedOperator
        case expectedExpression
        case expected(String)
        case notDefined(String)
        case invalidParameters(toFunction: String)
        case alreadyDefined(String)
    }
    
    let tokens: [Token]
    var index = 0
    
    init(tokens: [Token]) {
        self.tokens = tokens
    }
    
    var canPop: Bool {
        return index < tokens.count
    }
    
    func peek() -> Token {
        return tokens[index]
    }
    
    func popToken() -> Token {
        let token = tokens[index]
        index += 1
        return token
    }
    
    func parseIdentifier() throws -> Node { //
        guard case let .identifier(name) = popToken() else {
            throw Error.expectedIdentifier
        }
        return name
    }
    
    func parseNumber() throws -> Node {
        guard case let .number(float) = popToken() else {
            throw Error.expectedNumber
        }
        return float
    }
    
    func parseParens() throws -> Node {
        guard case .parensOpen = popToken() else {
            throw Error.expected("(")
        }
        
        let expressionNode = try parseExpression() // ADDED - was "try parse()"
        
        guard case .parensClose = popToken() else {
            throw Error.expected("(")
        }
        
        return expressionNode
    }
    
    func parseVariableDeclaration() throws -> Node {
        guard case .var = popToken() else {
            throw Error.expected("\"var\" in variable declaration")
        }
        
        guard case let .identifier(name) = popToken() else {
            throw Error.expectedIdentifier
        }
        
        guard case .equals = popToken() else {
            throw Error.expected("=")
        }
        
        let expression = try parseExpression()
        
        return VariableDeclaration(name: name, value: expression)
    }
    
    func parseExpression() throws -> Node { // ADDED this is the old parse method
        guard canPop else {
            throw Error.expectedExpression
        }
        let node = try parseValue()
        return try parseInfixOperation(node: node)
    }
    
    func parse() throws -> Node {
        var nodes: [Node] = []
        while canPop {
            let token = peek()
            switch token {
            case .var:
                let declaration = try parseVariableDeclaration()
                nodes.append(declaration)
            case .function:
                let definition = try parseFunctionDefinition()
                nodes.append(definition)
            case .if:
                let statement = try parseIfStatement()
                nodes.append(statement)
            default:
                let expression = try parseExpression()
                nodes.append(expression)
            }
        }
        return Block(nodes: nodes)
    }
    
    func parseValue() throws -> Node {
        switch (peek()) {
        case .number:
            return try parseNumber()
        case .parensOpen:
            return try parseParens()
        case .identifier:
            guard let identifier = try parseIdentifier() as? String else {
                throw Error.expectedIdentifier
            }
            guard canPop, case .parensOpen = peek() else {
                return identifier
            }

            let params = try parseParameterList()
            return FunctionCall(identifier: identifier,
                                parameters: params)
        default:
            throw Error.expectedExpression
        }
    }
    
    func peekPrecedence() throws -> Int {
        guard canPop, case let .op(op) = peek() else {
            return -1
        }
        
        return op.precedence
    }
    
    func parseInfixOperation(node: Node, nodePrecedence: Int = 0) throws -> Node {
        var leftNode = node

        while let precedence = try peekPrecedence() as? Int, precedence >= nodePrecedence {
            guard case let .op(op) = popToken() else {
                throw Error.expectedOperator
            }
            
            var rightNode = try parseValue()

            let nextPrecedence = try peekPrecedence()
            
            if precedence < nextPrecedence {
                rightNode = try parseInfixOperation(node: rightNode, nodePrecedence: precedence + 1)
            }
            leftNode = InfixOperation(op: op, lhs: leftNode, rhs: rightNode)
        }
        return leftNode
    }
    
    func parseParameterList() throws -> [Node] {
        var params: [Node] = []

        guard case .parensOpen = popToken() else {
            throw Error.expected("(")
        }

        while self.canPop {
            guard let value = try? parseValue() else {
                break
            }

            guard case .comma = peek() else {
                params.append(value)
                break
            }

            popToken()
            params.append(value)
        }
        print(peek())
        guard canPop, case .parensClose = popToken() else {
            throw Error.expected(")")
        }

        return params
    }
    
    func parseFunctionDefinition() throws -> Node {
        guard case .function = popToken() else {
            throw Error.expected("function")
        }
        
        guard case let .identifier(identifier) = popToken() else {
            throw Error.expectedIdentifier
        }

        let paramNodes = try parseParameterList()

        // Convert the nodes to their String values
        let paramList = try paramNodes
            .map { node -> String in
                guard let string = node as? String else {
                    throw Error.expectedIdentifier
                }
                return string
            }
        
        let codeBlock = try parseCurlyCodeBlock()
        
        return FunctionDefinition(identifier: identifier,
                                  parameters: paramList,
                                  block: codeBlock)
    }
    
    func parseCurlyCodeBlock() throws -> Node {
        guard canPop, case .curlyOpen = popToken() else {
            throw Parser.Error.expected("{")
        }
        
        var depth = 1
        
        let startIndex = index
        
        while canPop {
            guard case .curlyClose = peek() else {
                if case .curlyOpen = peek() {
                    depth += 1
                }

                index += 1
                continue
            }
            
            depth -= 1

            guard depth == 0 else {
                index += 1
                continue
            }

            break
        }
        
        let endIndex = index
        
        guard canPop, case .curlyClose = popToken() else {
            throw Error.expected("}")
        }
        
        let tokens = Array(self.tokens[startIndex..<endIndex])
        return try Parser(tokens: tokens).parse()
    }
    
    func parseIfStatement() throws -> Node {
        guard canPop, case .if = popToken() else {
            throw Parser.Error.expected("if")
        }
        
        let firstExpression = try parseExpression()
        
        let codeBlock = try parseCurlyCodeBlock()

        let ifsAndElseIfs: [(Node, Node)] = [ (firstExpression, codeBlock) ]
        
        guard canPop, case .else = peek() else {
            return IfStatement(ifsAndElseIfs: ifsAndElseIfs, elseBlock: nil)
        }
        
        popToken()

        guard case .if = peek() else {
            let elseBlock = try parseCurlyCodeBlock()
            
            return IfStatement(ifsAndElseIfs: ifsAndElseIfs,
                               elseBlock: elseBlock)
        }
        
        let ifStatement = try parseIfStatement() as! IfStatement
        
        return IfStatement(ifsAndElseIfs: ifsAndElseIfs + ifStatement.ifsAndElseIfs,
                           elseBlock: ifStatement.elseBlock)
    }
}

extension Float: Node {
    func interpret() throws -> Float {
        return self
    }
}

extension String: Node {
    func interpret() throws -> Float {
        guard let definition = identifiers[self],
            case let .variable(value) = definition else { // CHANGED WAS: guard let value = identifiers[self] else {
            throw Parser.Error.notDefined(self)
        }
        return value
    }
}

enum Definition {
    case variable(value: Float)
    case function(FunctionDefinition)
}

struct FunctionDefinition: Node { // ADDED
    let identifier: String
    let parameters: [String]
    let block: Node

    func interpret() throws -> Float {
        identifiers[identifier] = .function(self)
        return 1
    }
}

struct IfStatement: Node {
    let ifsAndElseIfs: [(expression: Node, block: Node)]
    let elseBlock: Node?
    
    func interpret() throws -> Float {
        for ifOrElseIf in ifsAndElseIfs {
            guard (try ifOrElseIf.expression.interpret()) >= 1 else {
                continue
            }
            
            return try ifOrElseIf.block.interpret()
        }
        
        guard let elseBlock = self.elseBlock else {
            return -1
        }
        
        return try elseBlock.interpret()
    }
}

var identifiers: [String: Definition] = [ // CHANGED
    "PI": .variable(value: Float.pi),
]

public extension String {
    public func getPrefix(regex: String) -> String? {
        let expression = try! NSRegularExpression(pattern: "^\(regex)", options: [])
        
        let range = expression.rangeOfFirstMatch(in: self,
                                                 options: [],
                                                 range: NSRange(location: 0,
                                                                length: self.utf16.count))
        if range.location == 0 {
            return (self as NSString).substring(with: range)
        }
        return nil
    }
    
    public mutating func trimLeadingWhitespace() {
        let i = startIndex
        while i < endIndex {
            guard CharacterSet.whitespacesAndNewlines
                .contains(self[i].unicodeScalars.first!) else {
                return
            }
            self.remove(at: i)
        }
    }
}

extension Sequence {
    func count(where: (Iterator.Element) -> Bool) -> Int {
        var cnt = 0
        for element in self {
            if `where`(element) {
                cnt += 1
            }
        }
        return cnt
    }
}

class Lexer {
    let tokens: [Token]
    
    
    private static func getNextPrefix(code: String) -> (regex: String, prefix: String)? {
        
        var keyValue: (key: String, value: Token.Generator)?

        for (regex, generator) in Token.generators {
            
            if code.getPrefix(regex: regex) != nil {
                keyValue = (regex, generator)
            }
        }
        
        guard let regex = keyValue?.key,
            keyValue?.value != nil else {
                return nil
        }
        
        
        return (regex, code.getPrefix(regex: regex)!)
    }
    
    
    init(code: String) {
        var code = code
        code.trimLeadingWhitespace()
        var tokens: [Token] = []
        while let next = Lexer.getNextPrefix(code: code) {
            let (regex, prefix) = next
            code = String(code[prefix.endIndex...])
            code.trimLeadingWhitespace()
            
            
            guard let generator = Token.generators[regex],
                let token = generator(prefix) else {
                    fatalError()
            }
            
            
            tokens.append(token)
        }
        self.tokens = tokens
    }
}

var code = """
function sumN(n, sum) {
    if n {
        sumN((n - 1), (sum + n))
    }
    sum
}

sumN(5, 0)
"""

let tokens = Lexer(code: code).tokens
let node = try Parser(tokens: tokens).parse()
do {
    print((try node.interpret()) == 5 + 4 + 3 + 2 + 1)
} catch {
    print((error as? Parser.Error))
}

