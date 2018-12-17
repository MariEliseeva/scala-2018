package ru.hse.spb

class Calculator() extends ExpBaseVisitor[Int] {
  override def visitExpression(ctx: ExpParser.ExpressionContext): Int = {
    ctx.Literal() match {
      case null => visitNonLiteralExpression(ctx)
      case literal => Calculator.getLiteral(literal.getText)
    }
  }

  private def visitNonLiteralExpression(ctx: ExpParser.ExpressionContext): Int = {
    ctx.expression().size match {
      case 1 =>
        ctx.operation match {
          case null => visitExpression(ctx.expression(0))
          case operation => Calculator.unaryOperation(operation.getText, visit(ctx.expression(0)))
        }
      case 2 => binaryOperation(ctx)
    }
  }

  private def binaryOperation(ctx: ExpParser.ExpressionContext): Int = {
    val left = visit(ctx.expression(0))
    val right = visit(ctx.expression(1))
    ctx.operation.getText match {
      case "+" => left + right
      case "-" => left - right
      case "*" => left * right
      case "/" => left / right
      case "%" => left % right
      case otherOperation => Calculator.logicalOperation(left, right, otherOperation)
    }
  }
}
object Calculator {
  private final val TRUE: Int = 1
  private final val FALSE: Int = 0

  private def unaryOperation(operation: String, operand: Int): Int = {
    operation match {
      case "-" => -operand
      case "!" => Calculator.toNumber(operand == Calculator.FALSE)
      case _ => throw new UnsupportedOperationException()
    }
  }

  private def logicalOperation(left: Int, right: Int, operation: String): Int = {
    Calculator.toNumber(
      operation match {
        case ">" => left > right
        case "<" => left < right
        case ">=" => left >= right
        case "<=" => left <= right
        case "==" => left == right
        case "!=" => left != right
        case "&&" => isTrue(left) && isTrue(right)
        case "||" => isTrue(left) || isTrue(right)
        case _ => throw new UnsupportedOperationException()
      }
    )
  }

  private def getLiteral(literal: String): Int = {
    literal match {
      case "true" => Calculator.TRUE
      case "false" => Calculator.FALSE
      case integer => integer.toInt
    }
  }

  private def toNumber(value: Boolean): Int = if (value) TRUE else FALSE

  private def isTrue(value: Int): Boolean = if (value != FALSE) true else false
}
