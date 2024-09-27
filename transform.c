#include "transform.h"

#include <assert.h>
#include <float.h>
#include <math.h>
#include "parser.c"
#include "lexer.c"
#include "string.c"
#include "list.c"


// @NOTE: Put transformer functions prototypes here
static bool power_to_multiply(Expression* const expression);

// Make deep copy of a given expression.
static Expression* expression_copy(const Expression* const expression);

// Compare two expressions, lhs and rhs and return true if two
// expressions are identical, otherwise - false.
static bool expression_equal(const Expression* const lhs,
                             const Expression* const rhs);

void simplify_expression(Expression* const expression)
{
	assert(expression != NULL);

	// @NOTE: Put simplification transformer functions here
}

void expand_expression(Expression* const expression)
{
	assert(expression != NULL);

	// @NOTE: Put expander transformer functions here
    power_to_multiply(expression);
}


double evaluate_expression(const Expression* const expression) {
	assert(expression != NULL);

	switch (expression->type) {

	case ExpressionType_Literal:
    {
        Literal* literal = (Literal *) expression;

        if (literal->tag == LiteralTag_Number)
            return literal->number;
    }

	case ExpressionType_Unary:
    {
        UnaryExpression* const unary = (UnaryExpression *) expression;

        if (unary->operator == TokenType_Minus)
            return -evaluate_expression(unary->subexpression);

        return evaluate_expression(unary->subexpression);
    }

	case ExpressionType_Binary:
    {
        BinaryExpression *const binary = (BinaryExpression *) expression;

        switch (binary->operator) {
            case TokenType_Plus:
                return evaluate_expression(binary->left) +
                       evaluate_expression(binary->right);

            case TokenType_Minus:
                return evaluate_expression(binary->left) -
                       evaluate_expression(binary->right);

            case TokenType_Multiply:
                return evaluate_expression(binary->left) *
                       evaluate_expression(binary->right);

            case TokenType_Divide:
                return evaluate_expression(binary->left) /
                       evaluate_expression(binary->right);

            case TokenType_Exponent:
                return pow(evaluate_expression(binary->left),
                           evaluate_expression(binary->right));
        }
    }

	}

	// @NOTE: ExpressionType_Empty and LiteralTag_Symbol case
	return 0;
}


static bool power_to_multiply(Expression* const expression) {
    assert(expression != NULL);

    switch (expression->type) {

    // Look for a pow in subexpression
    case ExpressionType_Unary:
    {
        UnaryExpression* unary = (UnaryExpression *) expression;
        return power_to_multiply(unary->subexpression);
    }

    case ExpressionType_Binary:
    {
        BinaryExpression *const binary = (BinaryExpression *) expression;
        // Look for pow in the sides of binary expression first
        const bool other_result = power_to_multiply(binary->left) || power_to_multiply(binary->right);

        // Find pow operation
        if (binary->operator == TokenType_Exponent && binary->left->type == ExpressionType_Literal && binary->right->type == ExpressionType_Binary) {

            binary->right->parenthesised = true;
            BinaryExpression *rse = (BinaryExpression*) binary->right;
            bool pow_is_a_sum = false;

            if (rse->operator == TokenType_Plus) {
                pow_is_a_sum = true;

                // Create some ops
                Expression* a = binary->left;
                Expression* b = rse->left;
                Expression* c = rse->right;
                b->parenthesised = c->parenthesised = true;

                BinaryExpression* pow1 = expression_binary_create(
                        TokenType_Exponent,
                        expression_copy(a),
                        expression_copy(b));

                BinaryExpression* pow2 = expression_binary_create(
                        TokenType_Exponent,
                        expression_copy(a),
                        expression_copy(c));

                // Destroy some useless ops
                expression_destroy(&binary->left);
                expression_destroy(&binary->right);

                // Create a multiplication with that pow subexpressions
                binary->operator = TokenType_Multiply;
                binary->left = (Expression*) pow1;
                binary->right = (Expression*) pow2;
                return pow_is_a_sum;
            }
            return other_result;
        }
    }

    }
    return false;
}

//
// Helper functions
//

static Expression* expression_copy(const Expression* const expression)
{
	assert(expression != NULL);

	Expression* result = NULL;

	switch (expression->type) {
	case ExpressionType_Literal: {
		Literal* const literal = (Literal*)expression;

		switch (literal->tag) {
		case LiteralTag_Number:
			result = (Expression*)expression_literal_create_number(literal->number);
			break;

		case LiteralTag_Symbol:
			result = (Expression*)expression_literal_create_symbol(&literal->symbol);
			break;
		}
	} break;

	case ExpressionType_Unary: {
		UnaryExpression* const unary = (UnaryExpression*)expression;
		result = (Expression*)expression_unary_create(
			unary->operator, expression_copy(unary->subexpression));
	} break;

	case ExpressionType_Binary: {
		BinaryExpression* const binary = (BinaryExpression*)expression;
		result = (Expression*)expression_binary_create(
			binary->operator,
			expression_copy(binary->left),
			expression_copy(binary->right));
	} break;
	}

	result->parenthesised = expression->parenthesised;

	return result;
}

static bool expression_equal(const Expression* const lhs,
                             const Expression* const rhs)
{
	assert(lhs != NULL);
	assert(rhs != NULL);

	if (lhs->type != rhs->type)
		return false;

	switch (lhs->type) {
	case ExpressionType_Literal: {
		Literal* const lhs_literal = (Literal*)lhs;
		Literal* const rhs_literal = (Literal*)rhs;

		if (lhs_literal->tag != rhs_literal->tag)
			return false;

		switch (lhs_literal->tag) {
		case LiteralTag_Number:
			if (fabs(lhs_literal->number - rhs_literal->number) < DBL_EPSILON)
				return true;
			break;

		case LiteralTag_Symbol:
			if (string_equal(&lhs_literal->symbol, &rhs_literal->symbol))
				return true;
			break;
		}
	} break;

	case ExpressionType_Unary: {
		UnaryExpression* const lhs_unary = (UnaryExpression*)lhs;
		UnaryExpression* const rhs_unary = (UnaryExpression*)rhs;

		if (lhs_unary->operator != rhs_unary->operator)
			return false;

		return expression_equal(lhs_unary->subexpression, rhs_unary->subexpression);
	} break;

	case ExpressionType_Binary: {
		BinaryExpression* const lhs_binary = (BinaryExpression*)lhs;
		BinaryExpression* const rhs_binary = (BinaryExpression*)rhs;

		if (lhs_binary->operator != rhs_binary->operator)
			return false;

		return expression_equal(lhs_binary->left, rhs_binary->left) &&
		       expression_equal(lhs_binary->right, rhs_binary->right);
	} break;
	}

	return false;
}


#if 0
int main() {

    String a = string_init("a");
    String b = string_init("b");
    String c = string_init("c");
    String d = string_init("d");

    BinaryExpression* sumcd = expression_binary_create(
            TokenType_Plus,
            (Expression*) expression_literal_create_symbol(&c),
            (Expression*) expression_literal_create_symbol(&d)
    );

    BinaryExpression* powbcd = expression_binary_create(
            TokenType_Exponent,
              (Expression*) expression_literal_create_symbol(&b),
              (Expression*) sumcd
    );

    BinaryExpression* pow = expression_binary_create(
            TokenType_Exponent,
            (Expression*) expression_literal_create_symbol(&a),
            (Expression*) powbcd
    );

    power_to_multiply((Expression*) pow);
    expression_print((Expression*) pow);
    return 1;
}
#endif