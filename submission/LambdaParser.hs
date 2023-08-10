{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder
import Data.Char





-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.


{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('

-- Parsers for characters
lambdaChar :: Parser Char
lambdaChar = is '/' ||| is 'λ'

period :: Parser Char
period = is '.'

variable :: Parser Char
variable = spaces *> oneof "abcdefghijklmnopqrstuvwxyz" <* spaces

openB :: Parser Char
openB = is '('

closedB :: Parser Char
closedB = is ')'

brackets :: Parser a -> Parser a
brackets = between (spaces >> openB >> spaces) (spaces >> closedB >> spaces)

-- This parser corresponds to the nonterminal <term> in the long form BNF grammer
termB1 :: Parser Builder
termB1 = (term <$> variable) ||| expressionB1 ||| brackets (list1 termB1 >>= \(x:xs) -> pure (foldl ap x xs ))

-- This parser corresponds to the nonterminal <expression> in the long form BNF grammer
expressionB1 :: Parser Builder
expressionB1 = do
    openB
    lambdaChar
    l <- variable
    period
    t1 <- list1 termB1
    let (x:xs) = t1
    let t = foldl ap x xs -- folding a list of terms with ap
    closedB
    pure (lam l t)

longLambdaP :: Parser Lambda
longLambdaP = build <$> expressionB1

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

-- Matches the <expression> in the short form bnf
expressionB2 :: Parser Builder
expressionB2 = do
    lambdaChar
    l1 <- list1 variable
    let (x: xs) = lam <$> l1
    let l2 = foldl (.) x xs -- Compose together to get nested lam's
    period
    t1 <- list1 termB2
    let (x2:xs2) = t1
    let t2 = foldl ap x2 xs2 -- ap terms together
    pure $ l2 t2
    ||| (list1 (brackets expressionB2) >>= \(x:xs) -> pure (foldl ap x xs )) -- handling things such as (xx)

-- Matches the <expression> in the long form bnf
termB2 :: Parser Builder
termB2 = (term <$> variable) ||| expressionB2 ||| brackets (list1 termB1 >>= \(x:xs) -> pure (foldl ap x xs ))

shortLambdaP :: Parser Lambda
shortLambdaP = build <$> expressionB2

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'
--

lambdaP :: Parser Lambda
lambdaP = shortLambdaP ||| longLambdaP

{-|
    Part 2
-}

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

-- Taken from tute
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
 where
  rest a =
    (do
        f <- op
        b <- p
        rest (f a b)
      )
      ||| pure a

-- Definitions for individual parsers and builders for their corresponding lambda expressions
spaces1 :: Parser String
spaces1 = list1 space

true :: Builder
true = boolToLam True
trueP :: Parser Builder
trueP = string "True" >> pure true

false :: Builder
false = boolToLam False
falseP :: Parser Builder
falseP = string "False" >> pure false


iff :: Builder
iff = lam 'b' $ lam 't' $ lam 'f' $ term 'b' `ap` term 't' `ap` term 'f'

andd :: Builder
andd = lam 'x' $ lam 'y' $ iff `ap` term 'x' `ap` term 'y' `ap` false

anddP :: Parser (Builder -> Builder -> Builder)
anddP = spaces >> string "and" >> spaces >> pure (ap.ap andd)

orr :: Builder
orr = lam 'x' $ lam 'y' $ iff `ap` term 'x' `ap` true `ap` term 'y'

orrP :: Parser (Builder -> Builder -> Builder)
orrP = spaces >> string "or" >> spaces >> pure (ap.ap orr)

nott :: Builder
nott = lam 'x' $ iff `ap` term 'x' `ap` false `ap` true
nottP :: Parser Builder
nottP = string "not" >> spaces >> boolTerm >>= \x -> pure $ nott `ap` x

-- Highest precedence
boolP :: Parser Builder
boolP = brackets boolTerm ||| trueP ||| falseP ||| nottP

-- Chaining together to handle precendence
-- boolP has highest presecende followed by anddP then orrP
boolTerm :: Parser Builder
boolTerm = chain (chain boolP anddP) orrP ||| iffP

-- if Parser using a do block
-- takes the three boolTerms and applys them together with the if
iffP :: Parser Builder
iffP = do
    string "if"
    spaces
    b <- boolTerm
    spaces
    string "then"
    spaces
    t <- boolTerm
    spaces
    string "else"
    spaces
    f <-boolTerm
    pure (iff `ap` b `ap` t `ap` f)


logicP :: Parser Lambda
logicP = build <$> boolTerm

-- logicP = (orrP ||| anddP ||| boolP) >>= pure.build

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ m
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- Definitions for builders and their parsers
-- Each Parser returns a (Builder -> Builder -> Builder) to use with chain
add :: Builder
add = lam 'x' $ lam 'y' $ term 'y' `ap` succc `ap` term 'x'
addP :: Parser (Builder -> Builder -> Builder)
addP = spaces >> is '+' >> spaces >> pure (ap.ap add)

minus :: Builder
minus = lam 'x' $ lam 'y' $ term 'y' `ap` predd `ap` term 'x'
minusP :: Parser (Builder -> Builder -> Builder)
minusP = spaces >> is '-' >> spaces >> pure (ap.ap minus)

multiply :: Builder
multiply = lam 'x' $ lam 'y' $ lam 'f' $ term 'x' `ap` (term 'y' `ap` term 'f')
multiplyP :: Parser (Builder -> Builder -> Builder)
multiplyP = spaces >> is '*' >> spaces >> pure (ap.ap multiply)

expo :: Builder
expo = lam 'x' $ lam 'y' $ term 'y' `ap` term 'x'
expoP :: Parser (Builder -> Builder -> Builder)
expoP = string "**" >> pure (ap.ap expo)

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

succc :: Builder
succc = lam 'n' $ lam 'f' $ lam 'x' $ term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x')

predd :: Builder
predd = lam 'n' $ lam 'f' $ lam 'x' $ term 'n' `ap` lam 'g' (lam 'h' $ term 'h' `ap` (term 'g' `ap` term 'f')) `ap` lam 'u' (term 'x') `ap` lam 'u' (term 'u')

-- Parser for a singular int
int :: Parser Int
int = read <$> list1 (satisfy isDigit)

-- Parser for a number
number :: Parser Builder
number = intToLam <$> int


-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13


basicArithmeticP :: Parser Lambda
basicArithmeticP = build <$> chain number (addP ||| minusP)

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68

-- Again chaining to handle precendence
lamTerm :: Parser Builder
lamTerm = chain (chain (number ||| brackets lamExpression) expoP ) multiplyP

lamExpression :: Parser Builder
lamExpression = chain lamTerm (addP ||| minusP)


arithmeticP :: Parser Lambda
arithmeticP = build <$> lamExpression


-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)


-- Definitions for builders and their parsers
leq :: Builder
leq = lam 'm' $ lam 'n' $ isZero `ap` (minus `ap` term 'm' `ap` term 'n')
leqP :: Parser (Builder -> Builder -> Builder)
leqP = spaces >> string "<=" >> spaces >> pure (ap.ap leq)

less :: Builder
less = lam 'x' $ lam 'y' $ andd `ap` (leq `ap` term 'x' `ap` term 'y') `ap` (neq `ap` term 'x' `ap` term 'y')
lessP :: Parser (Builder -> Builder -> Builder)
lessP = spaces >> is '<' >> spaces >> pure (ap.ap less)

geq :: Builder
geq = lam 'x' $ lam 'y' $ leq `ap` term 'y' `ap` term 'x'
geqP :: Parser (Builder -> Builder -> Builder)
geqP = spaces >> string ">=" >> spaces >> pure (ap.ap geq)

greater :: Builder
greater = lam 'x' $ lam 'y' $ less `ap` term 'y' `ap` term 'x'
greaterP :: Parser (Builder -> Builder -> Builder)
greaterP = spaces >> is '>' >> spaces >> pure (ap.ap greater)

eq :: Builder
eq = lam 'm' $ lam 'n' $ andd `ap` (leq `ap` term 'm' `ap` term 'n') `ap` (leq `ap` term 'n' `ap` term 'm')
eqP :: Parser (Builder -> Builder -> Builder)
eqP = spaces >> string "==" >> spaces >> pure (ap.ap eq)

neq :: Builder
neq = lam 'x' $ lam 'y' $ nott `ap` (eq `ap` term 'x' `ap` term 'y')
neqP :: Parser (Builder -> Builder -> Builder)
neqP = spaces >> string "!=" >> spaces >> pure (ap.ap neq)


-- Builder for Boolean equality eg. True == True returns True
-- Done by using if statement -> if (true and true) or (false and false) then true else false
beq :: Builder
beq = lam 'x' $ lam 'y' $ iff `ap` (orr `ap` (andd `ap` term 'x' `ap` term 'y') `ap` (nott `ap` (orr `ap` term 'x' `ap` term 'y')))`ap` true `ap` false
beqP :: Parser (Builder -> Builder -> Builder)
beqP = spaces >> string "==" >> spaces >> pure (ap.ap beq)


-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True

isZero :: Builder
isZero = lam 'n' $ term 'n' `ap` lam 'x' false `ap` true

-- |
-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False

-- Using chains once again for precendence
complexTerm :: Parser Builder
complexTerm = chain lamExpression (leqP ||| lessP ||| geqP ||| greaterP ||| eqP ||| neqP)

complexExpression :: Parser Builder
complexExpression = chain (chain (boolP|||complexTerm|||brackets complexTerm) (anddP|||beqP)) orrP ||| iffP

complexCalcP :: Parser Lambda
complexCalcP = build <$> complexExpression


{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- Builders for the church encoding of list constructs
nullC :: Builder
nullC = lam 'c' $ lam 'n' $ term 'n'

isNull :: Builder
isNull = lam 'l' $ term 'l' `ap` lam 'h' (lam 't' false) `ap` true

consC :: Builder
consC = lam 'h' $ lam 't' $ lam 'c' $ lam 'n' $ term 'c' `ap` term 'h' `ap` (term 't' `ap` term 'c' `ap` term 'n')

headC :: Builder
headC = lam 'l' $ term 'l' `ap` lam 'h' (lam 't' $ term 'h') `ap` false

tailC :: Builder
tailC = lam 'l' $ lam 'c' $ lam 'n' $ term 'l' `ap` lam 'h' (lam 't' $ lam 'g' $ term 'g' `ap` term 'h' `ap` (term 't' `ap` term 'c')) `ap` lam 't' (term 'n') `ap` lam 'h' (lam 't' $ term 't')
-- |
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\t_.t)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof

-- Parse a [ and then an item
-- note complexExpression parses any arithmetic or boolean expression and items can be a also be a list
openArray :: Parser Builder
openArray = is '[' >> (complexExpression ||| itemsP)

listSep :: Parser Builder
listSep = is ',' >> spaces >> (complexExpression ||| itemsP)

-- Parse the items in a list a build a cons list
-- Done by using list listSep to a get a list of Builders and then folding from the right with first element being null and a function the applys the cons
itemsP :: Parser Builder
itemsP =  (do
    x <- openArray
    xs <- list listSep
    is ']'
    pure (foldr (ap.ap consC) nullC (x:xs))) ||| (string "[]" >> pure nullC)

listP :: Parser Lambda
listP = build <$> itemsP

-- |
-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False


-- Parsers for the list operations
-- can take either another list operator or a list as an argument
-- applys from the right as listOpB recurses until it finds a list (itemsP)
isNullP :: Parser Builder
isNullP = string "isNull" >> spaces1 >> (itemsP ||| listOpB) >>= \a -> pure (isNull `ap` a)

headP :: Parser Builder
headP = string "head" >> spaces1 >> (itemsP ||| listOpB) >>= \a -> pure (headC `ap` a)

restP :: Parser Builder
restP = string "rest" >> spaces1 >> (itemsP ||| listOpB) >>= \a -> pure (tailC `ap` a)

listOpB :: Parser Builder
listOpB = isNullP ||| headP ||| restP

listOpP :: Parser Lambda
listOpP = build <$> listOpB


-- | Exercise 2

-- | Implement your function(s) of choice below!

-- Z=λf.(λx.f(λv.xxv))(λx.f(λv.xxv))

-- Builder for Z-Combinator
z :: Builder
z = lam 'f' $ lam 'x' (term 'f' `ap` lam 'v' (term 'x' `ap` term 'x' `ap` term 'v'))`ap`lam 'x' (term 'f' `ap` lam 'v' (term 'x' `ap` term 'x' `ap` term 'v'))

-- Recursive fibonacci
lamFib :: Builder
lamFib = lam 'f' $ lam 'n' $ iff `ap` (eq `ap` term 'n' `ap` intToLam 0) `ap` intToLam 0 `ap` (iff `ap` (eq `ap` term 'n' `ap` intToLam 1) `ap` intToLam 1 `ap` (add `ap` (term 'f' `ap` (predd `ap` term 'n' )) `ap` (term 'f' `ap` (minus `ap` term 'n' `ap` intToLam 2))))

fibP :: Parser Lambda
fibP = string "Fib" >> brackets lamExpression >>= \x -> pure (build (z `ap` lamFib `ap` x))

-- Recursive factorial
lamFactorial :: Builder
lamFactorial = lam 'f' $ lam 'n' $ iff `ap` (greater `ap` term 'n' `ap` intToLam 1) `ap` (multiply `ap` term 'n' `ap` (term 'f' `ap` (predd `ap` term 'n'))) `ap` intToLam 1

factorialP :: Parser Lambda
factorialP = do
    x <- lamExpression
    is '!'
    pure $ build $ z `ap` lamFactorial `ap` x

-- >>> lamToInt <$> parse factorialP "(5+1)!"
-- Result >< Just 720

-- >>> lamToInt <$> parse fibP "Fib(5+1)"
-- Result >< Just 8