{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}

module Network.AWS.DynamoDB.Schema.Expression
    (
    -- * Expressions
      KeyExpression
    , partition
    , sort

    , Expression

    -- * Conditions
    , Condition
    , Hash
    , Range

    -- ** Relational Equality
    , equals
    , notEquals
    , less
    , lessOrEqual
    , greater
    , greaterOrEqual

    -- ** Path Functions
    , exists
    , notExists
    , isType
    , size
    , contains
    , beginsWith

    -- ** Ranges and Membership
    , between
    , in_

    -- ** Logical Combinators
    , and
    , or
    , not
    , parens

    -- * Operands
    , Path         (..)
    , Operand      (..)

    -- * Evaluation
    , IsExpression
    , eval
    ) where

import Data.Foldable          (toList)
import Data.List              (intersperse)
import Data.List.NonEmpty     (NonEmpty (..))
import Data.Monoid            (Monoid (..))
import Data.Semigroup         (Semigroup (..))
import Data.String
import Data.Text              (Text)
import Data.Text.Lazy.Builder (Builder)

import Network.AWS.Data.Text
import Network.AWS.DynamoDB.Value (DynamoType)

import qualified Data.Text.Lazy.Builder as Build

import Prelude hiding (and, not, or)

newtype Path = Path Text
    deriving (Eq, Ord, Show, IsString, ToText)

-- A top-level attribute name, such as Id, Title, Description or ProductCategory
-- A document path that references a nested attribute
data Operand
    = AttributeName Text
    | DocumentPath  Path
      deriving (Eq, Ord, Show)

-- | Instantiates to a safe document 'Path'.
instance IsString Operand where
    fromString = DocumentPath . fromString

instance ToText Operand where
    toText = \case
        AttributeName t -> t
        DocumentPath  p -> toText p

-- | Denotes a valid condition for hash types such as partition keys.
data Hash

-- | Denotes a valid condition for range types such as sort keys.
data Range

data Relation a where
    Equal          :: Relation Hash
    NotEqual       :: Relation Operand
    Less           :: Relation Range
    LessOrEqual    :: Relation Range
    Greater        :: Relation Range
    GreaterOrEqual :: Relation Range

data Function a where
    Exists     :: Path               -> Function Operand
    NotExists  :: Path               -> Function Operand
    IsType     :: Path -> DynamoType -> Function Operand
    Contains   :: Path -> Operand    -> Function Operand
    Size       :: Path               -> Function Operand
    BeginsWith :: Path -> Text       -> Function Range

-- | A conditional sub-expression that can be used in both
-- 'Expression's and 'KeyExpression's.
data Condition a where
    Compare  :: Relation a -> Operand -> Operand -> Condition a
    Function :: Function a                       -> Condition a
    Between  :: Operand -> (Operand, Operand)    -> Condition Range

-- | A logical expression consisting of sub-expressions and conditions
-- that can be used as part of a filter expression for 'Query' and 'Scan'
-- operations.
data Expression where
    CondE  :: Condition a                    -> Expression
    InE    :: Operand    -> NonEmpty Operand -> Expression
    AndE   :: Expression -> Expression       -> Expression
    OrE    :: Expression -> Expression       -> Expression
    NotE   :: Expression                     -> Expression
    ParenE :: Expression                     -> Expression
    EmptyE ::                                   Expression

-- | The associative operation corresponds to conjunction using 'and'.
instance Semigroup Expression

-- | The identity of an expression will 'eval'uate to 'Nothing',
instance Monoid Expression where
    mempty  = EmptyE
    mappend = AndE

class IsExpression a where
    -- | Lift a condition or sub-expression to a top-level expression.
    liftE :: a -> Expression

instance IsExpression Expression    where liftE = id
instance IsExpression (Condition a) where liftE = CondE

-- | An expression which can be used as a condition expression
-- for 'Query' requests to provide a specific value for the partition key.
--
-- You can optionally narrow the scope of the 'Query' by specifying a sort
-- key condition.
--
-- /See:/ 'partition', 'sort'.
data KeyExpression a where
   Partition :: Condition Hash                    -> KeyExpression Hash
   Sort      :: Condition Hash -> Condition Range -> KeyExpression Range

instance IsExpression (KeyExpression a) where
    liftE = \case
        Partition h   -> liftE h
        Sort      h r -> liftE h <> liftE r

-- | Specify the exact partition key.
--
-- >>> partition (equals "my-key-name" "bar")
-- "my-key-name" = :sub
--
partition :: Condition Hash -> KeyExpression Hash
partition = Partition

-- | You can narrow the scope of a 'KeyExpression' by specifying a sort key
-- condition as follows:
--
-- >>> partition (equals "partition-key" "foo") `sort` lessThan "sort-key" "123"
-- "partition-key" = :sub1 AND "sort-key" < :sub2
--
sort :: KeyExpression Hash -> Condition Range -> KeyExpression Range
sort (Partition h) = Sort h

equals :: Operand -> Operand -> Condition Hash
equals = Compare Equal

notEquals :: Operand -> Operand -> Condition Operand
notEquals = Compare NotEqual

less :: Operand -> Operand -> Condition Range
less = Compare Less

lessOrEqual :: Operand -> Operand -> Condition Range
lessOrEqual = Compare LessOrEqual

greater :: Operand -> Operand -> Condition Range
greater = Compare Greater

greaterOrEqual :: Operand -> Operand -> Condition Range
greaterOrEqual = Compare GreaterOrEqual

-- | Test the if an attribute is within the specified range.
--
-- For example:
--
-- >>> between a (b, c)
-- a BETWEEN b AND c
--
-- Which results in true if @a@ is greater than or equal to @b@, and less than
-- or equal to @c@.
--
between :: Operand -> (Operand, Operand) -> Condition Range
between = Between

-- | Test the existence of an attribute.
--
-- Evaluates to true if the item contains the attribute specified by 'Path'.
-- For example, to check whether an item in the table has
-- a side view picture:
--
-- >>> exists "Pictures.SideView"
-- attribute_exists ("Pictures.SideView")
--
exists :: Path -> Condition Operand
exists p = Function (Exists p)

-- | Test the non-existence of an attribute.
--
-- Evaluates to true if the attribute specified by 'Path'
-- does not exist in the item.
-- For example, to check whether an item has a @Manufacturer@ attribute:
--
-- >>> notExists "Manufacturer"
-- attribute_not_exists ("Manufacturer")
--
notExists :: Path -> Condition Operand
notExists p = Function (NotExists p)

-- | Test if the attribute is of the specified 'DynamoType'.
--
-- Evaluates to true if the attribute at the specified path is of a particular
-- data type. For example, to check whether the @FiveStar@ attribute is
-- of type @L@ (list):
--
-- >>> isType "ProductReviews.FiveStar" L
-- attribute_type ("ProductReviews.FiveStar", :sub)
--
isType :: Path -> DynamoType -> Condition Operand
isType p t = Function (IsType p t)

-- | Return a number representing an attribute's size.
--
-- The following are valid data types for use with size:
--
-- * If the attribute is of type 'S' (string), size returns the length of the string.
--
-- * If the attribute is of type 'B' (binary), size returns the number of bytes in the attribute value.
--
-- * If the attribute is a Set data type, size returns the number of elements in the set.
--
-- * If the attribute is of type 'L' (list) or 'M' (map), size returns the number of child elements.
--
size :: Path -> Condition Operand
size p = Function (Size p)

-- | Test if the attribute contains a particular substring or set element.
--
-- Evalutes to true if the attribute specified by path is:
--
-- * A string that contains a particular substring.
--
-- * A set that contains a particular element within the set.
--
-- The path and the operand must be distinct; that is, @contains (a, a)@
-- will result in an error.
--
-- For example, to check whether the Brand string attribute contains
-- the substring Company:
--
-- >>> contains ("Brand", "Company")
-- contains ("Brand", :sub)
--
contains :: Path -> Operand -> Condition Operand
contains p o = Function (Contains p o)

-- | Test if the attribute begins with a particular substring.
--
-- For example, to check whether the first few characters of the front view
-- picture attribute is URL:
--
-- >>> beginsWith ("Pictures.FrontView", "http://")
-- begins_with ("Pictures.FrontView", :sub)
--
beginsWith :: Path -> Text -> Condition Range
beginsWith p x = Function (BeginsWith p x)

-- | Test that operand is a member of the specified set, @x ∈ xs@.
--
-- Evalutes to true if the operand is equal to any value in the set. For example:
--
-- >>> in_ "a" (pure "b" <> pure "a")
-- "a" IN ("b", "a")
--
-- Will result in true.
--
in_ :: Operand -> NonEmpty Operand -> Expression
in_ = InE

-- | Logical conjunction, where the resulting expression is true if both
-- sub-expressions are true.
--
-- >>> equals "a" "b" `and` greater "c" "d"
-- "a" = "b" AND "c" > "d"
--
-- /See:/ '<>', 'mappend'.
and :: (IsExpression a, IsExpression b) => a -> b -> Expression
and a b = AndE (liftE a) (liftE b)

-- | Logical disjunction, where the resulting expression is true if either
-- sub-expression is true.
--
-- >>> equals "a" "b" `or` equals "c" "d"
-- "a" = "b" OR "c" = "d"
--
or :: (IsExpression a, IsExpression b) => a -> b -> Expression
or a b = OrE (liftE a) (liftE b)

-- | Logical negation, where the resulting expression is true if
-- the sub-expression is false, and false if the sub-expression is true.
--
-- >>> not (equals "a" "b")
-- NOT "a" = "b"
--
not :: IsExpression a => a -> Expression
not = NotE . liftE

-- | Explicitly enclose an expression in parentheses.
--
-- /Note:/ Typically you won't need to use this, and should rely on fixity
-- delcarations corresponding to the following order of expressions:
--
-- @
-- equals notEquals less lessOrEqual greater greaterOrEqual
-- in_
-- between
-- exists notExists isType contains size beginsWith
-- parens
-- not
-- and
-- or
-- @
--
-- To demonstrate the fixities outlined above without the use of 'parens',
-- suppose that conditions a and b are true, and that condition c is false. The
-- following expression will evaluate to true:
--
-- >>> a `or` b `and` c
-- a OR b AND c
--
-- However, if you enclose a condition in parentheses, it will be evaluated first.
-- For example, the following evaluates to false:
--
-- >>> (a `or` b) `and` c
-- (a OR b) AND c
--
parens :: IsExpression a => a -> Expression
parens = ParenE . liftE

-- Precedence

infixl 9 `equals`, `notEquals`, `less`, `lessOrEqual`, `greater`, `greaterOrEqual`
infixl 8 `in_`
infixl 7 `between`
infixl 6 `exists`, `notExists`, `isType`, `contains`, `size`, `beginsWith`
infixl 5 `parens`
infixl 4 `not`
infixl 3 `and`
infixl 2 `or`

eval :: IsExpression a => a -> Maybe Builder
eval = expression . liftE

{-
@
expression ::=
      condition
    | operand IN ( operand (',' operand (, ...) ))
    | expression AND expression
    | expression OR expression
    | NOT expression
    | ( expression )
    | ''
@
-}
expression :: Expression -> Maybe Builder
expression = \case
    CondE  c    -> Just (condition c)
    InE    x xs -> Just (build x >>> "IN" >>> tupled (fmap build xs))

    AndE EmptyE b -> expression b
    AndE a EmptyE -> expression a
    AndE a b      -> do
        x <- expression a
        y <- expression b
        pure $! paren (x >>> "AND" >>> y)

    OrE a b -> do
        x <- expression a
        y <- expression b
        pure $! paren (x >>> "OR" >>> y)

    NotE   a -> ("NOT" >>>) <$> expression a
    ParenE a -> paren <$> expression a

    EmptyE   -> Nothing

{-
@
condition ::=
      operand comparator operand
    | operand BETWEEN operand AND operand
    | function
@
-}
condition :: Condition a -> Builder
condition = \case
    Compare  r a b    -> build a >>> comparator r >>> build b
    Function f        -> function f
    Between  a (b, c) -> build a >>> "BETWEEN" >>> build b >>> "AND" >>> build c

{-
@
function ::=
      attribute_exists (path)
    | attribute_not_exists (path)
    | attribute_type (path, type)
    | begins_with (path, substr)
    | contains (path, operand)
    | size (path)
@
-}
function :: Function a -> Builder
function = \case
    Exists     p   -> "attribute_exists"     >>> paren (build p)
    NotExists  p   -> "attribute_not_exists" >>> paren (build p)
    Size       p   -> "size"                 >>> paren (build p)
    IsType     p t -> "attribute_type"       >>> tupled [build p, build t]
    Contains   p o -> "contains"             >>> tupled [build p, build o]
    BeginsWith p x -> "begins_with"          >>> tupled [build p, build x]

{-
@
comparator ::=
      =
    | <>
    | <
    | <=
    | >
    | >=
@
-}
comparator :: Relation a -> Builder
comparator = \case
    Equal          -> "="
    NotEqual       -> "<>"
    Less           -> "<"
    LessOrEqual    -> "<="
    Greater        -> ">"
    GreaterOrEqual -> ">="

(>>>) :: Builder -> Builder -> Builder
(>>>) a b = a <> " " <> b
{-# INLINE (>>>) #-}

paren :: Builder -> Builder
paren a = "(" <> a <> ")"
{-# INLINE paren #-}

tupled :: Foldable f => f Builder -> Builder
tupled = paren . mconcat . intersperse ", " . toList
{-# INLINE tupled #-}

build :: ToText a => a -> Builder
build = Build.fromText . toText
{-# INLINE build #-}
