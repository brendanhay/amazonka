{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}

module Network.AWS.DynamoDB.Schema.Expression
    (
    -- * Expressions
      Expression
    , IsExpression (..)

    -- ** Evaluation
    , eval

    -- ** Key Expressions
    , KeyExpression

    , partition
    , partitionAndSort

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
    , between

    -- ** Functions
    , exists
    , notExists
    , isType
    , size
    , contains
    , beginsWith

    -- ** Combinators
    , in_
    , and
    , or
    , not
    , parens

    -- * Operands
    , Path         (..)
    , Operand      (..)
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

-- | Represents a conditional sub-expression.
data Condition a where
    Compare  :: Relation a -> Operand -> Operand -> Condition a
    Function :: Function a                       -> Condition a
    Between  :: Operand -> (Operand, Operand)    -> Condition Range

-- | Represents an expression that can be 'eval'uated
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

-- | An opaque expression which can be used as a condition expression
-- for 'Query' requests to provide a specific value for the partition key.
--
-- You can optionally narrow the scope of the 'Query' by specifying a sort
-- key condition.
--
-- /See:/ 'partition', 'partitionAndSort'.
newtype KeyExpression = KeyExpression Expression

instance IsExpression KeyExpression where
    liftE (KeyExpression e) = e

partition :: Condition Hash -> KeyExpression
partition = KeyExpression . liftE

partitionAndSort :: Condition Hash -> Condition Range -> KeyExpression
partitionAndSort a b = KeyExpression (liftE a <> liftE b)

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

between :: Operand -> (Operand, Operand) -> Condition Range
between = Between

exists :: Path -> Condition Operand
exists p = Function (Exists p)

notExists :: Path -> Condition Operand
notExists p = Function (NotExists p)

isType :: Path -> DynamoType -> Condition Operand
isType p t = Function (IsType p t)

size :: Path -> Condition Operand
size p = Function (Size p)

contains :: Path -> Operand -> Condition Operand
contains p o = Function (Contains p o)

beginsWith :: Path -> Text -> Condition Range
beginsWith p x = Function (BeginsWith p x)

-- | Test that the operand is an element of a set, @x ∈ xs@.
in_ :: Operand -> NonEmpty Operand -> Expression
in_ = InE

-- | Conjunction.
--
-- /See:/ '<>', 'mappend'.
and :: (IsExpression a, IsExpression b) => a -> b -> Expression
and a b = AndE (liftE a) (liftE b)

-- | Disjunction.
or :: (IsExpression a, IsExpression b) => a -> b -> Expression
or a b = OrE (liftE a) (liftE b)

-- | Negation.
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
