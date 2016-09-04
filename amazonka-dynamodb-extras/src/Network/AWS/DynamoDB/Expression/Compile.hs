{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Network.AWS.DynamoDB.Expression.Compile
    (
    -- * Compiling Expressions
      compile
    , evaluate

    -- ** Components
    , run

    , expression
    , condition
    , function
    , comparator
    , path
    , operand
    ) where

import Control.Monad.Trans.Maybe (MaybeT (..))

import Data.Foldable          (toList)
import Data.List              (intersperse)
import Data.Monoid
import Data.Text.Lazy.Builder (Builder)

import Network.AWS.Data.Text (ToText (..))

import Network.AWS.DynamoDB.Expression.Internal
import Network.AWS.DynamoDB.Expression.Placeholder

import qualified Data.Text.Lazy.Builder as Build

-- | Compilation of an expression can result in either an empty expression,
-- or a rendered DynamoDB compatible textual representation.
compile :: IsExpression a => a -> Maybe (Builder, NamesAndValues)
compile = run (mempty, mempty) . expression . liftE
{-# INLINE compile #-}

-- | Evaluation doesn't perform any attribute name placeholder subsitutition.
evaluate :: IsExpression a => a -> Maybe (Builder, Values)
evaluate = run mempty . expression . liftE
{-# INLINE evaluate #-}

run :: s -> MaybeT (Substitute s) a -> Maybe (a, s)
run s = go . substituteAll s . runMaybeT
  where
    go (mb, r) = (,r) <$> mb
{-# INLINE run #-}

{-|
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
expression :: Placeholders m => Expression -> MaybeT m Builder
expression = \case
    CondE  c -> condition c
    NotE   a -> ("NOT" >>>) <$> expression a
    ParenE a -> parens <$> expression a
    EmptyE   -> MaybeT $ pure Nothing

    AndE EmptyE b      -> expression b
    AndE a      EmptyE -> expression a
    AndE a      b      -> do
        x <- expression a
        y <- expression b
        pure $! parens (x >>> "AND" >>> y)

    OrE a b -> do
        x <- expression a
        y <- expression b
        pure $! parens (x >>> "OR" >>> y)

{-|
@
condition ::=
      operand comparator operand
    | operand BETWEEN operand AND operand
    | function
@
-}
condition :: Placeholders m => Condition a -> MaybeT m Builder
condition = \case
    Function f -> function f

    Compare r a b -> do
        x <- operand a
        y <- operand b
        pure $! x >>> comparator r >>> y

    Between a (b, c) -> do
        x <- operand a
        y <- operand b
        z <- operand c
        pure$! x >>> "BETWEEN" >>> y >>> "AND" >>> z

    In x xs -> do
        y  <- operand x
        ys <- traverse operand xs
        pure $! y >>> "IN" >>> tupled ys

{-|
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
function :: Placeholders m => Function a -> MaybeT m Builder
function = \case
    Exists     p   -> prefix "attribute_exists"     p Nothing
    NotExists  p   -> prefix "attribute_not_exists" p Nothing
    Size       p   -> prefix "size"                 p Nothing
    Contains   p o -> prefix "contains"             p . Just =<< operand o
    IsType     p x -> do
        y <- substituteValue x
        prefix "attribute_type" p (Just y)
    BeginsWith p x -> do
        y <- substituteValue x
        prefix "begins_with" p (Just y)
  where
    prefix n p vs = do
        v <- path p
        pure $! n >>> maybe (parens v) (\x -> tupled [v, x]) vs

{-|
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

path :: Placeholders m => Path -> MaybeT m Builder
path = \case
    Name   t   -> substituteName t
    Nested a b -> (\x y -> x <> "." <> y) <$> path a <*> path b
    Index  p i -> flip mappend ("[" <> build i <> "]") <$> path p

operand :: Placeholders m => Operand -> MaybeT m Builder
operand = \case
    Path  p -> path p
    Value v -> substituteValue v

(>>>) :: Builder -> Builder -> Builder
(>>>) a b = a <> " " <> b
{-# INLINE (>>>) #-}

parens :: Builder -> Builder
parens a = "(" <> a <> ")"
{-# INLINE parens #-}

tupled :: Foldable f => f Builder -> Builder
tupled = parens . mconcat . intersperse ", " . toList
{-# INLINE tupled #-}

build :: ToText a => a -> Builder
build = Build.fromText . toText
{-# INLINE build #-}
