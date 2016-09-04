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
relation ::=
      operand =  operand
    | operand <> operand
    | operand <  operand
    | operand <= operand
    | operand >  operand
    | operand >= operand

function ::=
      attribute_exists     (path)
    | attribute_not_exists (path)
    | attribute_type       (path, type)
    | begins_with          (path, substr)
    | contains             (path, operand)
    | size                 (path)

condition ::=
      relation
    | function
    | operand BETWEEN operand AND operand
@
-}
condition :: Placeholders m => Condition a -> MaybeT m Builder
condition = \case
    Equal          a b -> concat3 <$> operand (liftO a) <*> pure "="  <*> operand (liftO b)
    NotEqual       a b -> concat3 <$> operand (liftO a) <*> pure "<>" <*> operand (liftO b)
    Less           a b -> concat3 <$> operand (liftO a) <*> pure "<"  <*> operand (liftO b)
    LessOrEqual    a b -> concat3 <$> operand (liftO a) <*> pure "<=" <*> operand (liftO b)
    Greater        a b -> concat3 <$> operand (liftO a) <*> pure ">"  <*> operand (liftO b)
    GreaterOrEqual a b -> concat3 <$> operand (liftO a) <*> pure ">=" <*> operand (liftO b)

    Exists     p   -> concat2 "attribute_exists"     <$> fmap parens (path p)
    NotExists  p   -> concat2 "attribute_not_exists" <$> fmap parens (path p)
    Size       p   -> concat2 "size"                 <$> fmap parens (path p)
    Contains   p o -> concat2 "contains"             <$> tuple [path p, operand o]
    IsType     p t -> concat2 "attribute_type"       <$> tuple [path p, substituteValue t]
    BeginsWith p x -> concat2 "begins_with"          <$> tuple [path p, substituteValue x]

    Between a (b, c) ->
        concat5 <$> operand a <*> pure "BETWEEN" <*> operand b <*> pure "AND" <*> operand c
    In      x xs     ->
        concat3 <$> operand x <*> pure "IN" <*> fmap tupled (traverse operand xs)
  where
    tuple :: Placeholders f => [f Builder] -> f Builder
    tuple = fmap tupled . traverse id

path :: Placeholders m => Path -> MaybeT m Builder
path = \case
    Name   t   -> substituteName t
    Nested a b -> concat3 <$> path a <*> pure "." <*> path b
    Index  p i -> flip mappend ("[" <> build i <> "]") <$> path p

operand :: Placeholders m => Operand -> MaybeT m Builder
operand = \case
    Path  p -> path p
    Value v -> substituteValue v

concat2 :: Builder -> Builder -> Builder
concat2 = (>>>)
{-# INLINE concat2 #-}

concat3 :: Builder -> Builder -> Builder -> Builder
concat3 a b c = a >>> b >>> c
{-# INLINE concat3 #-}

concat5 :: Builder -> Builder -> Builder -> Builder -> Builder -> Builder
concat5 a b c d e = a >>> b >>> c >>> d >>> e
{-# INLINE concat5 #-}

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
