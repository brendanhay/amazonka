{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- |
-- Module      : Amazonka.DynamoDB.Expression.Compile
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Amazonka.DynamoDB.Expression.Compile
    (
    -- * Compilation
      compile
    , bicompile

    -- * Grammar
    , conditionExpression
    , condition

    , updateExpression
    , update

    , projectionExpression

    , operand
    , path
    , attribute
    ) where

import Amazonka.DynamoDB.Expression.Internal
import Amazonka.DynamoDB.Expression.Placeholder (bisubstitute, substitute)

import Control.Applicative              ((<**>))
import Control.Monad.Trans.State.Strict (runState)

import Data.Bitraversable     (Bitraversable (..))
import Data.Foldable          (fold, foldl', toList)
import Data.Hashable          (Hashable)
import Data.HashMap.Strict    (HashMap)
import Data.List              (intersperse)
import Data.Maybe             (catMaybes)
import Data.Semigroup         ((<>))
import Data.Sequence          (Seq, ViewL (..), (<|))
import Data.Text.Lazy.Builder (Builder)

import Network.AWS.Data.Text (ToText (..))

import qualified Data.Sequence          as Seq
import qualified Data.Text.Lazy.Builder as Build

-- | Given an expression evaluator, compile the expression and return
-- the result along with any substituted placeholders for the functor's
-- argument.
--
-- Typical usage specializes as follows:
--
-- @
-- compile projectionExpression
--     :: ProjectionExpression Name -> (Maybe Builder, HashMap Name Builder)
-- @
--
-- Or to compile a bifunctor and render the first argument (such as a 'Name') verbatim:
--
-- @
-- compile conditionExpression . first name
--     :: ConditionExpression Name Value -> (Maybe Builder, HashMap Value Builder)
-- @
--
compile :: (Traversable t, Eq b, Hashable b)
        => (t Builder -> a) -- ^ Reduce a functor to the returned result.
        -> t b              -- ^ A functor perform substitution on.
        -> (a, HashMap b Builder)
compile f = flip runState mempty . fmap f . substitute
{-# INLINE compile #-}

-- | Given an expression evaluator, compile the expression and return
-- the result along with the substituted placeholders for the bifunctor's
-- first and second arguments.
--
-- Typical usage specializes as follows:
--
-- @
-- bicompile conditionExpression
--     :: UpdateExpression Name Value -> (Maybe Builder, (HashMap Name Builder, HashMap Value Builder))
-- @
--
bicompile :: (Bitraversable p, Eq b, Hashable b, Eq c, Hashable c)
          => (p Builder Builder -> a) -- ^ Reduce a bifunctor to the returned result.
          -> p b c                    -- ^ A bifunctor perform substitution on.
          -> (a, (HashMap b Builder, HashMap c Builder))
bicompile f = flip runState (mempty, mempty) . fmap f . bisubstitute
{-# INLINE bicompile #-}

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
conditionExpression :: ConditionExpression Builder Builder -> Maybe Builder
conditionExpression = \case
    CondE c -> Just (condition c)

    NotE  a -> ("NOT" >>>) <$> conditionExpression a

    AndE EmptyE b      -> conditionExpression b
    AndE a      EmptyE -> conditionExpression a
    AndE a      b      -> do
        x <- conditionExpression a
        y <- conditionExpression b
        Just $! parens (x >>> "AND" >>> y)

    OrE a b -> do
        x <- conditionExpression a
        y <- conditionExpression b
        Just $! parens (x >>> "OR" >>> y)

    EmptyE  -> Nothing

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
condition :: Condition a Builder Builder -> Builder
condition = \case
    Equal          a b -> operand a >>> "="  >>> operand b
    NotEqual       a b -> operand a >>> "<>" >>> operand b
    Less           a b -> operand a >>> "<"  >>> operand b
    LessOrEqual    a b -> operand a >>> "<=" >>> operand b
    Greater        a b -> operand a >>> ">"  >>> operand b
    GreaterOrEqual a b -> operand a >>> ">=" >>> operand b

    Exists     p   -> "attribute_exists"     >>> parens (path p)
    NotExists  p   -> "attribute_not_exists" >>> parens (path p)
    Size       p   -> "size"                 >>> parens (path p)
    Contains   p o -> "contains"             >>> tupled [path p, operand o]
    IsType     p t -> "attribute_type"       >>> tupled [path p, t]
    BeginsWith p x -> "begins_with"          >>> tupled [path p, x]

    Between a (b, c) ->
        operand a >>> "BETWEEN" >>> operand b >>> "AND" >>> operand c
    In      x xs     ->
        operand x >>> "IN" >>> tupled (operand <$> xs)
{-|
@
expression ::=
      SET set-action , ...
    | REMOVE remove-action , ...
    | ADD add-action , ...
    | DELETE delete-action , ...
@
-}
updateExpression :: UpdateExpression Builder Builder -> Maybe Builder
updateExpression UpdateExpression{..}
    | null statements = Nothing
    | otherwise       = Just (mconcat (intersperse " " statements))
  where
    statements =
        catMaybes
            [ command "SET"    setf _set
            , command "REMOVE" path _remove
            , command "ADD"    addf _add
            , command "DELETE" delf _delete
            ]

    command :: Builder -> (a -> Builder) -> Seq a -> Maybe Builder
    command cmd f s =
        case Seq.viewl s of
            EmptyL  -> Nothing
            x :< xs -> Just $ foldl' (\r z -> r <> "," >>> f z) (cmd >>> f x) xs

    setf (p, u) = path p >>> "=" >>> update u
    addf (p, v) = path p >>> v
    delf (p, v) = path p >>> v

{-|
@

@
-}
projectionExpression :: ProjectionExpression Builder -> Builder
projectionExpression = fold . sintersperse ", " . fmap path . _project
  where
    -- In containers >= 0.5.8
    sintersperse y xs =
        case Seq.viewl xs of
            EmptyL -> Seq.empty
            p :< ps -> p <| (ps <**> (const y <| Seq.singleton id))

{-|
@
value ::=
      operand
    | operand \'+\' operand
    | operand \'-\' operand

operand ::=
      path
    | function

function ::=
      if_not_exists (path, operand)
    | list_append   (operand, operand)
@
-}
update :: Update Builder Builder -> Builder
update = \case
    Operand     o   -> operand o
    Plus        p u -> path p >>> "+" >>> update u
    Minus       p u -> path p >>> "-" >>> update u
    IfNotExists p u -> "if_not_exists" >>> tupled [path p, update u]
    ListAppend  a b -> "list_append"   >>> tupled [update a, update b]

{-|
@
operand ::=
      path
    | value
@
-}
operand :: Operand Builder Builder -> Builder
operand = \case
    Path  p -> path p
    Value v -> v
{-# INLINE operand #-}

{-|
@
path ::=
      attribute
    | path \'.\' path
    | path [integer]
@
-}
path :: Path Builder -> Builder
path = \case
    Attr   x   -> x
    Index  x i -> x <> "[" <> build i <> "]"
    Nested a b -> path a <> "." <> path b
{-# INLINE path #-}

{-|
@
attribute ::= string
@
-}
attribute :: Name -> Builder
attribute = Build.fromText . fromName

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
