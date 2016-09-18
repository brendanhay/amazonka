{-# LANGUAGE FlexibleContexts  #-}
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
-- Compilation of an expression consists of substituting either the first argument of a 'Traversable' or
-- both arguments of a 'Bitraversable' (such as 'Name' and 'Path') with
-- safe placeholders.
--
-- The expression is then pretty printed and the resulting placeholder mapping
-- is also returned.
module Amazonka.DynamoDB.Expression.Compile
    (
    -- * Compilation
      compileNames
    , compileValues
    , compile

    , compileNamesT
    , compileValuesT
    , compileT

    , finalize

    -- * Grammar
    , keyConditionExpression

    , conditionExpression
    , condition

    , updateExpression
    , update

    , projectionExpression

    , operand
    , path
    , name
    ) where

import Amazonka.DynamoDB.Expression.Internal    hiding (name)
import Amazonka.DynamoDB.Expression.Placeholder
import Amazonka.DynamoDB.Item.Value             (Value)

import Control.Monad.Trans.State.Strict (StateT)

import Control.Monad.Trans.State.Strict (runState)
import Data.Bifunctor
import Data.Bitraversable               (Bitraversable (..))
import Data.Foldable                    (foldl', toList)
import Data.Functor.Identity            (runIdentity)
import Data.Hashable                    (Hashable)
import Data.List                        (intersperse)
import Data.Maybe                       (catMaybes)
import Data.Semigroup                   ((<>))
import Data.Sequence                    (Seq, ViewL (..))
import Data.Text                        (Text)
import Data.Text.Lazy.Builder           (Builder)

import Network.AWS.Data.Text (ToText (..))

import qualified Data.Sequence          as Seq
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as Build

-- NOTE: about why compileT vs compile is needed, carrying around Placeholders
-- state when compiling disparate expressions, which result in a single
-- attributeNames and attributeValues map for requests.

compileNames :: (Traversable p, Eq b, Hashable b)
             => (p Builder -> r)
             -> p Name
             -> (r, Placeholders Name b)
compileNames f =
      first runIdentity
    . flip runState placeholders
    . compileNamesT f
    . pure

compileValues :: (Traversable p, Eq a, Hashable a)
              => (p Builder -> r)
              -> p Value
              -> (r, Placeholders a Value)
compileValues f =
      first runIdentity
    . flip runState placeholders
    . compileValuesT f
    . pure

compile :: Bitraversable p
        => (p Builder Builder -> r) -- ^ An expression grammar.
        -> p Name Value             -- ^ An expression to perform substitution on.
        -> (r, Placeholders Name Value)
compile f =
      first runIdentity
    . flip runState placeholders
    . compileT f
    . pure

-- | Given an expression evaluator, compile the expression and return
-- the result along with any substituted placeholders for the 'Traversable'
-- argument.
--
-- Typical usage specializes as follows:
--
-- @
-- flip runState placeholders . compileNames projectionExpression . pure
--     :: ProjectionExpression Name -> (Identity Builder, Placeholders Name b)
-- @
--
compileNamesT :: (Monad m, Traversable t, Traversable p)
              => (p Builder -> r) -- ^ An expression grammar.
              -> t (p Name)       -- ^ An expression to perform substitution on.
              -> StateT (Placeholders Name b) m (t r)
compileNamesT f = sequenceA . fmap (fmap f . traverse substituteName)
{-# INLINE compileNamesT #-}

-- |
--
-- To compile a 'Bitraversable', substituing the 'Value's for placeholders
-- and rendering the first argument (such as a 'Name') verbatim:
--
-- @
-- flip runState placeholders . compileValues conditionExpression . pure . first name
--     :: ConditionExpression Name Value -> (Identity Builder, Placeholders a Value)
-- @
--
compileValuesT :: (Monad m, Traversable t, Traversable p)
               => (p Builder -> r) -- ^ An expression grammar.
               -> t (p Value)      -- ^ An expression to perform substitution on.
               -> StateT (Placeholders a Value) m (t r)
compileValuesT f = sequenceA . fmap (fmap f . traverse substituteValue)
{-# INLINE compileValuesT #-}

-- | Given an expression evaluator, compile the expression and return
-- the result along with the substituted placeholders for the 'Bitraversable's
-- first and second arguments.
--
-- Typical usage specializes as follows:
--
-- @
-- flip runState placeholders . compile conditionExpression . pure
--     :: ConditionExpression Name Value -> (Identity Builder, Placeholders Name Value)
-- @
--
compileT :: (Monad m, Traversable t, Bitraversable p)
         => (p Builder Builder -> r) -- ^ An expression grammar.
         -> t (p Name Value)         -- ^ An expression to perform substitution on.
         -> StateT (Placeholders Name Value) m (t r)
compileT f = sequenceA . fmap (fmap f . bitraverse substituteName substituteValue)
{-# INLINE compileT #-}

{-|
@
TODO
@
-}
keyConditionExpression :: KeyConditionExpression Builder Builder -> Builder
keyConditionExpression = \case
    Partition h   -> condition h
    Sort      h r -> conditionExpression (CondE h <> CondE r)

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
conditionExpression :: ConditionExpression Builder Builder -> Builder
conditionExpression = \case
    CondE c   -> condition c
    NotE  a   -> parens ("NOT" >>> conditionExpression a)
    AndE  a b -> parens (conditionExpression a >>> "AND" >>> conditionExpression b)
    OrE   a b -> parens (conditionExpression a >>> "OR"  >>> conditionExpression b)

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

FIXME: Assumes the invariant that an UpdateExpression has no empty/identity is held.
-}
updateExpression :: UpdateExpression Builder Builder -> Builder
updateExpression UnsafeUpdateExpression{..} =
    mconcat (intersperse " " statements)
  where
    statements =
        catMaybes
            [ command "SET"    setf _unsafeSet
            , command "REMOVE" path _unsafeRemove
            , command "ADD"    addf _unsafeAdd
            , command "DELETE" delf _unsafeDelete
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
projection ::= path , ...
@
-}
projectionExpression :: ProjectionExpression Builder -> Builder
projectionExpression (ProjectionExpression (x, xs)) =
    path x <> foldMap (mappend ", " . path) xs

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
      name
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
name ::= string
@
-}
name :: Name -> Builder
name = Build.fromText . fromName

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

finalize :: Builder -> Text
finalize = LText.toStrict . Build.toLazyText
{-# INLINE finalize #-}
