{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.AST
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST where

import           Compiler.AST.Cofree
import           Compiler.AST.Data
import           Compiler.AST.Override
import           Compiler.AST.Prefix
import           Compiler.AST.Subst
import           Compiler.AST.TypeOf
import           Compiler.Formatting
import           Compiler.Types
import           Control.Arrow
import           Control.Error
import           Control.Lens
import           Control.Monad.Except  (throwError)
import           Control.Monad.State
import qualified Data.HashMap.Strict   as Map
import qualified Data.HashSet          as Set
import           Data.Monoid
import           Debug.Trace

-- FIXME: Relations need to be updated by the solving step.

rewrite :: Versions
        -> Config
        -> Service Maybe (RefF ()) (ShapeF ()) Waiter
        -> Either Error Library
rewrite v cfg s' = Library v cfg
    <$> (rewriteService cfg (deprecate s')
         >>= renderShapes cfg)

deprecate :: Service f a b c -> Service f a b c
deprecate = operations %~ Map.filter (not . view opDeprecated)

rewriteService :: Config
               -> Service Maybe (RefF ()) (ShapeF ()) Waiter
               -> Either Error (Service Identity (RefF ()) (Shape Related) Waiter)
rewriteService cfg s = do
        -- Determine which direction (input, output, or both) shapes are used.
    rs <- relations (s ^. operations) (s ^. shapes)
        -- Elaborate the shape map into a comonadic strucutre for traversing.
    elaborate (s ^. shapes)
        -- Annotate the comonadic tree with the associated
        -- bi/unidirectional (input/output/both) relation for shapes.
        >>= traverse (pure . attach Related rs)
        -- Apply the override configuration to the service, and default any
        -- optional fields from the JSON where needed.
        >>= return . (\ss -> override (cfg ^. typeOverrides) (s { _shapes = ss }))
        -- Ensure no empty operation references exist, and that operation shapes
        -- are considered 'unique', so they can be lifted into the operation's
        -- module, separately from .Types.
        >>= substitute

renderShapes :: Config
             -> Service Identity (RefF ()) (Shape Related) Waiter
             -> Either Error (Service Identity Data Data Rendered)
renderShapes cfg svc = do
        -- Generate unique prefixes for struct (product) members and
        -- enum (sum) branches to avoid ambiguity.
    (x, y) <- prefixes (svc ^. shapes)
        -- Determine the appropriate Haskell AST type, auto deriveable instances,
        -- and fully rendered instances.
        >>= return . solve cfg
        -- Separate the operation input/output shapes from the .Types shapes.
        >>= separate (svc ^. operations)

    let prune = Map.filter (not . isOrphan)

    -- Convert shape ASTs into a rendered Haskell AST declaration,
    xs <- traverse (operationData svc) x
    ys <- kvTraverseMaybe (const (shapeData svc)) (prune y)
    zs <- Map.traverseWithKey waiterData (svc ^. waiters)

    return $! svc
        { _operations = xs
        , _shapes     = ys
        , _waiters    = zs
        }

type MemoR = StateT (Map Id Relation, Set (Id, Direction, Id)) (Either Error)

-- | Determine the relation for operation payloads, both input and output.
--
-- /Note:/ This currently doesn't operate over the free AST, since it's also
-- used by 'setDefaults'.
relations :: Show b
          => Map Id (Operation Maybe (RefF a))
          -> Map Id (ShapeF b)
          -> Either Error (Map Id Relation)
relations os ss = fst <$> execStateT (traverse go os) (mempty, mempty)
  where
    -- FIXME: opName here is incorrect as a parent.
    go :: Operation Maybe (RefF a) -> MemoR ()
    go o = count Nothing Input  (o ^? opInput  . _Just . refShape)
        >> count Nothing Output (o ^? opOutput . _Just . refShape)

    -- | Inserts a valid relation containing an referring shape's id,
    -- and the direction the parent is used in.
    count :: Maybe Id -> Direction -> Maybe Id -> MemoR ()
    count _ _ Nothing  = pure ()
    count p d (Just n) = do
        _1 %= Map.insertWith (<>) n (mkRelation p d)
        check p d n $ do
            s <- lift (safe n)
            shape n d s

    shape :: Id -> Direction -> ShapeF a -> MemoR ()
    shape p d = mapM_ (count (Just p) d . Just . view refShape)
        . toListOf references

    -- Ensure cyclic dependencies are only checked once per direction/parent.
    check Nothing  _ _ f = f
    check (Just p) d n f = do
        let k = (p, d, n)
        m <- uses _2 (Set.member k)
        if m
            then pure ()
            else _2 %= Set.insert k >> f

    safe n = note
        (format ("Missing shape "            % iprimary %
                 " when counting relations " %
                 ", possible matches: "      % partial)
                n (n, ss))
        (Map.lookup n ss)

-- FIXME: Necessary to update the Relation?
solve :: (Traversable t)
      => Config
      -> t (Shape Prefixed)
      -> t (Shape Solved)
solve cfg = (`evalState` replaced def cfg)
    . traverse (annotate Solved id (pure . typeOf))
 where
    def x = TType
        (x ^. replaceName . typeId)
        (x ^. replaceDeriving . to Set.toList)

    replaced :: (Replace -> a) -> Config -> Map Id a
    replaced f =
          Map.fromList
        . map (_replaceName &&& f)
        . Map.elems
        . vMapMaybe _replacedBy
        . _typeOverrides

type MemoS a = StateT (Map Id a) (Either Error)

-- | Filter the ids representing operation input/outputs from the supplied map,
-- and attach the associated shape to the appropriate operation.
separate :: (Show a, HasRelation a) => Map Id (Operation Identity (RefF b))
         -> Map Id a
         -> Either Error (Map Id (Operation Identity (RefF a)), Map Id a)
separate os ss = runStateT (traverse go os) ss
  where
    go :: HasRelation b
       => Operation Identity (RefF a) -> MemoS b (Operation Identity (RefF b))
    go o = do
        x <- remove Input  (o ^. inputName)
        y <- remove Output (o ^. outputName)
        return $! o
            { _opInput  = Identity (o ^. opInput  . _Identity & refAnn .~ x)
            , _opOutput = Identity (o ^. opOutput . _Identity & refAnn .~ y)
            }

    remove :: HasRelation a => Direction -> Id -> MemoS a a
    remove d n = do
        s <- get
        let m = "Failure separating operation wrapper " % iprimary %
                " from " % shown
        case Map.lookup n s of
            Nothing -> throwError $ format m n (Map.map (const ()) s)
            Just x  -> do
                when (d == Input || not (isShared x)) $
                    modify (Map.delete n)
                return x
