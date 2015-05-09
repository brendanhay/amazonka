{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.Rewrite.Subst
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Subst
    ( substitute
    ) where

import           Compiler.Types
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Foldable       (traverse_)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import           Data.Maybe
import           Data.Traversable    (for)

type Subst a = State (Map Id (ShapeF a))

substitute :: Service Maybe (RefF   a) (ShapeF a)
           -> Service Maybe (ShapeF a) (ShapeF a)
substitute svc@Service{..} = svc
    { _operations = os
    , _shapes     = ss
    }
  where
    (os, ss) = runState (traverse go _operations) _shapes

    go :: Operation Maybe (RefF a) -> Subst a (Operation Maybe (ShapeF a))
    go o = do
        rq <- wrap (o ^. opInput)
        rs <- wrap (o ^. opOutput)
        return $! o
            { _opInput  = Just rq
            , _opOutput = Just rs
            }

    -- If shared, create a newtype pointing to the shared type.
    -- FIXME: Will either provide an iso or some suitable lenses.
    wrap :: Maybe (RefF a) -> Subst a (ShapeF a)
    wrap Nothing  = return $! empty Nothing mempty
    wrap (Just r) = do
        let h = r ^. refDocumentation
            n = r ^. refShape
        if n `Set.member` shared
            then return $! empty h $ Map.fromList [(n, r)]
            else do
                m <- gets (Map.lookup n)
                modify (Map.delete n)
                return $! fromMaybe (empty h mempty) m

    shared :: Set Id
    shared = sharing _operations _shapes

    empty :: Maybe Help -> Map Id (RefF a) -> ShapeF a
    empty d rs = Struct i ms
      where
        i = Info
            { _infoDocumentation = d
            , _infoMin           = Nothing
            , _infoMax           = Nothing
            , _infoFlattened     = False
            , _infoSensitive     = False
            , _infoStreaming     = False
            , _infoException     = False
            }

        ms = StructF
            { _members  = rs
            , _required = mempty
            , _payload  = Nothing
            , _wrapper  = not (Map.null rs)
            }

type Count = State (Map Id Int)

-- | Determine the usage of operation input/output shapes.
--
-- A shape is considered 'shared' if it is used as a field of another shape,
-- as opposed to only being referenced by the operation itself.
sharing :: Map Id (Operation Maybe (RefF a))
        -> Map Id (ShapeF b)
        -> Set Id
sharing os ss = count (execState (ops >> traverse_ shape ss) mempty)
  where
    count = Set.fromList . Map.keys . Map.filter (> 1)

    ops :: Count ()
    ops = void . for os $ \o -> do
        ref (o ^? opInput  . _Just . refShape)
        ref (o ^? opOutput . _Just . refShape)

    ref :: Maybe Id -> Count ()
    ref Nothing  = pure ()
    ref (Just n) = incr n >> maybe (pure ()) shape (Map.lookup n ss)

    shape :: ShapeF a -> Count ()
    shape = traverse_ incr . toListOf (references . refShape)

    incr :: Id -> Count ()
    incr n = modify (Map.insertWith (+) n 1)
