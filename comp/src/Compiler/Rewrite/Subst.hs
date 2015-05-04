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
import           Data.Text           (Text)
import           Data.Traversable    (for)

type Subst = State (Map Text (Shape Maybe))

substitute :: Service Maybe Ref Shape -> Service Maybe Shape Shape
substitute svc@Service{..} = svc
    { _operations = os
    , _shapes     = ss
    }
  where
    (os, ss) = runState (traverse go _operations) _shapes

    go :: Operation Maybe Ref -> Subst (Operation Maybe Shape)
    go o = do
        rq <- wrap (o ^. opInput)
        rs <- wrap (o ^. opOutput)
        return $! o
            { _opInput  = Just rq
            , _opOutput = Just rs
            }

    -- If shared, create a newtype pointing to the shared type.
    -- FIXME: Will either provide an iso or some suitable lenses.
    wrap :: Maybe (Ref Maybe) -> Subst (Shape Maybe)
    wrap Nothing          = return $! empty Nothing mempty
    wrap (Just r@Ref{..}) = do
        if _refShape `Set.member` shared
            then return $! empty _refDocumentation $ Map.fromList [(_refShape, r)]
            else do
                 m <- gets (Map.lookup _refShape)
                 modify (Map.delete _refShape)
                 return $! fromMaybe (empty _refDocumentation mempty) m

    shared = sharing _operations _shapes

    -- FIXME: How to annotate that this is a reference to a shared type?
    empty :: f Help -> Map Text (Ref f) -> Shape f
    empty d rs = Struct i s
      where
        i = Info
            { _infoDocumentation = d
            , _infoMin           = 0
            , _infoMax           = Nothing
            , _infoFlattened     = False
            , _infoSensitive     = False
            , _infoStreaming     = False
            , _infoException     = False
            }

        s = Struct'
            { _members  = rs
            , _required = mempty
            , _payload  = Nothing
            }

type Count = State (Map Text Int)

-- | Determine the usage of operation input/output shapes.
--
-- A shape is considered 'shared' if it is used as a field of another shape,
-- as opposed to only being referenced by the operation itself.
sharing :: Map Text (Operation Maybe Ref)
        -> Map Text (Shape Maybe)
        -> Set Text
sharing os ss = count (execState (ops >> traverse_ shape ss) mempty)
  where
    count = Set.fromList . Map.keys . Map.filter (> 1)

    ops :: Count ()
    ops = void . for os $ \o -> do
        ref (o ^? opInput  . _Just . refShape)
        ref (o ^? opOutput . _Just . refShape)

    ref :: Maybe Text -> Count ()
    ref Nothing  = pure ()
    ref (Just n) = incr n >> maybe (pure ()) shape (Map.lookup n ss)

    shape :: Shape Maybe -> Count ()
    shape = traverse_ incr . toListOf (references . refShape)

    incr :: Text -> Count ()
    incr n = modify (Map.insertWith (+) n 1)
