{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-- Module      : Compiler.Rewrite.Subst
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Subst where

import           Compiler.AST
import           Compiler.Types
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer.Strict
import           Data.Foldable               (traverse_)
import qualified Data.HashMap.Strict         as Map
import qualified Data.HashSet                as Set
import           Data.Text                   (Text)

type Replace = [(Text, Text)]
type Shapes  = Map Text (Shape Maybe)
type Subst   = WriterT Replace (Reader Shapes)

substitute :: Config
           -> Service Maybe Ref Shape
           -> (Config, Service Maybe Shape Shape)
substitute cfg@Config{..} svc@Service{..} =
    ( cfg { _typeOverrides = overrides }
    , svc { _operations    = os        }
    )
  where
    overrides = Map.fromList (map replace r) <> _typeOverrides

    replace (k, v) = (k,) $
        case Map.lookup k _typeOverrides of
            Just x  -> x & replacedBy ?~ v
            Nothing -> Override
                { _renamedTo      = Nothing
                , _replacedBy     = Just v
                , _enumPrefix     = Nothing
                , _requiredFields = mempty
                , _optionalFields = mempty
                , _renamedFields  = mempty
                }

    (os, r) = runReader (runWriterT (traverse go _operations)) _shapes

    go :: Operation Maybe Ref -> Subst (Operation Maybe Shape)
    go o = do
        rq <- elaborate (o ^. requestName)  (o ^. opInput)
        rs <- elaborate (o ^. responseName) (o ^. opOutput)
        return $! o
            { _opInput  = Just rq
            , _opOutput = Just rs
            }

    elaborate :: Text -> Maybe (Ref Maybe) -> Subst (Shape Maybe)
    elaborate _ Nothing        = return $! emptyStruct Nothing
    elaborate k (Just Ref{..}) = do
        m <- asks (Map.lookup _refShape)

        let x | Just s <- m  = s
              | otherwise    = emptyStruct _refDocumentation

        unless (Set.member _refShape shared) $
            tell [(_refShape, k)]

        return $! x

    shared = sharing _operations _shapes

emptyStruct :: f Help -> Shape f
emptyStruct d = Struct i s
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
        { _members  = mempty
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
sharing os ss = Set.fromList . Map.keys . Map.filter (> 1) $ execState go mempty
  where
    -- FIXME: Need to correctly count a shape being used as a ref as shared.
    go :: Count ()
    go = forM_ (Map.elems os) $ \o -> do
        ref (o ^? opInput . _Just . refShape)
        ref (o ^? opOutput . _Just . refShape)

    ref :: Maybe Text -> Count ()
    ref Nothing  = pure ()
    ref (Just n) = incr n >> maybe (pure ()) shape (Map.lookup n ss)

    shape :: Shape Maybe -> Count ()
    shape = traverse_ (incr . view refShape) . toListOf references

    incr :: Text -> Count ()
    incr n  = modify (Map.insertWith (+) n 1)
