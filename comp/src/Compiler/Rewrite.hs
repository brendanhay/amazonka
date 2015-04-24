{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Compiler.Rewrite
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite where

import           Compiler.AST
import           Compiler.Types
import           Control.Error
import           Control.Error
import           Control.Lens
import           Data.Functor.Identity
import qualified Data.HashMap.Strict   as Map
import           Data.Monoid
import           Data.Text             (Text)

createPackage :: Monad m
              => SemVer
              -> API Maybe
              -> EitherT LazyText m Package
createPackage ver api = do
    return $! Package (setDefaults api) ver [] []

setDefaults :: API Maybe -> API Identity
setDefaults api = undefined -- api & shapes %~ setShape
  where
    setShape :: Shape Maybe -> Shape Identity
    setShape = \case
        List   i e      -> List   (setInfo i) (setRef e)
        Map    i k v    -> Map    (setInfo i) (setRef k) (setRef v)
        Struct i ms r p -> Struct (setInfo i) (Map.map setRef ms) r p
        Enum   i m      -> Enum   (setInfo i) m
        Lit    i l      -> Lit    (setInfo i) l

    Use the API protocol etc to correctly default things here.

    setInfo :: Info Maybe -> Info Identity
    setInfo i@Info{..} = i
        { _infoDocumentation =
            def "FIXME: Undocumented shape." _infoDocumentation
        }

    setRef :: Ref Maybe -> Ref Identity
    setRef r@Ref{..} = r
        { _refDocumentation =
            def "FIXME: Undocumented reference." _refDocumentation
        , _refLocation =
            def Querystring _refLocation
        , _refLocationName =
            def _refShape _refLocationName
        , _refQueryName =
            def _refShape _refQueryName
        , _refXMLNamespace =
            def undefined _refXMLNamespace
        }

-- (%!~) :: Lens s t (Maybe a) (Identity a) -> a -> s -> t
-- (%!~) l x = l %~ def x

def x = maybe (Identity x) Identity
