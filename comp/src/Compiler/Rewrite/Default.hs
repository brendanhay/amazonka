{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Compiler.Rewrite.Default
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Default
    ( defaults
    ) where

import           Compiler.AST
import           Control.Lens
import qualified Data.HashMap.Strict as Map

defaults :: API Maybe -> API Identity
defaults api@API{..} = api
    { _metadata' = setMeta _metadata'
    , _shapes    = Map.map setShape _shapes
    }
  where
    setMeta :: Metadata Maybe -> Metadata Identity
    setMeta m@Metadata{..} = m
        { _timestampFormat = _timestampFormat .! defaultTimestamp _protocol
        , _checksumFormat  = _checksumFormat  .! SHA256
        }

    setShape :: Shape Maybe -> Shape Identity
    setShape = \case
        List   i e      -> List   (setInfo i) (setRef e)
        Map    i k v    -> Map    (setInfo i) (setRef k) (setRef v)
        Struct i ms r p -> Struct (setInfo i) (Map.map setRef ms) r p
        Enum   i m      -> Enum   (setInfo i) m
        Lit    i l      -> Lit    (setInfo i) l

    setInfo :: Info Maybe -> Info Identity
    setInfo i@Info{..} = i
        { _infoDocumentation = _infoDocumentation .! "FIXME: Undocumented shape."
        }

    setRef :: Ref Maybe -> Ref Identity
    setRef r@Ref{..} = r
        { _refDocumentation = _refDocumentation .! "FIXME: Undocumented reference."
        , _refLocation      = _refLocation      .! Querystring
        , _refLocationName  = _refLocationName  .! _refShape
        , _refQueryName     = _refQueryName     .! _refShape
        , _refXMLNamespace  = _refXMLNamespace  .! NS "" ""
        }

defaultTimestamp :: Protocol -> Timestamp
defaultTimestamp = \case
    JSON     -> POSIX
    RestJSON -> POSIX
    XML      -> ISO8601
    RestXML  -> ISO8601
    Query    -> ISO8601
    EC2      -> ISO8601

infixl 7 .!

(.!) :: Maybe a -> a -> Identity a
m .! x = maybe (Identity x) Identity m
