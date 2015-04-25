{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Compiler.Defaults
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Defaults
    ( setDefaults
    ) where

import           Compiler.AST
import           Compiler.Types
import           Control.Error
import           Control.Error
import           Control.Lens
import           Data.Functor.Identity
import qualified Data.HashMap.Strict   as Map
import           Data.Monoid
import           Data.Text             (Text)

setDefaults :: API Maybe Name -> API Identity Name
setDefaults api@API{..} = api
    { _metadata = setMeta _metadata
    , _shapes   = Map.map setShape _shapes
    }
  where
    setMeta m@Meta{..} = m
        { _timestampFormat = _timestampFormat .! defTimestamp _protocol
        , _checksumFormat  = _checksumFormat  .! SHA256
        }

    setShape = \case
        List   i e      -> List   (setInfo i) (setRef e)
        Map    i k v    -> Map    (setInfo i) (setRef k) (setRef v)
        Struct i ms r p -> Struct (setInfo i) (Map.map setRef ms) r p
        Enum   i m      -> Enum   (setInfo i) m
        Lit    i l      -> Lit    (setInfo i) l

--    Use the API protocol etc to correctly default things here.

    setInfo i@Info{..} = i
        { _infoDocumentation = _infoDocumentation .! "FIXME: Undocumented shape."
        }

    setRef :: Ref Maybe Name -> Ref Identity Name
    setRef r@Ref{..} = r
        { _refDocumentation = _refDocumentation .! "FIXME: Undocumented reference."
        , _refLocation      = _refLocation      .! Querystring
        , _refLocationName  = _refLocationName  .! _refShape ^. _Name
        , _refQueryName     = _refQueryName     .! _refShape ^. _Name
        , _refXMLNamespace  = _refXMLNamespace  .! NS "" ""
        }

    defTimestamp = \case
        JSON     -> POSIX
        RestJSON -> POSIX
        XML      -> ISO8601
        RestXML  -> ISO8601
        Query    -> ISO8601
        EC2      -> ISO8601

infixl 7 .!

(.!) :: Maybe a -> a -> Identity a
m .! x = maybe (Identity x) Identity m
