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
import           Data.Maybe
import           Data.Text           (Text)

defaults :: API Maybe -> API Identity
defaults api@API{..} = api
    { _metadata'  = meta' _metadata'
    , _operations = Map.map operation' _operations
    , _shapes     = Map.map shape' _shapes
    }
  where
    meta' m@Metadata{..} = m
        { _timestampFormat = _timestampFormat .! defaultTimestamp _protocol
        , _checksumFormat  = _checksumFormat  .! SHA256
        }

    operation' o@Operation{..} = o
        { _opDocumentation = _opDocumentation .! "FIXME: Undocumented operation."
        , _opHTTP          = http' _opHTTP
        , _opInput         = rqrs'  _opInput
        , _opOutput        = rqrs'  _opOutput
        }

    http' h@HTTP{..} = h
        { _responseCode = _responseCode .! 200
        }

    rqrs' = Identity
        . ref'
        . fromMaybe (defaultRef "FIXME: Unnamed request/response type.")

    shape' = \case
        List   i e      -> List   (info' i) (ref' e)
        Map    i k v    -> Map    (info' i) (ref' k) (ref' v)
        Struct i ms r p -> Struct (info' i) (Map.map ref' ms) r p
        Enum   i m      -> Enum   (info' i) m
        Lit    i l      -> Lit    (info' i) l

    info' i@Info{..} = i
        { _infoDocumentation = _infoDocumentation .! "FIXME: Undocumented shape."
        }

    ref' r@Ref{..} = r
        { _refDocumentation = _refDocumentation .! "FIXME: Undocumented reference."
        , _refLocation      = _refLocation      .! Querystring
        , _refLocationName  = _refLocationName  .! _refShape
        , _refQueryName     = _refQueryName     .! _refShape
        , _refXMLNamespace  = _refXMLNamespace  .! NS "" ""
        }

defaultRef :: Text -> Ref Maybe
defaultRef s = Ref
    { _refShape         = s
    , _refDocumentation = Nothing
    , _refLocation      = Nothing
    , _refLocationName  = Nothing
    , _refQueryName     = Nothing
    , _refStreaming     = False
    , _refWrapper       = False
    , _refXMLAttribute  = False
    , _refXMLNamespace  = Nothing
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
