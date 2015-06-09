{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Compiler.Import
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Import
    ( operationImports
    , typeImports
    , waiterImports
    ) where

import           Compiler.Types
import           Control.Lens
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Monoid         ((<>))

operationImports :: Library -> Operation Identity Data -> [NS]
operationImports l o =
      "Network.AWS.Request"
    : "Network.AWS.Response"
    : "Network.AWS.Prelude"
    : protocolImport (l ^. protocol)
    : l ^. typesNS
    : l ^. operationModules

typeImports :: Library -> [NS]
typeImports l =
      "Network.AWS.Prelude"
    : protocolImport  (l ^. protocol)
    : signatureImport (l ^. signatureVersion)
    : l ^. typeModules

waiterImports :: Library -> [NS]
waiterImports l =
    [ "Network.AWS.Prelude"
    , l ^. typesNS
    ]

signatureImport :: Signature -> NS
signatureImport = \case
    V2 -> "Network.AWS.Sign.V2"
    _  -> "Network.AWS.Sign.V4"

protocolImport :: Protocol -> NS
protocolImport = \case
    JSON     -> "Network.AWS.Data.JSON"
    RestJSON -> "Network.AWS.Data.JSON"
    RestXML  -> "Network.AWS.Data.XML"
    Query    -> "Network.AWS.Data.XML"
    EC2      -> "Network.AWS.Data.XML"
