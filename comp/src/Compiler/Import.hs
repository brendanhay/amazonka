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
    , importsFor
    ) where

import           Compiler.Types
import           Control.Lens
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Monoid         ((<>))

operationImports :: Library -> Operation Identity Data -> [NS]
operationImports l o = uniq $
       "Network.AWS.Request"
     : "Network.AWS.Response"
     : "Network.AWS.Prelude"
     : l ^. typesNS
     : l ^. operationModules
    <> importsFor o

typeImports :: Library -> [NS]
typeImports l = uniq $
       "Network.AWS.Prelude"
     : l ^. typeModules
    <> importsFor (l ^. signatureVersion)
    <> concatMap importsFor (l ^. shapes)

waiterImports :: Library -> [NS]
waiterImports = view typeModules

class ImportsFor a where
    importsFor :: a -> [NS]

instance ImportsFor Signature where
    importsFor = (:[]) . \case
        V2 -> "Network.AWS.Sign.V2"
        _  -> "Network.AWS.Sign.V4"

instance ImportsFor Data where
    importsFor = uniq . \case
        Prod _ is -> mapMaybe instNS (Map.keys is)
        Sum  _ is -> mapMaybe instNS is
      where
        instNS = \case
            "FromJSON"  -> Just "Network.AWS.Data.JSON"
            "ToJSON"    -> Just "Network.AWS.Data.JSON"
            "FromXML"   -> Just "Network.AWS.Data.XML"
            "ToXML"     -> Just "Network.AWS.Data.XML"
            "ToElement" -> Just "Network.AWS.Data.XML"
            _           -> Nothing

instance ImportsFor (Operation Identity Data) where
    importsFor o =
          importsFor (o ^. opInput  . _Identity)
       <> importsFor (o ^. opOutput . _Identity)
