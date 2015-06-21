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
import           Data.List      (sort)
import           Data.Maybe

operationImports :: Library -> Operation Identity SData a -> [NS]
operationImports l o = sort $
      "Network.AWS.Request"
    : "Network.AWS.Response"
    : "Network.AWS.Prelude"
    : l ^. typesNS
    : l ^. operationModules
   ++ maybeToList (const "Network.AWS.Pagers" <$> o ^. opPager)

typeImports :: Library -> [NS]
typeImports l = sort $
      "Network.AWS.Prelude"
    : signatureImport (l ^. signatureVersion)
    : l ^. typeModules

waiterImports :: Library -> [NS]
waiterImports l = sort $
      "Network.AWS.Prelude"
    : "Network.AWS.Waiters"
    : l ^. typesNS
    : map (operationNS ns . _waitOpName) (l ^.. waiters . each)
  where
    ns = l ^. libraryNS

signatureImport :: Signature -> NS
signatureImport = \case
    V2 -> "Network.AWS.Sign.V2"
    _  -> "Network.AWS.Sign.V4"
