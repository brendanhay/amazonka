{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SimpleDB.V2009_04_15.Lenses where

import Control.Lens.TH
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.SimpleDB.V2009_04_15.BatchDeleteAttributes
import Network.AWS.SimpleDB.V2009_04_15.BatchPutAttributes
import Network.AWS.SimpleDB.V2009_04_15.GetAttributes
import Network.AWS.SimpleDB.V2009_04_15.CreateDomain
import Network.AWS.SimpleDB.V2009_04_15.DomainMetadata
import Network.AWS.SimpleDB.V2009_04_15.Select
import Network.AWS.SimpleDB.V2009_04_15.DeleteAttributes
import Network.AWS.SimpleDB.V2009_04_15.PutAttributes
import Network.AWS.SimpleDB.V2009_04_15.DeleteDomain
import Network.AWS.SimpleDB.V2009_04_15.ListDomains

-- Newtypes

-- Products
makeLenses ''Attribute
makeLenses ''DeletableItem
makeLenses ''Item
makeLenses ''ReplaceableAttribute
makeLenses ''ReplaceableItem
makeLenses ''UpdateCondition

-- Requests
makeLenses ''BatchDeleteAttributes
makeLenses ''BatchPutAttributes
makeLenses ''GetAttributes
makeLenses ''CreateDomain
makeLenses ''DomainMetadata
makeLenses ''Select
makeLenses ''DeleteAttributes
makeLenses ''PutAttributes
makeLenses ''DeleteDomain
makeLenses ''ListDomains

-- Responses
makeLenses ''BatchDeleteAttributesResponse
makeLenses ''BatchPutAttributesResponse
makeLenses ''GetAttributesResponse
makeLenses ''CreateDomainResponse
makeLenses ''DomainMetadataResponse
makeLenses ''SelectResponse
makeLenses ''DeleteAttributesResponse
makeLenses ''PutAttributesResponse
makeLenses ''DeleteDomainResponse
makeLenses ''ListDomainsResponse
