{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.PurchaseReservedCacheNodesOffering
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The PurchaseReservedCacheNodesOffering operation allows you to purchase a
-- reserved cache node offering. https://elasticache.amazonaws.com/
-- ?Action=PurchaseReservedCacheNodesOffering
-- &ReservedCacheNodeId=myreservationID
-- &ReservedCacheNodesOfferingId=438012d3-4052-4cc7-b2e3-8d3372e0e706
-- &CacheNodeCount=1 &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- Medium Utilization memcached 438012d3-4052-4cc7-b2e3-8d3372e0e706
-- payment-pending myreservationID 10 2014-03-18T23:24:56.577Z 31536000 123.0
-- 0.123 cache.m1.small 7f099901-29cf-11e1-bd06-6fe008f046c3.
module Network.AWS.ElastiCache.V2014_07_15.PurchaseReservedCacheNodesOffering where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PurchaseReservedCacheNodesOffering' request.
purchaseReservedCacheNodesOffering :: Text -- ^ '_prcnomReservedCacheNodesOfferingId'
                                   -> PurchaseReservedCacheNodesOffering
purchaseReservedCacheNodesOffering p1 = PurchaseReservedCacheNodesOffering
    { _prcnomReservedCacheNodesOfferingId = p1
    , _prcnomCacheNodeCount = Nothing
    , _prcnomReservedCacheNodeId = Nothing
    }

data PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOffering
    { _prcnomReservedCacheNodesOfferingId :: Text
      -- ^ The ID of the reserved cache node offering to purchase. Example:
      -- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
    , _prcnomCacheNodeCount :: Maybe Integer
      -- ^ The number of cache node instances to reserve. Default: 1.
    , _prcnomReservedCacheNodeId :: Maybe Text
      -- ^ A customer-specified identifier to track this reservation.
      -- Example: myreservationID.
    } deriving (Show, Generic)

makeLenses ''PurchaseReservedCacheNodesOffering

instance ToQuery PurchaseReservedCacheNodesOffering where
    toQuery = genericQuery def

data PurchaseReservedCacheNodesOfferingResponse = PurchaseReservedCacheNodesOfferingResponse
    { _rcnwReservedCacheNode :: Maybe ReservedCacheNode
      -- ^ Represents the output of a PurchaseReservedCacheNodesOffering
      -- operation.
    } deriving (Show, Generic)

makeLenses ''PurchaseReservedCacheNodesOfferingResponse

instance FromXML PurchaseReservedCacheNodesOfferingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest PurchaseReservedCacheNodesOffering where
    type Sv PurchaseReservedCacheNodesOffering = ElastiCache
    type Rs PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOfferingResponse

    request = post "PurchaseReservedCacheNodesOffering"
    response _ = xmlResponse
