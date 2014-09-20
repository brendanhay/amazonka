{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
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
module Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
    (
    -- * Request
      PurchaseReservedCacheNodesOffering
    -- ** Request constructor
    , purchaseReservedCacheNodesOffering
    -- ** Request lenses
    , prcnoReservedCacheNodesOfferingId
    , prcnoReservedCacheNodeId
    , prcnoCacheNodeCount

    -- * Response
    , PurchaseReservedCacheNodesOfferingResponse
    -- ** Response constructor
    , purchaseReservedCacheNodesOfferingResponse
    -- ** Response lenses
    , prcnorReservedCacheNode
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a PurchaseReservedCacheNodesOffering operation.
data PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOffering
    { _prcnoReservedCacheNodesOfferingId :: Text
    , _prcnoReservedCacheNodeId :: Maybe Text
    , _prcnoCacheNodeCount :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PurchaseReservedCacheNodesOffering' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedCacheNodesOfferingId ::@ @Text@
--
-- * @ReservedCacheNodeId ::@ @Maybe Text@
--
-- * @CacheNodeCount ::@ @Maybe Integer@
--
purchaseReservedCacheNodesOffering :: Text -- ^ 'prcnoReservedCacheNodesOfferingId'
                                   -> PurchaseReservedCacheNodesOffering
purchaseReservedCacheNodesOffering p1 = PurchaseReservedCacheNodesOffering
    { _prcnoReservedCacheNodesOfferingId = p1
    , _prcnoReservedCacheNodeId = Nothing
    , _prcnoCacheNodeCount = Nothing
    }

-- | The ID of the reserved cache node offering to purchase. Example:
-- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
prcnoReservedCacheNodesOfferingId :: Lens' PurchaseReservedCacheNodesOffering Text
prcnoReservedCacheNodesOfferingId =
    lens _prcnoReservedCacheNodesOfferingId
         (\s a -> s { _prcnoReservedCacheNodesOfferingId = a })

-- | A customer-specified identifier to track this reservation. Example:
-- myreservationID.
prcnoReservedCacheNodeId :: Lens' PurchaseReservedCacheNodesOffering (Maybe Text)
prcnoReservedCacheNodeId =
    lens _prcnoReservedCacheNodeId
         (\s a -> s { _prcnoReservedCacheNodeId = a })

-- | The number of cache node instances to reserve. Default: 1.
prcnoCacheNodeCount :: Lens' PurchaseReservedCacheNodesOffering (Maybe Integer)
prcnoCacheNodeCount =
    lens _prcnoCacheNodeCount (\s a -> s { _prcnoCacheNodeCount = a })

instance ToQuery PurchaseReservedCacheNodesOffering where
    toQuery = genericQuery def

newtype PurchaseReservedCacheNodesOfferingResponse = PurchaseReservedCacheNodesOfferingResponse
    { _prcnorReservedCacheNode :: Maybe ReservedCacheNode
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PurchaseReservedCacheNodesOfferingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedCacheNode ::@ @Maybe ReservedCacheNode@
--
purchaseReservedCacheNodesOfferingResponse :: PurchaseReservedCacheNodesOfferingResponse
purchaseReservedCacheNodesOfferingResponse = PurchaseReservedCacheNodesOfferingResponse
    { _prcnorReservedCacheNode = Nothing
    }

-- | Represents the output of a PurchaseReservedCacheNodesOffering operation.
prcnorReservedCacheNode :: Lens' PurchaseReservedCacheNodesOfferingResponse (Maybe ReservedCacheNode)
prcnorReservedCacheNode =
    lens _prcnorReservedCacheNode
         (\s a -> s { _prcnorReservedCacheNode = a })

instance FromXML PurchaseReservedCacheNodesOfferingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest PurchaseReservedCacheNodesOffering where
    type Sv PurchaseReservedCacheNodesOffering = ElastiCache
    type Rs PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOfferingResponse

    request = post "PurchaseReservedCacheNodesOffering"
    response _ = xmlResponse
