{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
-- reserved cache node offering.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_PurchaseReservedCacheNodesOffering.html>
module Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
    (
    -- * Request
      PurchaseReservedCacheNodesOffering
    -- ** Request constructor
    , purchaseReservedCacheNodesOffering
    -- ** Request lenses
    , prcnoCacheNodeCount
    , prcnoReservedCacheNodeId
    , prcnoReservedCacheNodesOfferingId

    -- * Response
    , PurchaseReservedCacheNodesOfferingResponse
    -- ** Response constructor
    , purchaseReservedCacheNodesOfferingResponse
    -- ** Response lenses
    , prcnorReservedCacheNode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOffering
    { _prcnoCacheNodeCount               :: Maybe Int
    , _prcnoReservedCacheNodeId          :: Maybe Text
    , _prcnoReservedCacheNodesOfferingId :: Text
    } deriving (Eq, Ord, Show)

-- | 'PurchaseReservedCacheNodesOffering' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prcnoCacheNodeCount' @::@ 'Maybe' 'Int'
--
-- * 'prcnoReservedCacheNodeId' @::@ 'Maybe' 'Text'
--
-- * 'prcnoReservedCacheNodesOfferingId' @::@ 'Text'
--
purchaseReservedCacheNodesOffering :: Text -- ^ 'prcnoReservedCacheNodesOfferingId'
                                   -> PurchaseReservedCacheNodesOffering
purchaseReservedCacheNodesOffering p1 = PurchaseReservedCacheNodesOffering
    { _prcnoReservedCacheNodesOfferingId = p1
    , _prcnoReservedCacheNodeId          = Nothing
    , _prcnoCacheNodeCount               = Nothing
    }

-- | The number of cache node instances to reserve. Default: 1.
prcnoCacheNodeCount :: Lens' PurchaseReservedCacheNodesOffering (Maybe Int)
prcnoCacheNodeCount =
    lens _prcnoCacheNodeCount (\s a -> s { _prcnoCacheNodeCount = a })

-- | A customer-specified identifier to track this reservation. Example:
-- myreservationID.
prcnoReservedCacheNodeId :: Lens' PurchaseReservedCacheNodesOffering (Maybe Text)
prcnoReservedCacheNodeId =
    lens _prcnoReservedCacheNodeId
        (\s a -> s { _prcnoReservedCacheNodeId = a })

-- | The ID of the reserved cache node offering to purchase. Example:
-- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
prcnoReservedCacheNodesOfferingId :: Lens' PurchaseReservedCacheNodesOffering Text
prcnoReservedCacheNodesOfferingId =
    lens _prcnoReservedCacheNodesOfferingId
        (\s a -> s { _prcnoReservedCacheNodesOfferingId = a })

newtype PurchaseReservedCacheNodesOfferingResponse = PurchaseReservedCacheNodesOfferingResponse
    { _prcnorReservedCacheNode :: Maybe ReservedCacheNode
    } deriving (Eq, Show)

-- | 'PurchaseReservedCacheNodesOfferingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prcnorReservedCacheNode' @::@ 'Maybe' 'ReservedCacheNode'
--
purchaseReservedCacheNodesOfferingResponse :: PurchaseReservedCacheNodesOfferingResponse
purchaseReservedCacheNodesOfferingResponse = PurchaseReservedCacheNodesOfferingResponse
    { _prcnorReservedCacheNode = Nothing
    }

prcnorReservedCacheNode :: Lens' PurchaseReservedCacheNodesOfferingResponse (Maybe ReservedCacheNode)
prcnorReservedCacheNode =
    lens _prcnorReservedCacheNode (\s a -> s { _prcnorReservedCacheNode = a })

instance ToPath PurchaseReservedCacheNodesOffering where
    toPath = const "/"

instance ToQuery PurchaseReservedCacheNodesOffering where
    toQuery PurchaseReservedCacheNodesOffering{..} = mconcat
        [ "CacheNodeCount"               =? _prcnoCacheNodeCount
        , "ReservedCacheNodeId"          =? _prcnoReservedCacheNodeId
        , "ReservedCacheNodesOfferingId" =? _prcnoReservedCacheNodesOfferingId
        ]

instance ToHeaders PurchaseReservedCacheNodesOffering

query

instance AWSRequest PurchaseReservedCacheNodesOffering where
    type Sv PurchaseReservedCacheNodesOffering = ElastiCache
    type Rs PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOfferingResponse

    request  = post "PurchaseReservedCacheNodesOffering"
    response = xmlResponse

instance FromXML PurchaseReservedCacheNodesOfferingResponse where
    parseXML = withElement "PurchaseReservedCacheNodesOfferingResult" $ \x -> PurchaseReservedCacheNodesOfferingResponse
        <$> x .@? "ReservedCacheNode"
