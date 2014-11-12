{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
    (
    -- * Request
      PurchaseReservedCacheNodesOfferingMessage
    -- ** Request constructor
    , purchaseReservedCacheNodesOfferingMessage
    -- ** Request lenses
    , prcnomCacheNodeCount
    , prcnomReservedCacheNodeId
    , prcnomReservedCacheNodesOfferingId

    -- * Response
    , PurchaseReservedCacheNodesOfferingResult
    -- ** Response constructor
    , purchaseReservedCacheNodesOfferingResult
    -- ** Response lenses
    , prcnorReservedCacheNode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data PurchaseReservedCacheNodesOfferingMessage = PurchaseReservedCacheNodesOfferingMessage
    { _prcnomCacheNodeCount               :: Maybe Int
    , _prcnomReservedCacheNodeId          :: Maybe Text
    , _prcnomReservedCacheNodesOfferingId :: Text
    } (Eq, Ord, Show, Generic)

-- | 'PurchaseReservedCacheNodesOfferingMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prcnomCacheNodeCount' @::@ 'Maybe' 'Int'
--
-- * 'prcnomReservedCacheNodeId' @::@ 'Maybe' 'Text'
--
-- * 'prcnomReservedCacheNodesOfferingId' @::@ 'Text'
--
purchaseReservedCacheNodesOfferingMessage :: Text -- ^ 'prcnomReservedCacheNodesOfferingId'
                                          -> PurchaseReservedCacheNodesOfferingMessage
purchaseReservedCacheNodesOfferingMessage p1 = PurchaseReservedCacheNodesOfferingMessage
    { _prcnomReservedCacheNodesOfferingId = p1
    , _prcnomReservedCacheNodeId          = Nothing
    , _prcnomCacheNodeCount               = Nothing
    }

-- | The number of cache node instances to reserve. Default: 1.
prcnomCacheNodeCount :: Lens' PurchaseReservedCacheNodesOfferingMessage (Maybe Int)
prcnomCacheNodeCount =
    lens _prcnomCacheNodeCount (\s a -> s { _prcnomCacheNodeCount = a })

-- | A customer-specified identifier to track this reservation. Example:
-- myreservationID.
prcnomReservedCacheNodeId :: Lens' PurchaseReservedCacheNodesOfferingMessage (Maybe Text)
prcnomReservedCacheNodeId =
    lens _prcnomReservedCacheNodeId
        (\s a -> s { _prcnomReservedCacheNodeId = a })

-- | The ID of the reserved cache node offering to purchase. Example:
-- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
prcnomReservedCacheNodesOfferingId :: Lens' PurchaseReservedCacheNodesOfferingMessage Text
prcnomReservedCacheNodesOfferingId =
    lens _prcnomReservedCacheNodesOfferingId
        (\s a -> s { _prcnomReservedCacheNodesOfferingId = a })
instance ToQuery PurchaseReservedCacheNodesOfferingMessage

instance ToPath PurchaseReservedCacheNodesOfferingMessage where
    toPath = const "/"

newtype PurchaseReservedCacheNodesOfferingResult = PurchaseReservedCacheNodesOfferingResult
    { _prcnorReservedCacheNode :: Maybe ReservedCacheNode
    } (Eq, Show, Generic)

-- | 'PurchaseReservedCacheNodesOfferingResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prcnorReservedCacheNode' @::@ 'Maybe' 'ReservedCacheNode'
--
purchaseReservedCacheNodesOfferingResult :: PurchaseReservedCacheNodesOfferingResult
purchaseReservedCacheNodesOfferingResult = PurchaseReservedCacheNodesOfferingResult
    { _prcnorReservedCacheNode = Nothing
    }

prcnorReservedCacheNode :: Lens' PurchaseReservedCacheNodesOfferingResult (Maybe ReservedCacheNode)
prcnorReservedCacheNode =
    lens _prcnorReservedCacheNode (\s a -> s { _prcnorReservedCacheNode = a })

instance FromXML PurchaseReservedCacheNodesOfferingResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PurchaseReservedCacheNodesOfferingResult"

instance AWSRequest PurchaseReservedCacheNodesOfferingMessage where
    type Sv PurchaseReservedCacheNodesOfferingMessage = ElastiCache
    type Rs PurchaseReservedCacheNodesOfferingMessage = PurchaseReservedCacheNodesOfferingResult

    request  = post "PurchaseReservedCacheNodesOffering"
    response = xmlResponse $ \h x -> PurchaseReservedCacheNodesOfferingResult
        <$> x %| "ReservedCacheNode"
