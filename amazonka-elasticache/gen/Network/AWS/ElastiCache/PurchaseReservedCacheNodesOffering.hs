{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /PurchaseReservedCacheNodesOffering/ action allows you to purchase a
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
    , prcnorStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /PurchaseReservedCacheNodesOffering/ action.
--
-- /See:/ 'purchaseReservedCacheNodesOffering' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prcnoCacheNodeCount'
--
-- * 'prcnoReservedCacheNodeId'
--
-- * 'prcnoReservedCacheNodesOfferingId'
data PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOffering'
    { _prcnoCacheNodeCount               :: !(Maybe Int)
    , _prcnoReservedCacheNodeId          :: !(Maybe Text)
    , _prcnoReservedCacheNodesOfferingId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PurchaseReservedCacheNodesOffering' smart constructor.
purchaseReservedCacheNodesOffering :: Text -> PurchaseReservedCacheNodesOffering
purchaseReservedCacheNodesOffering pReservedCacheNodesOfferingId =
    PurchaseReservedCacheNodesOffering'
    { _prcnoCacheNodeCount = Nothing
    , _prcnoReservedCacheNodeId = Nothing
    , _prcnoReservedCacheNodesOfferingId = pReservedCacheNodesOfferingId
    }

-- | The number of cache node instances to reserve.
--
-- Default: @1@
prcnoCacheNodeCount :: Lens' PurchaseReservedCacheNodesOffering (Maybe Int)
prcnoCacheNodeCount = lens _prcnoCacheNodeCount (\ s a -> s{_prcnoCacheNodeCount = a});

-- | A customer-specified identifier to track this reservation.
--
-- Example: myreservationID
prcnoReservedCacheNodeId :: Lens' PurchaseReservedCacheNodesOffering (Maybe Text)
prcnoReservedCacheNodeId = lens _prcnoReservedCacheNodeId (\ s a -> s{_prcnoReservedCacheNodeId = a});

-- | The ID of the reserved cache node offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
prcnoReservedCacheNodesOfferingId :: Lens' PurchaseReservedCacheNodesOffering Text
prcnoReservedCacheNodesOfferingId = lens _prcnoReservedCacheNodesOfferingId (\ s a -> s{_prcnoReservedCacheNodesOfferingId = a});

instance AWSRequest
         PurchaseReservedCacheNodesOffering where
        type Sv PurchaseReservedCacheNodesOffering =
             ElastiCache
        type Rs PurchaseReservedCacheNodesOffering =
             PurchaseReservedCacheNodesOfferingResponse
        request = post
        response
          = receiveXMLWrapper
              "PurchaseReservedCacheNodesOfferingResult"
              (\ s h x ->
                 PurchaseReservedCacheNodesOfferingResponse' <$>
                   (x .@? "ReservedCacheNode") <*> (pure (fromEnum s)))

instance ToHeaders PurchaseReservedCacheNodesOffering
         where
        toHeaders = const mempty

instance ToPath PurchaseReservedCacheNodesOffering
         where
        toPath = const "/"

instance ToQuery PurchaseReservedCacheNodesOffering
         where
        toQuery PurchaseReservedCacheNodesOffering'{..}
          = mconcat
              ["Action" =:
                 ("PurchaseReservedCacheNodesOffering" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheNodeCount" =: _prcnoCacheNodeCount,
               "ReservedCacheNodeId" =: _prcnoReservedCacheNodeId,
               "ReservedCacheNodesOfferingId" =:
                 _prcnoReservedCacheNodesOfferingId]

-- | /See:/ 'purchaseReservedCacheNodesOfferingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prcnorReservedCacheNode'
--
-- * 'prcnorStatus'
data PurchaseReservedCacheNodesOfferingResponse = PurchaseReservedCacheNodesOfferingResponse'
    { _prcnorReservedCacheNode :: !(Maybe ReservedCacheNode)
    , _prcnorStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PurchaseReservedCacheNodesOfferingResponse' smart constructor.
purchaseReservedCacheNodesOfferingResponse :: Int -> PurchaseReservedCacheNodesOfferingResponse
purchaseReservedCacheNodesOfferingResponse pStatus =
    PurchaseReservedCacheNodesOfferingResponse'
    { _prcnorReservedCacheNode = Nothing
    , _prcnorStatus = pStatus
    }

-- | FIXME: Undocumented member.
prcnorReservedCacheNode :: Lens' PurchaseReservedCacheNodesOfferingResponse (Maybe ReservedCacheNode)
prcnorReservedCacheNode = lens _prcnorReservedCacheNode (\ s a -> s{_prcnorReservedCacheNode = a});

-- | FIXME: Undocumented member.
prcnorStatus :: Lens' PurchaseReservedCacheNodesOfferingResponse Int
prcnorStatus = lens _prcnorStatus (\ s a -> s{_prcnorStatus = a});
