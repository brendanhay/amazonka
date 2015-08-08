{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /PurchaseReservedCacheNodesOffering/ action allows you to purchase a
-- reserved cache node offering.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_PurchaseReservedCacheNodesOffering.html AWS API Reference> for PurchaseReservedCacheNodesOffering.
module Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
    (
    -- * Creating a Request
      PurchaseReservedCacheNodesOffering
    , purchaseReservedCacheNodesOffering
    -- * Request Lenses
    , prcnoCacheNodeCount
    , prcnoReservedCacheNodeId
    , prcnoReservedCacheNodesOfferingId

    -- * Destructuring the Response
    , PurchaseReservedCacheNodesOfferingResponse
    , purchaseReservedCacheNodesOfferingResponse
    -- * Response Lenses
    , prcnorsReservedCacheNode
    , prcnorsStatus
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
purchaseReservedCacheNodesOffering pReservedCacheNodesOfferingId_ =
    PurchaseReservedCacheNodesOffering'
    { _prcnoCacheNodeCount = Nothing
    , _prcnoReservedCacheNodeId = Nothing
    , _prcnoReservedCacheNodesOfferingId = pReservedCacheNodesOfferingId_
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
        request = postQuery
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
-- * 'prcnorsReservedCacheNode'
--
-- * 'prcnorsStatus'
data PurchaseReservedCacheNodesOfferingResponse = PurchaseReservedCacheNodesOfferingResponse'
    { _prcnorsReservedCacheNode :: !(Maybe ReservedCacheNode)
    , _prcnorsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PurchaseReservedCacheNodesOfferingResponse' smart constructor.
purchaseReservedCacheNodesOfferingResponse :: Int -> PurchaseReservedCacheNodesOfferingResponse
purchaseReservedCacheNodesOfferingResponse pStatus_ =
    PurchaseReservedCacheNodesOfferingResponse'
    { _prcnorsReservedCacheNode = Nothing
    , _prcnorsStatus = pStatus_
    }

-- | Undocumented member.
prcnorsReservedCacheNode :: Lens' PurchaseReservedCacheNodesOfferingResponse (Maybe ReservedCacheNode)
prcnorsReservedCacheNode = lens _prcnorsReservedCacheNode (\ s a -> s{_prcnorsReservedCacheNode = a});

-- | Undocumented member.
prcnorsStatus :: Lens' PurchaseReservedCacheNodesOfferingResponse Int
prcnorsStatus = lens _prcnorsStatus (\ s a -> s{_prcnorsStatus = a});
