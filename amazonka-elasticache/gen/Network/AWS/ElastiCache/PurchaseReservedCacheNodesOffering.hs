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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /PurchaseReservedCacheNodesOffering/ action allows you to purchase a
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
    , prcnorqCacheNodeCount
    , prcnorqReservedCacheNodeId
    , prcnorqReservedCacheNodesOfferingId

    -- * Response
    , PurchaseReservedCacheNodesOfferingResponse
    -- ** Response constructor
    , purchaseReservedCacheNodesOfferingResponse
    -- ** Response lenses
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
-- * 'prcnorqCacheNodeCount'
--
-- * 'prcnorqReservedCacheNodeId'
--
-- * 'prcnorqReservedCacheNodesOfferingId'
data PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOffering'
    { _prcnorqCacheNodeCount               :: !(Maybe Int)
    , _prcnorqReservedCacheNodeId          :: !(Maybe Text)
    , _prcnorqReservedCacheNodesOfferingId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PurchaseReservedCacheNodesOffering' smart constructor.
purchaseReservedCacheNodesOffering :: Text -> PurchaseReservedCacheNodesOffering
purchaseReservedCacheNodesOffering pReservedCacheNodesOfferingId =
    PurchaseReservedCacheNodesOffering'
    { _prcnorqCacheNodeCount = Nothing
    , _prcnorqReservedCacheNodeId = Nothing
    , _prcnorqReservedCacheNodesOfferingId = pReservedCacheNodesOfferingId
    }

-- | The number of cache node instances to reserve.
--
-- Default: @1@
prcnorqCacheNodeCount :: Lens' PurchaseReservedCacheNodesOffering (Maybe Int)
prcnorqCacheNodeCount = lens _prcnorqCacheNodeCount (\ s a -> s{_prcnorqCacheNodeCount = a});

-- | A customer-specified identifier to track this reservation.
--
-- Example: myreservationID
prcnorqReservedCacheNodeId :: Lens' PurchaseReservedCacheNodesOffering (Maybe Text)
prcnorqReservedCacheNodeId = lens _prcnorqReservedCacheNodeId (\ s a -> s{_prcnorqReservedCacheNodeId = a});

-- | The ID of the reserved cache node offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
prcnorqReservedCacheNodesOfferingId :: Lens' PurchaseReservedCacheNodesOffering Text
prcnorqReservedCacheNodesOfferingId = lens _prcnorqReservedCacheNodesOfferingId (\ s a -> s{_prcnorqReservedCacheNodesOfferingId = a});

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
               "CacheNodeCount" =: _prcnorqCacheNodeCount,
               "ReservedCacheNodeId" =: _prcnorqReservedCacheNodeId,
               "ReservedCacheNodesOfferingId" =:
                 _prcnorqReservedCacheNodesOfferingId]

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
purchaseReservedCacheNodesOfferingResponse pStatus =
    PurchaseReservedCacheNodesOfferingResponse'
    { _prcnorsReservedCacheNode = Nothing
    , _prcnorsStatus = pStatus
    }

-- | FIXME: Undocumented member.
prcnorsReservedCacheNode :: Lens' PurchaseReservedCacheNodesOfferingResponse (Maybe ReservedCacheNode)
prcnorsReservedCacheNode = lens _prcnorsReservedCacheNode (\ s a -> s{_prcnorsReservedCacheNode = a});

-- | FIXME: Undocumented member.
prcnorsStatus :: Lens' PurchaseReservedCacheNodesOfferingResponse Int
prcnorsStatus = lens _prcnorsStatus (\ s a -> s{_prcnorsStatus = a});
