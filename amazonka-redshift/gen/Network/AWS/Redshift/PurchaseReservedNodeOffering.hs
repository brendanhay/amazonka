{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.PurchaseReservedNodeOffering
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase reserved nodes. Amazon Redshift offers a
-- predefined set of reserved node offerings. You can purchase one or more
-- of the offerings. You can call the DescribeReservedNodeOfferings API to
-- obtain the available reserved node offerings. You can call this API by
-- providing a specific reserved node offering and the number of nodes you
-- want to reserve.
--
-- For more information about reserved node offerings, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html Purchasing Reserved Nodes>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_PurchaseReservedNodeOffering.html>
module Network.AWS.Redshift.PurchaseReservedNodeOffering
    (
    -- * Request
      PurchaseReservedNodeOffering
    -- ** Request constructor
    , purchaseReservedNodeOffering
    -- ** Request lenses
    , prnoNodeCount
    , prnoReservedNodeOfferingId

    -- * Response
    , PurchaseReservedNodeOfferingResponse
    -- ** Response constructor
    , purchaseReservedNodeOfferingResponse
    -- ** Response lenses
    , prnorReservedNode
    , prnorStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'purchaseReservedNodeOffering' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prnoNodeCount'
--
-- * 'prnoReservedNodeOfferingId'
data PurchaseReservedNodeOffering = PurchaseReservedNodeOffering'
    { _prnoNodeCount              :: !(Maybe Int)
    , _prnoReservedNodeOfferingId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PurchaseReservedNodeOffering' smart constructor.
purchaseReservedNodeOffering :: Text -> PurchaseReservedNodeOffering
purchaseReservedNodeOffering pReservedNodeOfferingId =
    PurchaseReservedNodeOffering'
    { _prnoNodeCount = Nothing
    , _prnoReservedNodeOfferingId = pReservedNodeOfferingId
    }

-- | The number of reserved nodes you want to purchase.
--
-- Default: @1@
prnoNodeCount :: Lens' PurchaseReservedNodeOffering (Maybe Int)
prnoNodeCount = lens _prnoNodeCount (\ s a -> s{_prnoNodeCount = a});

-- | The unique identifier of the reserved node offering you want to
-- purchase.
prnoReservedNodeOfferingId :: Lens' PurchaseReservedNodeOffering Text
prnoReservedNodeOfferingId = lens _prnoReservedNodeOfferingId (\ s a -> s{_prnoReservedNodeOfferingId = a});

instance AWSRequest PurchaseReservedNodeOffering
         where
        type Sv PurchaseReservedNodeOffering = Redshift
        type Rs PurchaseReservedNodeOffering =
             PurchaseReservedNodeOfferingResponse
        request = post
        response
          = receiveXMLWrapper
              "PurchaseReservedNodeOfferingResult"
              (\ s h x ->
                 PurchaseReservedNodeOfferingResponse' <$>
                   (x .@? "ReservedNode") <*> (pure (fromEnum s)))

instance ToHeaders PurchaseReservedNodeOffering where
        toHeaders = const mempty

instance ToPath PurchaseReservedNodeOffering where
        toPath = const "/"

instance ToQuery PurchaseReservedNodeOffering where
        toQuery PurchaseReservedNodeOffering'{..}
          = mconcat
              ["Action" =:
                 ("PurchaseReservedNodeOffering" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "NodeCount" =: _prnoNodeCount,
               "ReservedNodeOfferingId" =:
                 _prnoReservedNodeOfferingId]

-- | /See:/ 'purchaseReservedNodeOfferingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prnorReservedNode'
--
-- * 'prnorStatus'
data PurchaseReservedNodeOfferingResponse = PurchaseReservedNodeOfferingResponse'
    { _prnorReservedNode :: !(Maybe ReservedNode)
    , _prnorStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PurchaseReservedNodeOfferingResponse' smart constructor.
purchaseReservedNodeOfferingResponse :: Int -> PurchaseReservedNodeOfferingResponse
purchaseReservedNodeOfferingResponse pStatus =
    PurchaseReservedNodeOfferingResponse'
    { _prnorReservedNode = Nothing
    , _prnorStatus = pStatus
    }

-- | FIXME: Undocumented member.
prnorReservedNode :: Lens' PurchaseReservedNodeOfferingResponse (Maybe ReservedNode)
prnorReservedNode = lens _prnorReservedNode (\ s a -> s{_prnorReservedNode = a});

-- | FIXME: Undocumented member.
prnorStatus :: Lens' PurchaseReservedNodeOfferingResponse Int
prnorStatus = lens _prnorStatus (\ s a -> s{_prnorStatus = a});
