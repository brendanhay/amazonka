{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.PurchaseReservedDBInstancesOffering
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases a reserved DB instance offering.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_PurchaseReservedDBInstancesOffering.html AWS API Reference> for PurchaseReservedDBInstancesOffering.
module Network.AWS.RDS.PurchaseReservedDBInstancesOffering
    (
    -- * Creating a Request
      PurchaseReservedDBInstancesOffering
    , purchaseReservedDBInstancesOffering
    -- * Request Lenses
    , prdioDBInstanceCount
    , prdioReservedDBInstanceId
    , prdioTags
    , prdioReservedDBInstancesOfferingId

    -- * Destructuring the Response
    , PurchaseReservedDBInstancesOfferingResponse
    , purchaseReservedDBInstancesOfferingResponse
    -- * Response Lenses
    , prdiorsReservedDBInstance
    , prdiorsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'purchaseReservedDBInstancesOffering' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prdioDBInstanceCount'
--
-- * 'prdioReservedDBInstanceId'
--
-- * 'prdioTags'
--
-- * 'prdioReservedDBInstancesOfferingId'
data PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOffering'
    { _prdioDBInstanceCount               :: !(Maybe Int)
    , _prdioReservedDBInstanceId          :: !(Maybe Text)
    , _prdioTags                          :: !(Maybe [Tag])
    , _prdioReservedDBInstancesOfferingId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PurchaseReservedDBInstancesOffering' smart constructor.
purchaseReservedDBInstancesOffering :: Text -> PurchaseReservedDBInstancesOffering
purchaseReservedDBInstancesOffering pReservedDBInstancesOfferingId_ =
    PurchaseReservedDBInstancesOffering'
    { _prdioDBInstanceCount = Nothing
    , _prdioReservedDBInstanceId = Nothing
    , _prdioTags = Nothing
    , _prdioReservedDBInstancesOfferingId = pReservedDBInstancesOfferingId_
    }

-- | The number of instances to reserve.
--
-- Default: @1@
prdioDBInstanceCount :: Lens' PurchaseReservedDBInstancesOffering (Maybe Int)
prdioDBInstanceCount = lens _prdioDBInstanceCount (\ s a -> s{_prdioDBInstanceCount = a});

-- | Customer-specified identifier to track this reservation.
--
-- Example: myreservationID
prdioReservedDBInstanceId :: Lens' PurchaseReservedDBInstancesOffering (Maybe Text)
prdioReservedDBInstanceId = lens _prdioReservedDBInstanceId (\ s a -> s{_prdioReservedDBInstanceId = a});

-- | Undocumented member.
prdioTags :: Lens' PurchaseReservedDBInstancesOffering [Tag]
prdioTags = lens _prdioTags (\ s a -> s{_prdioTags = a}) . _Default . _Coerce;

-- | The ID of the Reserved DB instance offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
prdioReservedDBInstancesOfferingId :: Lens' PurchaseReservedDBInstancesOffering Text
prdioReservedDBInstancesOfferingId = lens _prdioReservedDBInstancesOfferingId (\ s a -> s{_prdioReservedDBInstancesOfferingId = a});

instance AWSRequest
         PurchaseReservedDBInstancesOffering where
        type Sv PurchaseReservedDBInstancesOffering = RDS
        type Rs PurchaseReservedDBInstancesOffering =
             PurchaseReservedDBInstancesOfferingResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "PurchaseReservedDBInstancesOfferingResult"
              (\ s h x ->
                 PurchaseReservedDBInstancesOfferingResponse' <$>
                   (x .@? "ReservedDBInstance") <*> (pure (fromEnum s)))

instance ToHeaders
         PurchaseReservedDBInstancesOffering where
        toHeaders = const mempty

instance ToPath PurchaseReservedDBInstancesOffering
         where
        toPath = const "/"

instance ToQuery PurchaseReservedDBInstancesOffering
         where
        toQuery PurchaseReservedDBInstancesOffering'{..}
          = mconcat
              ["Action" =:
                 ("PurchaseReservedDBInstancesOffering" ::
                    ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBInstanceCount" =: _prdioDBInstanceCount,
               "ReservedDBInstanceId" =: _prdioReservedDBInstanceId,
               "Tags" =: toQuery (toQueryList "Tag" <$> _prdioTags),
               "ReservedDBInstancesOfferingId" =:
                 _prdioReservedDBInstancesOfferingId]

-- | /See:/ 'purchaseReservedDBInstancesOfferingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prdiorsReservedDBInstance'
--
-- * 'prdiorsStatus'
data PurchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse'
    { _prdiorsReservedDBInstance :: !(Maybe ReservedDBInstance)
    , _prdiorsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PurchaseReservedDBInstancesOfferingResponse' smart constructor.
purchaseReservedDBInstancesOfferingResponse :: Int -> PurchaseReservedDBInstancesOfferingResponse
purchaseReservedDBInstancesOfferingResponse pStatus_ =
    PurchaseReservedDBInstancesOfferingResponse'
    { _prdiorsReservedDBInstance = Nothing
    , _prdiorsStatus = pStatus_
    }

-- | Undocumented member.
prdiorsReservedDBInstance :: Lens' PurchaseReservedDBInstancesOfferingResponse (Maybe ReservedDBInstance)
prdiorsReservedDBInstance = lens _prdiorsReservedDBInstance (\ s a -> s{_prdiorsReservedDBInstance = a});

-- | Undocumented member.
prdiorsStatus :: Lens' PurchaseReservedDBInstancesOfferingResponse Int
prdiorsStatus = lens _prdiorsStatus (\ s a -> s{_prdiorsStatus = a});
