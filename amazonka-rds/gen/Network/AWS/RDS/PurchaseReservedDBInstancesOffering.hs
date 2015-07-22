{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.PurchaseReservedDBInstancesOffering
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Purchases a reserved DB instance offering.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_PurchaseReservedDBInstancesOffering.html>
module Network.AWS.RDS.PurchaseReservedDBInstancesOffering
    (
    -- * Request
      PurchaseReservedDBInstancesOffering
    -- ** Request constructor
    , purchaseReservedDBInstancesOffering
    -- ** Request lenses
    , prdiorqDBInstanceCount
    , prdiorqReservedDBInstanceId
    , prdiorqTags
    , prdiorqReservedDBInstancesOfferingId

    -- * Response
    , PurchaseReservedDBInstancesOfferingResponse
    -- ** Response constructor
    , purchaseReservedDBInstancesOfferingResponse
    -- ** Response lenses
    , prdiorsReservedDBInstance
    , prdiorsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'purchaseReservedDBInstancesOffering' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prdiorqDBInstanceCount'
--
-- * 'prdiorqReservedDBInstanceId'
--
-- * 'prdiorqTags'
--
-- * 'prdiorqReservedDBInstancesOfferingId'
data PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOffering'
    { _prdiorqDBInstanceCount               :: !(Maybe Int)
    , _prdiorqReservedDBInstanceId          :: !(Maybe Text)
    , _prdiorqTags                          :: !(Maybe [Tag])
    , _prdiorqReservedDBInstancesOfferingId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PurchaseReservedDBInstancesOffering' smart constructor.
purchaseReservedDBInstancesOffering :: Text -> PurchaseReservedDBInstancesOffering
purchaseReservedDBInstancesOffering pReservedDBInstancesOfferingId =
    PurchaseReservedDBInstancesOffering'
    { _prdiorqDBInstanceCount = Nothing
    , _prdiorqReservedDBInstanceId = Nothing
    , _prdiorqTags = Nothing
    , _prdiorqReservedDBInstancesOfferingId = pReservedDBInstancesOfferingId
    }

-- | The number of instances to reserve.
--
-- Default: @1@
prdiorqDBInstanceCount :: Lens' PurchaseReservedDBInstancesOffering (Maybe Int)
prdiorqDBInstanceCount = lens _prdiorqDBInstanceCount (\ s a -> s{_prdiorqDBInstanceCount = a});

-- | Customer-specified identifier to track this reservation.
--
-- Example: myreservationID
prdiorqReservedDBInstanceId :: Lens' PurchaseReservedDBInstancesOffering (Maybe Text)
prdiorqReservedDBInstanceId = lens _prdiorqReservedDBInstanceId (\ s a -> s{_prdiorqReservedDBInstanceId = a});

-- | FIXME: Undocumented member.
prdiorqTags :: Lens' PurchaseReservedDBInstancesOffering [Tag]
prdiorqTags = lens _prdiorqTags (\ s a -> s{_prdiorqTags = a}) . _Default;

-- | The ID of the Reserved DB instance offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
prdiorqReservedDBInstancesOfferingId :: Lens' PurchaseReservedDBInstancesOffering Text
prdiorqReservedDBInstancesOfferingId = lens _prdiorqReservedDBInstancesOfferingId (\ s a -> s{_prdiorqReservedDBInstancesOfferingId = a});

instance AWSRequest
         PurchaseReservedDBInstancesOffering where
        type Sv PurchaseReservedDBInstancesOffering = RDS
        type Rs PurchaseReservedDBInstancesOffering =
             PurchaseReservedDBInstancesOfferingResponse
        request = post
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
               "DBInstanceCount" =: _prdiorqDBInstanceCount,
               "ReservedDBInstanceId" =:
                 _prdiorqReservedDBInstanceId,
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _prdiorqTags),
               "ReservedDBInstancesOfferingId" =:
                 _prdiorqReservedDBInstancesOfferingId]

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
purchaseReservedDBInstancesOfferingResponse pStatus =
    PurchaseReservedDBInstancesOfferingResponse'
    { _prdiorsReservedDBInstance = Nothing
    , _prdiorsStatus = pStatus
    }

-- | FIXME: Undocumented member.
prdiorsReservedDBInstance :: Lens' PurchaseReservedDBInstancesOfferingResponse (Maybe ReservedDBInstance)
prdiorsReservedDBInstance = lens _prdiorsReservedDBInstance (\ s a -> s{_prdiorsReservedDBInstance = a});

-- | FIXME: Undocumented member.
prdiorsStatus :: Lens' PurchaseReservedDBInstancesOfferingResponse Int
prdiorsStatus = lens _prdiorsStatus (\ s a -> s{_prdiorsStatus = a});
