{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.PurchaseReservedDBInstancesOffering
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Purchases a reserved DB instance offering.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_PurchaseReservedDBInstancesOffering.html>
module Network.AWS.RDS.PurchaseReservedDBInstancesOffering
    (
    -- * Request
      PurchaseReservedDBInstancesOffering
    -- ** Request constructor
    , purchaseReservedDBInstancesOffering
    -- ** Request lenses
    , prdioDBInstanceCount
    , prdioReservedDBInstanceId
    , prdioTags
    , prdioReservedDBInstancesOfferingId

    -- * Response
    , PurchaseReservedDBInstancesOfferingResponse
    -- ** Response constructor
    , purchaseReservedDBInstancesOfferingResponse
    -- ** Response lenses
    , prdiorReservedDBInstance
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.RDS.Types

-- | /See:/ 'purchaseReservedDBInstancesOffering' smart constructor.
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
data PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOffering'{_prdioDBInstanceCount :: Maybe Int, _prdioReservedDBInstanceId :: Maybe Text, _prdioTags :: Maybe [Tag], _prdioReservedDBInstancesOfferingId :: Text} deriving (Eq, Read, Show)

-- | 'PurchaseReservedDBInstancesOffering' smart constructor.
purchaseReservedDBInstancesOffering :: Text -> PurchaseReservedDBInstancesOffering
purchaseReservedDBInstancesOffering pReservedDBInstancesOfferingId = PurchaseReservedDBInstancesOffering'{_prdioDBInstanceCount = Nothing, _prdioReservedDBInstanceId = Nothing, _prdioTags = Nothing, _prdioReservedDBInstancesOfferingId = pReservedDBInstancesOfferingId};

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

-- | FIXME: Undocumented member.
prdioTags :: Lens' PurchaseReservedDBInstancesOffering [Tag]
prdioTags = lens _prdioTags (\ s a -> s{_prdioTags = a}) . _Default;

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
        request = post
        response
          = receiveXMLWrapper
              "PurchaseReservedDBInstancesOfferingResult"
              (\ s h x ->
                 PurchaseReservedDBInstancesOfferingResponse' <$>
                   (x .@? "ReservedDBInstance"))

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
-- * 'prdiorReservedDBInstance'
newtype PurchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse'{_prdiorReservedDBInstance :: Maybe ReservedDBInstance} deriving (Eq, Read, Show)

-- | 'PurchaseReservedDBInstancesOfferingResponse' smart constructor.
purchaseReservedDBInstancesOfferingResponse :: PurchaseReservedDBInstancesOfferingResponse
purchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse'{_prdiorReservedDBInstance = Nothing};

-- | FIXME: Undocumented member.
prdiorReservedDBInstance :: Lens' PurchaseReservedDBInstancesOfferingResponse (Maybe ReservedDBInstance)
prdiorReservedDBInstance = lens _prdiorReservedDBInstance (\ s a -> s{_prdiorReservedDBInstance = a});
