{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.PurchaseReservedInstancesOffering
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Purchases a Reserved Instance for use with your account. With Amazon EC2
-- Reserved Instances, you obtain a capacity reservation for a certain
-- instance configuration over a specified period of time and pay a lower
-- hourly rate compared to on-Demand Instance pricing.
--
-- Use DescribeReservedInstancesOfferings to get a list of Reserved
-- Instance offerings that match your specifications. After you\'ve
-- purchased a Reserved Instance, you can check for your new Reserved
-- Instance with DescribeReservedInstances.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-on-demand-reserved-instances.html Reserved Instances>
-- and
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-PurchaseReservedInstancesOffering.html>
module Network.AWS.EC2.PurchaseReservedInstancesOffering
    (
    -- * Request
      PurchaseReservedInstancesOffering
    -- ** Request constructor
    , purchaseReservedInstancesOffering
    -- ** Request lenses
    , priorqLimitPrice
    , priorqDryRun
    , priorqReservedInstancesOfferingId
    , priorqInstanceCount

    -- * Response
    , PurchaseReservedInstancesOfferingResponse
    -- ** Response constructor
    , purchaseReservedInstancesOfferingResponse
    -- ** Response lenses
    , priorsReservedInstancesId
    , priorsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'purchaseReservedInstancesOffering' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'priorqLimitPrice'
--
-- * 'priorqDryRun'
--
-- * 'priorqReservedInstancesOfferingId'
--
-- * 'priorqInstanceCount'
data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering'
    { _priorqLimitPrice                  :: !(Maybe ReservedInstanceLimitPrice)
    , _priorqDryRun                      :: !(Maybe Bool)
    , _priorqReservedInstancesOfferingId :: !Text
    , _priorqInstanceCount               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PurchaseReservedInstancesOffering' smart constructor.
purchaseReservedInstancesOffering :: Text -> Int -> PurchaseReservedInstancesOffering
purchaseReservedInstancesOffering pReservedInstancesOfferingId_ pInstanceCount_ =
    PurchaseReservedInstancesOffering'
    { _priorqLimitPrice = Nothing
    , _priorqDryRun = Nothing
    , _priorqReservedInstancesOfferingId = pReservedInstancesOfferingId_
    , _priorqInstanceCount = pInstanceCount_
    }

-- | Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at
-- unexpected prices.
priorqLimitPrice :: Lens' PurchaseReservedInstancesOffering (Maybe ReservedInstanceLimitPrice)
priorqLimitPrice = lens _priorqLimitPrice (\ s a -> s{_priorqLimitPrice = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
priorqDryRun :: Lens' PurchaseReservedInstancesOffering (Maybe Bool)
priorqDryRun = lens _priorqDryRun (\ s a -> s{_priorqDryRun = a});

-- | The ID of the Reserved Instance offering to purchase.
priorqReservedInstancesOfferingId :: Lens' PurchaseReservedInstancesOffering Text
priorqReservedInstancesOfferingId = lens _priorqReservedInstancesOfferingId (\ s a -> s{_priorqReservedInstancesOfferingId = a});

-- | The number of Reserved Instances to purchase.
priorqInstanceCount :: Lens' PurchaseReservedInstancesOffering Int
priorqInstanceCount = lens _priorqInstanceCount (\ s a -> s{_priorqInstanceCount = a});

instance AWSRequest PurchaseReservedInstancesOffering
         where
        type Sv PurchaseReservedInstancesOffering = EC2
        type Rs PurchaseReservedInstancesOffering =
             PurchaseReservedInstancesOfferingResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 PurchaseReservedInstancesOfferingResponse' <$>
                   (x .@? "reservedInstancesId") <*>
                     (pure (fromEnum s)))

instance ToHeaders PurchaseReservedInstancesOffering
         where
        toHeaders = const mempty

instance ToPath PurchaseReservedInstancesOffering
         where
        toPath = const "/"

instance ToQuery PurchaseReservedInstancesOffering
         where
        toQuery PurchaseReservedInstancesOffering'{..}
          = mconcat
              ["Action" =:
                 ("PurchaseReservedInstancesOffering" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "LimitPrice" =: _priorqLimitPrice,
               "DryRun" =: _priorqDryRun,
               "ReservedInstancesOfferingId" =:
                 _priorqReservedInstancesOfferingId,
               "InstanceCount" =: _priorqInstanceCount]

-- | /See:/ 'purchaseReservedInstancesOfferingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'priorsReservedInstancesId'
--
-- * 'priorsStatus'
data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse'
    { _priorsReservedInstancesId :: !(Maybe Text)
    , _priorsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PurchaseReservedInstancesOfferingResponse' smart constructor.
purchaseReservedInstancesOfferingResponse :: Int -> PurchaseReservedInstancesOfferingResponse
purchaseReservedInstancesOfferingResponse pStatus_ =
    PurchaseReservedInstancesOfferingResponse'
    { _priorsReservedInstancesId = Nothing
    , _priorsStatus = pStatus_
    }

-- | The IDs of the purchased Reserved Instances.
priorsReservedInstancesId :: Lens' PurchaseReservedInstancesOfferingResponse (Maybe Text)
priorsReservedInstancesId = lens _priorsReservedInstancesId (\ s a -> s{_priorsReservedInstancesId = a});

-- | FIXME: Undocumented member.
priorsStatus :: Lens' PurchaseReservedInstancesOfferingResponse Int
priorsStatus = lens _priorsStatus (\ s a -> s{_priorsStatus = a});
