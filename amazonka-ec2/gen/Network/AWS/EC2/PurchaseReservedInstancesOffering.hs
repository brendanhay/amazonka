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
-- Module      : Network.AWS.EC2.PurchaseReservedInstancesOffering
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases a Reserved Instance for use with your account. With Reserved
-- Instances, you obtain a capacity reservation for a certain instance
-- configuration over a specified period of time and pay a lower hourly
-- rate compared to On-Demand instance pricing.
--
-- Use < DescribeReservedInstancesOfferings> to get a list of Reserved
-- Instance offerings that match your specifications. After you\'ve
-- purchased a Reserved Instance, you can check for your new Reserved
-- Instance with < DescribeReservedInstances>.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-on-demand-reserved-instances.html Reserved Instances>
-- and
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-PurchaseReservedInstancesOffering.html AWS API Reference> for PurchaseReservedInstancesOffering.
module Network.AWS.EC2.PurchaseReservedInstancesOffering
    (
    -- * Creating a Request
      purchaseReservedInstancesOffering
    , PurchaseReservedInstancesOffering
    -- * Request Lenses
    , prioLimitPrice
    , prioDryRun
    , prioReservedInstancesOfferingId
    , prioInstanceCount

    -- * Destructuring the Response
    , purchaseReservedInstancesOfferingResponse
    , PurchaseReservedInstancesOfferingResponse
    -- * Response Lenses
    , priorsReservedInstancesId
    , priorsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'purchaseReservedInstancesOffering' smart constructor.
data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering'
    { _prioLimitPrice                  :: !(Maybe ReservedInstanceLimitPrice)
    , _prioDryRun                      :: !(Maybe Bool)
    , _prioReservedInstancesOfferingId :: !Text
    , _prioInstanceCount               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PurchaseReservedInstancesOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prioLimitPrice'
--
-- * 'prioDryRun'
--
-- * 'prioReservedInstancesOfferingId'
--
-- * 'prioInstanceCount'
purchaseReservedInstancesOffering
    :: Text -- ^ 'prioReservedInstancesOfferingId'
    -> Int -- ^ 'prioInstanceCount'
    -> PurchaseReservedInstancesOffering
purchaseReservedInstancesOffering pReservedInstancesOfferingId_ pInstanceCount_ =
    PurchaseReservedInstancesOffering'
    { _prioLimitPrice = Nothing
    , _prioDryRun = Nothing
    , _prioReservedInstancesOfferingId = pReservedInstancesOfferingId_
    , _prioInstanceCount = pInstanceCount_
    }

-- | Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at
-- unexpected prices.
prioLimitPrice :: Lens' PurchaseReservedInstancesOffering (Maybe ReservedInstanceLimitPrice)
prioLimitPrice = lens _prioLimitPrice (\ s a -> s{_prioLimitPrice = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
prioDryRun :: Lens' PurchaseReservedInstancesOffering (Maybe Bool)
prioDryRun = lens _prioDryRun (\ s a -> s{_prioDryRun = a});

-- | The ID of the Reserved Instance offering to purchase.
prioReservedInstancesOfferingId :: Lens' PurchaseReservedInstancesOffering Text
prioReservedInstancesOfferingId = lens _prioReservedInstancesOfferingId (\ s a -> s{_prioReservedInstancesOfferingId = a});

-- | The number of Reserved Instances to purchase.
prioInstanceCount :: Lens' PurchaseReservedInstancesOffering Int
prioInstanceCount = lens _prioInstanceCount (\ s a -> s{_prioInstanceCount = a});

instance AWSRequest PurchaseReservedInstancesOffering
         where
        type Rs PurchaseReservedInstancesOffering =
             PurchaseReservedInstancesOfferingResponse
        request = postQuery eC2
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
               "Version" =: ("2015-10-01" :: ByteString),
               "LimitPrice" =: _prioLimitPrice,
               "DryRun" =: _prioDryRun,
               "ReservedInstancesOfferingId" =:
                 _prioReservedInstancesOfferingId,
               "InstanceCount" =: _prioInstanceCount]

-- | /See:/ 'purchaseReservedInstancesOfferingResponse' smart constructor.
data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse'
    { _priorsReservedInstancesId :: !(Maybe Text)
    , _priorsResponseStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PurchaseReservedInstancesOfferingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'priorsReservedInstancesId'
--
-- * 'priorsResponseStatus'
purchaseReservedInstancesOfferingResponse
    :: Int -- ^ 'priorsResponseStatus'
    -> PurchaseReservedInstancesOfferingResponse
purchaseReservedInstancesOfferingResponse pResponseStatus_ =
    PurchaseReservedInstancesOfferingResponse'
    { _priorsReservedInstancesId = Nothing
    , _priorsResponseStatus = pResponseStatus_
    }

-- | The IDs of the purchased Reserved Instances.
priorsReservedInstancesId :: Lens' PurchaseReservedInstancesOfferingResponse (Maybe Text)
priorsReservedInstancesId = lens _priorsReservedInstancesId (\ s a -> s{_priorsReservedInstancesId = a});

-- | The response status code.
priorsResponseStatus :: Lens' PurchaseReservedInstancesOfferingResponse Int
priorsResponseStatus = lens _priorsResponseStatus (\ s a -> s{_priorsResponseStatus = a});
