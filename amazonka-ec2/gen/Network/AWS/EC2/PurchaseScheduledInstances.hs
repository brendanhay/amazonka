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
-- Module      : Network.AWS.EC2.PurchaseScheduledInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases one or more Scheduled Instances with the specified schedule.
--
--
-- Scheduled Instances enable you to purchase Amazon EC2 compute capacity by the hour for a one-year term. Before you can purchase a Scheduled Instance, you must call 'DescribeScheduledInstanceAvailability' to check for available schedules and obtain a purchase token. After you purchase a Scheduled Instance, you must call 'RunScheduledInstances' during each scheduled time period.
--
-- After you purchase a Scheduled Instance, you can't cancel, modify, or resell your purchase.
--
module Network.AWS.EC2.PurchaseScheduledInstances
    (
    -- * Creating a Request
      purchaseScheduledInstances
    , PurchaseScheduledInstances
    -- * Request Lenses
    , psiClientToken
    , psiDryRun
    , psiPurchaseRequests

    -- * Destructuring the Response
    , purchaseScheduledInstancesResponse
    , PurchaseScheduledInstancesResponse
    -- * Response Lenses
    , psirsScheduledInstanceSet
    , psirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for PurchaseScheduledInstances.
--
--
--
-- /See:/ 'purchaseScheduledInstances' smart constructor.
data PurchaseScheduledInstances = PurchaseScheduledInstances'
  { _psiClientToken      :: !(Maybe Text)
  , _psiDryRun           :: !(Maybe Bool)
  , _psiPurchaseRequests :: !(List1 PurchaseRequest)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseScheduledInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psiClientToken' - Unique, case-sensitive identifier that ensures the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'psiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'psiPurchaseRequests' - One or more purchase requests.
purchaseScheduledInstances
    :: NonEmpty PurchaseRequest -- ^ 'psiPurchaseRequests'
    -> PurchaseScheduledInstances
purchaseScheduledInstances pPurchaseRequests_ =
  PurchaseScheduledInstances'
    { _psiClientToken = Nothing
    , _psiDryRun = Nothing
    , _psiPurchaseRequests = _List1 # pPurchaseRequests_
    }


-- | Unique, case-sensitive identifier that ensures the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
psiClientToken :: Lens' PurchaseScheduledInstances (Maybe Text)
psiClientToken = lens _psiClientToken (\ s a -> s{_psiClientToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
psiDryRun :: Lens' PurchaseScheduledInstances (Maybe Bool)
psiDryRun = lens _psiDryRun (\ s a -> s{_psiDryRun = a})

-- | One or more purchase requests.
psiPurchaseRequests :: Lens' PurchaseScheduledInstances (NonEmpty PurchaseRequest)
psiPurchaseRequests = lens _psiPurchaseRequests (\ s a -> s{_psiPurchaseRequests = a}) . _List1

instance AWSRequest PurchaseScheduledInstances where
        type Rs PurchaseScheduledInstances =
             PurchaseScheduledInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 PurchaseScheduledInstancesResponse' <$>
                   (x .@? "scheduledInstanceSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable PurchaseScheduledInstances where

instance NFData PurchaseScheduledInstances where

instance ToHeaders PurchaseScheduledInstances where
        toHeaders = const mempty

instance ToPath PurchaseScheduledInstances where
        toPath = const "/"

instance ToQuery PurchaseScheduledInstances where
        toQuery PurchaseScheduledInstances'{..}
          = mconcat
              ["Action" =:
                 ("PurchaseScheduledInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _psiClientToken,
               "DryRun" =: _psiDryRun,
               toQueryList "PurchaseRequest" _psiPurchaseRequests]

-- | Contains the output of PurchaseScheduledInstances.
--
--
--
-- /See:/ 'purchaseScheduledInstancesResponse' smart constructor.
data PurchaseScheduledInstancesResponse = PurchaseScheduledInstancesResponse'
  { _psirsScheduledInstanceSet :: !(Maybe [ScheduledInstance])
  , _psirsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseScheduledInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psirsScheduledInstanceSet' - Information about the Scheduled Instances.
--
-- * 'psirsResponseStatus' - -- | The response status code.
purchaseScheduledInstancesResponse
    :: Int -- ^ 'psirsResponseStatus'
    -> PurchaseScheduledInstancesResponse
purchaseScheduledInstancesResponse pResponseStatus_ =
  PurchaseScheduledInstancesResponse'
    { _psirsScheduledInstanceSet = Nothing
    , _psirsResponseStatus = pResponseStatus_
    }


-- | Information about the Scheduled Instances.
psirsScheduledInstanceSet :: Lens' PurchaseScheduledInstancesResponse [ScheduledInstance]
psirsScheduledInstanceSet = lens _psirsScheduledInstanceSet (\ s a -> s{_psirsScheduledInstanceSet = a}) . _Default . _Coerce

-- | -- | The response status code.
psirsResponseStatus :: Lens' PurchaseScheduledInstancesResponse Int
psirsResponseStatus = lens _psirsResponseStatus (\ s a -> s{_psirsResponseStatus = a})

instance NFData PurchaseScheduledInstancesResponse
         where
