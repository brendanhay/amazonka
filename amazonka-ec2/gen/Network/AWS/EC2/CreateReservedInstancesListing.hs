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
-- Module      : Network.AWS.EC2.CreateReservedInstancesListing
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a listing for Amazon EC2 Reserved Instances to be sold in the
-- Reserved Instance Marketplace. You can submit one Reserved Instance
-- listing at a time. To get a list of your Reserved Instances, you can use
-- the DescribeReservedInstances operation.
--
-- The Reserved Instance Marketplace matches sellers who want to resell
-- Reserved Instance capacity that they no longer need with buyers who want
-- to purchase additional capacity. Reserved Instances bought and sold
-- through the Reserved Instance Marketplace work like any other Reserved
-- Instances.
--
-- To sell your Reserved Instances, you must first register as a seller in
-- the Reserved Instance Marketplace. After completing the registration
-- process, you can create a Reserved Instance Marketplace listing of some
-- or all of your Reserved Instances, and specify the upfront price to
-- receive for them. Your Reserved Instance listings then become available
-- for purchase. To view the details of your Reserved Instance listing, you
-- can use the DescribeReservedInstancesListings operation.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateReservedInstancesListing.html AWS API Reference> for CreateReservedInstancesListing.
module Network.AWS.EC2.CreateReservedInstancesListing
    (
    -- * Creating a Request
      createReservedInstancesListing
    , CreateReservedInstancesListing
    -- * Request Lenses
    , crilReservedInstancesId
    , crilInstanceCount
    , crilPriceSchedules
    , crilClientToken

    -- * Destructuring the Response
    , createReservedInstancesListingResponse
    , CreateReservedInstancesListingResponse
    -- * Response Lenses
    , crersReservedInstancesListings
    , crersResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createReservedInstancesListing' smart constructor.
data CreateReservedInstancesListing = CreateReservedInstancesListing'
    { _crilReservedInstancesId :: !Text
    , _crilInstanceCount       :: !Int
    , _crilPriceSchedules      :: ![PriceScheduleSpecification]
    , _crilClientToken         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateReservedInstancesListing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crilReservedInstancesId'
--
-- * 'crilInstanceCount'
--
-- * 'crilPriceSchedules'
--
-- * 'crilClientToken'
createReservedInstancesListing
    :: Text -- ^ 'crilReservedInstancesId'
    -> Int -- ^ 'crilInstanceCount'
    -> Text -- ^ 'crilClientToken'
    -> CreateReservedInstancesListing
createReservedInstancesListing pReservedInstancesId_ pInstanceCount_ pClientToken_ =
    CreateReservedInstancesListing'
    { _crilReservedInstancesId = pReservedInstancesId_
    , _crilInstanceCount = pInstanceCount_
    , _crilPriceSchedules = mempty
    , _crilClientToken = pClientToken_
    }

-- | The ID of the active Reserved Instance.
crilReservedInstancesId :: Lens' CreateReservedInstancesListing Text
crilReservedInstancesId = lens _crilReservedInstancesId (\ s a -> s{_crilReservedInstancesId = a});

-- | The number of instances that are a part of a Reserved Instance account
-- to be listed in the Reserved Instance Marketplace. This number should be
-- less than or equal to the instance count associated with the Reserved
-- Instance ID specified in this call.
crilInstanceCount :: Lens' CreateReservedInstancesListing Int
crilInstanceCount = lens _crilInstanceCount (\ s a -> s{_crilInstanceCount = a});

-- | A list specifying the price of the Reserved Instance for each month
-- remaining in the Reserved Instance term.
crilPriceSchedules :: Lens' CreateReservedInstancesListing [PriceScheduleSpecification]
crilPriceSchedules = lens _crilPriceSchedules (\ s a -> s{_crilPriceSchedules = a}) . _Coerce;

-- | Unique, case-sensitive identifier you provide to ensure idempotency of
-- your listings. This helps avoid duplicate listings. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
crilClientToken :: Lens' CreateReservedInstancesListing Text
crilClientToken = lens _crilClientToken (\ s a -> s{_crilClientToken = a});

instance AWSRequest CreateReservedInstancesListing
         where
        type Rs CreateReservedInstancesListing =
             CreateReservedInstancesListingResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 CreateReservedInstancesListingResponse' <$>
                   (x .@? "reservedInstancesListingsSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders CreateReservedInstancesListing
         where
        toHeaders = const mempty

instance ToPath CreateReservedInstancesListing where
        toPath = const "/"

instance ToQuery CreateReservedInstancesListing where
        toQuery CreateReservedInstancesListing'{..}
          = mconcat
              ["Action" =:
                 ("CreateReservedInstancesListing" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "ReservedInstancesId" =: _crilReservedInstancesId,
               "InstanceCount" =: _crilInstanceCount,
               toQueryList "PriceSchedules" _crilPriceSchedules,
               "ClientToken" =: _crilClientToken]

-- | /See:/ 'createReservedInstancesListingResponse' smart constructor.
data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse'
    { _crersReservedInstancesListings :: !(Maybe [ReservedInstancesListing])
    , _crersResponseStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateReservedInstancesListingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersReservedInstancesListings'
--
-- * 'crersResponseStatus'
createReservedInstancesListingResponse
    :: Int -- ^ 'crersResponseStatus'
    -> CreateReservedInstancesListingResponse
createReservedInstancesListingResponse pResponseStatus_ =
    CreateReservedInstancesListingResponse'
    { _crersReservedInstancesListings = Nothing
    , _crersResponseStatus = pResponseStatus_
    }

-- | Information about the Reserved Instances listing.
crersReservedInstancesListings :: Lens' CreateReservedInstancesListingResponse [ReservedInstancesListing]
crersReservedInstancesListings = lens _crersReservedInstancesListings (\ s a -> s{_crersReservedInstancesListings = a}) . _Default . _Coerce;

-- | The response status code.
crersResponseStatus :: Lens' CreateReservedInstancesListingResponse Int
crersResponseStatus = lens _crersResponseStatus (\ s a -> s{_crersResponseStatus = a});
