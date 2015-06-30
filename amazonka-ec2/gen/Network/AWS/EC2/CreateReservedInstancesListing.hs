{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.CreateReservedInstancesListing
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

-- | Creates a listing for Amazon EC2 Reserved Instances to be sold in the
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateReservedInstancesListing.html>
module Network.AWS.EC2.CreateReservedInstancesListing
    (
    -- * Request
      CreateReservedInstancesListing
    -- ** Request constructor
    , createReservedInstancesListing
    -- ** Request lenses
    , crilReservedInstancesId
    , crilInstanceCount
    , crilPriceSchedules
    , crilClientToken

    -- * Response
    , CreateReservedInstancesListingResponse
    -- ** Response constructor
    , createReservedInstancesListingResponse
    -- ** Response lenses
    , cReservedInstancesListings
    , cStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createReservedInstancesListing' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crilReservedInstancesId'
--
-- * 'crilInstanceCount'
--
-- * 'crilPriceSchedules'
--
-- * 'crilClientToken'
data CreateReservedInstancesListing = CreateReservedInstancesListing'
    { _crilReservedInstancesId :: !Text
    , _crilInstanceCount       :: !Int
    , _crilPriceSchedules      :: ![PriceScheduleSpecification]
    , _crilClientToken         :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateReservedInstancesListing' smart constructor.
createReservedInstancesListing :: Text -> Int -> Text -> CreateReservedInstancesListing
createReservedInstancesListing pReservedInstancesId pInstanceCount pClientToken =
    CreateReservedInstancesListing'
    { _crilReservedInstancesId = pReservedInstancesId
    , _crilInstanceCount = pInstanceCount
    , _crilPriceSchedules = mempty
    , _crilClientToken = pClientToken
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
crilPriceSchedules = lens _crilPriceSchedules (\ s a -> s{_crilPriceSchedules = a});

-- | Unique, case-sensitive identifier you provide to ensure idempotency of
-- your listings. This helps avoid duplicate listings. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
crilClientToken :: Lens' CreateReservedInstancesListing Text
crilClientToken = lens _crilClientToken (\ s a -> s{_crilClientToken = a});

instance AWSRequest CreateReservedInstancesListing
         where
        type Sv CreateReservedInstancesListing = EC2
        type Rs CreateReservedInstancesListing =
             CreateReservedInstancesListingResponse
        request = post
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
               "Version" =: ("2015-04-15" :: ByteString),
               "ReservedInstancesId" =: _crilReservedInstancesId,
               "InstanceCount" =: _crilInstanceCount,
               toQueryList "item" _crilPriceSchedules,
               "ClientToken" =: _crilClientToken]

-- | /See:/ 'createReservedInstancesListingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cReservedInstancesListings'
--
-- * 'cStatus'
data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse'
    { _cReservedInstancesListings :: !(Maybe [ReservedInstancesListing])
    , _cStatus                    :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateReservedInstancesListingResponse' smart constructor.
createReservedInstancesListingResponse :: Int -> CreateReservedInstancesListingResponse
createReservedInstancesListingResponse pStatus =
    CreateReservedInstancesListingResponse'
    { _cReservedInstancesListings = Nothing
    , _cStatus = pStatus
    }

-- | Information about the Reserved Instances listing.
cReservedInstancesListings :: Lens' CreateReservedInstancesListingResponse [ReservedInstancesListing]
cReservedInstancesListings = lens _cReservedInstancesListings (\ s a -> s{_cReservedInstancesListings = a}) . _Default;

-- | FIXME: Undocumented member.
cStatus :: Lens' CreateReservedInstancesListingResponse Int
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});
