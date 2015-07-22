{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateReservedInstancesListing
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateReservedInstancesListing.html>
module Network.AWS.EC2.CreateReservedInstancesListing
    (
    -- * Request
      CreateReservedInstancesListing
    -- ** Request constructor
    , createReservedInstancesListing
    -- ** Request lenses
    , crilrqReservedInstancesId
    , crilrqInstanceCount
    , crilrqPriceSchedules
    , crilrqClientToken

    -- * Response
    , CreateReservedInstancesListingResponse
    -- ** Response constructor
    , createReservedInstancesListingResponse
    -- ** Response lenses
    , crersReservedInstancesListings
    , crersStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createReservedInstancesListing' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crilrqReservedInstancesId'
--
-- * 'crilrqInstanceCount'
--
-- * 'crilrqPriceSchedules'
--
-- * 'crilrqClientToken'
data CreateReservedInstancesListing = CreateReservedInstancesListing'
    { _crilrqReservedInstancesId :: !Text
    , _crilrqInstanceCount       :: !Int
    , _crilrqPriceSchedules      :: ![PriceScheduleSpecification]
    , _crilrqClientToken         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateReservedInstancesListing' smart constructor.
createReservedInstancesListing :: Text -> Int -> Text -> CreateReservedInstancesListing
createReservedInstancesListing pReservedInstancesId pInstanceCount pClientToken =
    CreateReservedInstancesListing'
    { _crilrqReservedInstancesId = pReservedInstancesId
    , _crilrqInstanceCount = pInstanceCount
    , _crilrqPriceSchedules = mempty
    , _crilrqClientToken = pClientToken
    }

-- | The ID of the active Reserved Instance.
crilrqReservedInstancesId :: Lens' CreateReservedInstancesListing Text
crilrqReservedInstancesId = lens _crilrqReservedInstancesId (\ s a -> s{_crilrqReservedInstancesId = a});

-- | The number of instances that are a part of a Reserved Instance account
-- to be listed in the Reserved Instance Marketplace. This number should be
-- less than or equal to the instance count associated with the Reserved
-- Instance ID specified in this call.
crilrqInstanceCount :: Lens' CreateReservedInstancesListing Int
crilrqInstanceCount = lens _crilrqInstanceCount (\ s a -> s{_crilrqInstanceCount = a});

-- | A list specifying the price of the Reserved Instance for each month
-- remaining in the Reserved Instance term.
crilrqPriceSchedules :: Lens' CreateReservedInstancesListing [PriceScheduleSpecification]
crilrqPriceSchedules = lens _crilrqPriceSchedules (\ s a -> s{_crilrqPriceSchedules = a});

-- | Unique, case-sensitive identifier you provide to ensure idempotency of
-- your listings. This helps avoid duplicate listings. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
crilrqClientToken :: Lens' CreateReservedInstancesListing Text
crilrqClientToken = lens _crilrqClientToken (\ s a -> s{_crilrqClientToken = a});

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
               "ReservedInstancesId" =: _crilrqReservedInstancesId,
               "InstanceCount" =: _crilrqInstanceCount,
               toQueryList "item" _crilrqPriceSchedules,
               "ClientToken" =: _crilrqClientToken]

-- | /See:/ 'createReservedInstancesListingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crersReservedInstancesListings'
--
-- * 'crersStatus'
data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse'
    { _crersReservedInstancesListings :: !(Maybe [ReservedInstancesListing])
    , _crersStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateReservedInstancesListingResponse' smart constructor.
createReservedInstancesListingResponse :: Int -> CreateReservedInstancesListingResponse
createReservedInstancesListingResponse pStatus =
    CreateReservedInstancesListingResponse'
    { _crersReservedInstancesListings = Nothing
    , _crersStatus = pStatus
    }

-- | Information about the Reserved Instances listing.
crersReservedInstancesListings :: Lens' CreateReservedInstancesListingResponse [ReservedInstancesListing]
crersReservedInstancesListings = lens _crersReservedInstancesListings (\ s a -> s{_crersReservedInstancesListings = a}) . _Default;

-- | FIXME: Undocumented member.
crersStatus :: Lens' CreateReservedInstancesListingResponse Int
crersStatus = lens _crersStatus (\ s a -> s{_crersStatus = a});
