{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeReservedInstancesListings
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

-- | Describes your account\'s Reserved Instance listings in the Reserved
-- Instance Marketplace.
--
-- The Reserved Instance Marketplace matches sellers who want to resell
-- Reserved Instance capacity that they no longer need with buyers who want
-- to purchase additional capacity. Reserved Instances bought and sold
-- through the Reserved Instance Marketplace work like any other Reserved
-- Instances.
--
-- As a seller, you choose to list some or all of your Reserved Instances,
-- and you specify the upfront price to receive for them. Your Reserved
-- Instances are then listed in the Reserved Instance Marketplace and are
-- available for purchase.
--
-- As a buyer, you specify the configuration of the Reserved Instance to
-- purchase, and the Marketplace matches what you\'re searching for with
-- what\'s available. The Marketplace first sells the lowest priced
-- Reserved Instances to you, and continues to sell available Reserved
-- Instance listings to you until your demand is met. You are charged based
-- on the total price of all of the listings that you purchase.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstancesListings.html>
module Network.AWS.EC2.DescribeReservedInstancesListings
    (
    -- * Request
      DescribeReservedInstancesListings
    -- ** Request constructor
    , describeReservedInstancesListings
    -- ** Request lenses
    , drilFilters
    , drilReservedInstancesId
    , drilReservedInstancesListingId

    -- * Response
    , DescribeReservedInstancesListingsResponse
    -- ** Response constructor
    , describeReservedInstancesListingsResponse
    -- ** Response lenses
    , drilrReservedInstancesListings
    , drilrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeReservedInstancesListings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drilFilters'
--
-- * 'drilReservedInstancesId'
--
-- * 'drilReservedInstancesListingId'
data DescribeReservedInstancesListings = DescribeReservedInstancesListings'
    { _drilFilters                    :: !(Maybe [Filter])
    , _drilReservedInstancesId        :: !(Maybe Text)
    , _drilReservedInstancesListingId :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeReservedInstancesListings' smart constructor.
describeReservedInstancesListings :: DescribeReservedInstancesListings
describeReservedInstancesListings =
    DescribeReservedInstancesListings'
    { _drilFilters = Nothing
    , _drilReservedInstancesId = Nothing
    , _drilReservedInstancesListingId = Nothing
    }

-- | One or more filters.
--
-- -   @reserved-instances-id@ - The ID of the Reserved Instances.
--
-- -   @reserved-instances-listing-id@ - The ID of the Reserved Instances
--     listing.
--
-- -   @status@ - The status of the Reserved Instance listing (@pending@ |
--     @active@ | @cancelled@ | @closed@).
--
-- -   @status-message@ - The reason for the status.
--
drilFilters :: Lens' DescribeReservedInstancesListings [Filter]
drilFilters = lens _drilFilters (\ s a -> s{_drilFilters = a}) . _Default;

-- | One or more Reserved Instance IDs.
drilReservedInstancesId :: Lens' DescribeReservedInstancesListings (Maybe Text)
drilReservedInstancesId = lens _drilReservedInstancesId (\ s a -> s{_drilReservedInstancesId = a});

-- | One or more Reserved Instance Listing IDs.
drilReservedInstancesListingId :: Lens' DescribeReservedInstancesListings (Maybe Text)
drilReservedInstancesListingId = lens _drilReservedInstancesListingId (\ s a -> s{_drilReservedInstancesListingId = a});

instance AWSRequest DescribeReservedInstancesListings
         where
        type Sv DescribeReservedInstancesListings = EC2
        type Rs DescribeReservedInstancesListings =
             DescribeReservedInstancesListingsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeReservedInstancesListingsResponse' <$>
                   (may (parseXMLList "item") x) <*> (pure s))

instance ToHeaders DescribeReservedInstancesListings
         where
        toHeaders = const mempty

instance ToPath DescribeReservedInstancesListings
         where
        toPath = const "/"

instance ToQuery DescribeReservedInstancesListings
         where
        toQuery DescribeReservedInstancesListings'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReservedInstancesListings" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _drilFilters),
               "ReservedInstancesId" =: _drilReservedInstancesId,
               "ReservedInstancesListingId" =:
                 _drilReservedInstancesListingId]

-- | /See:/ 'describeReservedInstancesListingsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drilrReservedInstancesListings'
--
-- * 'drilrStatus'
data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse'
    { _drilrReservedInstancesListings :: !(Maybe [ReservedInstancesListing])
    , _drilrStatus                    :: !Status
    } deriving (Eq,Show)

-- | 'DescribeReservedInstancesListingsResponse' smart constructor.
describeReservedInstancesListingsResponse :: Status -> DescribeReservedInstancesListingsResponse
describeReservedInstancesListingsResponse pStatus =
    DescribeReservedInstancesListingsResponse'
    { _drilrReservedInstancesListings = Nothing
    , _drilrStatus = pStatus
    }

-- | Information about the Reserved Instance listing.
drilrReservedInstancesListings :: Lens' DescribeReservedInstancesListingsResponse [ReservedInstancesListing]
drilrReservedInstancesListings = lens _drilrReservedInstancesListings (\ s a -> s{_drilrReservedInstancesListings = a}) . _Default;

-- | FIXME: Undocumented member.
drilrStatus :: Lens' DescribeReservedInstancesListingsResponse Status
drilrStatus = lens _drilrStatus (\ s a -> s{_drilrStatus = a});
