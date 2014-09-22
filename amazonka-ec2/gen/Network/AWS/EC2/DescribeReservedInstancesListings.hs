{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeReservedInstancesListings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes your account's Reserved Instance listings in the Reserved
-- Instance Marketplace. For more information, see Reserved Instance
-- Marketplace in the Amazon Elastic Compute Cloud User Guide. Example This
-- example shows all the listings associated with your account.
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesListings
-- &amp;AUTHPARAMS cec5c904-8f3a-4de5-8f5a-ff7f9EXAMPLE
-- 253dfbf9-c335-4808-b956-d942cEXAMPLE e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE
-- 2012-07-06T19:35:29.000Z 2012-07-06T19:35:30.000Z active ACTIVE Available
-- 20 Sold 0 Cancelled 0 Pending 0 8 480.0 USD false 7 420.0 USD false 6 360.0
-- USD active 5 300.0 USD false 4 240.0 USD false 3 180.0 USD false 2 120.0
-- USD false 1 60.0 USD false myclienttoken1.
module Network.AWS.EC2.DescribeReservedInstancesListings
    (
    -- * Request
      DescribeReservedInstancesListings
    -- ** Request constructor
    , describeReservedInstancesListings
    -- ** Request lenses
    , drilReservedInstancesId
    , drilReservedInstancesListingId
    , drilFilters

    -- * Response
    , DescribeReservedInstancesListingsResponse
    -- ** Response constructor
    , describeReservedInstancesListingsResponse
    -- ** Response lenses
    , drilrReservedInstancesListings
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeReservedInstancesListings = DescribeReservedInstancesListings
    { _drilReservedInstancesId :: Maybe Text
    , _drilReservedInstancesListingId :: Maybe Text
    , _drilFilters :: [Filter]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedInstancesListings' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesId ::@ @Maybe Text@
--
-- * @ReservedInstancesListingId ::@ @Maybe Text@
--
-- * @Filters ::@ @[Filter]@
--
describeReservedInstancesListings :: DescribeReservedInstancesListings
describeReservedInstancesListings = DescribeReservedInstancesListings
    { _drilReservedInstancesId = Nothing
    , _drilReservedInstancesListingId = Nothing
    , _drilFilters = mempty
    }

-- | One or more Reserved Instance IDs.
drilReservedInstancesId :: Lens' DescribeReservedInstancesListings (Maybe Text)
drilReservedInstancesId =
    lens _drilReservedInstancesId
         (\s a -> s { _drilReservedInstancesId = a })

-- | One or more Reserved Instance Listing IDs.
drilReservedInstancesListingId :: Lens' DescribeReservedInstancesListings (Maybe Text)
drilReservedInstancesListingId =
    lens _drilReservedInstancesListingId
         (\s a -> s { _drilReservedInstancesListingId = a })

-- | One or more filters. reserved-instances-id - The ID of the Reserved
-- Instances. reserved-instances-listing-id - The ID of the Reserved Instances
-- listing. status - The status of the Reserved Instance listing (pending |
-- active | cancelled | closed). status-message - The reason for the status.
drilFilters :: Lens' DescribeReservedInstancesListings [Filter]
drilFilters = lens _drilFilters (\s a -> s { _drilFilters = a })

instance ToQuery DescribeReservedInstancesListings where
    toQuery = genericQuery def

newtype DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse
    { _drilrReservedInstancesListings :: [ReservedInstancesListing]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedInstancesListingsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesListings ::@ @[ReservedInstancesListing]@
--
describeReservedInstancesListingsResponse :: DescribeReservedInstancesListingsResponse
describeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse
    { _drilrReservedInstancesListings = mempty
    }

-- | Information about the Reserved Instance listing.
drilrReservedInstancesListings :: Lens' DescribeReservedInstancesListingsResponse [ReservedInstancesListing]
drilrReservedInstancesListings =
    lens _drilrReservedInstancesListings
         (\s a -> s { _drilrReservedInstancesListings = a })

instance FromXML DescribeReservedInstancesListingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedInstancesListings where
    type Sv DescribeReservedInstancesListings = EC2
    type Rs DescribeReservedInstancesListings = DescribeReservedInstancesListingsResponse

    request = post "DescribeReservedInstancesListings"
    response _ = xmlResponse
