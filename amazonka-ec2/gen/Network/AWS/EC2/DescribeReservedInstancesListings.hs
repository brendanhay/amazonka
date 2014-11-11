{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- Instance Marketplace. The Reserved Instance Marketplace matches sellers who
-- want to resell Reserved Instance capacity that they no longer need with
-- buyers who want to purchase additional capacity. Reserved Instances bought
-- and sold through the Reserved Instance Marketplace work like any other
-- Reserved Instances. As a seller, you choose to list some or all of your
-- Reserved Instances, and you specify the upfront price to receive for them.
-- Your Reserved Instances are then listed in the Reserved Instance
-- Marketplace and are available for purchase. As a buyer, you specify the
-- configuration of the Reserved Instance to purchase, and the Marketplace
-- matches what you're searching for with what's available. The Marketplace
-- first sells the lowest priced Reserved Instances to you, and continues to
-- sell available Reserved Instance listings to you until your demand is met.
-- You are charged based on the total price of all of the listings that you
-- purchase. For more information, see Reserved Instance Marketplace in the
-- Amazon Elastic Compute Cloud User Guide.
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
    , DescribeReservedInstancesListingsResult
    -- ** Response constructor
    , describeReservedInstancesListingsResult
    -- ** Response lenses
    , drilrReservedInstancesListings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeReservedInstancesListings = DescribeReservedInstancesListings
    { _drilFilters                    :: [Filter]
    , _drilReservedInstancesId        :: Maybe Text
    , _drilReservedInstancesListingId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeReservedInstancesListings' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drilFilters' @::@ ['Filter']
--
-- * 'drilReservedInstancesId' @::@ 'Maybe' 'Text'
--
-- * 'drilReservedInstancesListingId' @::@ 'Maybe' 'Text'
--
describeReservedInstancesListings :: DescribeReservedInstancesListings
describeReservedInstancesListings = DescribeReservedInstancesListings
    { _drilReservedInstancesId        = Nothing
    , _drilReservedInstancesListingId = Nothing
    , _drilFilters                    = mempty
    }

-- | One or more filters. reserved-instances-id - The ID of the Reserved
-- Instances. reserved-instances-listing-id - The ID of the Reserved
-- Instances listing. status - The status of the Reserved Instance listing
-- (pending | active | cancelled | closed). status-message - The reason for
-- the status.
drilFilters :: Lens' DescribeReservedInstancesListings [Filter]
drilFilters = lens _drilFilters (\s a -> s { _drilFilters = a })

-- | One or more Reserved Instance IDs.
drilReservedInstancesId :: Lens' DescribeReservedInstancesListings (Maybe Text)
drilReservedInstancesId =
    lens _drilReservedInstancesId (\s a -> s { _drilReservedInstancesId = a })

-- | One or more Reserved Instance Listing IDs.
drilReservedInstancesListingId :: Lens' DescribeReservedInstancesListings (Maybe Text)
drilReservedInstancesListingId =
    lens _drilReservedInstancesListingId
        (\s a -> s { _drilReservedInstancesListingId = a })
instance ToQuery DescribeReservedInstancesListings

instance ToPath DescribeReservedInstancesListings where
    toPath = const "/"

newtype DescribeReservedInstancesListingsResult = DescribeReservedInstancesListingsResult
    { _drilrReservedInstancesListings :: [ReservedInstancesListing]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DescribeReservedInstancesListingsResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drilrReservedInstancesListings' @::@ ['ReservedInstancesListing']
--
describeReservedInstancesListingsResult :: DescribeReservedInstancesListingsResult
describeReservedInstancesListingsResult = DescribeReservedInstancesListingsResult
    { _drilrReservedInstancesListings = mempty
    }

-- | Information about the Reserved Instance listing.
drilrReservedInstancesListings :: Lens' DescribeReservedInstancesListingsResult [ReservedInstancesListing]
drilrReservedInstancesListings =
    lens _drilrReservedInstancesListings
        (\s a -> s { _drilrReservedInstancesListings = a })
instance FromXML DescribeReservedInstancesListingsResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeReservedInstancesListingsResult"

instance AWSRequest DescribeReservedInstancesListings where
    type Sv DescribeReservedInstancesListings = EC2
    type Rs DescribeReservedInstancesListings = DescribeReservedInstancesListingsResult

    request  = post "DescribeReservedInstancesListings"
    response = xmlResponse $ \h x -> DescribeReservedInstancesListingsResult
        <$> x %| "reservedInstancesListingsSet"
