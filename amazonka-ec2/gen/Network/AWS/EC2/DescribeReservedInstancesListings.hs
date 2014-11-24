{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
-- purchase. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html
-- Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User
-- Guide/.
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
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeReservedInstancesListings = DescribeReservedInstancesListings
    { _drilFilters                    :: List "Filter" Filter
    , _drilReservedInstancesId        :: Maybe Text
    , _drilReservedInstancesListingId :: Maybe Text
    } deriving (Eq, Show)

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

-- | One or more filters. @reserved-instances-id@ - The ID of the Reserved
-- Instances. @reserved-instances-listing-id@ - The ID of the Reserved
-- Instances listing. @status@ - The status of the Reserved Instance listing
-- (@pending@ | @active@ | @cancelled@ | @closed@). @status-message@ - The
-- reason for the status.
drilFilters :: Lens' DescribeReservedInstancesListings [Filter]
drilFilters = lens _drilFilters (\s a -> s { _drilFilters = a }) . _List

-- | One or more Reserved Instance IDs.
drilReservedInstancesId :: Lens' DescribeReservedInstancesListings (Maybe Text)
drilReservedInstancesId =
    lens _drilReservedInstancesId (\s a -> s { _drilReservedInstancesId = a })

-- | One or more Reserved Instance Listing IDs.
drilReservedInstancesListingId :: Lens' DescribeReservedInstancesListings (Maybe Text)
drilReservedInstancesListingId =
    lens _drilReservedInstancesListingId
        (\s a -> s { _drilReservedInstancesListingId = a })

newtype DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse
    { _drilrReservedInstancesListings :: List "item" ReservedInstancesListing
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeReservedInstancesListingsResponse where
    type Item DescribeReservedInstancesListingsResponse = ReservedInstancesListing

    fromList = DescribeReservedInstancesListingsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _drilrReservedInstancesListings

-- | 'DescribeReservedInstancesListingsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drilrReservedInstancesListings' @::@ ['ReservedInstancesListing']
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
            . _List

instance ToPath DescribeReservedInstancesListings where
    toPath = const "/"

instance ToQuery DescribeReservedInstancesListings where
    toQuery DescribeReservedInstancesListings{..} = mconcat
        [ "filters"                    =? _drilFilters
        , "reservedInstancesId"        =? _drilReservedInstancesId
        , "reservedInstancesListingId" =? _drilReservedInstancesListingId
        ]

instance ToHeaders DescribeReservedInstancesListings

instance AWSRequest DescribeReservedInstancesListings where
    type Sv DescribeReservedInstancesListings = EC2
    type Rs DescribeReservedInstancesListings = DescribeReservedInstancesListingsResponse

    request  = post "DescribeReservedInstancesListings"
    response = xmlResponse

instance FromXML DescribeReservedInstancesListingsResponse where
    parseXML x = DescribeReservedInstancesListingsResponse
        <$> x .@  "reservedInstancesListingsSet"
