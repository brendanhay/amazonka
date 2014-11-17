{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateReservedInstancesListing
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a listing for Amazon EC2 Reserved Instances to be sold in the
-- Reserved Instance Marketplace. You can submit one Reserved Instance listing
-- at a time. To get a list of your Reserved Instances, you can use the
-- DescribeReservedInstances operation. The Reserved Instance Marketplace
-- matches sellers who want to resell Reserved Instance capacity that they no
-- longer need with buyers who want to purchase additional capacity. Reserved
-- Instances bought and sold through the Reserved Instance Marketplace work
-- like any other Reserved Instances. To sell your Reserved Instances, you
-- must first register as a Seller in the Reserved Instance Marketplace. After
-- completing the registration process, you can create a Reserved Instance
-- Marketplace listing of some or all of your Reserved Instances, and specify
-- the upfront price to receive for them. Your Reserved Instance listings then
-- become available for purchase. To view the details of your Reserved
-- Instance listing, you can use the DescribeReservedInstancesListings
-- operation. For more information, see Reserved Instance Marketplace in the
-- Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateReservedInstancesListing.html>
module Network.AWS.EC2.CreateReservedInstancesListing
    (
    -- * Request
      CreateReservedInstancesListing
    -- ** Request constructor
    , createReservedInstancesListing
    -- ** Request lenses
    , crilClientToken
    , crilInstanceCount
    , crilPriceSchedules
    , crilReservedInstancesId

    -- * Response
    , CreateReservedInstancesListingResponse
    -- ** Response constructor
    , createReservedInstancesListingResponse
    -- ** Response lenses
    , crilr1ReservedInstancesListings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateReservedInstancesListing = CreateReservedInstancesListing
    { _crilClientToken         :: Text
    , _crilInstanceCount       :: Int
    , _crilPriceSchedules      :: [PriceScheduleSpecification]
    , _crilReservedInstancesId :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreateReservedInstancesListing' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crilClientToken' @::@ 'Text'
--
-- * 'crilInstanceCount' @::@ 'Int'
--
-- * 'crilPriceSchedules' @::@ ['PriceScheduleSpecification']
--
-- * 'crilReservedInstancesId' @::@ 'Text'
--
createReservedInstancesListing :: Text -- ^ 'crilReservedInstancesId'
                               -> Int -- ^ 'crilInstanceCount'
                               -> Text -- ^ 'crilClientToken'
                               -> CreateReservedInstancesListing
createReservedInstancesListing p1 p2 p3 = CreateReservedInstancesListing
    { _crilReservedInstancesId = p1
    , _crilInstanceCount       = p2
    , _crilClientToken         = p3
    , _crilPriceSchedules      = mempty
    }

-- | Unique, case-sensitive identifier you provide to ensure idempotency of
-- your listings. This helps avoid duplicate listings. For more information,
-- see Ensuring Idempotency in the Amazon Elastic Compute Cloud User Guide.
crilClientToken :: Lens' CreateReservedInstancesListing Text
crilClientToken = lens _crilClientToken (\s a -> s { _crilClientToken = a })

-- | The number of instances that are a part of a Reserved Instance account to
-- be listed in the Reserved Instance Marketplace. This number should be
-- less than or equal to the instance count associated with the Reserved
-- Instance ID specified in this call.
crilInstanceCount :: Lens' CreateReservedInstancesListing Int
crilInstanceCount =
    lens _crilInstanceCount (\s a -> s { _crilInstanceCount = a })

-- | A list specifying the price of the Reserved Instance for each month
-- remaining in the Reserved Instance term.
crilPriceSchedules :: Lens' CreateReservedInstancesListing [PriceScheduleSpecification]
crilPriceSchedules =
    lens _crilPriceSchedules (\s a -> s { _crilPriceSchedules = a })

-- | The ID of the active Reserved Instance.
crilReservedInstancesId :: Lens' CreateReservedInstancesListing Text
crilReservedInstancesId =
    lens _crilReservedInstancesId (\s a -> s { _crilReservedInstancesId = a })

newtype CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
    { _crilr1ReservedInstancesListings :: [ReservedInstancesListing]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList CreateReservedInstancesListingResponse where
    type Item CreateReservedInstancesListingResponse = ReservedInstancesListing

    fromList = CreateReservedInstancesListingResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _crilr1ReservedInstancesListings

-- | 'CreateReservedInstancesListingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crilr1ReservedInstancesListings' @::@ ['ReservedInstancesListing']
--
createReservedInstancesListingResponse :: CreateReservedInstancesListingResponse
createReservedInstancesListingResponse = CreateReservedInstancesListingResponse
    { _crilr1ReservedInstancesListings = mempty
    }

-- | Information about the Reserved Instances listing.
crilr1ReservedInstancesListings :: Lens' CreateReservedInstancesListingResponse [ReservedInstancesListing]
crilr1ReservedInstancesListings =
    lens _crilr1ReservedInstancesListings
        (\s a -> s { _crilr1ReservedInstancesListings = a })

instance AWSRequest CreateReservedInstancesListing where
    type Sv CreateReservedInstancesListing = EC2
    type Rs CreateReservedInstancesListing = CreateReservedInstancesListingResponse

    request  = post "CreateReservedInstancesListing"
    response = xmlResponse

instance FromXML CreateReservedInstancesListingResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateReservedInstancesListingResponse"

instance ToPath CreateReservedInstancesListing where
    toPath = const "/"

instance ToHeaders CreateReservedInstancesListing

instance ToQuery CreateReservedInstancesListing
