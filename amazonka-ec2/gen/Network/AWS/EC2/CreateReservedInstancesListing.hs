{-# LANGUAGE DeriveGeneric               #-}
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
-- at a time. For more information, see Reserved Instance Marketplace in the
-- Amazon Elastic Compute Cloud User Guide. Example This example creates a
-- Reserved Instance Marketplace listing from the specified Reserved Instance,
-- which has 11 months remaining in its term. In this example, we set the
-- upfront price at $2.50, and the price drops over the course of the 11-month
-- term if the instance is still not sold.
-- https://ec2.amazonaws.com/?Action=CreateReservedInstancesListing
-- &amp;ClientToken=myIdempToken1 &amp;InstanceCount=1
-- &amp;PriceSchedules.0.Price=2.5 &amp;PriceSchedules.0.Term=11
-- &amp;PriceSchedules.1.Price=2.0 &amp;PriceSchedules.1.Term=8
-- &amp;PriceSchedules.2.Price=1.5 &amp;PriceSchedules.2.Term=5
-- &amp;PriceSchedules.3.Price=0.7 &amp;PriceSchedules.3.Term=3
-- &amp;PriceSchedules.4.Price=0.1 &amp;PriceSchedules.4.Term=1
-- &amp;ReservedInstancesId=e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE
-- &amp;AUTHPARAMS a42481af-335a-4e9e-b291-bd18dexample
-- 5ec28771-05ff-4b9b-aa31-9e57dEXAMPLE e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE
-- 2012-07-17T17:11:09.449Z 2012-07-17T17:11:09.468Z active ACTIVE Available 1
-- Sold 0 Cancelled 0 Pending 0 11 2.5 USD true 10 2.5 USD false 9 2.5 USD
-- false 8 2.0 USD false 7 2.0 USD false 6 2.0 USD false 5 1.5 USD false 4 1.5
-- USD false 3 0.7 USD false 2 0.7 USD false 1 0.1 USD false myIdempToken1.
module Network.AWS.EC2.CreateReservedInstancesListing
    (
    -- * Request
      CreateReservedInstancesListing
    -- ** Request constructor
    , mkCreateReservedInstancesListing
    -- ** Request lenses
    , cril1ReservedInstancesId
    , cril1InstanceCount
    , cril1PriceSchedules
    , cril1ClientToken

    -- * Response
    , CreateReservedInstancesListingResponse
    -- ** Response constructor
    , mkCreateReservedInstancesListingResponse
    -- ** Response lenses
    , crilrrReservedInstancesListings
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data CreateReservedInstancesListing = CreateReservedInstancesListing
    { _cril1ReservedInstancesId :: !Text
    , _cril1InstanceCount :: !Integer
    , _cril1PriceSchedules :: [PriceScheduleSpecification]
    , _cril1ClientToken :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateReservedInstancesListing' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesId ::@ @Text@
--
-- * @InstanceCount ::@ @Integer@
--
-- * @PriceSchedules ::@ @[PriceScheduleSpecification]@
--
-- * @ClientToken ::@ @Text@
--
mkCreateReservedInstancesListing :: Text -- ^ 'cril1ReservedInstancesId'
                                 -> Integer -- ^ 'cril1InstanceCount'
                                 -> [PriceScheduleSpecification] -- ^ 'cril1PriceSchedules'
                                 -> Text -- ^ 'cril1ClientToken'
                                 -> CreateReservedInstancesListing
mkCreateReservedInstancesListing p1 p2 p3 p4 = CreateReservedInstancesListing
    { _cril1ReservedInstancesId = p1
    , _cril1InstanceCount = p2
    , _cril1PriceSchedules = p3
    , _cril1ClientToken = p4
    }

-- | The ID of the active Reserved Instance.
cril1ReservedInstancesId :: Lens' CreateReservedInstancesListing Text
cril1ReservedInstancesId =
    lens _cril1ReservedInstancesId
         (\s a -> s { _cril1ReservedInstancesId = a })

-- | The number of instances that are a part of a Reserved Instance account to
-- be listed in the Reserved Instance Marketplace. This number should be less
-- than or equal to the instance count associated with the Reserved Instance
-- ID specified in this call.
cril1InstanceCount :: Lens' CreateReservedInstancesListing Integer
cril1InstanceCount =
    lens _cril1InstanceCount (\s a -> s { _cril1InstanceCount = a })

-- | A list specifying the price of the Reserved Instance for each month
-- remaining in the Reserved Instance term.
cril1PriceSchedules :: Lens' CreateReservedInstancesListing [PriceScheduleSpecification]
cril1PriceSchedules =
    lens _cril1PriceSchedules (\s a -> s { _cril1PriceSchedules = a })

-- | Unique, case-sensitive identifier you provide to ensure idempotency of your
-- listings. This helps avoid duplicate listings. For more information, see
-- Ensuring Idempotency in the Amazon Elastic Compute Cloud User Guide.
cril1ClientToken :: Lens' CreateReservedInstancesListing Text
cril1ClientToken =
    lens _cril1ClientToken (\s a -> s { _cril1ClientToken = a })

instance ToQuery CreateReservedInstancesListing where
    toQuery = genericQuery def

newtype CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
    { _crilrrReservedInstancesListings :: [ReservedInstancesListing]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateReservedInstancesListingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesListings ::@ @[ReservedInstancesListing]@
--
mkCreateReservedInstancesListingResponse :: CreateReservedInstancesListingResponse
mkCreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
    { _crilrrReservedInstancesListings = mempty
    }

-- | Information about the Reserved Instances listing.
crilrrReservedInstancesListings :: Lens' CreateReservedInstancesListingResponse [ReservedInstancesListing]
crilrrReservedInstancesListings =
    lens _crilrrReservedInstancesListings
         (\s a -> s { _crilrrReservedInstancesListings = a })

instance FromXML CreateReservedInstancesListingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateReservedInstancesListing where
    type Sv CreateReservedInstancesListing = EC2
    type Rs CreateReservedInstancesListing = CreateReservedInstancesListingResponse

    request = post "CreateReservedInstancesListing"
    response _ = xmlResponse
