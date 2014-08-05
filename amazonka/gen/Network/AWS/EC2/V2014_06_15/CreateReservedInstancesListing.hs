{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateReservedInstancesListing
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
module Network.AWS.EC2.V2014_06_15.CreateReservedInstancesListing where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data CreateReservedInstancesListing = CreateReservedInstancesListing
    { _criltInstanceCount :: Integer
      -- ^ The number of instances that are a part of a Reserved Instance
      -- account to be listed in the Reserved Instance Marketplace. This
      -- number should be less than or equal to the instance count
      -- associated with the Reserved Instance ID specified in this call.
    , _criltPriceSchedules :: [PriceScheduleSpecification]
      -- ^ A list specifying the price of the Reserved Instance for each
      -- month remaining in the Reserved Instance term.
    , _criltClientToken :: Text
      -- ^ Unique, case-sensitive identifier you provide to ensure
      -- idempotency of your listings. This helps avoid duplicate
      -- listings. For more information, see Ensuring Idempotency in the
      -- Amazon Elastic Compute Cloud User Guide.
    , _criltReservedInstancesId :: Text
      -- ^ The ID of the active Reserved Instance.
    } deriving (Show, Generic)

makeLenses ''CreateReservedInstancesListing

instance ToQuery CreateReservedInstancesListing where
    toQuery = genericToQuery def

data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
    { _criluReservedInstancesListings :: [ReservedInstancesListing]
      -- ^ Information about the Reserved Instances listing.
    } deriving (Show, Generic)

makeLenses ''CreateReservedInstancesListingResponse

instance AWSRequest CreateReservedInstancesListing where
    type Sv CreateReservedInstancesListing = EC2
    type Rs CreateReservedInstancesListing = CreateReservedInstancesListingResponse

    request = post "CreateReservedInstancesListing"
    response _ = cursorResponse $ \hs xml ->
        pure CreateReservedInstancesListingResponse
            <*> xml %| "ReservedInstancesListingList"
