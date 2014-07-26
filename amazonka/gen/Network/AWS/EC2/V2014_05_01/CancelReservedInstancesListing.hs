{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.CancelReservedInstancesListing
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels the specified Reserved Instance listing in the Reserved Instance
-- Marketplace. For more information, see Reserved Instance Marketplace in the
-- Amazon Elastic Compute Cloud User Guide. Example This example request
-- cancels a Reserved Instance listing in the Reserved Instance Marketplace.
-- https://ec2.amazonaws.com/?Action=CancelReservedInstancesListing
-- &amp;ReservedInstancesListingId=3ebe97b5-f273-43b6-a204-7a18cEXAMPLE
-- &amp;AUTHPARAMS bec2cf62-98ef-434a-8a15-886fcexample
-- 3ebe97b5-f273-43b6-a204-7a18cEXAMPLE e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE
-- 2012-07-12T16:55:28.000Z 2012-07-12T16:55:28.000Z cancelled CANCELLED
-- Available 0 Sold 0 Cancelled 1 Pending 0 5 166.64 USD false 4 133.32 USD
-- false 3 99.99 USD false 2 66.66 USD false 1 33.33 USD false
-- XqJIt1342112125076.
module Network.AWS.EC2.V2014_05_01.CancelReservedInstancesListing where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CancelReservedInstancesListing = CancelReservedInstancesListing
    { _crilrReservedInstancesListingId :: Text
      -- ^ The ID of the Reserved Instance listing.
    } deriving (Generic)

instance ToQuery CancelReservedInstancesListing where
    toQuery = genericToQuery def

instance AWSRequest CancelReservedInstancesListing where
    type Sv CancelReservedInstancesListing = EC2
    type Rs CancelReservedInstancesListing = CancelReservedInstancesListingResponse

    request = post "CancelReservedInstancesListing"

    response _ = xmlResponse

data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { _crilsReservedInstancesListings :: [ReservedInstancesListing]
      -- ^ The Reserved Instance listing.
    } deriving (Generic)

instance FromXML CancelReservedInstancesListingResponse where
    fromXMLOptions = xmlOptions
