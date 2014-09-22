{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CancelReservedInstancesListing
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
module Network.AWS.EC2.CancelReservedInstancesListing
    (
    -- * Request
      CancelReservedInstancesListing
    -- ** Request constructor
    , cancelReservedInstancesListing
    -- ** Request lenses
    , crilReservedInstancesListingId

    -- * Response
    , CancelReservedInstancesListingResponse
    -- ** Response constructor
    , cancelReservedInstancesListingResponse
    -- ** Response lenses
    , crilrItem
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype CancelReservedInstancesListing = CancelReservedInstancesListing
    { _crilReservedInstancesListingId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CancelReservedInstancesListing' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesListingId ::@ @Text@
--
cancelReservedInstancesListing :: Text -- ^ 'crilReservedInstancesListingId'
                               -> CancelReservedInstancesListing
cancelReservedInstancesListing p1 = CancelReservedInstancesListing
    { _crilReservedInstancesListingId = p1
    }

-- | The ID of the Reserved Instance listing.
crilReservedInstancesListingId :: Lens' CancelReservedInstancesListing Text
crilReservedInstancesListingId =
    lens _crilReservedInstancesListingId
         (\s a -> s { _crilReservedInstancesListingId = a })

instance ToQuery CancelReservedInstancesListing where
    toQuery = genericQuery def

newtype CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { _crilrItem :: [ReservedInstancesListing]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CancelReservedInstancesListingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @[ReservedInstancesListing]@
--
cancelReservedInstancesListingResponse :: CancelReservedInstancesListingResponse
cancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { _crilrItem = mempty
    }

-- | The Reserved Instance listing.
crilrItem :: Lens' CancelReservedInstancesListingResponse [ReservedInstancesListing]
crilrItem = lens _crilrItem (\s a -> s { _crilrItem = a })

instance FromXML CancelReservedInstancesListingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CancelReservedInstancesListing where
    type Sv CancelReservedInstancesListing = EC2
    type Rs CancelReservedInstancesListing = CancelReservedInstancesListingResponse

    request = post "CancelReservedInstancesListing"
    response _ = xmlResponse
