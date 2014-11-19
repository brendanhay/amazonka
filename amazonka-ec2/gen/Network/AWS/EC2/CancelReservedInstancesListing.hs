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
-- Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelReservedInstancesListing.html>
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
    , crilrReservedInstancesListings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

newtype CancelReservedInstancesListing = CancelReservedInstancesListing
    { _crilReservedInstancesListingId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'CancelReservedInstancesListing' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crilReservedInstancesListingId' @::@ 'Text'
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

newtype CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { _crilrReservedInstancesListings :: List "item" ReservedInstancesListing
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList CancelReservedInstancesListingResponse where
    type Item CancelReservedInstancesListingResponse = ReservedInstancesListing

    fromList = CancelReservedInstancesListingResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _crilrReservedInstancesListings

-- | 'CancelReservedInstancesListingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crilrReservedInstancesListings' @::@ ['ReservedInstancesListing']
--
cancelReservedInstancesListingResponse :: CancelReservedInstancesListingResponse
cancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { _crilrReservedInstancesListings = mempty
    }

-- | The Reserved Instance listing.
crilrReservedInstancesListings :: Lens' CancelReservedInstancesListingResponse [ReservedInstancesListing]
crilrReservedInstancesListings =
    lens _crilrReservedInstancesListings
        (\s a -> s { _crilrReservedInstancesListings = a })
            . _List

instance ToPath CancelReservedInstancesListing where
    toPath = const "/"

instance ToQuery CancelReservedInstancesListing

instance ToHeaders CancelReservedInstancesListing

instance AWSRequest CancelReservedInstancesListing where
    type Sv CancelReservedInstancesListing = EC2
    type Rs CancelReservedInstancesListing = CancelReservedInstancesListingResponse

    request  = post "CancelReservedInstancesListing"
    response = xmlResponse

instance FromXML CancelReservedInstancesListingResponse where
    parseXML x = CancelReservedInstancesListingResponse
        <$> x .@ "reservedInstancesListingsSet"
