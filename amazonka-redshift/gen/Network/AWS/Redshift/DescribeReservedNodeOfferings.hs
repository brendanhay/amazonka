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

-- Module      : Network.AWS.Redshift.DescribeReservedNodeOfferings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the available reserved node offerings by Amazon Redshift
-- with their descriptions including the node type, the fixed and recurring
-- costs of reserving the node and duration the node will be reserved for you.
-- These descriptions help you determine which reserve node offering you want
-- to purchase. You then use the unique offering ID in you call to
-- PurchaseReservedNodeOffering to reserve one or more nodes for your Amazon
-- Redshift cluster. For more information about managing parameter groups, go
-- to Purchasing Reserved Nodes in the Amazon Redshift Management Guide.
module Network.AWS.Redshift.DescribeReservedNodeOfferings
    (
    -- * Request
      DescribeReservedNodeOfferingsMessage
    -- ** Request constructor
    , describeReservedNodeOfferings
    -- ** Request lenses
    , drnomMarker
    , drnomMaxRecords
    , drnomReservedNodeOfferingId

    -- * Response
    , ReservedNodeOfferingsMessage
    -- ** Response constructor
    , describeReservedNodeOfferingsResponse
    -- ** Response lenses
    , rnomMarker
    , rnomReservedNodeOfferings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeReservedNodeOfferingsMessage = DescribeReservedNodeOfferingsMessage
    { _drnomMarker                 :: Maybe Text
    , _drnomMaxRecords             :: Maybe Int
    , _drnomReservedNodeOfferingId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeReservedNodeOfferingsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drnomMarker' @::@ 'Maybe' 'Text'
--
-- * 'drnomMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drnomReservedNodeOfferingId' @::@ 'Maybe' 'Text'
--
describeReservedNodeOfferings :: DescribeReservedNodeOfferingsMessage
describeReservedNodeOfferings = DescribeReservedNodeOfferingsMessage
    { _drnomReservedNodeOfferingId = Nothing
    , _drnomMaxRecords             = Nothing
    , _drnomMarker                 = Nothing
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodeOfferings
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
drnomMarker :: Lens' DescribeReservedNodeOfferingsMessage (Maybe Text)
drnomMarker = lens _drnomMarker (\s a -> s { _drnomMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
drnomMaxRecords :: Lens' DescribeReservedNodeOfferingsMessage (Maybe Int)
drnomMaxRecords = lens _drnomMaxRecords (\s a -> s { _drnomMaxRecords = a })

-- | The unique identifier for the offering.
drnomReservedNodeOfferingId :: Lens' DescribeReservedNodeOfferingsMessage (Maybe Text)
drnomReservedNodeOfferingId =
    lens _drnomReservedNodeOfferingId
        (\s a -> s { _drnomReservedNodeOfferingId = a })

instance ToPath DescribeReservedNodeOfferingsMessage where
    toPath = const "/"

instance ToQuery DescribeReservedNodeOfferingsMessage

data ReservedNodeOfferingsMessage = ReservedNodeOfferingsMessage
    { _rnomMarker                :: Maybe Text
    , _rnomReservedNodeOfferings :: [ReservedNodeOffering]
    } deriving (Eq, Show, Generic)

-- | 'ReservedNodeOfferingsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnomMarker' @::@ 'Maybe' 'Text'
--
-- * 'rnomReservedNodeOfferings' @::@ ['ReservedNodeOffering']
--
describeReservedNodeOfferingsResponse :: ReservedNodeOfferingsMessage
describeReservedNodeOfferingsResponse = ReservedNodeOfferingsMessage
    { _rnomMarker                = Nothing
    , _rnomReservedNodeOfferings = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
rnomMarker :: Lens' ReservedNodeOfferingsMessage (Maybe Text)
rnomMarker = lens _rnomMarker (\s a -> s { _rnomMarker = a })

-- | A list of reserved node offerings.
rnomReservedNodeOfferings :: Lens' ReservedNodeOfferingsMessage [ReservedNodeOffering]
rnomReservedNodeOfferings =
    lens _rnomReservedNodeOfferings
        (\s a -> s { _rnomReservedNodeOfferings = a })

instance AWSRequest DescribeReservedNodeOfferingsMessage where
    type Sv DescribeReservedNodeOfferingsMessage = Redshift
    type Rs DescribeReservedNodeOfferingsMessage = ReservedNodeOfferingsMessage

    request  = post "DescribeReservedNodeOfferings"
    response = xmlResponse $ \h x -> ReservedNodeOfferingsMessage
        <$> x %| "Marker"
        <*> x %| "ReservedNodeOfferings"
