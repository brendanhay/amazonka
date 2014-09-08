{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeReservedNodes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the descriptions of the reserved nodes.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeReservedNodes
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130125/us-east-1/redshift/aws4_request
-- &x-amz-date=20130125T202355Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 2013-01-22T18:46:48.600Z
-- Medium Utilization 31536000 800.0 0.158 payment-pending dw1.xlarge 1
-- 4357912c-9266-469d-beb0-0f1b775e1bc9 2013-01-22T20:09:16.630Z Heavy
-- Utilization 94608000 Hourly 0.21 12452.0 0.0 payment-pending dw1.8xlarge 2
-- 93bbbca2-e88c-4b8b-a600-b64eaabf18a3 2013-01-23T21:49:32.517Z Medium
-- Utilization 31536000 800.0 0.158 payment-pending dw1.xlarge 1
-- bbcd9749-f2ea-4d01-9b1b-b576f618eb4e 24dc90c8-672d-11e2-b2e1-8f41f0379151.
module Network.AWS.Redshift.V2012_12_01.DescribeReservedNodes
    (
    -- * Request
      DescribeReservedNodes
    -- ** Request constructor
    , mkDescribeReservedNodes
    -- ** Request lenses
    , drnReservedNodeId
    , drnMaxRecords
    , drnMarker

    -- * Response
    , DescribeReservedNodesResponse
    -- ** Response lenses
    , drnrMarker
    , drnrReservedNodes
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
data DescribeReservedNodes = DescribeReservedNodes
    { _drnReservedNodeId :: Maybe Text
    , _drnMaxRecords :: Maybe Integer
    , _drnMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedNodes' request.
mkDescribeReservedNodes :: DescribeReservedNodes
mkDescribeReservedNodes = DescribeReservedNodes
    { _drnReservedNodeId = Nothing
    , _drnMaxRecords = Nothing
    , _drnMarker = Nothing
    }

-- | Identifier for the node reservation.
drnReservedNodeId :: Lens' DescribeReservedNodes (Maybe Text)
drnReservedNodeId =
    lens _drnReservedNodeId (\s a -> s { _drnReservedNodeId = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
drnMaxRecords :: Lens' DescribeReservedNodes (Maybe Integer)
drnMaxRecords = lens _drnMaxRecords (\s a -> s { _drnMaxRecords = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeReservedNodes request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
drnMarker :: Lens' DescribeReservedNodes (Maybe Text)
drnMarker = lens _drnMarker (\s a -> s { _drnMarker = a })

instance ToQuery DescribeReservedNodes where
    toQuery = genericQuery def

-- | Contains the output from the DescribeReservedNodes action.
data DescribeReservedNodesResponse = DescribeReservedNodesResponse
    { _drnrMarker :: Maybe Text
    , _drnrReservedNodes :: [ReservedNode]
    } deriving (Show, Generic)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
drnrMarker :: Lens' DescribeReservedNodesResponse (Maybe Text)
drnrMarker = lens _drnrMarker (\s a -> s { _drnrMarker = a })

-- | The list of reserved nodes.
drnrReservedNodes :: Lens' DescribeReservedNodesResponse [ReservedNode]
drnrReservedNodes =
    lens _drnrReservedNodes (\s a -> s { _drnrReservedNodes = a })

instance FromXML DescribeReservedNodesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedNodes where
    type Sv DescribeReservedNodes = Redshift
    type Rs DescribeReservedNodes = DescribeReservedNodesResponse

    request = post "DescribeReservedNodes"
    response _ = xmlResponse

instance AWSPager DescribeReservedNodes where
    next rq rs = (\x -> rq & drnMarker ?~ x)
        <$> (rs ^. drnrMarker)
