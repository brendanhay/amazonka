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

-- Module      : Network.AWS.Redshift.DescribeReservedNodes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the descriptions of the reserved nodes.
module Network.AWS.Redshift.DescribeReservedNodes
    (
    -- * Request
      DescribeReservedNodes
    -- ** Request constructor
    , describeReservedNodes
    -- ** Request lenses
    , drnMarker
    , drnMaxRecords
    , drnReservedNodeId

    -- * Response
    , DescribeReservedNodesResponse
    -- ** Response constructor
    , describeReservedNodesResponse
    -- ** Response lenses
    , drnrMarker
    , drnrReservedNodes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeReservedNodes = DescribeReservedNodes
    { _drnMarker         :: Maybe Text
    , _drnMaxRecords     :: Maybe Int
    , _drnReservedNodeId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeReservedNodes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drnMarker' @::@ 'Maybe' 'Text'
--
-- * 'drnMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drnReservedNodeId' @::@ 'Maybe' 'Text'
--
describeReservedNodes :: DescribeReservedNodes
describeReservedNodes = DescribeReservedNodes
    { _drnReservedNodeId = Nothing
    , _drnMaxRecords     = Nothing
    , _drnMarker         = Nothing
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodes request
-- exceed the value specified in MaxRecords, AWS returns a value in the
-- Marker field of the response. You can retrieve the next set of response
-- records by providing the returned marker value in the Marker parameter
-- and retrying the request.
drnMarker :: Lens' DescribeReservedNodes (Maybe Text)
drnMarker = lens _drnMarker (\s a -> s { _drnMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
drnMaxRecords :: Lens' DescribeReservedNodes (Maybe Int)
drnMaxRecords = lens _drnMaxRecords (\s a -> s { _drnMaxRecords = a })

-- | Identifier for the node reservation.
drnReservedNodeId :: Lens' DescribeReservedNodes (Maybe Text)
drnReservedNodeId =
    lens _drnReservedNodeId (\s a -> s { _drnReservedNodeId = a })

instance ToQuery DescribeReservedNodes

instance ToPath DescribeReservedNodes where
    toPath = const "/"

data DescribeReservedNodesResponse = DescribeReservedNodesResponse
    { _drnrMarker        :: Maybe Text
    , _drnrReservedNodes :: [ReservedNode]
    } deriving (Eq, Show, Generic)

-- | 'DescribeReservedNodesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drnrMarker' @::@ 'Maybe' 'Text'
--
-- * 'drnrReservedNodes' @::@ ['ReservedNode']
--
describeReservedNodesResponse :: DescribeReservedNodesResponse
describeReservedNodesResponse = DescribeReservedNodesResponse
    { _drnrMarker        = Nothing
    , _drnrReservedNodes = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
drnrMarker :: Lens' DescribeReservedNodesResponse (Maybe Text)
drnrMarker = lens _drnrMarker (\s a -> s { _drnrMarker = a })

-- | The list of reserved nodes.
drnrReservedNodes :: Lens' DescribeReservedNodesResponse [ReservedNode]
drnrReservedNodes =
    lens _drnrReservedNodes (\s a -> s { _drnrReservedNodes = a })

instance FromXML DescribeReservedNodesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeReservedNodesResponse"

instance AWSRequest DescribeReservedNodes where
    type Sv DescribeReservedNodes = Redshift
    type Rs DescribeReservedNodes = DescribeReservedNodesResponse

    request  = post "DescribeReservedNodes"
    response = xmlResponse $ \h x -> DescribeReservedNodesResponse
        <$> x %| "Marker"
        <*> x %| "ReservedNodes"
