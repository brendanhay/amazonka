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
      DescribeReservedNodesMessage
    -- ** Request constructor
    , describeReservedNodes
    -- ** Request lenses
    , drnmMarker
    , drnmMaxRecords
    , drnmReservedNodeId

    -- * Response
    , ReservedNodesMessage
    -- ** Response constructor
    , describeReservedNodesResponse
    -- ** Response lenses
    , rnmMarker
    , rnmReservedNodes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeReservedNodesMessage = DescribeReservedNodesMessage
    { _drnmMarker         :: Maybe Text
    , _drnmMaxRecords     :: Maybe Int
    , _drnmReservedNodeId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeReservedNodesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drnmMarker' @::@ 'Maybe' 'Text'
--
-- * 'drnmMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drnmReservedNodeId' @::@ 'Maybe' 'Text'
--
describeReservedNodes :: DescribeReservedNodesMessage
describeReservedNodes = DescribeReservedNodesMessage
    { _drnmReservedNodeId = Nothing
    , _drnmMaxRecords     = Nothing
    , _drnmMarker         = Nothing
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodes request
-- exceed the value specified in MaxRecords, AWS returns a value in the
-- Marker field of the response. You can retrieve the next set of response
-- records by providing the returned marker value in the Marker parameter
-- and retrying the request.
drnmMarker :: Lens' DescribeReservedNodesMessage (Maybe Text)
drnmMarker = lens _drnmMarker (\s a -> s { _drnmMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
drnmMaxRecords :: Lens' DescribeReservedNodesMessage (Maybe Int)
drnmMaxRecords = lens _drnmMaxRecords (\s a -> s { _drnmMaxRecords = a })

-- | Identifier for the node reservation.
drnmReservedNodeId :: Lens' DescribeReservedNodesMessage (Maybe Text)
drnmReservedNodeId =
    lens _drnmReservedNodeId (\s a -> s { _drnmReservedNodeId = a })

instance ToQuery DescribeReservedNodesMessage

instance ToPath DescribeReservedNodesMessage where
    toPath = const "/"

data ReservedNodesMessage = ReservedNodesMessage
    { _rnmMarker        :: Maybe Text
    , _rnmReservedNodes :: [ReservedNode]
    } deriving (Eq, Show, Generic)

-- | 'ReservedNodesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnmMarker' @::@ 'Maybe' 'Text'
--
-- * 'rnmReservedNodes' @::@ ['ReservedNode']
--
describeReservedNodesResponse :: ReservedNodesMessage
describeReservedNodesResponse = ReservedNodesMessage
    { _rnmMarker        = Nothing
    , _rnmReservedNodes = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
rnmMarker :: Lens' ReservedNodesMessage (Maybe Text)
rnmMarker = lens _rnmMarker (\s a -> s { _rnmMarker = a })

-- | The list of reserved nodes.
rnmReservedNodes :: Lens' ReservedNodesMessage [ReservedNode]
rnmReservedNodes = lens _rnmReservedNodes (\s a -> s { _rnmReservedNodes = a })

instance FromXML ReservedNodesMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedNodesMessage"

instance AWSRequest DescribeReservedNodesMessage where
    type Sv DescribeReservedNodesMessage = Redshift
    type Rs DescribeReservedNodesMessage = ReservedNodesMessage

    request  = post "DescribeReservedNodes"
    response = xmlResponse $ \h x -> ReservedNodesMessage
        <$> x %| "Marker"
        <*> x %| "ReservedNodes"
