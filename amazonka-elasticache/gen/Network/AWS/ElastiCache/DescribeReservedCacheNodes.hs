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

-- Module      : Network.AWS.ElastiCache.DescribeReservedCacheNodes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeReservedCacheNodes operation returns information about reserved
-- cache nodes for this account, or about a specified reserved cache node.
module Network.AWS.ElastiCache.DescribeReservedCacheNodes
    (
    -- * Request
      DescribeReservedCacheNodesMessage
    -- ** Request constructor
    , describeReservedCacheNodes
    -- ** Request lenses
    , drcnmCacheNodeType
    , drcnmDuration
    , drcnmMarker
    , drcnmMaxRecords
    , drcnmOfferingType
    , drcnmProductDescription
    , drcnmReservedCacheNodeId
    , drcnmReservedCacheNodesOfferingId

    -- * Response
    , ReservedCacheNodeMessage
    -- ** Response constructor
    , describeReservedCacheNodesResponse
    -- ** Response lenses
    , rcnmMarker
    , rcnmReservedCacheNodes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeReservedCacheNodesMessage = DescribeReservedCacheNodesMessage
    { _drcnmCacheNodeType                :: Maybe Text
    , _drcnmDuration                     :: Maybe Text
    , _drcnmMarker                       :: Maybe Text
    , _drcnmMaxRecords                   :: Maybe Int
    , _drcnmOfferingType                 :: Maybe Text
    , _drcnmProductDescription           :: Maybe Text
    , _drcnmReservedCacheNodeId          :: Maybe Text
    , _drcnmReservedCacheNodesOfferingId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeReservedCacheNodesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drcnmCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'drcnmDuration' @::@ 'Maybe' 'Text'
--
-- * 'drcnmMarker' @::@ 'Maybe' 'Text'
--
-- * 'drcnmMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drcnmOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'drcnmProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'drcnmReservedCacheNodeId' @::@ 'Maybe' 'Text'
--
-- * 'drcnmReservedCacheNodesOfferingId' @::@ 'Maybe' 'Text'
--
describeReservedCacheNodes :: DescribeReservedCacheNodesMessage
describeReservedCacheNodes = DescribeReservedCacheNodesMessage
    { _drcnmReservedCacheNodeId          = Nothing
    , _drcnmReservedCacheNodesOfferingId = Nothing
    , _drcnmCacheNodeType                = Nothing
    , _drcnmDuration                     = Nothing
    , _drcnmProductDescription           = Nothing
    , _drcnmOfferingType                 = Nothing
    , _drcnmMaxRecords                   = Nothing
    , _drcnmMarker                       = Nothing
    }

-- | The cache node type filter value. Use this parameter to show only those
-- reservations matching the specified cache node type.
drcnmCacheNodeType :: Lens' DescribeReservedCacheNodesMessage (Maybe Text)
drcnmCacheNodeType =
    lens _drcnmCacheNodeType (\s a -> s { _drcnmCacheNodeType = a })

-- | The duration filter value, specified in years or seconds. Use this
-- parameter to show only reservations for this duration. Valid Values: 1 |
-- 3 | 31536000 | 94608000.
drcnmDuration :: Lens' DescribeReservedCacheNodesMessage (Maybe Text)
drcnmDuration = lens _drcnmDuration (\s a -> s { _drcnmDuration = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
drcnmMarker :: Lens' DescribeReservedCacheNodesMessage (Maybe Text)
drcnmMarker = lens _drcnmMarker (\s a -> s { _drcnmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drcnmMaxRecords :: Lens' DescribeReservedCacheNodesMessage (Maybe Int)
drcnmMaxRecords = lens _drcnmMaxRecords (\s a -> s { _drcnmMaxRecords = a })

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drcnmOfferingType :: Lens' DescribeReservedCacheNodesMessage (Maybe Text)
drcnmOfferingType =
    lens _drcnmOfferingType (\s a -> s { _drcnmOfferingType = a })

-- | The product description filter value. Use this parameter to show only
-- those reservations matching the specified product description.
drcnmProductDescription :: Lens' DescribeReservedCacheNodesMessage (Maybe Text)
drcnmProductDescription =
    lens _drcnmProductDescription (\s a -> s { _drcnmProductDescription = a })

-- | The reserved cache node identifier filter value. Use this parameter to
-- show only the reservation that matches the specified reservation ID.
drcnmReservedCacheNodeId :: Lens' DescribeReservedCacheNodesMessage (Maybe Text)
drcnmReservedCacheNodeId =
    lens _drcnmReservedCacheNodeId
        (\s a -> s { _drcnmReservedCacheNodeId = a })

-- | The offering identifier filter value. Use this parameter to show only
-- purchased reservations matching the specified offering identifier.
drcnmReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodesMessage (Maybe Text)
drcnmReservedCacheNodesOfferingId =
    lens _drcnmReservedCacheNodesOfferingId
        (\s a -> s { _drcnmReservedCacheNodesOfferingId = a })

instance ToQuery DescribeReservedCacheNodesMessage

instance ToPath DescribeReservedCacheNodesMessage where
    toPath = const "/"

data ReservedCacheNodeMessage = ReservedCacheNodeMessage
    { _rcnmMarker             :: Maybe Text
    , _rcnmReservedCacheNodes :: [ReservedCacheNode]
    } deriving (Eq, Show, Generic)

-- | 'ReservedCacheNodeMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcnmMarker' @::@ 'Maybe' 'Text'
--
-- * 'rcnmReservedCacheNodes' @::@ ['ReservedCacheNode']
--
describeReservedCacheNodesResponse :: ReservedCacheNodeMessage
describeReservedCacheNodesResponse = ReservedCacheNodeMessage
    { _rcnmMarker             = Nothing
    , _rcnmReservedCacheNodes = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
rcnmMarker :: Lens' ReservedCacheNodeMessage (Maybe Text)
rcnmMarker = lens _rcnmMarker (\s a -> s { _rcnmMarker = a })

-- | A list of reserved cache nodes. Each element in the list contains
-- detailed information about one node.
rcnmReservedCacheNodes :: Lens' ReservedCacheNodeMessage [ReservedCacheNode]
rcnmReservedCacheNodes =
    lens _rcnmReservedCacheNodes (\s a -> s { _rcnmReservedCacheNodes = a })

instance FromXML ReservedCacheNodeMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedCacheNodeMessage"

instance AWSRequest DescribeReservedCacheNodesMessage where
    type Sv DescribeReservedCacheNodesMessage = ElastiCache
    type Rs DescribeReservedCacheNodesMessage = ReservedCacheNodeMessage

    request  = post "DescribeReservedCacheNodes"
    response = xmlResponse $ \h x -> ReservedCacheNodeMessage
        <$> x %| "Marker"
        <*> x %| "ReservedCacheNodes"
