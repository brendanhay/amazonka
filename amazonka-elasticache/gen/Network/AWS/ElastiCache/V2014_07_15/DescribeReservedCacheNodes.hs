{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodes
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
-- https://elasticache.amazonaws.com/ ?Action=DescribeReservedCacheNodes
-- &ReservedCacheNodeId=customerSpecifiedID &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= Medium Utilization memcached
-- 649fd0c8-cf6d-47a0-bfa6-060f8e75e95f payment-failed myreservationid 1
-- 2010-12-15T00:25:14.131Z 31536000 227.5 0.046 cache.m1.small
-- c695119b-2961-11e1-bd06-6fe008f046c3.
module Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodes
    (
    -- * Request
      DescribeReservedCacheNodes
    -- ** Request constructor
    , mkDescribeReservedCacheNodesMessage
    -- ** Request lenses
    , drcnmReservedCacheNodeId
    , drcnmReservedCacheNodesOfferingId
    , drcnmCacheNodeType
    , drcnmDuration
    , drcnmProductDescription
    , drcnmOfferingType
    , drcnmMaxRecords
    , drcnmMarker

    -- * Response
    , DescribeReservedCacheNodesResponse
    -- ** Response lenses
    , rcnmMarker
    , rcnmReservedCacheNodes
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedCacheNodes' request.
mkDescribeReservedCacheNodesMessage :: DescribeReservedCacheNodes
mkDescribeReservedCacheNodesMessage = DescribeReservedCacheNodes
    { _drcnmReservedCacheNodeId = Nothing
    , _drcnmReservedCacheNodesOfferingId = Nothing
    , _drcnmCacheNodeType = Nothing
    , _drcnmDuration = Nothing
    , _drcnmProductDescription = Nothing
    , _drcnmOfferingType = Nothing
    , _drcnmMaxRecords = Nothing
    , _drcnmMarker = Nothing
    }
{-# INLINE mkDescribeReservedCacheNodesMessage #-}

data DescribeReservedCacheNodes = DescribeReservedCacheNodes
    { _drcnmReservedCacheNodeId :: Maybe Text
      -- ^ The reserved cache node identifier filter value. Use this
      -- parameter to show only the reservation that matches the specified
      -- reservation ID.
    , _drcnmReservedCacheNodesOfferingId :: Maybe Text
      -- ^ The offering identifier filter value. Use this parameter to show
      -- only purchased reservations matching the specified offering
      -- identifier.
    , _drcnmCacheNodeType :: Maybe Text
      -- ^ The cache node type filter value. Use this parameter to show only
      -- those reservations matching the specified cache node type.
    , _drcnmDuration :: Maybe Text
      -- ^ The duration filter value, specified in years or seconds. Use
      -- this parameter to show only reservations for this duration. Valid
      -- Values: 1 | 3 | 31536000 | 94608000.
    , _drcnmProductDescription :: Maybe Text
      -- ^ The product description filter value. Use this parameter to show
      -- only those reservations matching the specified product
      -- description.
    , _drcnmOfferingType :: Maybe Text
      -- ^ The offering type filter value. Use this parameter to show only
      -- the available offerings matching the specified offering type.
      -- Valid values: "Light Utilization" | "Medium Utilization" | "Heavy
      -- Utilization".
    , _drcnmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _drcnmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The reserved cache node identifier filter value. Use this parameter to show
-- only the reservation that matches the specified reservation ID.
drcnmReservedCacheNodeId :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnmReservedCacheNodeId = lens _drcnmReservedCacheNodeId (\s a -> s { _drcnmReservedCacheNodeId = a })
{-# INLINE drcnmReservedCacheNodeId #-}

-- | The offering identifier filter value. Use this parameter to show only
-- purchased reservations matching the specified offering identifier.
drcnmReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnmReservedCacheNodesOfferingId = lens _drcnmReservedCacheNodesOfferingId (\s a -> s { _drcnmReservedCacheNodesOfferingId = a })
{-# INLINE drcnmReservedCacheNodesOfferingId #-}

-- | The cache node type filter value. Use this parameter to show only those
-- reservations matching the specified cache node type.
drcnmCacheNodeType :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnmCacheNodeType = lens _drcnmCacheNodeType (\s a -> s { _drcnmCacheNodeType = a })
{-# INLINE drcnmCacheNodeType #-}

-- | The duration filter value, specified in years or seconds. Use this
-- parameter to show only reservations for this duration. Valid Values: 1 | 3
-- | 31536000 | 94608000.
drcnmDuration :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnmDuration = lens _drcnmDuration (\s a -> s { _drcnmDuration = a })
{-# INLINE drcnmDuration #-}

-- | The product description filter value. Use this parameter to show only those
-- reservations matching the specified product description.
drcnmProductDescription :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnmProductDescription = lens _drcnmProductDescription (\s a -> s { _drcnmProductDescription = a })
{-# INLINE drcnmProductDescription #-}

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drcnmOfferingType :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnmOfferingType = lens _drcnmOfferingType (\s a -> s { _drcnmOfferingType = a })
{-# INLINE drcnmOfferingType #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drcnmMaxRecords :: Lens' DescribeReservedCacheNodes (Maybe Integer)
drcnmMaxRecords = lens _drcnmMaxRecords (\s a -> s { _drcnmMaxRecords = a })
{-# INLINE drcnmMaxRecords #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
drcnmMarker :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnmMarker = lens _drcnmMarker (\s a -> s { _drcnmMarker = a })
{-# INLINE drcnmMarker #-}

instance ToQuery DescribeReservedCacheNodes where
    toQuery = genericQuery def

data DescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse
    { _rcnmMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , _rcnmReservedCacheNodes :: [ReservedCacheNode]
      -- ^ A list of reserved cache nodes. Each element in the list contains
      -- detailed information about one node.
    } deriving (Show, Generic)

-- | Provides an identifier to allow retrieval of paginated results.
rcnmMarker :: Lens' DescribeReservedCacheNodesResponse (Maybe Text)
rcnmMarker = lens _rcnmMarker (\s a -> s { _rcnmMarker = a })
{-# INLINE rcnmMarker #-}

-- | A list of reserved cache nodes. Each element in the list contains detailed
-- information about one node.
rcnmReservedCacheNodes :: Lens' DescribeReservedCacheNodesResponse ([ReservedCacheNode])
rcnmReservedCacheNodes = lens _rcnmReservedCacheNodes (\s a -> s { _rcnmReservedCacheNodes = a })
{-# INLINE rcnmReservedCacheNodes #-}

instance FromXML DescribeReservedCacheNodesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedCacheNodes where
    type Sv DescribeReservedCacheNodes = ElastiCache
    type Rs DescribeReservedCacheNodes = DescribeReservedCacheNodesResponse

    request = post "DescribeReservedCacheNodes"
    response _ = xmlResponse

instance AWSPager DescribeReservedCacheNodes where
    next rq rs = (\x -> rq { _drcnmMarker = Just x })
        <$> (_rcnmMarker rs)
