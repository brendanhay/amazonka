{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodes
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
-- &ReservedCacheNodeId=customerSpecifiedID &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2012-12-18T18%3A31%3A36.118Z
-- &AWSAccessKeyId= &Signature= Medium Utilization memcached
-- 649fd0c8-cf6d-47a0-bfa6-060f8e75e95f payment-failed myreservationid 1
-- 2010-12-15T00:25:14.131Z 31536000 227.5 0.046 cache.m1.small
-- c695119b-2961-11e1-bd06-6fe008f046c3.
module Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodes where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeReservedCacheNodes' request.
describeReservedCacheNodes :: DescribeReservedCacheNodes
describeReservedCacheNodes = DescribeReservedCacheNodes
    { _drcnmMaxRecords = Nothing
    , _drcnmCacheNodeType = Nothing
    , _drcnmProductDescription = Nothing
    , _drcnmMarker = Nothing
    , _drcnmReservedCacheNodeId = Nothing
    , _drcnmOfferingType = Nothing
    , _drcnmDuration = Nothing
    , _drcnmReservedCacheNodesOfferingId = Nothing
    }

data DescribeReservedCacheNodes = DescribeReservedCacheNodes
    { _drcnmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _drcnmCacheNodeType :: Maybe Text
      -- ^ The cache node type filter value. Use this parameter to show only
      -- those reservations matching the specified cache node type.
    , _drcnmProductDescription :: Maybe Text
      -- ^ The product description filter value. Use this parameter to show
      -- only those reservations matching the specified product
      -- description.
    , _drcnmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    , _drcnmReservedCacheNodeId :: Maybe Text
      -- ^ The reserved cache node identifier filter value. Use this
      -- parameter to show only the reservation that matches the specified
      -- reservation ID.
    , _drcnmOfferingType :: Maybe Text
      -- ^ The offering type filter value. Use this parameter to show only
      -- the available offerings matching the specified offering type.
      -- Valid values: "Light Utilization" | "Medium Utilization" | "Heavy
      -- Utilization".
    , _drcnmDuration :: Maybe Text
      -- ^ The duration filter value, specified in years or seconds. Use
      -- this parameter to show only reservations for this duration. Valid
      -- Values: 1 | 3 | 31536000 | 94608000.
    , _drcnmReservedCacheNodesOfferingId :: Maybe Text
      -- ^ The offering identifier filter value. Use this parameter to show
      -- only purchased reservations matching the specified offering
      -- identifier.
    } deriving (Generic)

makeLenses ''DescribeReservedCacheNodes

instance ToQuery DescribeReservedCacheNodes where
    toQuery = genericToQuery def

data DescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse
    { _rcnmReservedCacheNodes :: [ReservedCacheNode]
      -- ^ A list of reserved cache nodes. Each element in the list contains
      -- detailed information about one node.
    , _rcnmMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Generic)

makeLenses ''DescribeReservedCacheNodesResponse

instance FromXML DescribeReservedCacheNodesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedCacheNodes where
    type Sv DescribeReservedCacheNodes = ElastiCache
    type Rs DescribeReservedCacheNodes = DescribeReservedCacheNodesResponse

    request = post "DescribeReservedCacheNodes"
    response _ = xmlResponse

instance AWSPager DescribeReservedCacheNodes where
    next rq rs = (\x -> rq { Keyed "_drcnmMarker" = Just x })
        <$> (Keyed "_rcnmMarker" rs)
