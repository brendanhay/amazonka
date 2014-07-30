{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodesOfferings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeReservedCacheNodesOfferings operation lists available reserved
-- cache node offerings. https://elasticache.amazonaws.com/
-- ?Action=DescribeReservedCacheNodesOfferings
-- &ReservedCacheNodesOfferingId=438012d3-4052-4cc7-b2e3-8d3372e0e706
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-18T18%3A31%3A36.118Z &AWSAccessKeyId= &Signature=
-- 31536000 Heavy Utilization Hourly 0.123 162.0 memcached 0.0
-- SampleOfferingId cache.m1.small 521b420a-2961-11e1-bd06-6fe008f046c3.
module Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodesOfferings where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ElastiCache.V2014_03_24.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeReservedCacheNodesOfferings' request.
describeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings
describeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings
    { _drcnomMaxRecords = Nothing
    , _drcnomCacheNodeType = Nothing
    , _drcnomProductDescription = Nothing
    , _drcnomMarker = Nothing
    , _drcnomOfferingType = Nothing
    , _drcnomDuration = Nothing
    , _drcnomReservedCacheNodesOfferingId = Nothing
    }

data DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings
    { _drcnomMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _drcnomCacheNodeType :: Maybe Text
      -- ^ The cache node type filter value. Use this parameter to show only
      -- the available offerings matching the specified cache node type.
    , _drcnomProductDescription :: Maybe Text
      -- ^ The product description filter value. Use this parameter to show
      -- only the available offerings matching the specified product
      -- description.
    , _drcnomMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    , _drcnomOfferingType :: Maybe Text
      -- ^ The offering type filter value. Use this parameter to show only
      -- the available offerings matching the specified offering type.
      -- Valid Values: "Light Utilization" | "Medium Utilization" | "Heavy
      -- Utilization".
    , _drcnomDuration :: Maybe Text
      -- ^ Duration filter value, specified in years or seconds. Use this
      -- parameter to show only reservations for a given duration. Valid
      -- Values: 1 | 3 | 31536000 | 94608000.
    , _drcnomReservedCacheNodesOfferingId :: Maybe Text
      -- ^ The offering identifier filter value. Use this parameter to show
      -- only the available offering that matches the specified
      -- reservation identifier. Example:
      -- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
    } deriving (Generic)

instance ToQuery DescribeReservedCacheNodesOfferings where
    toQuery = genericToQuery def

instance AWSRequest DescribeReservedCacheNodesOfferings where
    type Sv DescribeReservedCacheNodesOfferings = ElastiCache
    type Rs DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferingsResponse

    request = post "DescribeReservedCacheNodesOfferings"
    response _ = xmlResponse

instance AWSPager DescribeReservedCacheNodesOfferings where
    next rq rs = (\x -> rq { _drcnomMarker = Just x })
        <$> _rcnomMarker rs

data DescribeReservedCacheNodesOfferingsResponse = DescribeReservedCacheNodesOfferingsResponse
    { _rcnomReservedCacheNodesOfferings :: [ReservedCacheNodesOffering]
      -- ^ A list of reserved cache node offerings. Each element in the list
      -- contains detailed information about one offering.
    , _rcnomMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Generic)

instance FromXML DescribeReservedCacheNodesOfferingsResponse where
    fromXMLOptions = xmlOptions
