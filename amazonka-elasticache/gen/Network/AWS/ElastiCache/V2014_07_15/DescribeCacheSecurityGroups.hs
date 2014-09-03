{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeCacheSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheSecurityGroups operation returns a list of cache security
-- group descriptions. If a cache security group name is specified, the list
-- will contain only the description of that group.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheSecurityGroups &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= default 123456789012 default mycachesecuritygroup
-- 123456789012 My Security Group a95360ae-b7fc-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheSecurityGroups
    (
    -- * Request
      DescribeCacheSecurityGroups
    -- ** Request constructor
    , describeCacheSecurityGroups
    -- ** Request lenses
    , dcsgoMaxRecords
    , dcsgoCacheSecurityGroupName
    , dcsgoMarker

    -- * Response
    , DescribeCacheSecurityGroupsResponse
    -- ** Response lenses
    , csgpCacheSecurityGroups
    , csgpMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeCacheSecurityGroups' request.
describeCacheSecurityGroups :: DescribeCacheSecurityGroups
describeCacheSecurityGroups = DescribeCacheSecurityGroups
    { _dcsgoMaxRecords = Nothing
    , _dcsgoCacheSecurityGroupName = Nothing
    , _dcsgoMarker = Nothing
    }

data DescribeCacheSecurityGroups = DescribeCacheSecurityGroups
    { _dcsgoMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _dcsgoCacheSecurityGroupName :: Maybe Text
      -- ^ The name of the cache security group to return details for.
    , _dcsgoMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcsgoMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeCacheSecurityGroups
    -> f DescribeCacheSecurityGroups
dcsgoMaxRecords f x =
    (\y -> x { _dcsgoMaxRecords = y })
       <$> f (_dcsgoMaxRecords x)
{-# INLINE dcsgoMaxRecords #-}

-- | The name of the cache security group to return details for.
dcsgoCacheSecurityGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCacheSecurityGroups
    -> f DescribeCacheSecurityGroups
dcsgoCacheSecurityGroupName f x =
    (\y -> x { _dcsgoCacheSecurityGroupName = y })
       <$> f (_dcsgoCacheSecurityGroupName x)
{-# INLINE dcsgoCacheSecurityGroupName #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcsgoMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCacheSecurityGroups
    -> f DescribeCacheSecurityGroups
dcsgoMarker f x =
    (\y -> x { _dcsgoMarker = y })
       <$> f (_dcsgoMarker x)
{-# INLINE dcsgoMarker #-}

instance ToQuery DescribeCacheSecurityGroups where
    toQuery = genericQuery def

data DescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse
    { _csgpCacheSecurityGroups :: [CacheSecurityGroup]
      -- ^ A list of cache security groups. Each element in the list
      -- contains detailed information about one group.
    , _csgpMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Show, Generic)

-- | A list of cache security groups. Each element in the list contains detailed
-- information about one group.
csgpCacheSecurityGroups
    :: Functor f
    => ([CacheSecurityGroup]
    -> f ([CacheSecurityGroup]))
    -> DescribeCacheSecurityGroupsResponse
    -> f DescribeCacheSecurityGroupsResponse
csgpCacheSecurityGroups f x =
    (\y -> x { _csgpCacheSecurityGroups = y })
       <$> f (_csgpCacheSecurityGroups x)
{-# INLINE csgpCacheSecurityGroups #-}

-- | Provides an identifier to allow retrieval of paginated results.
csgpMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCacheSecurityGroupsResponse
    -> f DescribeCacheSecurityGroupsResponse
csgpMarker f x =
    (\y -> x { _csgpMarker = y })
       <$> f (_csgpMarker x)
{-# INLINE csgpMarker #-}

instance FromXML DescribeCacheSecurityGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheSecurityGroups where
    type Sv DescribeCacheSecurityGroups = ElastiCache
    type Rs DescribeCacheSecurityGroups = DescribeCacheSecurityGroupsResponse

    request = post "DescribeCacheSecurityGroups"
    response _ = xmlResponse

instance AWSPager DescribeCacheSecurityGroups where
    next rq rs = (\x -> rq { _dcsgoMarker = Just x })
        <$> (_csgpMarker rs)
