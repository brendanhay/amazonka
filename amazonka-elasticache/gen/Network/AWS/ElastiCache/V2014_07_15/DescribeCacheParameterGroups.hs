{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeCacheParameterGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheParameterGroups operation returns a list of cache
-- parameter group descriptions. If a cache parameter group name is specified,
-- the list will contain only the descriptions for that group.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheParameterGroups &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= default.memcached1.4 memcached1.4 Default parameter
-- group for memcached1.4 mycacheparametergroup memcached1.4 My cache
-- parameter group mycacheparametergroup1 memcached1.4 My first cache
-- parameter group mycacheparametergroup3 memcached1.4 My first cache
-- parameter group 7193fbb8-b7fc-11e0-9b0b-a9261be2b354.
module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheParameterGroups
    (
    -- * Request
      DescribeCacheParameterGroups
    -- ** Request constructor
    , mkDescribeCacheParameterGroups
    -- ** Request lenses
    , dcpg1CacheParameterGroupName
    , dcpg1MaxRecords
    , dcpg1Marker

    -- * Response
    , DescribeCacheParameterGroupsResponse
    -- ** Response lenses
    , dcpgrsMarker
    , dcpgrsCacheParameterGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Represents the input of a DescribeCacheParameterGroups operation.
data DescribeCacheParameterGroups = DescribeCacheParameterGroups
    { _dcpg1CacheParameterGroupName :: Maybe Text
    , _dcpg1MaxRecords :: Maybe Integer
    , _dcpg1Marker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheParameterGroups' request.
mkDescribeCacheParameterGroups :: DescribeCacheParameterGroups
mkDescribeCacheParameterGroups = DescribeCacheParameterGroups
    { _dcpg1CacheParameterGroupName = Nothing
    , _dcpg1MaxRecords = Nothing
    , _dcpg1Marker = Nothing
    }
{-# INLINE mkDescribeCacheParameterGroups #-}

-- | The name of a specific cache parameter group to return details for.
dcpg1CacheParameterGroupName :: Lens' DescribeCacheParameterGroups (Maybe Text)
dcpg1CacheParameterGroupName =
    lens _dcpg1CacheParameterGroupName
         (\s a -> s { _dcpg1CacheParameterGroupName = a })
{-# INLINE dcpg1CacheParameterGroupName #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcpg1MaxRecords :: Lens' DescribeCacheParameterGroups (Maybe Integer)
dcpg1MaxRecords = lens _dcpg1MaxRecords (\s a -> s { _dcpg1MaxRecords = a })
{-# INLINE dcpg1MaxRecords #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcpg1Marker :: Lens' DescribeCacheParameterGroups (Maybe Text)
dcpg1Marker = lens _dcpg1Marker (\s a -> s { _dcpg1Marker = a })
{-# INLINE dcpg1Marker #-}

instance ToQuery DescribeCacheParameterGroups where
    toQuery = genericQuery def

-- | Represents the output of a DescribeCacheParameterGroups operation.
data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse
    { _dcpgrsMarker :: Maybe Text
    , _dcpgrsCacheParameterGroups :: [CacheParameterGroup]
    } deriving (Show, Generic)

-- | Provides an identifier to allow retrieval of paginated results.
dcpgrsMarker :: Lens' DescribeCacheParameterGroupsResponse (Maybe Text)
dcpgrsMarker = lens _dcpgrsMarker (\s a -> s { _dcpgrsMarker = a })
{-# INLINE dcpgrsMarker #-}

-- | A list of cache parameter groups. Each element in the list contains
-- detailed information about one cache parameter group.
dcpgrsCacheParameterGroups :: Lens' DescribeCacheParameterGroupsResponse [CacheParameterGroup]
dcpgrsCacheParameterGroups =
    lens _dcpgrsCacheParameterGroups
         (\s a -> s { _dcpgrsCacheParameterGroups = a })
{-# INLINE dcpgrsCacheParameterGroups #-}

instance FromXML DescribeCacheParameterGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheParameterGroups where
    type Sv DescribeCacheParameterGroups = ElastiCache
    type Rs DescribeCacheParameterGroups = DescribeCacheParameterGroupsResponse

    request = post "DescribeCacheParameterGroups"
    response _ = xmlResponse

instance AWSPager DescribeCacheParameterGroups where
    next rq rs = (\x -> rq { _dcpg1Marker = Just x })
        <$> (_dcpgrsMarker rs)
