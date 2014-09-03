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
    , describeCacheParameterGroups
    -- ** Request lenses
    , dcpgnMaxRecords
    , dcpgnCacheParameterGroupName
    , dcpgnMarker

    -- * Response
    , DescribeCacheParameterGroupsResponse
    -- ** Response lenses
    , cpgmCacheParameterGroups
    , cpgmMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeCacheParameterGroups' request.
describeCacheParameterGroups :: DescribeCacheParameterGroups
describeCacheParameterGroups = DescribeCacheParameterGroups
    { _dcpgnMaxRecords = Nothing
    , _dcpgnCacheParameterGroupName = Nothing
    , _dcpgnMarker = Nothing
    }

data DescribeCacheParameterGroups = DescribeCacheParameterGroups
    { _dcpgnMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _dcpgnCacheParameterGroupName :: Maybe Text
      -- ^ The name of a specific cache parameter group to return details
      -- for.
    , _dcpgnMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcpgnMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeCacheParameterGroups
    -> f DescribeCacheParameterGroups
dcpgnMaxRecords f x =
    (\y -> x { _dcpgnMaxRecords = y })
       <$> f (_dcpgnMaxRecords x)
{-# INLINE dcpgnMaxRecords #-}

-- | The name of a specific cache parameter group to return details for.
dcpgnCacheParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCacheParameterGroups
    -> f DescribeCacheParameterGroups
dcpgnCacheParameterGroupName f x =
    (\y -> x { _dcpgnCacheParameterGroupName = y })
       <$> f (_dcpgnCacheParameterGroupName x)
{-# INLINE dcpgnCacheParameterGroupName #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcpgnMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCacheParameterGroups
    -> f DescribeCacheParameterGroups
dcpgnMarker f x =
    (\y -> x { _dcpgnMarker = y })
       <$> f (_dcpgnMarker x)
{-# INLINE dcpgnMarker #-}

instance ToQuery DescribeCacheParameterGroups where
    toQuery = genericQuery def

data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse
    { _cpgmCacheParameterGroups :: [CacheParameterGroup]
      -- ^ A list of cache parameter groups. Each element in the list
      -- contains detailed information about one cache parameter group.
    , _cpgmMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Show, Generic)

-- | A list of cache parameter groups. Each element in the list contains
-- detailed information about one cache parameter group.
cpgmCacheParameterGroups
    :: Functor f
    => ([CacheParameterGroup]
    -> f ([CacheParameterGroup]))
    -> DescribeCacheParameterGroupsResponse
    -> f DescribeCacheParameterGroupsResponse
cpgmCacheParameterGroups f x =
    (\y -> x { _cpgmCacheParameterGroups = y })
       <$> f (_cpgmCacheParameterGroups x)
{-# INLINE cpgmCacheParameterGroups #-}

-- | Provides an identifier to allow retrieval of paginated results.
cpgmMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeCacheParameterGroupsResponse
    -> f DescribeCacheParameterGroupsResponse
cpgmMarker f x =
    (\y -> x { _cpgmMarker = y })
       <$> f (_cpgmMarker x)
{-# INLINE cpgmMarker #-}

instance FromXML DescribeCacheParameterGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheParameterGroups where
    type Sv DescribeCacheParameterGroups = ElastiCache
    type Rs DescribeCacheParameterGroups = DescribeCacheParameterGroupsResponse

    request = post "DescribeCacheParameterGroups"
    response _ = xmlResponse

instance AWSPager DescribeCacheParameterGroups where
    next rq rs = (\x -> rq { _dcpgnMarker = Just x })
        <$> (_cpgmMarker rs)
