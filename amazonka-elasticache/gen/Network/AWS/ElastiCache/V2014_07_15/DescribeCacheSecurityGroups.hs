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
    , mkDescribeCacheSecurityGroupsMessage
    -- ** Request lenses
    , dcsgoCacheSecurityGroupName
    , dcsgoMaxRecords
    , dcsgoMarker

    -- * Response
    , DescribeCacheSecurityGroupsResponse
    -- ** Response lenses
    , csgpMarker
    , csgpCacheSecurityGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheSecurityGroups' request.
mkDescribeCacheSecurityGroupsMessage :: DescribeCacheSecurityGroups
mkDescribeCacheSecurityGroupsMessage = DescribeCacheSecurityGroups
    { _dcsgoCacheSecurityGroupName = Nothing
    , _dcsgoMaxRecords = Nothing
    , _dcsgoMarker = Nothing
    }
{-# INLINE mkDescribeCacheSecurityGroupsMessage #-}

data DescribeCacheSecurityGroups = DescribeCacheSecurityGroups
    { _dcsgoCacheSecurityGroupName :: Maybe Text
      -- ^ The name of the cache security group to return details for.
    , _dcsgoMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _dcsgoMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The name of the cache security group to return details for.
dcsgoCacheSecurityGroupName :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsgoCacheSecurityGroupName = lens _dcsgoCacheSecurityGroupName (\s a -> s { _dcsgoCacheSecurityGroupName = a })
{-# INLINE dcsgoCacheSecurityGroupName #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcsgoMaxRecords :: Lens' DescribeCacheSecurityGroups (Maybe Integer)
dcsgoMaxRecords = lens _dcsgoMaxRecords (\s a -> s { _dcsgoMaxRecords = a })
{-# INLINE dcsgoMaxRecords #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcsgoMarker :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsgoMarker = lens _dcsgoMarker (\s a -> s { _dcsgoMarker = a })
{-# INLINE dcsgoMarker #-}

instance ToQuery DescribeCacheSecurityGroups where
    toQuery = genericQuery def

data DescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse
    { _csgpMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , _csgpCacheSecurityGroups :: [CacheSecurityGroup]
      -- ^ A list of cache security groups. Each element in the list
      -- contains detailed information about one group.
    } deriving (Show, Generic)

-- | Provides an identifier to allow retrieval of paginated results.
csgpMarker :: Lens' DescribeCacheSecurityGroupsResponse (Maybe Text)
csgpMarker = lens _csgpMarker (\s a -> s { _csgpMarker = a })
{-# INLINE csgpMarker #-}

-- | A list of cache security groups. Each element in the list contains detailed
-- information about one group.
csgpCacheSecurityGroups :: Lens' DescribeCacheSecurityGroupsResponse ([CacheSecurityGroup])
csgpCacheSecurityGroups = lens _csgpCacheSecurityGroups (\s a -> s { _csgpCacheSecurityGroups = a })
{-# INLINE csgpCacheSecurityGroups #-}

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
