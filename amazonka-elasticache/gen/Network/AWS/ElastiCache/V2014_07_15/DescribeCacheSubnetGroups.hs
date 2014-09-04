{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeCacheSubnetGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheSubnetGroups operation returns a list of cache subnet
-- group descriptions. If a subnet group name is specified, the list will
-- contain only the description of that group. Some of the output has been
-- omitted for brevity. https://elasticache.amazonaws.com/
-- ?Action=DescribeCacheSubnetGroups &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- 990524496922 description subnet_grp1 Active subnet-7c5b4115 us-east-1c
-- Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57 us-east-1d
-- (...output omitted...) 31d0faee-229b-11e1-81f1-df3a2a803dad.
module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheSubnetGroups
    (
    -- * Request
      DescribeCacheSubnetGroups
    -- ** Request constructor
    , mkDescribeCacheSubnetGroupsMessage
    -- ** Request lenses
    , dcsgpCacheSubnetGroupName
    , dcsgpMaxRecords
    , dcsgpMarker

    -- * Response
    , DescribeCacheSubnetGroupsResponse
    -- ** Response lenses
    , csgqMarker
    , csgqCacheSubnetGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheSubnetGroups' request.
mkDescribeCacheSubnetGroupsMessage :: DescribeCacheSubnetGroups
mkDescribeCacheSubnetGroupsMessage = DescribeCacheSubnetGroups
    { _dcsgpCacheSubnetGroupName = Nothing
    , _dcsgpMaxRecords = Nothing
    , _dcsgpMarker = Nothing
    }
{-# INLINE mkDescribeCacheSubnetGroupsMessage #-}

data DescribeCacheSubnetGroups = DescribeCacheSubnetGroups
    { _dcsgpCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group to return details for.
    , _dcsgpMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _dcsgpMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The name of the cache subnet group to return details for.
dcsgpCacheSubnetGroupName :: Lens' DescribeCacheSubnetGroups (Maybe Text)
dcsgpCacheSubnetGroupName = lens _dcsgpCacheSubnetGroupName (\s a -> s { _dcsgpCacheSubnetGroupName = a })
{-# INLINE dcsgpCacheSubnetGroupName #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcsgpMaxRecords :: Lens' DescribeCacheSubnetGroups (Maybe Integer)
dcsgpMaxRecords = lens _dcsgpMaxRecords (\s a -> s { _dcsgpMaxRecords = a })
{-# INLINE dcsgpMaxRecords #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcsgpMarker :: Lens' DescribeCacheSubnetGroups (Maybe Text)
dcsgpMarker = lens _dcsgpMarker (\s a -> s { _dcsgpMarker = a })
{-# INLINE dcsgpMarker #-}

instance ToQuery DescribeCacheSubnetGroups where
    toQuery = genericQuery def

data DescribeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse
    { _csgqMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , _csgqCacheSubnetGroups :: [CacheSubnetGroup]
      -- ^ A list of cache subnet groups. Each element in the list contains
      -- detailed information about one group.
    } deriving (Show, Generic)

-- | Provides an identifier to allow retrieval of paginated results.
csgqMarker :: Lens' DescribeCacheSubnetGroupsResponse (Maybe Text)
csgqMarker = lens _csgqMarker (\s a -> s { _csgqMarker = a })
{-# INLINE csgqMarker #-}

-- | A list of cache subnet groups. Each element in the list contains detailed
-- information about one group.
csgqCacheSubnetGroups :: Lens' DescribeCacheSubnetGroupsResponse ([CacheSubnetGroup])
csgqCacheSubnetGroups = lens _csgqCacheSubnetGroups (\s a -> s { _csgqCacheSubnetGroups = a })
{-# INLINE csgqCacheSubnetGroups #-}

instance FromXML DescribeCacheSubnetGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheSubnetGroups where
    type Sv DescribeCacheSubnetGroups = ElastiCache
    type Rs DescribeCacheSubnetGroups = DescribeCacheSubnetGroupsResponse

    request = post "DescribeCacheSubnetGroups"
    response _ = xmlResponse

instance AWSPager DescribeCacheSubnetGroups where
    next rq rs = (\x -> rq { _dcsgpMarker = Just x })
        <$> (_csgqMarker rs)
