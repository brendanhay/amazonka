{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheSubnetGroups
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
module Network.AWS.ElastiCache.DescribeCacheSubnetGroups
    (
    -- * Request
      DescribeCacheSubnetGroups
    -- ** Request constructor
    , describeCacheSubnetGroups
    -- ** Request lenses
    , dcsg3CacheSubnetGroupName
    , dcsg3MaxRecords
    , dcsg3Marker

    -- * Response
    , DescribeCacheSubnetGroupsResponse
    -- ** Response constructor
    , describeCacheSubnetGroupsResponse
    -- ** Response lenses
    , dcsgrrMarker
    , dcsgrrCacheSubnetGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a DescribeCacheSubnetGroups operation.
data DescribeCacheSubnetGroups = DescribeCacheSubnetGroups
    { _dcsg3CacheSubnetGroupName :: Maybe Text
    , _dcsg3MaxRecords :: Maybe Integer
    , _dcsg3Marker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheSubnetGroups' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSubnetGroupName ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
describeCacheSubnetGroups :: DescribeCacheSubnetGroups
describeCacheSubnetGroups = DescribeCacheSubnetGroups
    { _dcsg3CacheSubnetGroupName = Nothing
    , _dcsg3MaxRecords = Nothing
    , _dcsg3Marker = Nothing
    }

-- | The name of the cache subnet group to return details for.
dcsg3CacheSubnetGroupName :: Lens' DescribeCacheSubnetGroups (Maybe Text)
dcsg3CacheSubnetGroupName =
    lens _dcsg3CacheSubnetGroupName
         (\s a -> s { _dcsg3CacheSubnetGroupName = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcsg3MaxRecords :: Lens' DescribeCacheSubnetGroups (Maybe Integer)
dcsg3MaxRecords = lens _dcsg3MaxRecords (\s a -> s { _dcsg3MaxRecords = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcsg3Marker :: Lens' DescribeCacheSubnetGroups (Maybe Text)
dcsg3Marker = lens _dcsg3Marker (\s a -> s { _dcsg3Marker = a })

instance ToQuery DescribeCacheSubnetGroups where
    toQuery = genericQuery def

-- | Represents the output of a DescribeCacheSubnetGroups operation.
data DescribeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse
    { _dcsgrrMarker :: Maybe Text
    , _dcsgrrCacheSubnetGroups :: [CacheSubnetGroup]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheSubnetGroupsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @CacheSubnetGroups ::@ @[CacheSubnetGroup]@
--
describeCacheSubnetGroupsResponse :: DescribeCacheSubnetGroupsResponse
describeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse
    { _dcsgrrMarker = Nothing
    , _dcsgrrCacheSubnetGroups = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
dcsgrrMarker :: Lens' DescribeCacheSubnetGroupsResponse (Maybe Text)
dcsgrrMarker = lens _dcsgrrMarker (\s a -> s { _dcsgrrMarker = a })

-- | A list of cache subnet groups. Each element in the list contains detailed
-- information about one group.
dcsgrrCacheSubnetGroups :: Lens' DescribeCacheSubnetGroupsResponse [CacheSubnetGroup]
dcsgrrCacheSubnetGroups =
    lens _dcsgrrCacheSubnetGroups
         (\s a -> s { _dcsgrrCacheSubnetGroups = a })

instance FromXML DescribeCacheSubnetGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheSubnetGroups where
    type Sv DescribeCacheSubnetGroups = ElastiCache
    type Rs DescribeCacheSubnetGroups = DescribeCacheSubnetGroupsResponse

    request = post "DescribeCacheSubnetGroups"
    response _ = xmlResponse

instance AWSPager DescribeCacheSubnetGroups where
    next rq rs = (\x -> rq & dcsg3Marker ?~ x)
        <$> (rs ^. dcsgrrMarker)
