{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheSecurityGroups
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
module Network.AWS.ElastiCache
    (
    -- * Request
      DescribeCacheSecurityGroups
    -- ** Request constructor
    , mkDescribeCacheSecurityGroups
    -- ** Request lenses
    , dcsg2CacheSecurityGroupName
    , dcsg2MaxRecords
    , dcsg2Marker

    -- * Response
    , DescribeCacheSecurityGroupsResponse
    -- ** Response constructor
    , mkDescribeCacheSecurityGroupsResponse
    -- ** Response lenses
    , dcsgrMarker
    , dcsgrCacheSecurityGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a DescribeCacheSecurityGroups operation.
data DescribeCacheSecurityGroups = DescribeCacheSecurityGroups
    { _dcsg2CacheSecurityGroupName :: !(Maybe Text)
    , _dcsg2MaxRecords :: !(Maybe Integer)
    , _dcsg2Marker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheSecurityGroups' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSecurityGroupName ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
mkDescribeCacheSecurityGroups :: DescribeCacheSecurityGroups
mkDescribeCacheSecurityGroups = DescribeCacheSecurityGroups
    { _dcsg2CacheSecurityGroupName = Nothing
    , _dcsg2MaxRecords = Nothing
    , _dcsg2Marker = Nothing
    }

-- | The name of the cache security group to return details for.
dcsg2CacheSecurityGroupName :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsg2CacheSecurityGroupName =
    lens _dcsg2CacheSecurityGroupName
         (\s a -> s { _dcsg2CacheSecurityGroupName = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcsg2MaxRecords :: Lens' DescribeCacheSecurityGroups (Maybe Integer)
dcsg2MaxRecords = lens _dcsg2MaxRecords (\s a -> s { _dcsg2MaxRecords = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dcsg2Marker :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsg2Marker = lens _dcsg2Marker (\s a -> s { _dcsg2Marker = a })

instance ToQuery DescribeCacheSecurityGroups where
    toQuery = genericQuery def

-- | Represents the output of a DescribeCacheSecurityGroups operation.
data DescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse
    { _dcsgrMarker :: !(Maybe Text)
    , _dcsgrCacheSecurityGroups :: [CacheSecurityGroup]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCacheSecurityGroupsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @CacheSecurityGroups ::@ @[CacheSecurityGroup]@
--
mkDescribeCacheSecurityGroupsResponse :: DescribeCacheSecurityGroupsResponse
mkDescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse
    { _dcsgrMarker = Nothing
    , _dcsgrCacheSecurityGroups = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
dcsgrMarker :: Lens' DescribeCacheSecurityGroupsResponse (Maybe Text)
dcsgrMarker = lens _dcsgrMarker (\s a -> s { _dcsgrMarker = a })

-- | A list of cache security groups. Each element in the list contains detailed
-- information about one group.
dcsgrCacheSecurityGroups :: Lens' DescribeCacheSecurityGroupsResponse [CacheSecurityGroup]
dcsgrCacheSecurityGroups =
    lens _dcsgrCacheSecurityGroups
         (\s a -> s { _dcsgrCacheSecurityGroups = a })

instance FromXML DescribeCacheSecurityGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCacheSecurityGroups where
    type Sv DescribeCacheSecurityGroups = ElastiCache
    type Rs DescribeCacheSecurityGroups = DescribeCacheSecurityGroupsResponse

    request = post "DescribeCacheSecurityGroups"
    response _ = xmlResponse

instance AWSPager DescribeCacheSecurityGroups where
    next rq rs = (\x -> rq & dcsg2Marker ?~ x)
        <$> (rs ^. dcsgrMarker)
