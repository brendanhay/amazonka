{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeCache
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns information about the cache of a gateway. This
-- operation is supported only for the gateway-cached volume architecture. The
-- response includes disk IDs that are configured as cache, and it includes
-- the amount of cache allocated and used.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeCache.html>
module Network.AWS.StorageGateway.DescribeCache
    (
    -- * Request
      DescribeCache
    -- ** Request constructor
    , describeCache
    -- ** Request lenses
    , dcGatewayARN

    -- * Response
    , DescribeCacheResponse
    -- ** Response constructor
    , describeCacheResponse
    -- ** Response lenses
    , dcrCacheAllocatedInBytes
    , dcrCacheDirtyPercentage
    , dcrCacheHitPercentage
    , dcrCacheMissPercentage
    , dcrCacheUsedPercentage
    , dcrDiskIds
    , dcrGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype DescribeCache = DescribeCache
    { _dcGatewayARN :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DescribeCache' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcGatewayARN' @::@ 'Text'
--
describeCache :: Text -- ^ 'dcGatewayARN'
              -> DescribeCache
describeCache p1 = DescribeCache
    { _dcGatewayARN = p1
    }

dcGatewayARN :: Lens' DescribeCache Text
dcGatewayARN = lens _dcGatewayARN (\s a -> s { _dcGatewayARN = a })

data DescribeCacheResponse = DescribeCacheResponse
    { _dcrCacheAllocatedInBytes :: Maybe Integer
    , _dcrCacheDirtyPercentage  :: Maybe Double
    , _dcrCacheHitPercentage    :: Maybe Double
    , _dcrCacheMissPercentage   :: Maybe Double
    , _dcrCacheUsedPercentage   :: Maybe Double
    , _dcrDiskIds               :: [Text]
    , _dcrGatewayARN            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeCacheResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrCacheAllocatedInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'dcrCacheDirtyPercentage' @::@ 'Maybe' 'Double'
--
-- * 'dcrCacheHitPercentage' @::@ 'Maybe' 'Double'
--
-- * 'dcrCacheMissPercentage' @::@ 'Maybe' 'Double'
--
-- * 'dcrCacheUsedPercentage' @::@ 'Maybe' 'Double'
--
-- * 'dcrDiskIds' @::@ ['Text']
--
-- * 'dcrGatewayARN' @::@ 'Maybe' 'Text'
--
describeCacheResponse :: DescribeCacheResponse
describeCacheResponse = DescribeCacheResponse
    { _dcrGatewayARN            = Nothing
    , _dcrDiskIds               = mempty
    , _dcrCacheAllocatedInBytes = Nothing
    , _dcrCacheUsedPercentage   = Nothing
    , _dcrCacheDirtyPercentage  = Nothing
    , _dcrCacheHitPercentage    = Nothing
    , _dcrCacheMissPercentage   = Nothing
    }

dcrCacheAllocatedInBytes :: Lens' DescribeCacheResponse (Maybe Integer)
dcrCacheAllocatedInBytes =
    lens _dcrCacheAllocatedInBytes
        (\s a -> s { _dcrCacheAllocatedInBytes = a })

dcrCacheDirtyPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrCacheDirtyPercentage =
    lens _dcrCacheDirtyPercentage (\s a -> s { _dcrCacheDirtyPercentage = a })

dcrCacheHitPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrCacheHitPercentage =
    lens _dcrCacheHitPercentage (\s a -> s { _dcrCacheHitPercentage = a })

dcrCacheMissPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrCacheMissPercentage =
    lens _dcrCacheMissPercentage (\s a -> s { _dcrCacheMissPercentage = a })

dcrCacheUsedPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrCacheUsedPercentage =
    lens _dcrCacheUsedPercentage (\s a -> s { _dcrCacheUsedPercentage = a })

dcrDiskIds :: Lens' DescribeCacheResponse [Text]
dcrDiskIds = lens _dcrDiskIds (\s a -> s { _dcrDiskIds = a })

dcrGatewayARN :: Lens' DescribeCacheResponse (Maybe Text)
dcrGatewayARN = lens _dcrGatewayARN (\s a -> s { _dcrGatewayARN = a })

instance ToPath DescribeCache where
    toPath = const "/"

instance ToQuery DescribeCache where
    toQuery = const mempty

instance ToHeaders DescribeCache

instance ToJSON DescribeCache where
    toJSON DescribeCache{..} = object
        [ "GatewayARN" .= _dcGatewayARN
        ]

instance AWSRequest DescribeCache where
    type Sv DescribeCache = StorageGateway
    type Rs DescribeCache = DescribeCacheResponse

    request  = post "DescribeCache"
    response = jsonResponse

instance FromJSON DescribeCacheResponse where
    parseJSON = withObject "DescribeCacheResponse" $ \o -> DescribeCacheResponse
        <$> o .: "CacheAllocatedInBytes"
        <*> o .: "CacheDirtyPercentage"
        <*> o .: "CacheHitPercentage"
        <*> o .: "CacheMissPercentage"
        <*> o .: "CacheUsedPercentage"
        <*> o .: "DiskIds"
        <*> o .: "GatewayARN"
