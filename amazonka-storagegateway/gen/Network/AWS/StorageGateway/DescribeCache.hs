{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.DescribeCache
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns information about the cache of a gateway. This
-- operation is supported only for the gateway-cached volume architecture.
--
-- The response includes disk IDs that are configured as cache, and it
-- includes the amount of cache allocated and used.
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
    , dcrGatewayARN
    , dcrDiskIds
    , dcrCacheUsedPercentage
    , dcrCacheHitPercentage
    , dcrCacheMissPercentage
    , dcrCacheAllocatedInBytes
    , dcrCacheDirtyPercentage
    , dcrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'describeCache' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcGatewayARN'
newtype DescribeCache = DescribeCache'
    { _dcGatewayARN :: Text
    } deriving (Eq,Read,Show)

-- | 'DescribeCache' smart constructor.
describeCache :: Text -> DescribeCache
describeCache pGatewayARN =
    DescribeCache'
    { _dcGatewayARN = pGatewayARN
    }

-- | FIXME: Undocumented member.
dcGatewayARN :: Lens' DescribeCache Text
dcGatewayARN = lens _dcGatewayARN (\ s a -> s{_dcGatewayARN = a});

instance AWSRequest DescribeCache where
        type Sv DescribeCache = StorageGateway
        type Rs DescribeCache = DescribeCacheResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCacheResponse' <$>
                   (x .?> "GatewayARN") <*> (x .?> "DiskIds" .!@ mempty)
                     <*> (x .?> "CacheUsedPercentage")
                     <*> (x .?> "CacheHitPercentage")
                     <*> (x .?> "CacheMissPercentage")
                     <*> (x .?> "CacheAllocatedInBytes")
                     <*> (x .?> "CacheDirtyPercentage")
                     <*> (pure s))

instance ToHeaders DescribeCache where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeCache" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCache where
        toJSON DescribeCache'{..}
          = object ["GatewayARN" .= _dcGatewayARN]

instance ToPath DescribeCache where
        toPath = const "/"

instance ToQuery DescribeCache where
        toQuery = const mempty

-- | /See:/ 'describeCacheResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrGatewayARN'
--
-- * 'dcrDiskIds'
--
-- * 'dcrCacheUsedPercentage'
--
-- * 'dcrCacheHitPercentage'
--
-- * 'dcrCacheMissPercentage'
--
-- * 'dcrCacheAllocatedInBytes'
--
-- * 'dcrCacheDirtyPercentage'
--
-- * 'dcrStatus'
data DescribeCacheResponse = DescribeCacheResponse'
    { _dcrGatewayARN            :: !(Maybe Text)
    , _dcrDiskIds               :: !(Maybe [Text])
    , _dcrCacheUsedPercentage   :: !(Maybe Double)
    , _dcrCacheHitPercentage    :: !(Maybe Double)
    , _dcrCacheMissPercentage   :: !(Maybe Double)
    , _dcrCacheAllocatedInBytes :: !(Maybe Integer)
    , _dcrCacheDirtyPercentage  :: !(Maybe Double)
    , _dcrStatus                :: !Status
    } deriving (Eq,Show)

-- | 'DescribeCacheResponse' smart constructor.
describeCacheResponse :: Status -> DescribeCacheResponse
describeCacheResponse pStatus =
    DescribeCacheResponse'
    { _dcrGatewayARN = Nothing
    , _dcrDiskIds = Nothing
    , _dcrCacheUsedPercentage = Nothing
    , _dcrCacheHitPercentage = Nothing
    , _dcrCacheMissPercentage = Nothing
    , _dcrCacheAllocatedInBytes = Nothing
    , _dcrCacheDirtyPercentage = Nothing
    , _dcrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dcrGatewayARN :: Lens' DescribeCacheResponse (Maybe Text)
dcrGatewayARN = lens _dcrGatewayARN (\ s a -> s{_dcrGatewayARN = a});

-- | FIXME: Undocumented member.
dcrDiskIds :: Lens' DescribeCacheResponse [Text]
dcrDiskIds = lens _dcrDiskIds (\ s a -> s{_dcrDiskIds = a}) . _Default;

-- | FIXME: Undocumented member.
dcrCacheUsedPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrCacheUsedPercentage = lens _dcrCacheUsedPercentage (\ s a -> s{_dcrCacheUsedPercentage = a});

-- | FIXME: Undocumented member.
dcrCacheHitPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrCacheHitPercentage = lens _dcrCacheHitPercentage (\ s a -> s{_dcrCacheHitPercentage = a});

-- | FIXME: Undocumented member.
dcrCacheMissPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrCacheMissPercentage = lens _dcrCacheMissPercentage (\ s a -> s{_dcrCacheMissPercentage = a});

-- | FIXME: Undocumented member.
dcrCacheAllocatedInBytes :: Lens' DescribeCacheResponse (Maybe Integer)
dcrCacheAllocatedInBytes = lens _dcrCacheAllocatedInBytes (\ s a -> s{_dcrCacheAllocatedInBytes = a});

-- | FIXME: Undocumented member.
dcrCacheDirtyPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrCacheDirtyPercentage = lens _dcrCacheDirtyPercentage (\ s a -> s{_dcrCacheDirtyPercentage = a});

-- | FIXME: Undocumented member.
dcrStatus :: Lens' DescribeCacheResponse Status
dcrStatus = lens _dcrStatus (\ s a -> s{_dcrStatus = a});
