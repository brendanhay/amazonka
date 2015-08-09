{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeCache
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about the cache of a gateway. This
-- operation is supported only for the gateway-cached volume architecture.
--
-- The response includes disk IDs that are configured as cache, and it
-- includes the amount of cache allocated and used.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeCache.html AWS API Reference> for DescribeCache.
module Network.AWS.StorageGateway.DescribeCache
    (
    -- * Creating a Request
      DescribeCache
    , describeCache
    -- * Request Lenses
    , dcGatewayARN

    -- * Destructuring the Response
    , DescribeCacheResponse
    , describeCacheResponse
    -- * Response Lenses
    , dcrsGatewayARN
    , dcrsDiskIds
    , dcrsCacheUsedPercentage
    , dcrsCacheHitPercentage
    , dcrsCacheMissPercentage
    , dcrsCacheAllocatedInBytes
    , dcrsCacheDirtyPercentage
    , dcrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'describeCache' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcGatewayARN'
newtype DescribeCache = DescribeCache'
    { _dcGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCache' smart constructor.
describeCache :: Text -> DescribeCache
describeCache pGatewayARN_ =
    DescribeCache'
    { _dcGatewayARN = pGatewayARN_
    }

-- | Undocumented member.
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
                     <*> (pure (fromEnum s)))

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
-- * 'dcrsGatewayARN'
--
-- * 'dcrsDiskIds'
--
-- * 'dcrsCacheUsedPercentage'
--
-- * 'dcrsCacheHitPercentage'
--
-- * 'dcrsCacheMissPercentage'
--
-- * 'dcrsCacheAllocatedInBytes'
--
-- * 'dcrsCacheDirtyPercentage'
--
-- * 'dcrsStatus'
data DescribeCacheResponse = DescribeCacheResponse'
    { _dcrsGatewayARN            :: !(Maybe Text)
    , _dcrsDiskIds               :: !(Maybe [Text])
    , _dcrsCacheUsedPercentage   :: !(Maybe Double)
    , _dcrsCacheHitPercentage    :: !(Maybe Double)
    , _dcrsCacheMissPercentage   :: !(Maybe Double)
    , _dcrsCacheAllocatedInBytes :: !(Maybe Integer)
    , _dcrsCacheDirtyPercentage  :: !(Maybe Double)
    , _dcrsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheResponse' smart constructor.
describeCacheResponse :: Int -> DescribeCacheResponse
describeCacheResponse pStatus_ =
    DescribeCacheResponse'
    { _dcrsGatewayARN = Nothing
    , _dcrsDiskIds = Nothing
    , _dcrsCacheUsedPercentage = Nothing
    , _dcrsCacheHitPercentage = Nothing
    , _dcrsCacheMissPercentage = Nothing
    , _dcrsCacheAllocatedInBytes = Nothing
    , _dcrsCacheDirtyPercentage = Nothing
    , _dcrsStatus = pStatus_
    }

-- | Undocumented member.
dcrsGatewayARN :: Lens' DescribeCacheResponse (Maybe Text)
dcrsGatewayARN = lens _dcrsGatewayARN (\ s a -> s{_dcrsGatewayARN = a});

-- | Undocumented member.
dcrsDiskIds :: Lens' DescribeCacheResponse [Text]
dcrsDiskIds = lens _dcrsDiskIds (\ s a -> s{_dcrsDiskIds = a}) . _Default . _Coerce;

-- | Undocumented member.
dcrsCacheUsedPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheUsedPercentage = lens _dcrsCacheUsedPercentage (\ s a -> s{_dcrsCacheUsedPercentage = a});

-- | Undocumented member.
dcrsCacheHitPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheHitPercentage = lens _dcrsCacheHitPercentage (\ s a -> s{_dcrsCacheHitPercentage = a});

-- | Undocumented member.
dcrsCacheMissPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheMissPercentage = lens _dcrsCacheMissPercentage (\ s a -> s{_dcrsCacheMissPercentage = a});

-- | Undocumented member.
dcrsCacheAllocatedInBytes :: Lens' DescribeCacheResponse (Maybe Integer)
dcrsCacheAllocatedInBytes = lens _dcrsCacheAllocatedInBytes (\ s a -> s{_dcrsCacheAllocatedInBytes = a});

-- | Undocumented member.
dcrsCacheDirtyPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheDirtyPercentage = lens _dcrsCacheDirtyPercentage (\ s a -> s{_dcrsCacheDirtyPercentage = a});

-- | Undocumented member.
dcrsStatus :: Lens' DescribeCacheResponse Int
dcrsStatus = lens _dcrsStatus (\ s a -> s{_dcrsStatus = a});
