{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the cache of a gateway. This operation is only supported in the cached volume, tape, and file gateway types.
--
--
-- The response includes disk IDs that are configured as cache, and it includes the amount of cache allocated and used.
module Network.AWS.StorageGateway.DescribeCache
  ( -- * Creating a Request
    describeCache,
    DescribeCache,

    -- * Request Lenses
    dcGatewayARN,

    -- * Destructuring the Response
    describeCacheResponse,
    DescribeCacheResponse,

    -- * Response Lenses
    dcrsGatewayARN,
    dcrsDiskIds,
    dcrsCacheUsedPercentage,
    dcrsCacheHitPercentage,
    dcrsCacheMissPercentage,
    dcrsCacheAllocatedInBytes,
    dcrsCacheDirtyPercentage,
    dcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'describeCache' smart constructor.
newtype DescribeCache = DescribeCache' {_dcGatewayARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcGatewayARN' - Undocumented member.
describeCache ::
  -- | 'dcGatewayARN'
  Text ->
  DescribeCache
describeCache pGatewayARN_ =
  DescribeCache' {_dcGatewayARN = pGatewayARN_}

-- | Undocumented member.
dcGatewayARN :: Lens' DescribeCache Text
dcGatewayARN = lens _dcGatewayARN (\s a -> s {_dcGatewayARN = a})

instance AWSRequest DescribeCache where
  type Rs DescribeCache = DescribeCacheResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          DescribeCacheResponse'
            <$> (x .?> "GatewayARN")
            <*> (x .?> "DiskIds" .!@ mempty)
            <*> (x .?> "CacheUsedPercentage")
            <*> (x .?> "CacheHitPercentage")
            <*> (x .?> "CacheMissPercentage")
            <*> (x .?> "CacheAllocatedInBytes")
            <*> (x .?> "CacheDirtyPercentage")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeCache

instance NFData DescribeCache

instance ToHeaders DescribeCache where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.DescribeCache" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeCache where
  toJSON DescribeCache' {..} =
    object (catMaybes [Just ("GatewayARN" .= _dcGatewayARN)])

instance ToPath DescribeCache where
  toPath = const "/"

instance ToQuery DescribeCache where
  toQuery = const mempty

-- | /See:/ 'describeCacheResponse' smart constructor.
data DescribeCacheResponse = DescribeCacheResponse'
  { _dcrsGatewayARN ::
      !(Maybe Text),
    _dcrsDiskIds :: !(Maybe [Text]),
    _dcrsCacheUsedPercentage :: !(Maybe Double),
    _dcrsCacheHitPercentage :: !(Maybe Double),
    _dcrsCacheMissPercentage :: !(Maybe Double),
    _dcrsCacheAllocatedInBytes :: !(Maybe Integer),
    _dcrsCacheDirtyPercentage :: !(Maybe Double),
    _dcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCacheResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsGatewayARN' - Undocumented member.
--
-- * 'dcrsDiskIds' - An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
--
-- * 'dcrsCacheUsedPercentage' - Percent use of the gateway's cache storage. This metric applies only to the gateway-cached volume setup. The sample is taken at the end of the reporting period.
--
-- * 'dcrsCacheHitPercentage' - Percent of application read operations from the file shares that are served from cache. The sample is taken at the end of the reporting period.
--
-- * 'dcrsCacheMissPercentage' - Percent of application read operations from the file shares that are not served from cache. The sample is taken at the end of the reporting period.
--
-- * 'dcrsCacheAllocatedInBytes' - The amount of cache in bytes allocated to a gateway.
--
-- * 'dcrsCacheDirtyPercentage' - The file share's contribution to the overall percentage of the gateway's cache that has not been persisted to AWS. The sample is taken at the end of the reporting period.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeCacheResponse ::
  -- | 'dcrsResponseStatus'
  Int ->
  DescribeCacheResponse
describeCacheResponse pResponseStatus_ =
  DescribeCacheResponse'
    { _dcrsGatewayARN = Nothing,
      _dcrsDiskIds = Nothing,
      _dcrsCacheUsedPercentage = Nothing,
      _dcrsCacheHitPercentage = Nothing,
      _dcrsCacheMissPercentage = Nothing,
      _dcrsCacheAllocatedInBytes = Nothing,
      _dcrsCacheDirtyPercentage = Nothing,
      _dcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dcrsGatewayARN :: Lens' DescribeCacheResponse (Maybe Text)
dcrsGatewayARN = lens _dcrsGatewayARN (\s a -> s {_dcrsGatewayARN = a})

-- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
dcrsDiskIds :: Lens' DescribeCacheResponse [Text]
dcrsDiskIds = lens _dcrsDiskIds (\s a -> s {_dcrsDiskIds = a}) . _Default . _Coerce

-- | Percent use of the gateway's cache storage. This metric applies only to the gateway-cached volume setup. The sample is taken at the end of the reporting period.
dcrsCacheUsedPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheUsedPercentage = lens _dcrsCacheUsedPercentage (\s a -> s {_dcrsCacheUsedPercentage = a})

-- | Percent of application read operations from the file shares that are served from cache. The sample is taken at the end of the reporting period.
dcrsCacheHitPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheHitPercentage = lens _dcrsCacheHitPercentage (\s a -> s {_dcrsCacheHitPercentage = a})

-- | Percent of application read operations from the file shares that are not served from cache. The sample is taken at the end of the reporting period.
dcrsCacheMissPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheMissPercentage = lens _dcrsCacheMissPercentage (\s a -> s {_dcrsCacheMissPercentage = a})

-- | The amount of cache in bytes allocated to a gateway.
dcrsCacheAllocatedInBytes :: Lens' DescribeCacheResponse (Maybe Integer)
dcrsCacheAllocatedInBytes = lens _dcrsCacheAllocatedInBytes (\s a -> s {_dcrsCacheAllocatedInBytes = a})

-- | The file share's contribution to the overall percentage of the gateway's cache that has not been persisted to AWS. The sample is taken at the end of the reporting period.
dcrsCacheDirtyPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheDirtyPercentage = lens _dcrsCacheDirtyPercentage (\s a -> s {_dcrsCacheDirtyPercentage = a})

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeCacheResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\s a -> s {_dcrsResponseStatus = a})

instance NFData DescribeCacheResponse
