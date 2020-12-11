{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- The response includes disk IDs that are configured as cache, and it includes the amount of cache allocated and used.
module Network.AWS.StorageGateway.DescribeCache
  ( -- * Creating a request
    DescribeCache (..),
    mkDescribeCache,

    -- ** Request lenses
    dcGatewayARN,

    -- * Destructuring the response
    DescribeCacheResponse (..),
    mkDescribeCacheResponse,

    -- ** Response lenses
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkDescribeCache' smart constructor.
newtype DescribeCache = DescribeCache' {gatewayARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCache' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
mkDescribeCache ::
  -- | 'gatewayARN'
  Lude.Text ->
  DescribeCache
mkDescribeCache pGatewayARN_ =
  DescribeCache' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcGatewayARN :: Lens.Lens' DescribeCache Lude.Text
dcGatewayARN = Lens.lens (gatewayARN :: DescribeCache -> Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeCache)
{-# DEPRECATED dcGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest DescribeCache where
  type Rs DescribeCache = DescribeCacheResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCacheResponse'
            Lude.<$> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "DiskIds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "CacheUsedPercentage")
            Lude.<*> (x Lude..?> "CacheHitPercentage")
            Lude.<*> (x Lude..?> "CacheMissPercentage")
            Lude.<*> (x Lude..?> "CacheAllocatedInBytes")
            Lude.<*> (x Lude..?> "CacheDirtyPercentage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.DescribeCache" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCache where
  toJSON DescribeCache' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath DescribeCache where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCache where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCacheResponse' smart constructor.
data DescribeCacheResponse = DescribeCacheResponse'
  { gatewayARN ::
      Lude.Maybe Lude.Text,
    diskIds :: Lude.Maybe [Lude.Text],
    cacheUsedPercentage :: Lude.Maybe Lude.Double,
    cacheHitPercentage :: Lude.Maybe Lude.Double,
    cacheMissPercentage :: Lude.Maybe Lude.Double,
    cacheAllocatedInBytes ::
      Lude.Maybe Lude.Integer,
    cacheDirtyPercentage :: Lude.Maybe Lude.Double,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCacheResponse' with the minimum fields required to make a request.
--
-- * 'cacheAllocatedInBytes' - The amount of cache in bytes allocated to a gateway.
-- * 'cacheDirtyPercentage' - The file share's contribution to the overall percentage of the gateway's cache that has not been persisted to AWS. The sample is taken at the end of the reporting period.
-- * 'cacheHitPercentage' - Percent of application read operations from the file shares that are served from cache. The sample is taken at the end of the reporting period.
-- * 'cacheMissPercentage' - Percent of application read operations from the file shares that are not served from cache. The sample is taken at the end of the reporting period.
-- * 'cacheUsedPercentage' - Percent use of the gateway's cache storage. This metric applies only to the gateway-cached volume setup. The sample is taken at the end of the reporting period.
-- * 'diskIds' - An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeCacheResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCacheResponse
mkDescribeCacheResponse pResponseStatus_ =
  DescribeCacheResponse'
    { gatewayARN = Lude.Nothing,
      diskIds = Lude.Nothing,
      cacheUsedPercentage = Lude.Nothing,
      cacheHitPercentage = Lude.Nothing,
      cacheMissPercentage = Lude.Nothing,
      cacheAllocatedInBytes = Lude.Nothing,
      cacheDirtyPercentage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsGatewayARN :: Lens.Lens' DescribeCacheResponse (Lude.Maybe Lude.Text)
dcrsGatewayARN = Lens.lens (gatewayARN :: DescribeCacheResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeCacheResponse)
{-# DEPRECATED dcrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsDiskIds :: Lens.Lens' DescribeCacheResponse (Lude.Maybe [Lude.Text])
dcrsDiskIds = Lens.lens (diskIds :: DescribeCacheResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {diskIds = a} :: DescribeCacheResponse)
{-# DEPRECATED dcrsDiskIds "Use generic-lens or generic-optics with 'diskIds' instead." #-}

-- | Percent use of the gateway's cache storage. This metric applies only to the gateway-cached volume setup. The sample is taken at the end of the reporting period.
--
-- /Note:/ Consider using 'cacheUsedPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCacheUsedPercentage :: Lens.Lens' DescribeCacheResponse (Lude.Maybe Lude.Double)
dcrsCacheUsedPercentage = Lens.lens (cacheUsedPercentage :: DescribeCacheResponse -> Lude.Maybe Lude.Double) (\s a -> s {cacheUsedPercentage = a} :: DescribeCacheResponse)
{-# DEPRECATED dcrsCacheUsedPercentage "Use generic-lens or generic-optics with 'cacheUsedPercentage' instead." #-}

-- | Percent of application read operations from the file shares that are served from cache. The sample is taken at the end of the reporting period.
--
-- /Note:/ Consider using 'cacheHitPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCacheHitPercentage :: Lens.Lens' DescribeCacheResponse (Lude.Maybe Lude.Double)
dcrsCacheHitPercentage = Lens.lens (cacheHitPercentage :: DescribeCacheResponse -> Lude.Maybe Lude.Double) (\s a -> s {cacheHitPercentage = a} :: DescribeCacheResponse)
{-# DEPRECATED dcrsCacheHitPercentage "Use generic-lens or generic-optics with 'cacheHitPercentage' instead." #-}

-- | Percent of application read operations from the file shares that are not served from cache. The sample is taken at the end of the reporting period.
--
-- /Note:/ Consider using 'cacheMissPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCacheMissPercentage :: Lens.Lens' DescribeCacheResponse (Lude.Maybe Lude.Double)
dcrsCacheMissPercentage = Lens.lens (cacheMissPercentage :: DescribeCacheResponse -> Lude.Maybe Lude.Double) (\s a -> s {cacheMissPercentage = a} :: DescribeCacheResponse)
{-# DEPRECATED dcrsCacheMissPercentage "Use generic-lens or generic-optics with 'cacheMissPercentage' instead." #-}

-- | The amount of cache in bytes allocated to a gateway.
--
-- /Note:/ Consider using 'cacheAllocatedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCacheAllocatedInBytes :: Lens.Lens' DescribeCacheResponse (Lude.Maybe Lude.Integer)
dcrsCacheAllocatedInBytes = Lens.lens (cacheAllocatedInBytes :: DescribeCacheResponse -> Lude.Maybe Lude.Integer) (\s a -> s {cacheAllocatedInBytes = a} :: DescribeCacheResponse)
{-# DEPRECATED dcrsCacheAllocatedInBytes "Use generic-lens or generic-optics with 'cacheAllocatedInBytes' instead." #-}

-- | The file share's contribution to the overall percentage of the gateway's cache that has not been persisted to AWS. The sample is taken at the end of the reporting period.
--
-- /Note:/ Consider using 'cacheDirtyPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCacheDirtyPercentage :: Lens.Lens' DescribeCacheResponse (Lude.Maybe Lude.Double)
dcrsCacheDirtyPercentage = Lens.lens (cacheDirtyPercentage :: DescribeCacheResponse -> Lude.Maybe Lude.Double) (\s a -> s {cacheDirtyPercentage = a} :: DescribeCacheResponse)
{-# DEPRECATED dcrsCacheDirtyPercentage "Use generic-lens or generic-optics with 'cacheDirtyPercentage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeCacheResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeCacheResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCacheResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
