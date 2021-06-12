{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the cache of a gateway. This operation is only
-- supported in the cached volume, tape, and file gateway types.
--
-- The response includes disk IDs that are configured as cache, and it
-- includes the amount of cache allocated and used.
module Network.AWS.StorageGateway.DescribeCache
  ( -- * Creating a Request
    DescribeCache (..),
    newDescribeCache,

    -- * Request Lenses
    describeCache_gatewayARN,

    -- * Destructuring the Response
    DescribeCacheResponse (..),
    newDescribeCacheResponse,

    -- * Response Lenses
    describeCacheResponse_cacheHitPercentage,
    describeCacheResponse_cacheDirtyPercentage,
    describeCacheResponse_cacheAllocatedInBytes,
    describeCacheResponse_cacheMissPercentage,
    describeCacheResponse_diskIds,
    describeCacheResponse_cacheUsedPercentage,
    describeCacheResponse_gatewayARN,
    describeCacheResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newDescribeCache' smart constructor.
data DescribeCache = DescribeCache'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'describeCache_gatewayARN' - Undocumented member.
newDescribeCache ::
  -- | 'gatewayARN'
  Core.Text ->
  DescribeCache
newDescribeCache pGatewayARN_ =
  DescribeCache' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
describeCache_gatewayARN :: Lens.Lens' DescribeCache Core.Text
describeCache_gatewayARN = Lens.lens (\DescribeCache' {gatewayARN} -> gatewayARN) (\s@DescribeCache' {} a -> s {gatewayARN = a} :: DescribeCache)

instance Core.AWSRequest DescribeCache where
  type
    AWSResponse DescribeCache =
      DescribeCacheResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCacheResponse'
            Core.<$> (x Core..?> "CacheHitPercentage")
            Core.<*> (x Core..?> "CacheDirtyPercentage")
            Core.<*> (x Core..?> "CacheAllocatedInBytes")
            Core.<*> (x Core..?> "CacheMissPercentage")
            Core.<*> (x Core..?> "DiskIds" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "CacheUsedPercentage")
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCache

instance Core.NFData DescribeCache

instance Core.ToHeaders DescribeCache where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeCache" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCache where
  toJSON DescribeCache' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath DescribeCache where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCache where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCacheResponse' smart constructor.
data DescribeCacheResponse = DescribeCacheResponse'
  { -- | Percent of application read operations from the file shares that are
    -- served from cache. The sample is taken at the end of the reporting
    -- period.
    cacheHitPercentage :: Core.Maybe Core.Double,
    -- | The file share\'s contribution to the overall percentage of the
    -- gateway\'s cache that has not been persisted to AWS. The sample is taken
    -- at the end of the reporting period.
    cacheDirtyPercentage :: Core.Maybe Core.Double,
    -- | The amount of cache in bytes allocated to a gateway.
    cacheAllocatedInBytes :: Core.Maybe Core.Integer,
    -- | Percent of application read operations from the file shares that are not
    -- served from cache. The sample is taken at the end of the reporting
    -- period.
    cacheMissPercentage :: Core.Maybe Core.Double,
    -- | An array of strings that identify disks that are to be configured as
    -- working storage. Each string has a minimum length of 1 and maximum
    -- length of 300. You can get the disk IDs from the ListLocalDisks API.
    diskIds :: Core.Maybe [Core.Text],
    -- | Percent use of the gateway\'s cache storage. This metric applies only to
    -- the gateway-cached volume setup. The sample is taken at the end of the
    -- reporting period.
    cacheUsedPercentage :: Core.Maybe Core.Double,
    gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheHitPercentage', 'describeCacheResponse_cacheHitPercentage' - Percent of application read operations from the file shares that are
-- served from cache. The sample is taken at the end of the reporting
-- period.
--
-- 'cacheDirtyPercentage', 'describeCacheResponse_cacheDirtyPercentage' - The file share\'s contribution to the overall percentage of the
-- gateway\'s cache that has not been persisted to AWS. The sample is taken
-- at the end of the reporting period.
--
-- 'cacheAllocatedInBytes', 'describeCacheResponse_cacheAllocatedInBytes' - The amount of cache in bytes allocated to a gateway.
--
-- 'cacheMissPercentage', 'describeCacheResponse_cacheMissPercentage' - Percent of application read operations from the file shares that are not
-- served from cache. The sample is taken at the end of the reporting
-- period.
--
-- 'diskIds', 'describeCacheResponse_diskIds' - An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
--
-- 'cacheUsedPercentage', 'describeCacheResponse_cacheUsedPercentage' - Percent use of the gateway\'s cache storage. This metric applies only to
-- the gateway-cached volume setup. The sample is taken at the end of the
-- reporting period.
--
-- 'gatewayARN', 'describeCacheResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'describeCacheResponse_httpStatus' - The response's http status code.
newDescribeCacheResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCacheResponse
newDescribeCacheResponse pHttpStatus_ =
  DescribeCacheResponse'
    { cacheHitPercentage =
        Core.Nothing,
      cacheDirtyPercentage = Core.Nothing,
      cacheAllocatedInBytes = Core.Nothing,
      cacheMissPercentage = Core.Nothing,
      diskIds = Core.Nothing,
      cacheUsedPercentage = Core.Nothing,
      gatewayARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Percent of application read operations from the file shares that are
-- served from cache. The sample is taken at the end of the reporting
-- period.
describeCacheResponse_cacheHitPercentage :: Lens.Lens' DescribeCacheResponse (Core.Maybe Core.Double)
describeCacheResponse_cacheHitPercentage = Lens.lens (\DescribeCacheResponse' {cacheHitPercentage} -> cacheHitPercentage) (\s@DescribeCacheResponse' {} a -> s {cacheHitPercentage = a} :: DescribeCacheResponse)

-- | The file share\'s contribution to the overall percentage of the
-- gateway\'s cache that has not been persisted to AWS. The sample is taken
-- at the end of the reporting period.
describeCacheResponse_cacheDirtyPercentage :: Lens.Lens' DescribeCacheResponse (Core.Maybe Core.Double)
describeCacheResponse_cacheDirtyPercentage = Lens.lens (\DescribeCacheResponse' {cacheDirtyPercentage} -> cacheDirtyPercentage) (\s@DescribeCacheResponse' {} a -> s {cacheDirtyPercentage = a} :: DescribeCacheResponse)

-- | The amount of cache in bytes allocated to a gateway.
describeCacheResponse_cacheAllocatedInBytes :: Lens.Lens' DescribeCacheResponse (Core.Maybe Core.Integer)
describeCacheResponse_cacheAllocatedInBytes = Lens.lens (\DescribeCacheResponse' {cacheAllocatedInBytes} -> cacheAllocatedInBytes) (\s@DescribeCacheResponse' {} a -> s {cacheAllocatedInBytes = a} :: DescribeCacheResponse)

-- | Percent of application read operations from the file shares that are not
-- served from cache. The sample is taken at the end of the reporting
-- period.
describeCacheResponse_cacheMissPercentage :: Lens.Lens' DescribeCacheResponse (Core.Maybe Core.Double)
describeCacheResponse_cacheMissPercentage = Lens.lens (\DescribeCacheResponse' {cacheMissPercentage} -> cacheMissPercentage) (\s@DescribeCacheResponse' {} a -> s {cacheMissPercentage = a} :: DescribeCacheResponse)

-- | An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
describeCacheResponse_diskIds :: Lens.Lens' DescribeCacheResponse (Core.Maybe [Core.Text])
describeCacheResponse_diskIds = Lens.lens (\DescribeCacheResponse' {diskIds} -> diskIds) (\s@DescribeCacheResponse' {} a -> s {diskIds = a} :: DescribeCacheResponse) Core.. Lens.mapping Lens._Coerce

-- | Percent use of the gateway\'s cache storage. This metric applies only to
-- the gateway-cached volume setup. The sample is taken at the end of the
-- reporting period.
describeCacheResponse_cacheUsedPercentage :: Lens.Lens' DescribeCacheResponse (Core.Maybe Core.Double)
describeCacheResponse_cacheUsedPercentage = Lens.lens (\DescribeCacheResponse' {cacheUsedPercentage} -> cacheUsedPercentage) (\s@DescribeCacheResponse' {} a -> s {cacheUsedPercentage = a} :: DescribeCacheResponse)

-- | Undocumented member.
describeCacheResponse_gatewayARN :: Lens.Lens' DescribeCacheResponse (Core.Maybe Core.Text)
describeCacheResponse_gatewayARN = Lens.lens (\DescribeCacheResponse' {gatewayARN} -> gatewayARN) (\s@DescribeCacheResponse' {} a -> s {gatewayARN = a} :: DescribeCacheResponse)

-- | The response's http status code.
describeCacheResponse_httpStatus :: Lens.Lens' DescribeCacheResponse Core.Int
describeCacheResponse_httpStatus = Lens.lens (\DescribeCacheResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheResponse' {} a -> s {httpStatus = a} :: DescribeCacheResponse)

instance Core.NFData DescribeCacheResponse
