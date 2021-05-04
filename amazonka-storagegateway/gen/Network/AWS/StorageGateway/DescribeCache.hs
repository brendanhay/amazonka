{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newDescribeCache' smart constructor.
data DescribeCache = DescribeCache'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeCache
newDescribeCache pGatewayARN_ =
  DescribeCache' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
describeCache_gatewayARN :: Lens.Lens' DescribeCache Prelude.Text
describeCache_gatewayARN = Lens.lens (\DescribeCache' {gatewayARN} -> gatewayARN) (\s@DescribeCache' {} a -> s {gatewayARN = a} :: DescribeCache)

instance Prelude.AWSRequest DescribeCache where
  type Rs DescribeCache = DescribeCacheResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCacheResponse'
            Prelude.<$> (x Prelude..?> "CacheHitPercentage")
            Prelude.<*> (x Prelude..?> "CacheDirtyPercentage")
            Prelude.<*> (x Prelude..?> "CacheAllocatedInBytes")
            Prelude.<*> (x Prelude..?> "CacheMissPercentage")
            Prelude.<*> (x Prelude..?> "DiskIds" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "CacheUsedPercentage")
            Prelude.<*> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCache

instance Prelude.NFData DescribeCache

instance Prelude.ToHeaders DescribeCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DescribeCache" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeCache where
  toJSON DescribeCache' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Prelude..= gatewayARN)]
      )

instance Prelude.ToPath DescribeCache where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCacheResponse' smart constructor.
data DescribeCacheResponse = DescribeCacheResponse'
  { -- | Percent of application read operations from the file shares that are
    -- served from cache. The sample is taken at the end of the reporting
    -- period.
    cacheHitPercentage :: Prelude.Maybe Prelude.Double,
    -- | The file share\'s contribution to the overall percentage of the
    -- gateway\'s cache that has not been persisted to AWS. The sample is taken
    -- at the end of the reporting period.
    cacheDirtyPercentage :: Prelude.Maybe Prelude.Double,
    -- | The amount of cache in bytes allocated to a gateway.
    cacheAllocatedInBytes :: Prelude.Maybe Prelude.Integer,
    -- | Percent of application read operations from the file shares that are not
    -- served from cache. The sample is taken at the end of the reporting
    -- period.
    cacheMissPercentage :: Prelude.Maybe Prelude.Double,
    -- | An array of strings that identify disks that are to be configured as
    -- working storage. Each string has a minimum length of 1 and maximum
    -- length of 300. You can get the disk IDs from the ListLocalDisks API.
    diskIds :: Prelude.Maybe [Prelude.Text],
    -- | Percent use of the gateway\'s cache storage. This metric applies only to
    -- the gateway-cached volume setup. The sample is taken at the end of the
    -- reporting period.
    cacheUsedPercentage :: Prelude.Maybe Prelude.Double,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeCacheResponse
newDescribeCacheResponse pHttpStatus_ =
  DescribeCacheResponse'
    { cacheHitPercentage =
        Prelude.Nothing,
      cacheDirtyPercentage = Prelude.Nothing,
      cacheAllocatedInBytes = Prelude.Nothing,
      cacheMissPercentage = Prelude.Nothing,
      diskIds = Prelude.Nothing,
      cacheUsedPercentage = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Percent of application read operations from the file shares that are
-- served from cache. The sample is taken at the end of the reporting
-- period.
describeCacheResponse_cacheHitPercentage :: Lens.Lens' DescribeCacheResponse (Prelude.Maybe Prelude.Double)
describeCacheResponse_cacheHitPercentage = Lens.lens (\DescribeCacheResponse' {cacheHitPercentage} -> cacheHitPercentage) (\s@DescribeCacheResponse' {} a -> s {cacheHitPercentage = a} :: DescribeCacheResponse)

-- | The file share\'s contribution to the overall percentage of the
-- gateway\'s cache that has not been persisted to AWS. The sample is taken
-- at the end of the reporting period.
describeCacheResponse_cacheDirtyPercentage :: Lens.Lens' DescribeCacheResponse (Prelude.Maybe Prelude.Double)
describeCacheResponse_cacheDirtyPercentage = Lens.lens (\DescribeCacheResponse' {cacheDirtyPercentage} -> cacheDirtyPercentage) (\s@DescribeCacheResponse' {} a -> s {cacheDirtyPercentage = a} :: DescribeCacheResponse)

-- | The amount of cache in bytes allocated to a gateway.
describeCacheResponse_cacheAllocatedInBytes :: Lens.Lens' DescribeCacheResponse (Prelude.Maybe Prelude.Integer)
describeCacheResponse_cacheAllocatedInBytes = Lens.lens (\DescribeCacheResponse' {cacheAllocatedInBytes} -> cacheAllocatedInBytes) (\s@DescribeCacheResponse' {} a -> s {cacheAllocatedInBytes = a} :: DescribeCacheResponse)

-- | Percent of application read operations from the file shares that are not
-- served from cache. The sample is taken at the end of the reporting
-- period.
describeCacheResponse_cacheMissPercentage :: Lens.Lens' DescribeCacheResponse (Prelude.Maybe Prelude.Double)
describeCacheResponse_cacheMissPercentage = Lens.lens (\DescribeCacheResponse' {cacheMissPercentage} -> cacheMissPercentage) (\s@DescribeCacheResponse' {} a -> s {cacheMissPercentage = a} :: DescribeCacheResponse)

-- | An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
describeCacheResponse_diskIds :: Lens.Lens' DescribeCacheResponse (Prelude.Maybe [Prelude.Text])
describeCacheResponse_diskIds = Lens.lens (\DescribeCacheResponse' {diskIds} -> diskIds) (\s@DescribeCacheResponse' {} a -> s {diskIds = a} :: DescribeCacheResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Percent use of the gateway\'s cache storage. This metric applies only to
-- the gateway-cached volume setup. The sample is taken at the end of the
-- reporting period.
describeCacheResponse_cacheUsedPercentage :: Lens.Lens' DescribeCacheResponse (Prelude.Maybe Prelude.Double)
describeCacheResponse_cacheUsedPercentage = Lens.lens (\DescribeCacheResponse' {cacheUsedPercentage} -> cacheUsedPercentage) (\s@DescribeCacheResponse' {} a -> s {cacheUsedPercentage = a} :: DescribeCacheResponse)

-- | Undocumented member.
describeCacheResponse_gatewayARN :: Lens.Lens' DescribeCacheResponse (Prelude.Maybe Prelude.Text)
describeCacheResponse_gatewayARN = Lens.lens (\DescribeCacheResponse' {gatewayARN} -> gatewayARN) (\s@DescribeCacheResponse' {} a -> s {gatewayARN = a} :: DescribeCacheResponse)

-- | The response's http status code.
describeCacheResponse_httpStatus :: Lens.Lens' DescribeCacheResponse Prelude.Int
describeCacheResponse_httpStatus = Lens.lens (\DescribeCacheResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheResponse' {} a -> s {httpStatus = a} :: DescribeCacheResponse)

instance Prelude.NFData DescribeCacheResponse
