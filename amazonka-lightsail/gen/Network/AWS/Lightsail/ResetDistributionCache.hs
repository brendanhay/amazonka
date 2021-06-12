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
-- Module      : Network.AWS.Lightsail.ResetDistributionCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes currently cached content from your Amazon Lightsail content
-- delivery network (CDN) distribution.
--
-- After resetting the cache, the next time a content request is made, your
-- distribution pulls, serves, and caches it from the origin.
module Network.AWS.Lightsail.ResetDistributionCache
  ( -- * Creating a Request
    ResetDistributionCache (..),
    newResetDistributionCache,

    -- * Request Lenses
    resetDistributionCache_distributionName,

    -- * Destructuring the Response
    ResetDistributionCacheResponse (..),
    newResetDistributionCacheResponse,

    -- * Response Lenses
    resetDistributionCacheResponse_status,
    resetDistributionCacheResponse_operation,
    resetDistributionCacheResponse_createTime,
    resetDistributionCacheResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResetDistributionCache' smart constructor.
data ResetDistributionCache = ResetDistributionCache'
  { -- | The name of the distribution for which to reset cache.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    distributionName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResetDistributionCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionName', 'resetDistributionCache_distributionName' - The name of the distribution for which to reset cache.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
newResetDistributionCache ::
  ResetDistributionCache
newResetDistributionCache =
  ResetDistributionCache'
    { distributionName =
        Core.Nothing
    }

-- | The name of the distribution for which to reset cache.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
resetDistributionCache_distributionName :: Lens.Lens' ResetDistributionCache (Core.Maybe Core.Text)
resetDistributionCache_distributionName = Lens.lens (\ResetDistributionCache' {distributionName} -> distributionName) (\s@ResetDistributionCache' {} a -> s {distributionName = a} :: ResetDistributionCache)

instance Core.AWSRequest ResetDistributionCache where
  type
    AWSResponse ResetDistributionCache =
      ResetDistributionCacheResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetDistributionCacheResponse'
            Core.<$> (x Core..?> "status")
            Core.<*> (x Core..?> "operation")
            Core.<*> (x Core..?> "createTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ResetDistributionCache

instance Core.NFData ResetDistributionCache

instance Core.ToHeaders ResetDistributionCache where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.ResetDistributionCache" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ResetDistributionCache where
  toJSON ResetDistributionCache' {..} =
    Core.object
      ( Core.catMaybes
          [ ("distributionName" Core..=)
              Core.<$> distributionName
          ]
      )

instance Core.ToPath ResetDistributionCache where
  toPath = Core.const "/"

instance Core.ToQuery ResetDistributionCache where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newResetDistributionCacheResponse' smart constructor.
data ResetDistributionCacheResponse = ResetDistributionCacheResponse'
  { -- | The status of the reset cache request.
    status :: Core.Maybe Core.Text,
    -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Core.Maybe Operation,
    -- | The timestamp of the reset cache request (e.g., @1479734909.17@) in Unix
    -- time format.
    createTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResetDistributionCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'resetDistributionCacheResponse_status' - The status of the reset cache request.
--
-- 'operation', 'resetDistributionCacheResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'createTime', 'resetDistributionCacheResponse_createTime' - The timestamp of the reset cache request (e.g., @1479734909.17@) in Unix
-- time format.
--
-- 'httpStatus', 'resetDistributionCacheResponse_httpStatus' - The response's http status code.
newResetDistributionCacheResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ResetDistributionCacheResponse
newResetDistributionCacheResponse pHttpStatus_ =
  ResetDistributionCacheResponse'
    { status =
        Core.Nothing,
      operation = Core.Nothing,
      createTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the reset cache request.
resetDistributionCacheResponse_status :: Lens.Lens' ResetDistributionCacheResponse (Core.Maybe Core.Text)
resetDistributionCacheResponse_status = Lens.lens (\ResetDistributionCacheResponse' {status} -> status) (\s@ResetDistributionCacheResponse' {} a -> s {status = a} :: ResetDistributionCacheResponse)

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
resetDistributionCacheResponse_operation :: Lens.Lens' ResetDistributionCacheResponse (Core.Maybe Operation)
resetDistributionCacheResponse_operation = Lens.lens (\ResetDistributionCacheResponse' {operation} -> operation) (\s@ResetDistributionCacheResponse' {} a -> s {operation = a} :: ResetDistributionCacheResponse)

-- | The timestamp of the reset cache request (e.g., @1479734909.17@) in Unix
-- time format.
resetDistributionCacheResponse_createTime :: Lens.Lens' ResetDistributionCacheResponse (Core.Maybe Core.UTCTime)
resetDistributionCacheResponse_createTime = Lens.lens (\ResetDistributionCacheResponse' {createTime} -> createTime) (\s@ResetDistributionCacheResponse' {} a -> s {createTime = a} :: ResetDistributionCacheResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
resetDistributionCacheResponse_httpStatus :: Lens.Lens' ResetDistributionCacheResponse Core.Int
resetDistributionCacheResponse_httpStatus = Lens.lens (\ResetDistributionCacheResponse' {httpStatus} -> httpStatus) (\s@ResetDistributionCacheResponse' {} a -> s {httpStatus = a} :: ResetDistributionCacheResponse)

instance Core.NFData ResetDistributionCacheResponse
