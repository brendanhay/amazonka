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
-- Module      : Network.AWS.AppSync.FlushApiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes an @ApiCache@ object.
module Network.AWS.AppSync.FlushApiCache
  ( -- * Creating a Request
    FlushApiCache (..),
    newFlushApiCache,

    -- * Request Lenses
    flushApiCache_apiId,

    -- * Destructuring the Response
    FlushApiCacheResponse (..),
    newFlushApiCacheResponse,

    -- * Response Lenses
    flushApiCacheResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @FlushApiCache@ operation.
--
-- /See:/ 'newFlushApiCache' smart constructor.
data FlushApiCache = FlushApiCache'
  { -- | The API ID.
    apiId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FlushApiCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'flushApiCache_apiId' - The API ID.
newFlushApiCache ::
  -- | 'apiId'
  Core.Text ->
  FlushApiCache
newFlushApiCache pApiId_ =
  FlushApiCache' {apiId = pApiId_}

-- | The API ID.
flushApiCache_apiId :: Lens.Lens' FlushApiCache Core.Text
flushApiCache_apiId = Lens.lens (\FlushApiCache' {apiId} -> apiId) (\s@FlushApiCache' {} a -> s {apiId = a} :: FlushApiCache)

instance Core.AWSRequest FlushApiCache where
  type
    AWSResponse FlushApiCache =
      FlushApiCacheResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          FlushApiCacheResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable FlushApiCache

instance Core.NFData FlushApiCache

instance Core.ToHeaders FlushApiCache where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath FlushApiCache where
  toPath FlushApiCache' {..} =
    Core.mconcat
      ["/v1/apis/", Core.toBS apiId, "/FlushCache"]

instance Core.ToQuery FlushApiCache where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @FlushApiCache@ operation.
--
-- /See:/ 'newFlushApiCacheResponse' smart constructor.
data FlushApiCacheResponse = FlushApiCacheResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FlushApiCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'flushApiCacheResponse_httpStatus' - The response's http status code.
newFlushApiCacheResponse ::
  -- | 'httpStatus'
  Core.Int ->
  FlushApiCacheResponse
newFlushApiCacheResponse pHttpStatus_ =
  FlushApiCacheResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
flushApiCacheResponse_httpStatus :: Lens.Lens' FlushApiCacheResponse Core.Int
flushApiCacheResponse_httpStatus = Lens.lens (\FlushApiCacheResponse' {httpStatus} -> httpStatus) (\s@FlushApiCacheResponse' {} a -> s {httpStatus = a} :: FlushApiCacheResponse)

instance Core.NFData FlushApiCacheResponse
