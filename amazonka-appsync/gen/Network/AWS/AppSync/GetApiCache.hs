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
-- Module      : Network.AWS.AppSync.GetApiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an @ApiCache@ object.
module Network.AWS.AppSync.GetApiCache
  ( -- * Creating a Request
    GetApiCache (..),
    newGetApiCache,

    -- * Request Lenses
    getApiCache_apiId,

    -- * Destructuring the Response
    GetApiCacheResponse (..),
    newGetApiCacheResponse,

    -- * Response Lenses
    getApiCacheResponse_apiCache,
    getApiCacheResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetApiCache@ operation.
--
-- /See:/ 'newGetApiCache' smart constructor.
data GetApiCache = GetApiCache'
  { -- | The API ID.
    apiId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApiCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getApiCache_apiId' - The API ID.
newGetApiCache ::
  -- | 'apiId'
  Core.Text ->
  GetApiCache
newGetApiCache pApiId_ =
  GetApiCache' {apiId = pApiId_}

-- | The API ID.
getApiCache_apiId :: Lens.Lens' GetApiCache Core.Text
getApiCache_apiId = Lens.lens (\GetApiCache' {apiId} -> apiId) (\s@GetApiCache' {} a -> s {apiId = a} :: GetApiCache)

instance Core.AWSRequest GetApiCache where
  type AWSResponse GetApiCache = GetApiCacheResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApiCacheResponse'
            Core.<$> (x Core..?> "apiCache")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetApiCache

instance Core.NFData GetApiCache

instance Core.ToHeaders GetApiCache where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetApiCache where
  toPath GetApiCache' {..} =
    Core.mconcat
      ["/v1/apis/", Core.toBS apiId, "/ApiCaches"]

instance Core.ToQuery GetApiCache where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetApiCache@ operation.
--
-- /See:/ 'newGetApiCacheResponse' smart constructor.
data GetApiCacheResponse = GetApiCacheResponse'
  { -- | The @ApiCache@ object.
    apiCache :: Core.Maybe ApiCache,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApiCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiCache', 'getApiCacheResponse_apiCache' - The @ApiCache@ object.
--
-- 'httpStatus', 'getApiCacheResponse_httpStatus' - The response's http status code.
newGetApiCacheResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetApiCacheResponse
newGetApiCacheResponse pHttpStatus_ =
  GetApiCacheResponse'
    { apiCache = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ApiCache@ object.
getApiCacheResponse_apiCache :: Lens.Lens' GetApiCacheResponse (Core.Maybe ApiCache)
getApiCacheResponse_apiCache = Lens.lens (\GetApiCacheResponse' {apiCache} -> apiCache) (\s@GetApiCacheResponse' {} a -> s {apiCache = a} :: GetApiCacheResponse)

-- | The response's http status code.
getApiCacheResponse_httpStatus :: Lens.Lens' GetApiCacheResponse Core.Int
getApiCacheResponse_httpStatus = Lens.lens (\GetApiCacheResponse' {httpStatus} -> httpStatus) (\s@GetApiCacheResponse' {} a -> s {httpStatus = a} :: GetApiCacheResponse)

instance Core.NFData GetApiCacheResponse
