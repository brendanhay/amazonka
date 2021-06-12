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
-- Module      : Network.AWS.AppSync.DeleteApiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an @ApiCache@ object.
module Network.AWS.AppSync.DeleteApiCache
  ( -- * Creating a Request
    DeleteApiCache (..),
    newDeleteApiCache,

    -- * Request Lenses
    deleteApiCache_apiId,

    -- * Destructuring the Response
    DeleteApiCacheResponse (..),
    newDeleteApiCacheResponse,

    -- * Response Lenses
    deleteApiCacheResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteApiCache@ operation.
--
-- /See:/ 'newDeleteApiCache' smart constructor.
data DeleteApiCache = DeleteApiCache'
  { -- | The API ID.
    apiId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApiCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteApiCache_apiId' - The API ID.
newDeleteApiCache ::
  -- | 'apiId'
  Core.Text ->
  DeleteApiCache
newDeleteApiCache pApiId_ =
  DeleteApiCache' {apiId = pApiId_}

-- | The API ID.
deleteApiCache_apiId :: Lens.Lens' DeleteApiCache Core.Text
deleteApiCache_apiId = Lens.lens (\DeleteApiCache' {apiId} -> apiId) (\s@DeleteApiCache' {} a -> s {apiId = a} :: DeleteApiCache)

instance Core.AWSRequest DeleteApiCache where
  type
    AWSResponse DeleteApiCache =
      DeleteApiCacheResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApiCacheResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteApiCache

instance Core.NFData DeleteApiCache

instance Core.ToHeaders DeleteApiCache where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteApiCache where
  toPath DeleteApiCache' {..} =
    Core.mconcat
      ["/v1/apis/", Core.toBS apiId, "/ApiCaches"]

instance Core.ToQuery DeleteApiCache where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @DeleteApiCache@ operation.
--
-- /See:/ 'newDeleteApiCacheResponse' smart constructor.
data DeleteApiCacheResponse = DeleteApiCacheResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApiCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApiCacheResponse_httpStatus' - The response's http status code.
newDeleteApiCacheResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteApiCacheResponse
newDeleteApiCacheResponse pHttpStatus_ =
  DeleteApiCacheResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteApiCacheResponse_httpStatus :: Lens.Lens' DeleteApiCacheResponse Core.Int
deleteApiCacheResponse_httpStatus = Lens.lens (\DeleteApiCacheResponse' {httpStatus} -> httpStatus) (\s@DeleteApiCacheResponse' {} a -> s {httpStatus = a} :: DeleteApiCacheResponse)

instance Core.NFData DeleteApiCacheResponse
