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
-- Module      : Amazonka.AppSync.DeleteApiCache
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an @ApiCache@ object.
module Amazonka.AppSync.DeleteApiCache
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

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DeleteApiCache@ operation.
--
-- /See:/ 'newDeleteApiCache' smart constructor.
data DeleteApiCache = DeleteApiCache'
  { -- | The API ID.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteApiCache
newDeleteApiCache pApiId_ =
  DeleteApiCache' {apiId = pApiId_}

-- | The API ID.
deleteApiCache_apiId :: Lens.Lens' DeleteApiCache Prelude.Text
deleteApiCache_apiId = Lens.lens (\DeleteApiCache' {apiId} -> apiId) (\s@DeleteApiCache' {} a -> s {apiId = a} :: DeleteApiCache)

instance Core.AWSRequest DeleteApiCache where
  type
    AWSResponse DeleteApiCache =
      DeleteApiCacheResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApiCacheResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApiCache where
  hashWithSalt _salt DeleteApiCache' {..} =
    _salt `Prelude.hashWithSalt` apiId

instance Prelude.NFData DeleteApiCache where
  rnf DeleteApiCache' {..} = Prelude.rnf apiId

instance Core.ToHeaders DeleteApiCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteApiCache where
  toPath DeleteApiCache' {..} =
    Prelude.mconcat
      ["/v1/apis/", Core.toBS apiId, "/ApiCaches"]

instance Core.ToQuery DeleteApiCache where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @DeleteApiCache@ operation.
--
-- /See:/ 'newDeleteApiCacheResponse' smart constructor.
data DeleteApiCacheResponse = DeleteApiCacheResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteApiCacheResponse
newDeleteApiCacheResponse pHttpStatus_ =
  DeleteApiCacheResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteApiCacheResponse_httpStatus :: Lens.Lens' DeleteApiCacheResponse Prelude.Int
deleteApiCacheResponse_httpStatus = Lens.lens (\DeleteApiCacheResponse' {httpStatus} -> httpStatus) (\s@DeleteApiCacheResponse' {} a -> s {httpStatus = a} :: DeleteApiCacheResponse)

instance Prelude.NFData DeleteApiCacheResponse where
  rnf DeleteApiCacheResponse' {..} =
    Prelude.rnf httpStatus
