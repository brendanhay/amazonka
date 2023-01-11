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
-- Module      : Amazonka.FSx.DeleteFileCache
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon File Cache resource. After deletion, the cache no
-- longer exists, and its data is gone.
--
-- The @DeleteFileCache@ operation returns while the cache has the
-- @DELETING@ status. You can check the cache deletion status by calling
-- the
-- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_DescribeFileCaches.html DescribeFileCaches>
-- operation, which returns a list of caches in your account. If you pass
-- the cache ID for a deleted cache, the @DescribeFileCaches@ operation
-- returns a @FileCacheNotFound@ error.
--
-- The data in a deleted cache is also deleted and can\'t be recovered by
-- any means.
module Amazonka.FSx.DeleteFileCache
  ( -- * Creating a Request
    DeleteFileCache (..),
    newDeleteFileCache,

    -- * Request Lenses
    deleteFileCache_clientRequestToken,
    deleteFileCache_fileCacheId,

    -- * Destructuring the Response
    DeleteFileCacheResponse (..),
    newDeleteFileCacheResponse,

    -- * Response Lenses
    deleteFileCacheResponse_fileCacheId,
    deleteFileCacheResponse_lifecycle,
    deleteFileCacheResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFileCache' smart constructor.
data DeleteFileCache = DeleteFileCache'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the cache that\'s being deleted.
    fileCacheId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'deleteFileCache_clientRequestToken' - Undocumented member.
--
-- 'fileCacheId', 'deleteFileCache_fileCacheId' - The ID of the cache that\'s being deleted.
newDeleteFileCache ::
  -- | 'fileCacheId'
  Prelude.Text ->
  DeleteFileCache
newDeleteFileCache pFileCacheId_ =
  DeleteFileCache'
    { clientRequestToken =
        Prelude.Nothing,
      fileCacheId = pFileCacheId_
    }

-- | Undocumented member.
deleteFileCache_clientRequestToken :: Lens.Lens' DeleteFileCache (Prelude.Maybe Prelude.Text)
deleteFileCache_clientRequestToken = Lens.lens (\DeleteFileCache' {clientRequestToken} -> clientRequestToken) (\s@DeleteFileCache' {} a -> s {clientRequestToken = a} :: DeleteFileCache)

-- | The ID of the cache that\'s being deleted.
deleteFileCache_fileCacheId :: Lens.Lens' DeleteFileCache Prelude.Text
deleteFileCache_fileCacheId = Lens.lens (\DeleteFileCache' {fileCacheId} -> fileCacheId) (\s@DeleteFileCache' {} a -> s {fileCacheId = a} :: DeleteFileCache)

instance Core.AWSRequest DeleteFileCache where
  type
    AWSResponse DeleteFileCache =
      DeleteFileCacheResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFileCacheResponse'
            Prelude.<$> (x Data..?> "FileCacheId")
            Prelude.<*> (x Data..?> "Lifecycle")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFileCache where
  hashWithSalt _salt DeleteFileCache' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` fileCacheId

instance Prelude.NFData DeleteFileCache where
  rnf DeleteFileCache' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf fileCacheId

instance Data.ToHeaders DeleteFileCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DeleteFileCache" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFileCache where
  toJSON DeleteFileCache' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("FileCacheId" Data..= fileCacheId)
          ]
      )

instance Data.ToPath DeleteFileCache where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFileCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFileCacheResponse' smart constructor.
data DeleteFileCacheResponse = DeleteFileCacheResponse'
  { -- | The ID of the cache that\'s being deleted.
    fileCacheId :: Prelude.Maybe Prelude.Text,
    -- | The cache lifecycle for the deletion request. If the @DeleteFileCache@
    -- operation is successful, this status is @DELETING@.
    lifecycle :: Prelude.Maybe FileCacheLifecycle,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileCacheId', 'deleteFileCacheResponse_fileCacheId' - The ID of the cache that\'s being deleted.
--
-- 'lifecycle', 'deleteFileCacheResponse_lifecycle' - The cache lifecycle for the deletion request. If the @DeleteFileCache@
-- operation is successful, this status is @DELETING@.
--
-- 'httpStatus', 'deleteFileCacheResponse_httpStatus' - The response's http status code.
newDeleteFileCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFileCacheResponse
newDeleteFileCacheResponse pHttpStatus_ =
  DeleteFileCacheResponse'
    { fileCacheId =
        Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the cache that\'s being deleted.
deleteFileCacheResponse_fileCacheId :: Lens.Lens' DeleteFileCacheResponse (Prelude.Maybe Prelude.Text)
deleteFileCacheResponse_fileCacheId = Lens.lens (\DeleteFileCacheResponse' {fileCacheId} -> fileCacheId) (\s@DeleteFileCacheResponse' {} a -> s {fileCacheId = a} :: DeleteFileCacheResponse)

-- | The cache lifecycle for the deletion request. If the @DeleteFileCache@
-- operation is successful, this status is @DELETING@.
deleteFileCacheResponse_lifecycle :: Lens.Lens' DeleteFileCacheResponse (Prelude.Maybe FileCacheLifecycle)
deleteFileCacheResponse_lifecycle = Lens.lens (\DeleteFileCacheResponse' {lifecycle} -> lifecycle) (\s@DeleteFileCacheResponse' {} a -> s {lifecycle = a} :: DeleteFileCacheResponse)

-- | The response's http status code.
deleteFileCacheResponse_httpStatus :: Lens.Lens' DeleteFileCacheResponse Prelude.Int
deleteFileCacheResponse_httpStatus = Lens.lens (\DeleteFileCacheResponse' {httpStatus} -> httpStatus) (\s@DeleteFileCacheResponse' {} a -> s {httpStatus = a} :: DeleteFileCacheResponse)

instance Prelude.NFData DeleteFileCacheResponse where
  rnf DeleteFileCacheResponse' {..} =
    Prelude.rnf fileCacheId
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf httpStatus
