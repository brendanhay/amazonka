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
-- Module      : Amazonka.FSx.UpdateFileCache
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an existing Amazon File Cache resource. You
-- can update multiple properties in a single request.
module Amazonka.FSx.UpdateFileCache
  ( -- * Creating a Request
    UpdateFileCache (..),
    newUpdateFileCache,

    -- * Request Lenses
    updateFileCache_clientRequestToken,
    updateFileCache_lustreConfiguration,
    updateFileCache_fileCacheId,

    -- * Destructuring the Response
    UpdateFileCacheResponse (..),
    newUpdateFileCacheResponse,

    -- * Response Lenses
    updateFileCacheResponse_fileCache,
    updateFileCacheResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFileCache' smart constructor.
data UpdateFileCache = UpdateFileCache'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The configuration updates for an Amazon File Cache resource.
    lustreConfiguration :: Prelude.Maybe UpdateFileCacheLustreConfiguration,
    -- | The ID of the cache that you are updating.
    fileCacheId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFileCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'updateFileCache_clientRequestToken' - Undocumented member.
--
-- 'lustreConfiguration', 'updateFileCache_lustreConfiguration' - The configuration updates for an Amazon File Cache resource.
--
-- 'fileCacheId', 'updateFileCache_fileCacheId' - The ID of the cache that you are updating.
newUpdateFileCache ::
  -- | 'fileCacheId'
  Prelude.Text ->
  UpdateFileCache
newUpdateFileCache pFileCacheId_ =
  UpdateFileCache'
    { clientRequestToken =
        Prelude.Nothing,
      lustreConfiguration = Prelude.Nothing,
      fileCacheId = pFileCacheId_
    }

-- | Undocumented member.
updateFileCache_clientRequestToken :: Lens.Lens' UpdateFileCache (Prelude.Maybe Prelude.Text)
updateFileCache_clientRequestToken = Lens.lens (\UpdateFileCache' {clientRequestToken} -> clientRequestToken) (\s@UpdateFileCache' {} a -> s {clientRequestToken = a} :: UpdateFileCache)

-- | The configuration updates for an Amazon File Cache resource.
updateFileCache_lustreConfiguration :: Lens.Lens' UpdateFileCache (Prelude.Maybe UpdateFileCacheLustreConfiguration)
updateFileCache_lustreConfiguration = Lens.lens (\UpdateFileCache' {lustreConfiguration} -> lustreConfiguration) (\s@UpdateFileCache' {} a -> s {lustreConfiguration = a} :: UpdateFileCache)

-- | The ID of the cache that you are updating.
updateFileCache_fileCacheId :: Lens.Lens' UpdateFileCache Prelude.Text
updateFileCache_fileCacheId = Lens.lens (\UpdateFileCache' {fileCacheId} -> fileCacheId) (\s@UpdateFileCache' {} a -> s {fileCacheId = a} :: UpdateFileCache)

instance Core.AWSRequest UpdateFileCache where
  type
    AWSResponse UpdateFileCache =
      UpdateFileCacheResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFileCacheResponse'
            Prelude.<$> (x Data..?> "FileCache")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFileCache where
  hashWithSalt _salt UpdateFileCache' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` lustreConfiguration
      `Prelude.hashWithSalt` fileCacheId

instance Prelude.NFData UpdateFileCache where
  rnf UpdateFileCache' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf lustreConfiguration
      `Prelude.seq` Prelude.rnf fileCacheId

instance Data.ToHeaders UpdateFileCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.UpdateFileCache" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFileCache where
  toJSON UpdateFileCache' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("LustreConfiguration" Data..=)
              Prelude.<$> lustreConfiguration,
            Prelude.Just ("FileCacheId" Data..= fileCacheId)
          ]
      )

instance Data.ToPath UpdateFileCache where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFileCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFileCacheResponse' smart constructor.
data UpdateFileCacheResponse = UpdateFileCacheResponse'
  { -- | A description of the cache that was updated.
    fileCache :: Prelude.Maybe FileCache,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFileCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileCache', 'updateFileCacheResponse_fileCache' - A description of the cache that was updated.
--
-- 'httpStatus', 'updateFileCacheResponse_httpStatus' - The response's http status code.
newUpdateFileCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFileCacheResponse
newUpdateFileCacheResponse pHttpStatus_ =
  UpdateFileCacheResponse'
    { fileCache =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the cache that was updated.
updateFileCacheResponse_fileCache :: Lens.Lens' UpdateFileCacheResponse (Prelude.Maybe FileCache)
updateFileCacheResponse_fileCache = Lens.lens (\UpdateFileCacheResponse' {fileCache} -> fileCache) (\s@UpdateFileCacheResponse' {} a -> s {fileCache = a} :: UpdateFileCacheResponse)

-- | The response's http status code.
updateFileCacheResponse_httpStatus :: Lens.Lens' UpdateFileCacheResponse Prelude.Int
updateFileCacheResponse_httpStatus = Lens.lens (\UpdateFileCacheResponse' {httpStatus} -> httpStatus) (\s@UpdateFileCacheResponse' {} a -> s {httpStatus = a} :: UpdateFileCacheResponse)

instance Prelude.NFData UpdateFileCacheResponse where
  rnf UpdateFileCacheResponse' {..} =
    Prelude.rnf fileCache
      `Prelude.seq` Prelude.rnf httpStatus
