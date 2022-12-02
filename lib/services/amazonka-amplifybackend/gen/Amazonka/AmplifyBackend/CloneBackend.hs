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
-- Module      : Amazonka.AmplifyBackend.CloneBackend
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation clones an existing backend.
module Amazonka.AmplifyBackend.CloneBackend
  ( -- * Creating a Request
    CloneBackend (..),
    newCloneBackend,

    -- * Request Lenses
    cloneBackend_appId,
    cloneBackend_backendEnvironmentName,
    cloneBackend_targetEnvironmentName,

    -- * Destructuring the Response
    CloneBackendResponse (..),
    newCloneBackendResponse,

    -- * Response Lenses
    cloneBackendResponse_jobId,
    cloneBackendResponse_status,
    cloneBackendResponse_error,
    cloneBackendResponse_operation,
    cloneBackendResponse_appId,
    cloneBackendResponse_backendEnvironmentName,
    cloneBackendResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for CloneBackend.
--
-- /See:/ 'newCloneBackend' smart constructor.
data CloneBackend = CloneBackend'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The name of the destination backend environment to be created.
    targetEnvironmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloneBackend' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'cloneBackend_appId' - The app ID.
--
-- 'backendEnvironmentName', 'cloneBackend_backendEnvironmentName' - The name of the backend environment.
--
-- 'targetEnvironmentName', 'cloneBackend_targetEnvironmentName' - The name of the destination backend environment to be created.
newCloneBackend ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'targetEnvironmentName'
  Prelude.Text ->
  CloneBackend
newCloneBackend
  pAppId_
  pBackendEnvironmentName_
  pTargetEnvironmentName_ =
    CloneBackend'
      { appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        targetEnvironmentName = pTargetEnvironmentName_
      }

-- | The app ID.
cloneBackend_appId :: Lens.Lens' CloneBackend Prelude.Text
cloneBackend_appId = Lens.lens (\CloneBackend' {appId} -> appId) (\s@CloneBackend' {} a -> s {appId = a} :: CloneBackend)

-- | The name of the backend environment.
cloneBackend_backendEnvironmentName :: Lens.Lens' CloneBackend Prelude.Text
cloneBackend_backendEnvironmentName = Lens.lens (\CloneBackend' {backendEnvironmentName} -> backendEnvironmentName) (\s@CloneBackend' {} a -> s {backendEnvironmentName = a} :: CloneBackend)

-- | The name of the destination backend environment to be created.
cloneBackend_targetEnvironmentName :: Lens.Lens' CloneBackend Prelude.Text
cloneBackend_targetEnvironmentName = Lens.lens (\CloneBackend' {targetEnvironmentName} -> targetEnvironmentName) (\s@CloneBackend' {} a -> s {targetEnvironmentName = a} :: CloneBackend)

instance Core.AWSRequest CloneBackend where
  type AWSResponse CloneBackend = CloneBackendResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CloneBackendResponse'
            Prelude.<$> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CloneBackend where
  hashWithSalt _salt CloneBackend' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` targetEnvironmentName

instance Prelude.NFData CloneBackend where
  rnf CloneBackend' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf targetEnvironmentName

instance Data.ToHeaders CloneBackend where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CloneBackend where
  toJSON CloneBackend' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "targetEnvironmentName"
                  Data..= targetEnvironmentName
              )
          ]
      )

instance Data.ToPath CloneBackend where
  toPath CloneBackend' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/environments/",
        Data.toBS backendEnvironmentName,
        "/clone"
      ]

instance Data.ToQuery CloneBackend where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCloneBackendResponse' smart constructor.
data CloneBackendResponse = CloneBackendResponse'
  { -- | The ID for the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the request.
    status :: Prelude.Maybe Prelude.Text,
    -- | If the request fails, this error is returned.
    error :: Prelude.Maybe Prelude.Text,
    -- | The name of the operation.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloneBackendResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'cloneBackendResponse_jobId' - The ID for the job.
--
-- 'status', 'cloneBackendResponse_status' - The current status of the request.
--
-- 'error', 'cloneBackendResponse_error' - If the request fails, this error is returned.
--
-- 'operation', 'cloneBackendResponse_operation' - The name of the operation.
--
-- 'appId', 'cloneBackendResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'cloneBackendResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'httpStatus', 'cloneBackendResponse_httpStatus' - The response's http status code.
newCloneBackendResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CloneBackendResponse
newCloneBackendResponse pHttpStatus_ =
  CloneBackendResponse'
    { jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      error = Prelude.Nothing,
      operation = Prelude.Nothing,
      appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID for the job.
cloneBackendResponse_jobId :: Lens.Lens' CloneBackendResponse (Prelude.Maybe Prelude.Text)
cloneBackendResponse_jobId = Lens.lens (\CloneBackendResponse' {jobId} -> jobId) (\s@CloneBackendResponse' {} a -> s {jobId = a} :: CloneBackendResponse)

-- | The current status of the request.
cloneBackendResponse_status :: Lens.Lens' CloneBackendResponse (Prelude.Maybe Prelude.Text)
cloneBackendResponse_status = Lens.lens (\CloneBackendResponse' {status} -> status) (\s@CloneBackendResponse' {} a -> s {status = a} :: CloneBackendResponse)

-- | If the request fails, this error is returned.
cloneBackendResponse_error :: Lens.Lens' CloneBackendResponse (Prelude.Maybe Prelude.Text)
cloneBackendResponse_error = Lens.lens (\CloneBackendResponse' {error} -> error) (\s@CloneBackendResponse' {} a -> s {error = a} :: CloneBackendResponse)

-- | The name of the operation.
cloneBackendResponse_operation :: Lens.Lens' CloneBackendResponse (Prelude.Maybe Prelude.Text)
cloneBackendResponse_operation = Lens.lens (\CloneBackendResponse' {operation} -> operation) (\s@CloneBackendResponse' {} a -> s {operation = a} :: CloneBackendResponse)

-- | The app ID.
cloneBackendResponse_appId :: Lens.Lens' CloneBackendResponse (Prelude.Maybe Prelude.Text)
cloneBackendResponse_appId = Lens.lens (\CloneBackendResponse' {appId} -> appId) (\s@CloneBackendResponse' {} a -> s {appId = a} :: CloneBackendResponse)

-- | The name of the backend environment.
cloneBackendResponse_backendEnvironmentName :: Lens.Lens' CloneBackendResponse (Prelude.Maybe Prelude.Text)
cloneBackendResponse_backendEnvironmentName = Lens.lens (\CloneBackendResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@CloneBackendResponse' {} a -> s {backendEnvironmentName = a} :: CloneBackendResponse)

-- | The response's http status code.
cloneBackendResponse_httpStatus :: Lens.Lens' CloneBackendResponse Prelude.Int
cloneBackendResponse_httpStatus = Lens.lens (\CloneBackendResponse' {httpStatus} -> httpStatus) (\s@CloneBackendResponse' {} a -> s {httpStatus = a} :: CloneBackendResponse)

instance Prelude.NFData CloneBackendResponse where
  rnf CloneBackendResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf httpStatus
