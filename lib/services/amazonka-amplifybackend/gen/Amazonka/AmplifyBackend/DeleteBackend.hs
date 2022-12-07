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
-- Module      : Amazonka.AmplifyBackend.DeleteBackend
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an existing environment from your Amplify project.
module Amazonka.AmplifyBackend.DeleteBackend
  ( -- * Creating a Request
    DeleteBackend (..),
    newDeleteBackend,

    -- * Request Lenses
    deleteBackend_appId,
    deleteBackend_backendEnvironmentName,

    -- * Destructuring the Response
    DeleteBackendResponse (..),
    newDeleteBackendResponse,

    -- * Response Lenses
    deleteBackendResponse_jobId,
    deleteBackendResponse_status,
    deleteBackendResponse_error,
    deleteBackendResponse_operation,
    deleteBackendResponse_appId,
    deleteBackendResponse_backendEnvironmentName,
    deleteBackendResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBackend' smart constructor.
data DeleteBackend = DeleteBackend'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackend' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteBackend_appId' - The app ID.
--
-- 'backendEnvironmentName', 'deleteBackend_backendEnvironmentName' - The name of the backend environment.
newDeleteBackend ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  DeleteBackend
newDeleteBackend pAppId_ pBackendEnvironmentName_ =
  DeleteBackend'
    { appId = pAppId_,
      backendEnvironmentName = pBackendEnvironmentName_
    }

-- | The app ID.
deleteBackend_appId :: Lens.Lens' DeleteBackend Prelude.Text
deleteBackend_appId = Lens.lens (\DeleteBackend' {appId} -> appId) (\s@DeleteBackend' {} a -> s {appId = a} :: DeleteBackend)

-- | The name of the backend environment.
deleteBackend_backendEnvironmentName :: Lens.Lens' DeleteBackend Prelude.Text
deleteBackend_backendEnvironmentName = Lens.lens (\DeleteBackend' {backendEnvironmentName} -> backendEnvironmentName) (\s@DeleteBackend' {} a -> s {backendEnvironmentName = a} :: DeleteBackend)

instance Core.AWSRequest DeleteBackend where
  type
    AWSResponse DeleteBackend =
      DeleteBackendResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBackendResponse'
            Prelude.<$> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBackend where
  hashWithSalt _salt DeleteBackend' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName

instance Prelude.NFData DeleteBackend where
  rnf DeleteBackend' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName

instance Data.ToHeaders DeleteBackend where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBackend where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DeleteBackend where
  toPath DeleteBackend' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/environments/",
        Data.toBS backendEnvironmentName,
        "/remove"
      ]

instance Data.ToQuery DeleteBackend where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBackendResponse' smart constructor.
data DeleteBackendResponse = DeleteBackendResponse'
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
-- Create a value of 'DeleteBackendResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'deleteBackendResponse_jobId' - The ID for the job.
--
-- 'status', 'deleteBackendResponse_status' - The current status of the request.
--
-- 'error', 'deleteBackendResponse_error' - If the request fails, this error is returned.
--
-- 'operation', 'deleteBackendResponse_operation' - The name of the operation.
--
-- 'appId', 'deleteBackendResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'deleteBackendResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'httpStatus', 'deleteBackendResponse_httpStatus' - The response's http status code.
newDeleteBackendResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBackendResponse
newDeleteBackendResponse pHttpStatus_ =
  DeleteBackendResponse'
    { jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      error = Prelude.Nothing,
      operation = Prelude.Nothing,
      appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID for the job.
deleteBackendResponse_jobId :: Lens.Lens' DeleteBackendResponse (Prelude.Maybe Prelude.Text)
deleteBackendResponse_jobId = Lens.lens (\DeleteBackendResponse' {jobId} -> jobId) (\s@DeleteBackendResponse' {} a -> s {jobId = a} :: DeleteBackendResponse)

-- | The current status of the request.
deleteBackendResponse_status :: Lens.Lens' DeleteBackendResponse (Prelude.Maybe Prelude.Text)
deleteBackendResponse_status = Lens.lens (\DeleteBackendResponse' {status} -> status) (\s@DeleteBackendResponse' {} a -> s {status = a} :: DeleteBackendResponse)

-- | If the request fails, this error is returned.
deleteBackendResponse_error :: Lens.Lens' DeleteBackendResponse (Prelude.Maybe Prelude.Text)
deleteBackendResponse_error = Lens.lens (\DeleteBackendResponse' {error} -> error) (\s@DeleteBackendResponse' {} a -> s {error = a} :: DeleteBackendResponse)

-- | The name of the operation.
deleteBackendResponse_operation :: Lens.Lens' DeleteBackendResponse (Prelude.Maybe Prelude.Text)
deleteBackendResponse_operation = Lens.lens (\DeleteBackendResponse' {operation} -> operation) (\s@DeleteBackendResponse' {} a -> s {operation = a} :: DeleteBackendResponse)

-- | The app ID.
deleteBackendResponse_appId :: Lens.Lens' DeleteBackendResponse (Prelude.Maybe Prelude.Text)
deleteBackendResponse_appId = Lens.lens (\DeleteBackendResponse' {appId} -> appId) (\s@DeleteBackendResponse' {} a -> s {appId = a} :: DeleteBackendResponse)

-- | The name of the backend environment.
deleteBackendResponse_backendEnvironmentName :: Lens.Lens' DeleteBackendResponse (Prelude.Maybe Prelude.Text)
deleteBackendResponse_backendEnvironmentName = Lens.lens (\DeleteBackendResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@DeleteBackendResponse' {} a -> s {backendEnvironmentName = a} :: DeleteBackendResponse)

-- | The response's http status code.
deleteBackendResponse_httpStatus :: Lens.Lens' DeleteBackendResponse Prelude.Int
deleteBackendResponse_httpStatus = Lens.lens (\DeleteBackendResponse' {httpStatus} -> httpStatus) (\s@DeleteBackendResponse' {} a -> s {httpStatus = a} :: DeleteBackendResponse)

instance Prelude.NFData DeleteBackendResponse where
  rnf DeleteBackendResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf httpStatus
