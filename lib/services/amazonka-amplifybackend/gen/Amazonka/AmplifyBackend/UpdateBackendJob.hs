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
-- Module      : Amazonka.AmplifyBackend.UpdateBackendJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specific job.
module Amazonka.AmplifyBackend.UpdateBackendJob
  ( -- * Creating a Request
    UpdateBackendJob (..),
    newUpdateBackendJob,

    -- * Request Lenses
    updateBackendJob_operation,
    updateBackendJob_status,
    updateBackendJob_appId,
    updateBackendJob_backendEnvironmentName,
    updateBackendJob_jobId,

    -- * Destructuring the Response
    UpdateBackendJobResponse (..),
    newUpdateBackendJobResponse,

    -- * Response Lenses
    updateBackendJobResponse_appId,
    updateBackendJobResponse_backendEnvironmentName,
    updateBackendJobResponse_createTime,
    updateBackendJobResponse_error,
    updateBackendJobResponse_jobId,
    updateBackendJobResponse_operation,
    updateBackendJobResponse_status,
    updateBackendJobResponse_updateTime,
    updateBackendJobResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for GetBackendJob.
--
-- /See:/ 'newUpdateBackendJob' smart constructor.
data UpdateBackendJob = UpdateBackendJob'
  { -- | Filters the list of response objects to include only those with the
    -- specified operation name.
    operation :: Prelude.Maybe Prelude.Text,
    -- | Filters the list of response objects to include only those with the
    -- specified status.
    status :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The ID for the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'updateBackendJob_operation' - Filters the list of response objects to include only those with the
-- specified operation name.
--
-- 'status', 'updateBackendJob_status' - Filters the list of response objects to include only those with the
-- specified status.
--
-- 'appId', 'updateBackendJob_appId' - The app ID.
--
-- 'backendEnvironmentName', 'updateBackendJob_backendEnvironmentName' - The name of the backend environment.
--
-- 'jobId', 'updateBackendJob_jobId' - The ID for the job.
newUpdateBackendJob ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  UpdateBackendJob
newUpdateBackendJob
  pAppId_
  pBackendEnvironmentName_
  pJobId_ =
    UpdateBackendJob'
      { operation = Prelude.Nothing,
        status = Prelude.Nothing,
        appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        jobId = pJobId_
      }

-- | Filters the list of response objects to include only those with the
-- specified operation name.
updateBackendJob_operation :: Lens.Lens' UpdateBackendJob (Prelude.Maybe Prelude.Text)
updateBackendJob_operation = Lens.lens (\UpdateBackendJob' {operation} -> operation) (\s@UpdateBackendJob' {} a -> s {operation = a} :: UpdateBackendJob)

-- | Filters the list of response objects to include only those with the
-- specified status.
updateBackendJob_status :: Lens.Lens' UpdateBackendJob (Prelude.Maybe Prelude.Text)
updateBackendJob_status = Lens.lens (\UpdateBackendJob' {status} -> status) (\s@UpdateBackendJob' {} a -> s {status = a} :: UpdateBackendJob)

-- | The app ID.
updateBackendJob_appId :: Lens.Lens' UpdateBackendJob Prelude.Text
updateBackendJob_appId = Lens.lens (\UpdateBackendJob' {appId} -> appId) (\s@UpdateBackendJob' {} a -> s {appId = a} :: UpdateBackendJob)

-- | The name of the backend environment.
updateBackendJob_backendEnvironmentName :: Lens.Lens' UpdateBackendJob Prelude.Text
updateBackendJob_backendEnvironmentName = Lens.lens (\UpdateBackendJob' {backendEnvironmentName} -> backendEnvironmentName) (\s@UpdateBackendJob' {} a -> s {backendEnvironmentName = a} :: UpdateBackendJob)

-- | The ID for the job.
updateBackendJob_jobId :: Lens.Lens' UpdateBackendJob Prelude.Text
updateBackendJob_jobId = Lens.lens (\UpdateBackendJob' {jobId} -> jobId) (\s@UpdateBackendJob' {} a -> s {jobId = a} :: UpdateBackendJob)

instance Core.AWSRequest UpdateBackendJob where
  type
    AWSResponse UpdateBackendJob =
      UpdateBackendJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBackendJobResponse'
            Prelude.<$> (x Data..?> "appId")
            Prelude.<*> (x Data..?> "backendEnvironmentName")
            Prelude.<*> (x Data..?> "createTime")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "updateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBackendJob where
  hashWithSalt _salt UpdateBackendJob' {..} =
    _salt `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData UpdateBackendJob where
  rnf UpdateBackendJob' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders UpdateBackendJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBackendJob where
  toJSON UpdateBackendJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("operation" Data..=) Prelude.<$> operation,
            ("status" Data..=) Prelude.<$> status
          ]
      )

instance Data.ToPath UpdateBackendJob where
  toPath UpdateBackendJob' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/job/",
        Data.toBS backendEnvironmentName,
        "/",
        Data.toBS jobId
      ]

instance Data.ToQuery UpdateBackendJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBackendJobResponse' smart constructor.
data UpdateBackendJobResponse = UpdateBackendJobResponse'
  { -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The time when the job was created.
    createTime :: Prelude.Maybe Prelude.Text,
    -- | If the request fails, this error is returned.
    error :: Prelude.Maybe Prelude.Text,
    -- | The ID for the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name of the operation.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The current status of the request.
    status :: Prelude.Maybe Prelude.Text,
    -- | The time when the job was last updated.
    updateTime :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'updateBackendJobResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'updateBackendJobResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'createTime', 'updateBackendJobResponse_createTime' - The time when the job was created.
--
-- 'error', 'updateBackendJobResponse_error' - If the request fails, this error is returned.
--
-- 'jobId', 'updateBackendJobResponse_jobId' - The ID for the job.
--
-- 'operation', 'updateBackendJobResponse_operation' - The name of the operation.
--
-- 'status', 'updateBackendJobResponse_status' - The current status of the request.
--
-- 'updateTime', 'updateBackendJobResponse_updateTime' - The time when the job was last updated.
--
-- 'httpStatus', 'updateBackendJobResponse_httpStatus' - The response's http status code.
newUpdateBackendJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBackendJobResponse
newUpdateBackendJobResponse pHttpStatus_ =
  UpdateBackendJobResponse'
    { appId = Prelude.Nothing,
      backendEnvironmentName = Prelude.Nothing,
      createTime = Prelude.Nothing,
      error = Prelude.Nothing,
      jobId = Prelude.Nothing,
      operation = Prelude.Nothing,
      status = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
updateBackendJobResponse_appId :: Lens.Lens' UpdateBackendJobResponse (Prelude.Maybe Prelude.Text)
updateBackendJobResponse_appId = Lens.lens (\UpdateBackendJobResponse' {appId} -> appId) (\s@UpdateBackendJobResponse' {} a -> s {appId = a} :: UpdateBackendJobResponse)

-- | The name of the backend environment.
updateBackendJobResponse_backendEnvironmentName :: Lens.Lens' UpdateBackendJobResponse (Prelude.Maybe Prelude.Text)
updateBackendJobResponse_backendEnvironmentName = Lens.lens (\UpdateBackendJobResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@UpdateBackendJobResponse' {} a -> s {backendEnvironmentName = a} :: UpdateBackendJobResponse)

-- | The time when the job was created.
updateBackendJobResponse_createTime :: Lens.Lens' UpdateBackendJobResponse (Prelude.Maybe Prelude.Text)
updateBackendJobResponse_createTime = Lens.lens (\UpdateBackendJobResponse' {createTime} -> createTime) (\s@UpdateBackendJobResponse' {} a -> s {createTime = a} :: UpdateBackendJobResponse)

-- | If the request fails, this error is returned.
updateBackendJobResponse_error :: Lens.Lens' UpdateBackendJobResponse (Prelude.Maybe Prelude.Text)
updateBackendJobResponse_error = Lens.lens (\UpdateBackendJobResponse' {error} -> error) (\s@UpdateBackendJobResponse' {} a -> s {error = a} :: UpdateBackendJobResponse)

-- | The ID for the job.
updateBackendJobResponse_jobId :: Lens.Lens' UpdateBackendJobResponse (Prelude.Maybe Prelude.Text)
updateBackendJobResponse_jobId = Lens.lens (\UpdateBackendJobResponse' {jobId} -> jobId) (\s@UpdateBackendJobResponse' {} a -> s {jobId = a} :: UpdateBackendJobResponse)

-- | The name of the operation.
updateBackendJobResponse_operation :: Lens.Lens' UpdateBackendJobResponse (Prelude.Maybe Prelude.Text)
updateBackendJobResponse_operation = Lens.lens (\UpdateBackendJobResponse' {operation} -> operation) (\s@UpdateBackendJobResponse' {} a -> s {operation = a} :: UpdateBackendJobResponse)

-- | The current status of the request.
updateBackendJobResponse_status :: Lens.Lens' UpdateBackendJobResponse (Prelude.Maybe Prelude.Text)
updateBackendJobResponse_status = Lens.lens (\UpdateBackendJobResponse' {status} -> status) (\s@UpdateBackendJobResponse' {} a -> s {status = a} :: UpdateBackendJobResponse)

-- | The time when the job was last updated.
updateBackendJobResponse_updateTime :: Lens.Lens' UpdateBackendJobResponse (Prelude.Maybe Prelude.Text)
updateBackendJobResponse_updateTime = Lens.lens (\UpdateBackendJobResponse' {updateTime} -> updateTime) (\s@UpdateBackendJobResponse' {} a -> s {updateTime = a} :: UpdateBackendJobResponse)

-- | The response's http status code.
updateBackendJobResponse_httpStatus :: Lens.Lens' UpdateBackendJobResponse Prelude.Int
updateBackendJobResponse_httpStatus = Lens.lens (\UpdateBackendJobResponse' {httpStatus} -> httpStatus) (\s@UpdateBackendJobResponse' {} a -> s {httpStatus = a} :: UpdateBackendJobResponse)

instance Prelude.NFData UpdateBackendJobResponse where
  rnf UpdateBackendJobResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf httpStatus
