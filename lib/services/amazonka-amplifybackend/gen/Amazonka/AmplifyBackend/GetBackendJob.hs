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
-- Module      : Amazonka.AmplifyBackend.GetBackendJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific job.
module Amazonka.AmplifyBackend.GetBackendJob
  ( -- * Creating a Request
    GetBackendJob (..),
    newGetBackendJob,

    -- * Request Lenses
    getBackendJob_appId,
    getBackendJob_backendEnvironmentName,
    getBackendJob_jobId,

    -- * Destructuring the Response
    GetBackendJobResponse (..),
    newGetBackendJobResponse,

    -- * Response Lenses
    getBackendJobResponse_appId,
    getBackendJobResponse_backendEnvironmentName,
    getBackendJobResponse_createTime,
    getBackendJobResponse_error,
    getBackendJobResponse_jobId,
    getBackendJobResponse_operation,
    getBackendJobResponse_status,
    getBackendJobResponse_updateTime,
    getBackendJobResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBackendJob' smart constructor.
data GetBackendJob = GetBackendJob'
  { -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text,
    -- | The ID for the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackendJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getBackendJob_appId' - The app ID.
--
-- 'backendEnvironmentName', 'getBackendJob_backendEnvironmentName' - The name of the backend environment.
--
-- 'jobId', 'getBackendJob_jobId' - The ID for the job.
newGetBackendJob ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  GetBackendJob
newGetBackendJob
  pAppId_
  pBackendEnvironmentName_
  pJobId_ =
    GetBackendJob'
      { appId = pAppId_,
        backendEnvironmentName = pBackendEnvironmentName_,
        jobId = pJobId_
      }

-- | The app ID.
getBackendJob_appId :: Lens.Lens' GetBackendJob Prelude.Text
getBackendJob_appId = Lens.lens (\GetBackendJob' {appId} -> appId) (\s@GetBackendJob' {} a -> s {appId = a} :: GetBackendJob)

-- | The name of the backend environment.
getBackendJob_backendEnvironmentName :: Lens.Lens' GetBackendJob Prelude.Text
getBackendJob_backendEnvironmentName = Lens.lens (\GetBackendJob' {backendEnvironmentName} -> backendEnvironmentName) (\s@GetBackendJob' {} a -> s {backendEnvironmentName = a} :: GetBackendJob)

-- | The ID for the job.
getBackendJob_jobId :: Lens.Lens' GetBackendJob Prelude.Text
getBackendJob_jobId = Lens.lens (\GetBackendJob' {jobId} -> jobId) (\s@GetBackendJob' {} a -> s {jobId = a} :: GetBackendJob)

instance Core.AWSRequest GetBackendJob where
  type
    AWSResponse GetBackendJob =
      GetBackendJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackendJobResponse'
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

instance Prelude.Hashable GetBackendJob where
  hashWithSalt _salt GetBackendJob' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetBackendJob where
  rnf GetBackendJob' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders GetBackendJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBackendJob where
  toPath GetBackendJob' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/job/",
        Data.toBS backendEnvironmentName,
        "/",
        Data.toBS jobId
      ]

instance Data.ToQuery GetBackendJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBackendJobResponse' smart constructor.
data GetBackendJobResponse = GetBackendJobResponse'
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
-- Create a value of 'GetBackendJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getBackendJobResponse_appId' - The app ID.
--
-- 'backendEnvironmentName', 'getBackendJobResponse_backendEnvironmentName' - The name of the backend environment.
--
-- 'createTime', 'getBackendJobResponse_createTime' - The time when the job was created.
--
-- 'error', 'getBackendJobResponse_error' - If the request fails, this error is returned.
--
-- 'jobId', 'getBackendJobResponse_jobId' - The ID for the job.
--
-- 'operation', 'getBackendJobResponse_operation' - The name of the operation.
--
-- 'status', 'getBackendJobResponse_status' - The current status of the request.
--
-- 'updateTime', 'getBackendJobResponse_updateTime' - The time when the job was last updated.
--
-- 'httpStatus', 'getBackendJobResponse_httpStatus' - The response's http status code.
newGetBackendJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBackendJobResponse
newGetBackendJobResponse pHttpStatus_ =
  GetBackendJobResponse'
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
getBackendJobResponse_appId :: Lens.Lens' GetBackendJobResponse (Prelude.Maybe Prelude.Text)
getBackendJobResponse_appId = Lens.lens (\GetBackendJobResponse' {appId} -> appId) (\s@GetBackendJobResponse' {} a -> s {appId = a} :: GetBackendJobResponse)

-- | The name of the backend environment.
getBackendJobResponse_backendEnvironmentName :: Lens.Lens' GetBackendJobResponse (Prelude.Maybe Prelude.Text)
getBackendJobResponse_backendEnvironmentName = Lens.lens (\GetBackendJobResponse' {backendEnvironmentName} -> backendEnvironmentName) (\s@GetBackendJobResponse' {} a -> s {backendEnvironmentName = a} :: GetBackendJobResponse)

-- | The time when the job was created.
getBackendJobResponse_createTime :: Lens.Lens' GetBackendJobResponse (Prelude.Maybe Prelude.Text)
getBackendJobResponse_createTime = Lens.lens (\GetBackendJobResponse' {createTime} -> createTime) (\s@GetBackendJobResponse' {} a -> s {createTime = a} :: GetBackendJobResponse)

-- | If the request fails, this error is returned.
getBackendJobResponse_error :: Lens.Lens' GetBackendJobResponse (Prelude.Maybe Prelude.Text)
getBackendJobResponse_error = Lens.lens (\GetBackendJobResponse' {error} -> error) (\s@GetBackendJobResponse' {} a -> s {error = a} :: GetBackendJobResponse)

-- | The ID for the job.
getBackendJobResponse_jobId :: Lens.Lens' GetBackendJobResponse (Prelude.Maybe Prelude.Text)
getBackendJobResponse_jobId = Lens.lens (\GetBackendJobResponse' {jobId} -> jobId) (\s@GetBackendJobResponse' {} a -> s {jobId = a} :: GetBackendJobResponse)

-- | The name of the operation.
getBackendJobResponse_operation :: Lens.Lens' GetBackendJobResponse (Prelude.Maybe Prelude.Text)
getBackendJobResponse_operation = Lens.lens (\GetBackendJobResponse' {operation} -> operation) (\s@GetBackendJobResponse' {} a -> s {operation = a} :: GetBackendJobResponse)

-- | The current status of the request.
getBackendJobResponse_status :: Lens.Lens' GetBackendJobResponse (Prelude.Maybe Prelude.Text)
getBackendJobResponse_status = Lens.lens (\GetBackendJobResponse' {status} -> status) (\s@GetBackendJobResponse' {} a -> s {status = a} :: GetBackendJobResponse)

-- | The time when the job was last updated.
getBackendJobResponse_updateTime :: Lens.Lens' GetBackendJobResponse (Prelude.Maybe Prelude.Text)
getBackendJobResponse_updateTime = Lens.lens (\GetBackendJobResponse' {updateTime} -> updateTime) (\s@GetBackendJobResponse' {} a -> s {updateTime = a} :: GetBackendJobResponse)

-- | The response's http status code.
getBackendJobResponse_httpStatus :: Lens.Lens' GetBackendJobResponse Prelude.Int
getBackendJobResponse_httpStatus = Lens.lens (\GetBackendJobResponse' {httpStatus} -> httpStatus) (\s@GetBackendJobResponse' {} a -> s {httpStatus = a} :: GetBackendJobResponse)

instance Prelude.NFData GetBackendJobResponse where
  rnf GetBackendJobResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf httpStatus
