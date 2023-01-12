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
-- Module      : Amazonka.HealthLake.StartFHIRExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins a FHIR export job.
module Amazonka.HealthLake.StartFHIRExportJob
  ( -- * Creating a Request
    StartFHIRExportJob (..),
    newStartFHIRExportJob,

    -- * Request Lenses
    startFHIRExportJob_jobName,
    startFHIRExportJob_outputDataConfig,
    startFHIRExportJob_datastoreId,
    startFHIRExportJob_dataAccessRoleArn,
    startFHIRExportJob_clientToken,

    -- * Destructuring the Response
    StartFHIRExportJobResponse (..),
    newStartFHIRExportJobResponse,

    -- * Response Lenses
    startFHIRExportJobResponse_datastoreId,
    startFHIRExportJobResponse_httpStatus,
    startFHIRExportJobResponse_jobId,
    startFHIRExportJobResponse_jobStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartFHIRExportJob' smart constructor.
data StartFHIRExportJob = StartFHIRExportJob'
  { -- | The user generated name for an export job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The output data configuration that was supplied when the export job was
    -- created.
    outputDataConfig :: OutputDataConfig,
    -- | The AWS generated ID for the Data Store from which files are being
    -- exported for an export job.
    datastoreId :: Prelude.Text,
    -- | The Amazon Resource Name used during the initiation of the job.
    dataAccessRoleArn :: Prelude.Text,
    -- | An optional user provided token used for ensuring idempotency.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFHIRExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'startFHIRExportJob_jobName' - The user generated name for an export job.
--
-- 'outputDataConfig', 'startFHIRExportJob_outputDataConfig' - The output data configuration that was supplied when the export job was
-- created.
--
-- 'datastoreId', 'startFHIRExportJob_datastoreId' - The AWS generated ID for the Data Store from which files are being
-- exported for an export job.
--
-- 'dataAccessRoleArn', 'startFHIRExportJob_dataAccessRoleArn' - The Amazon Resource Name used during the initiation of the job.
--
-- 'clientToken', 'startFHIRExportJob_clientToken' - An optional user provided token used for ensuring idempotency.
newStartFHIRExportJob ::
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'datastoreId'
  Prelude.Text ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  StartFHIRExportJob
newStartFHIRExportJob
  pOutputDataConfig_
  pDatastoreId_
  pDataAccessRoleArn_
  pClientToken_ =
    StartFHIRExportJob'
      { jobName = Prelude.Nothing,
        outputDataConfig = pOutputDataConfig_,
        datastoreId = pDatastoreId_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        clientToken = pClientToken_
      }

-- | The user generated name for an export job.
startFHIRExportJob_jobName :: Lens.Lens' StartFHIRExportJob (Prelude.Maybe Prelude.Text)
startFHIRExportJob_jobName = Lens.lens (\StartFHIRExportJob' {jobName} -> jobName) (\s@StartFHIRExportJob' {} a -> s {jobName = a} :: StartFHIRExportJob)

-- | The output data configuration that was supplied when the export job was
-- created.
startFHIRExportJob_outputDataConfig :: Lens.Lens' StartFHIRExportJob OutputDataConfig
startFHIRExportJob_outputDataConfig = Lens.lens (\StartFHIRExportJob' {outputDataConfig} -> outputDataConfig) (\s@StartFHIRExportJob' {} a -> s {outputDataConfig = a} :: StartFHIRExportJob)

-- | The AWS generated ID for the Data Store from which files are being
-- exported for an export job.
startFHIRExportJob_datastoreId :: Lens.Lens' StartFHIRExportJob Prelude.Text
startFHIRExportJob_datastoreId = Lens.lens (\StartFHIRExportJob' {datastoreId} -> datastoreId) (\s@StartFHIRExportJob' {} a -> s {datastoreId = a} :: StartFHIRExportJob)

-- | The Amazon Resource Name used during the initiation of the job.
startFHIRExportJob_dataAccessRoleArn :: Lens.Lens' StartFHIRExportJob Prelude.Text
startFHIRExportJob_dataAccessRoleArn = Lens.lens (\StartFHIRExportJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartFHIRExportJob' {} a -> s {dataAccessRoleArn = a} :: StartFHIRExportJob)

-- | An optional user provided token used for ensuring idempotency.
startFHIRExportJob_clientToken :: Lens.Lens' StartFHIRExportJob Prelude.Text
startFHIRExportJob_clientToken = Lens.lens (\StartFHIRExportJob' {clientToken} -> clientToken) (\s@StartFHIRExportJob' {} a -> s {clientToken = a} :: StartFHIRExportJob)

instance Core.AWSRequest StartFHIRExportJob where
  type
    AWSResponse StartFHIRExportJob =
      StartFHIRExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFHIRExportJobResponse'
            Prelude.<$> (x Data..?> "DatastoreId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "JobId")
            Prelude.<*> (x Data..:> "JobStatus")
      )

instance Prelude.Hashable StartFHIRExportJob where
  hashWithSalt _salt StartFHIRExportJob' {..} =
    _salt `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` datastoreId
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData StartFHIRExportJob where
  rnf StartFHIRExportJob' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf datastoreId
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders StartFHIRExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "HealthLake.StartFHIRExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartFHIRExportJob where
  toJSON StartFHIRExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobName" Data..=) Prelude.<$> jobName,
            Prelude.Just
              ("OutputDataConfig" Data..= outputDataConfig),
            Prelude.Just ("DatastoreId" Data..= datastoreId),
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath StartFHIRExportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartFHIRExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFHIRExportJobResponse' smart constructor.
data StartFHIRExportJobResponse = StartFHIRExportJobResponse'
  { -- | The AWS generated ID for the Data Store from which files are being
    -- exported for an export job.
    datastoreId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The AWS generated ID for an export job.
    jobId :: Prelude.Text,
    -- | The status of a FHIR export job. Possible statuses are SUBMITTED,
    -- IN_PROGRESS, COMPLETED, or FAILED.
    jobStatus :: JobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFHIRExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datastoreId', 'startFHIRExportJobResponse_datastoreId' - The AWS generated ID for the Data Store from which files are being
-- exported for an export job.
--
-- 'httpStatus', 'startFHIRExportJobResponse_httpStatus' - The response's http status code.
--
-- 'jobId', 'startFHIRExportJobResponse_jobId' - The AWS generated ID for an export job.
--
-- 'jobStatus', 'startFHIRExportJobResponse_jobStatus' - The status of a FHIR export job. Possible statuses are SUBMITTED,
-- IN_PROGRESS, COMPLETED, or FAILED.
newStartFHIRExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobId'
  Prelude.Text ->
  -- | 'jobStatus'
  JobStatus ->
  StartFHIRExportJobResponse
newStartFHIRExportJobResponse
  pHttpStatus_
  pJobId_
  pJobStatus_ =
    StartFHIRExportJobResponse'
      { datastoreId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        jobId = pJobId_,
        jobStatus = pJobStatus_
      }

-- | The AWS generated ID for the Data Store from which files are being
-- exported for an export job.
startFHIRExportJobResponse_datastoreId :: Lens.Lens' StartFHIRExportJobResponse (Prelude.Maybe Prelude.Text)
startFHIRExportJobResponse_datastoreId = Lens.lens (\StartFHIRExportJobResponse' {datastoreId} -> datastoreId) (\s@StartFHIRExportJobResponse' {} a -> s {datastoreId = a} :: StartFHIRExportJobResponse)

-- | The response's http status code.
startFHIRExportJobResponse_httpStatus :: Lens.Lens' StartFHIRExportJobResponse Prelude.Int
startFHIRExportJobResponse_httpStatus = Lens.lens (\StartFHIRExportJobResponse' {httpStatus} -> httpStatus) (\s@StartFHIRExportJobResponse' {} a -> s {httpStatus = a} :: StartFHIRExportJobResponse)

-- | The AWS generated ID for an export job.
startFHIRExportJobResponse_jobId :: Lens.Lens' StartFHIRExportJobResponse Prelude.Text
startFHIRExportJobResponse_jobId = Lens.lens (\StartFHIRExportJobResponse' {jobId} -> jobId) (\s@StartFHIRExportJobResponse' {} a -> s {jobId = a} :: StartFHIRExportJobResponse)

-- | The status of a FHIR export job. Possible statuses are SUBMITTED,
-- IN_PROGRESS, COMPLETED, or FAILED.
startFHIRExportJobResponse_jobStatus :: Lens.Lens' StartFHIRExportJobResponse JobStatus
startFHIRExportJobResponse_jobStatus = Lens.lens (\StartFHIRExportJobResponse' {jobStatus} -> jobStatus) (\s@StartFHIRExportJobResponse' {} a -> s {jobStatus = a} :: StartFHIRExportJobResponse)

instance Prelude.NFData StartFHIRExportJobResponse where
  rnf StartFHIRExportJobResponse' {..} =
    Prelude.rnf datastoreId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobStatus
