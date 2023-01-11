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
-- Module      : Amazonka.HealthLake.StartFHIRImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins a FHIR Import job.
module Amazonka.HealthLake.StartFHIRImportJob
  ( -- * Creating a Request
    StartFHIRImportJob (..),
    newStartFHIRImportJob,

    -- * Request Lenses
    startFHIRImportJob_jobName,
    startFHIRImportJob_inputDataConfig,
    startFHIRImportJob_jobOutputDataConfig,
    startFHIRImportJob_datastoreId,
    startFHIRImportJob_dataAccessRoleArn,
    startFHIRImportJob_clientToken,

    -- * Destructuring the Response
    StartFHIRImportJobResponse (..),
    newStartFHIRImportJobResponse,

    -- * Response Lenses
    startFHIRImportJobResponse_datastoreId,
    startFHIRImportJobResponse_httpStatus,
    startFHIRImportJobResponse_jobId,
    startFHIRImportJobResponse_jobStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartFHIRImportJob' smart constructor.
data StartFHIRImportJob = StartFHIRImportJob'
  { -- | The name of the FHIR Import job in the StartFHIRImport job request.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The input properties of the FHIR Import job in the StartFHIRImport job
    -- request.
    inputDataConfig :: InputDataConfig,
    jobOutputDataConfig :: OutputDataConfig,
    -- | The AWS-generated Data Store ID.
    datastoreId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon HealthLake access
    -- permission.
    dataAccessRoleArn :: Prelude.Text,
    -- | Optional user provided token used for ensuring idempotency.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFHIRImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'startFHIRImportJob_jobName' - The name of the FHIR Import job in the StartFHIRImport job request.
--
-- 'inputDataConfig', 'startFHIRImportJob_inputDataConfig' - The input properties of the FHIR Import job in the StartFHIRImport job
-- request.
--
-- 'jobOutputDataConfig', 'startFHIRImportJob_jobOutputDataConfig' - Undocumented member.
--
-- 'datastoreId', 'startFHIRImportJob_datastoreId' - The AWS-generated Data Store ID.
--
-- 'dataAccessRoleArn', 'startFHIRImportJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon HealthLake access
-- permission.
--
-- 'clientToken', 'startFHIRImportJob_clientToken' - Optional user provided token used for ensuring idempotency.
newStartFHIRImportJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'jobOutputDataConfig'
  OutputDataConfig ->
  -- | 'datastoreId'
  Prelude.Text ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  StartFHIRImportJob
newStartFHIRImportJob
  pInputDataConfig_
  pJobOutputDataConfig_
  pDatastoreId_
  pDataAccessRoleArn_
  pClientToken_ =
    StartFHIRImportJob'
      { jobName = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        jobOutputDataConfig = pJobOutputDataConfig_,
        datastoreId = pDatastoreId_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        clientToken = pClientToken_
      }

-- | The name of the FHIR Import job in the StartFHIRImport job request.
startFHIRImportJob_jobName :: Lens.Lens' StartFHIRImportJob (Prelude.Maybe Prelude.Text)
startFHIRImportJob_jobName = Lens.lens (\StartFHIRImportJob' {jobName} -> jobName) (\s@StartFHIRImportJob' {} a -> s {jobName = a} :: StartFHIRImportJob)

-- | The input properties of the FHIR Import job in the StartFHIRImport job
-- request.
startFHIRImportJob_inputDataConfig :: Lens.Lens' StartFHIRImportJob InputDataConfig
startFHIRImportJob_inputDataConfig = Lens.lens (\StartFHIRImportJob' {inputDataConfig} -> inputDataConfig) (\s@StartFHIRImportJob' {} a -> s {inputDataConfig = a} :: StartFHIRImportJob)

-- | Undocumented member.
startFHIRImportJob_jobOutputDataConfig :: Lens.Lens' StartFHIRImportJob OutputDataConfig
startFHIRImportJob_jobOutputDataConfig = Lens.lens (\StartFHIRImportJob' {jobOutputDataConfig} -> jobOutputDataConfig) (\s@StartFHIRImportJob' {} a -> s {jobOutputDataConfig = a} :: StartFHIRImportJob)

-- | The AWS-generated Data Store ID.
startFHIRImportJob_datastoreId :: Lens.Lens' StartFHIRImportJob Prelude.Text
startFHIRImportJob_datastoreId = Lens.lens (\StartFHIRImportJob' {datastoreId} -> datastoreId) (\s@StartFHIRImportJob' {} a -> s {datastoreId = a} :: StartFHIRImportJob)

-- | The Amazon Resource Name (ARN) that gives Amazon HealthLake access
-- permission.
startFHIRImportJob_dataAccessRoleArn :: Lens.Lens' StartFHIRImportJob Prelude.Text
startFHIRImportJob_dataAccessRoleArn = Lens.lens (\StartFHIRImportJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartFHIRImportJob' {} a -> s {dataAccessRoleArn = a} :: StartFHIRImportJob)

-- | Optional user provided token used for ensuring idempotency.
startFHIRImportJob_clientToken :: Lens.Lens' StartFHIRImportJob Prelude.Text
startFHIRImportJob_clientToken = Lens.lens (\StartFHIRImportJob' {clientToken} -> clientToken) (\s@StartFHIRImportJob' {} a -> s {clientToken = a} :: StartFHIRImportJob)

instance Core.AWSRequest StartFHIRImportJob where
  type
    AWSResponse StartFHIRImportJob =
      StartFHIRImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFHIRImportJobResponse'
            Prelude.<$> (x Data..?> "DatastoreId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "JobId")
            Prelude.<*> (x Data..:> "JobStatus")
      )

instance Prelude.Hashable StartFHIRImportJob where
  hashWithSalt _salt StartFHIRImportJob' {..} =
    _salt `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` jobOutputDataConfig
      `Prelude.hashWithSalt` datastoreId
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData StartFHIRImportJob where
  rnf StartFHIRImportJob' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf jobOutputDataConfig
      `Prelude.seq` Prelude.rnf datastoreId
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders StartFHIRImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "HealthLake.StartFHIRImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartFHIRImportJob where
  toJSON StartFHIRImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobName" Data..=) Prelude.<$> jobName,
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig),
            Prelude.Just
              ("JobOutputDataConfig" Data..= jobOutputDataConfig),
            Prelude.Just ("DatastoreId" Data..= datastoreId),
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath StartFHIRImportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartFHIRImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFHIRImportJobResponse' smart constructor.
data StartFHIRImportJobResponse = StartFHIRImportJobResponse'
  { -- | The AWS-generated Data Store ID.
    datastoreId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The AWS-generated job ID.
    jobId :: Prelude.Text,
    -- | The status of an import job.
    jobStatus :: JobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFHIRImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datastoreId', 'startFHIRImportJobResponse_datastoreId' - The AWS-generated Data Store ID.
--
-- 'httpStatus', 'startFHIRImportJobResponse_httpStatus' - The response's http status code.
--
-- 'jobId', 'startFHIRImportJobResponse_jobId' - The AWS-generated job ID.
--
-- 'jobStatus', 'startFHIRImportJobResponse_jobStatus' - The status of an import job.
newStartFHIRImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobId'
  Prelude.Text ->
  -- | 'jobStatus'
  JobStatus ->
  StartFHIRImportJobResponse
newStartFHIRImportJobResponse
  pHttpStatus_
  pJobId_
  pJobStatus_ =
    StartFHIRImportJobResponse'
      { datastoreId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        jobId = pJobId_,
        jobStatus = pJobStatus_
      }

-- | The AWS-generated Data Store ID.
startFHIRImportJobResponse_datastoreId :: Lens.Lens' StartFHIRImportJobResponse (Prelude.Maybe Prelude.Text)
startFHIRImportJobResponse_datastoreId = Lens.lens (\StartFHIRImportJobResponse' {datastoreId} -> datastoreId) (\s@StartFHIRImportJobResponse' {} a -> s {datastoreId = a} :: StartFHIRImportJobResponse)

-- | The response's http status code.
startFHIRImportJobResponse_httpStatus :: Lens.Lens' StartFHIRImportJobResponse Prelude.Int
startFHIRImportJobResponse_httpStatus = Lens.lens (\StartFHIRImportJobResponse' {httpStatus} -> httpStatus) (\s@StartFHIRImportJobResponse' {} a -> s {httpStatus = a} :: StartFHIRImportJobResponse)

-- | The AWS-generated job ID.
startFHIRImportJobResponse_jobId :: Lens.Lens' StartFHIRImportJobResponse Prelude.Text
startFHIRImportJobResponse_jobId = Lens.lens (\StartFHIRImportJobResponse' {jobId} -> jobId) (\s@StartFHIRImportJobResponse' {} a -> s {jobId = a} :: StartFHIRImportJobResponse)

-- | The status of an import job.
startFHIRImportJobResponse_jobStatus :: Lens.Lens' StartFHIRImportJobResponse JobStatus
startFHIRImportJobResponse_jobStatus = Lens.lens (\StartFHIRImportJobResponse' {jobStatus} -> jobStatus) (\s@StartFHIRImportJobResponse' {} a -> s {jobStatus = a} :: StartFHIRImportJobResponse)

instance Prelude.NFData StartFHIRImportJobResponse where
  rnf StartFHIRImportJobResponse' {..} =
    Prelude.rnf datastoreId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobStatus
