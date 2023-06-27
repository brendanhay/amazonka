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
-- Module      : Amazonka.SageMakerGeoSpatial.GetEarthObservationJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the details for a previously initiated Earth Observation job.
module Amazonka.SageMakerGeoSpatial.GetEarthObservationJob
  ( -- * Creating a Request
    GetEarthObservationJob (..),
    newGetEarthObservationJob,

    -- * Request Lenses
    getEarthObservationJob_arn,

    -- * Destructuring the Response
    GetEarthObservationJobResponse (..),
    newGetEarthObservationJobResponse,

    -- * Response Lenses
    getEarthObservationJobResponse_errorDetails,
    getEarthObservationJobResponse_executionRoleArn,
    getEarthObservationJobResponse_exportErrorDetails,
    getEarthObservationJobResponse_exportStatus,
    getEarthObservationJobResponse_kmsKeyId,
    getEarthObservationJobResponse_outputBands,
    getEarthObservationJobResponse_tags,
    getEarthObservationJobResponse_httpStatus,
    getEarthObservationJobResponse_arn,
    getEarthObservationJobResponse_creationTime,
    getEarthObservationJobResponse_durationInSeconds,
    getEarthObservationJobResponse_inputConfig,
    getEarthObservationJobResponse_jobConfig,
    getEarthObservationJobResponse_name,
    getEarthObservationJobResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newGetEarthObservationJob' smart constructor.
data GetEarthObservationJob = GetEarthObservationJob'
  { -- | The Amazon Resource Name (ARN) of the Earth Observation job.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEarthObservationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getEarthObservationJob_arn' - The Amazon Resource Name (ARN) of the Earth Observation job.
newGetEarthObservationJob ::
  -- | 'arn'
  Prelude.Text ->
  GetEarthObservationJob
newGetEarthObservationJob pArn_ =
  GetEarthObservationJob' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the Earth Observation job.
getEarthObservationJob_arn :: Lens.Lens' GetEarthObservationJob Prelude.Text
getEarthObservationJob_arn = Lens.lens (\GetEarthObservationJob' {arn} -> arn) (\s@GetEarthObservationJob' {} a -> s {arn = a} :: GetEarthObservationJob)

instance Core.AWSRequest GetEarthObservationJob where
  type
    AWSResponse GetEarthObservationJob =
      GetEarthObservationJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEarthObservationJobResponse'
            Prelude.<$> (x Data..?> "ErrorDetails")
            Prelude.<*> (x Data..?> "ExecutionRoleArn")
            Prelude.<*> (x Data..?> "ExportErrorDetails")
            Prelude.<*> (x Data..?> "ExportStatus")
            Prelude.<*> (x Data..?> "KmsKeyId")
            Prelude.<*> (x Data..?> "OutputBands" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "DurationInSeconds")
            Prelude.<*> (x Data..:> "InputConfig")
            Prelude.<*> (x Data..:> "JobConfig")
            Prelude.<*> (x Data..:> "Name")
            Prelude.<*> (x Data..:> "Status")
      )

instance Prelude.Hashable GetEarthObservationJob where
  hashWithSalt _salt GetEarthObservationJob' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetEarthObservationJob where
  rnf GetEarthObservationJob' {..} = Prelude.rnf arn

instance Data.ToHeaders GetEarthObservationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEarthObservationJob where
  toPath GetEarthObservationJob' {..} =
    Prelude.mconcat
      ["/earth-observation-jobs/", Data.toBS arn]

instance Data.ToQuery GetEarthObservationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEarthObservationJobResponse' smart constructor.
data GetEarthObservationJobResponse = GetEarthObservationJobResponse'
  { -- | Details about the errors generated during the Earth Observation job.
    errorDetails :: Prelude.Maybe EarthObservationJobErrorDetails,
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for
    -- the job.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Details about the errors generated during ExportEarthObservationJob.
    exportErrorDetails :: Prelude.Maybe ExportErrorDetails,
    -- | The status of the Earth Observation job.
    exportStatus :: Prelude.Maybe EarthObservationJobExportStatus,
    -- | The Key Management Service key ID for server-side encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Bands available in the output of an operation.
    outputBands :: Prelude.Maybe [OutputBand],
    -- | Each tag consists of a key and a value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Earth Observation job.
    arn :: Prelude.Text,
    -- | The creation time of the initiated Earth Observation job.
    creationTime :: Data.ISO8601,
    -- | The duration of Earth Observation job, in seconds.
    durationInSeconds :: Prelude.Int,
    -- | Input data for the Earth Observation job.
    inputConfig :: InputConfigOutput,
    -- | An object containing information about the job configuration.
    jobConfig :: JobConfigInput,
    -- | The name of the Earth Observation job.
    name :: Prelude.Text,
    -- | The status of a previously initiated Earth Observation job.
    status :: EarthObservationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEarthObservationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorDetails', 'getEarthObservationJobResponse_errorDetails' - Details about the errors generated during the Earth Observation job.
--
-- 'executionRoleArn', 'getEarthObservationJobResponse_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
--
-- 'exportErrorDetails', 'getEarthObservationJobResponse_exportErrorDetails' - Details about the errors generated during ExportEarthObservationJob.
--
-- 'exportStatus', 'getEarthObservationJobResponse_exportStatus' - The status of the Earth Observation job.
--
-- 'kmsKeyId', 'getEarthObservationJobResponse_kmsKeyId' - The Key Management Service key ID for server-side encryption.
--
-- 'outputBands', 'getEarthObservationJobResponse_outputBands' - Bands available in the output of an operation.
--
-- 'tags', 'getEarthObservationJobResponse_tags' - Each tag consists of a key and a value.
--
-- 'httpStatus', 'getEarthObservationJobResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getEarthObservationJobResponse_arn' - The Amazon Resource Name (ARN) of the Earth Observation job.
--
-- 'creationTime', 'getEarthObservationJobResponse_creationTime' - The creation time of the initiated Earth Observation job.
--
-- 'durationInSeconds', 'getEarthObservationJobResponse_durationInSeconds' - The duration of Earth Observation job, in seconds.
--
-- 'inputConfig', 'getEarthObservationJobResponse_inputConfig' - Input data for the Earth Observation job.
--
-- 'jobConfig', 'getEarthObservationJobResponse_jobConfig' - An object containing information about the job configuration.
--
-- 'name', 'getEarthObservationJobResponse_name' - The name of the Earth Observation job.
--
-- 'status', 'getEarthObservationJobResponse_status' - The status of a previously initiated Earth Observation job.
newGetEarthObservationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'durationInSeconds'
  Prelude.Int ->
  -- | 'inputConfig'
  InputConfigOutput ->
  -- | 'jobConfig'
  JobConfigInput ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  EarthObservationJobStatus ->
  GetEarthObservationJobResponse
newGetEarthObservationJobResponse
  pHttpStatus_
  pArn_
  pCreationTime_
  pDurationInSeconds_
  pInputConfig_
  pJobConfig_
  pName_
  pStatus_ =
    GetEarthObservationJobResponse'
      { errorDetails =
          Prelude.Nothing,
        executionRoleArn = Prelude.Nothing,
        exportErrorDetails = Prelude.Nothing,
        exportStatus = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        outputBands = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        durationInSeconds = pDurationInSeconds_,
        inputConfig = pInputConfig_,
        jobConfig = pJobConfig_,
        name = pName_,
        status = pStatus_
      }

-- | Details about the errors generated during the Earth Observation job.
getEarthObservationJobResponse_errorDetails :: Lens.Lens' GetEarthObservationJobResponse (Prelude.Maybe EarthObservationJobErrorDetails)
getEarthObservationJobResponse_errorDetails = Lens.lens (\GetEarthObservationJobResponse' {errorDetails} -> errorDetails) (\s@GetEarthObservationJobResponse' {} a -> s {errorDetails = a} :: GetEarthObservationJobResponse)

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
getEarthObservationJobResponse_executionRoleArn :: Lens.Lens' GetEarthObservationJobResponse (Prelude.Maybe Prelude.Text)
getEarthObservationJobResponse_executionRoleArn = Lens.lens (\GetEarthObservationJobResponse' {executionRoleArn} -> executionRoleArn) (\s@GetEarthObservationJobResponse' {} a -> s {executionRoleArn = a} :: GetEarthObservationJobResponse)

-- | Details about the errors generated during ExportEarthObservationJob.
getEarthObservationJobResponse_exportErrorDetails :: Lens.Lens' GetEarthObservationJobResponse (Prelude.Maybe ExportErrorDetails)
getEarthObservationJobResponse_exportErrorDetails = Lens.lens (\GetEarthObservationJobResponse' {exportErrorDetails} -> exportErrorDetails) (\s@GetEarthObservationJobResponse' {} a -> s {exportErrorDetails = a} :: GetEarthObservationJobResponse)

-- | The status of the Earth Observation job.
getEarthObservationJobResponse_exportStatus :: Lens.Lens' GetEarthObservationJobResponse (Prelude.Maybe EarthObservationJobExportStatus)
getEarthObservationJobResponse_exportStatus = Lens.lens (\GetEarthObservationJobResponse' {exportStatus} -> exportStatus) (\s@GetEarthObservationJobResponse' {} a -> s {exportStatus = a} :: GetEarthObservationJobResponse)

-- | The Key Management Service key ID for server-side encryption.
getEarthObservationJobResponse_kmsKeyId :: Lens.Lens' GetEarthObservationJobResponse (Prelude.Maybe Prelude.Text)
getEarthObservationJobResponse_kmsKeyId = Lens.lens (\GetEarthObservationJobResponse' {kmsKeyId} -> kmsKeyId) (\s@GetEarthObservationJobResponse' {} a -> s {kmsKeyId = a} :: GetEarthObservationJobResponse)

-- | Bands available in the output of an operation.
getEarthObservationJobResponse_outputBands :: Lens.Lens' GetEarthObservationJobResponse (Prelude.Maybe [OutputBand])
getEarthObservationJobResponse_outputBands = Lens.lens (\GetEarthObservationJobResponse' {outputBands} -> outputBands) (\s@GetEarthObservationJobResponse' {} a -> s {outputBands = a} :: GetEarthObservationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Each tag consists of a key and a value.
getEarthObservationJobResponse_tags :: Lens.Lens' GetEarthObservationJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getEarthObservationJobResponse_tags = Lens.lens (\GetEarthObservationJobResponse' {tags} -> tags) (\s@GetEarthObservationJobResponse' {} a -> s {tags = a} :: GetEarthObservationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEarthObservationJobResponse_httpStatus :: Lens.Lens' GetEarthObservationJobResponse Prelude.Int
getEarthObservationJobResponse_httpStatus = Lens.lens (\GetEarthObservationJobResponse' {httpStatus} -> httpStatus) (\s@GetEarthObservationJobResponse' {} a -> s {httpStatus = a} :: GetEarthObservationJobResponse)

-- | The Amazon Resource Name (ARN) of the Earth Observation job.
getEarthObservationJobResponse_arn :: Lens.Lens' GetEarthObservationJobResponse Prelude.Text
getEarthObservationJobResponse_arn = Lens.lens (\GetEarthObservationJobResponse' {arn} -> arn) (\s@GetEarthObservationJobResponse' {} a -> s {arn = a} :: GetEarthObservationJobResponse)

-- | The creation time of the initiated Earth Observation job.
getEarthObservationJobResponse_creationTime :: Lens.Lens' GetEarthObservationJobResponse Prelude.UTCTime
getEarthObservationJobResponse_creationTime = Lens.lens (\GetEarthObservationJobResponse' {creationTime} -> creationTime) (\s@GetEarthObservationJobResponse' {} a -> s {creationTime = a} :: GetEarthObservationJobResponse) Prelude.. Data._Time

-- | The duration of Earth Observation job, in seconds.
getEarthObservationJobResponse_durationInSeconds :: Lens.Lens' GetEarthObservationJobResponse Prelude.Int
getEarthObservationJobResponse_durationInSeconds = Lens.lens (\GetEarthObservationJobResponse' {durationInSeconds} -> durationInSeconds) (\s@GetEarthObservationJobResponse' {} a -> s {durationInSeconds = a} :: GetEarthObservationJobResponse)

-- | Input data for the Earth Observation job.
getEarthObservationJobResponse_inputConfig :: Lens.Lens' GetEarthObservationJobResponse InputConfigOutput
getEarthObservationJobResponse_inputConfig = Lens.lens (\GetEarthObservationJobResponse' {inputConfig} -> inputConfig) (\s@GetEarthObservationJobResponse' {} a -> s {inputConfig = a} :: GetEarthObservationJobResponse)

-- | An object containing information about the job configuration.
getEarthObservationJobResponse_jobConfig :: Lens.Lens' GetEarthObservationJobResponse JobConfigInput
getEarthObservationJobResponse_jobConfig = Lens.lens (\GetEarthObservationJobResponse' {jobConfig} -> jobConfig) (\s@GetEarthObservationJobResponse' {} a -> s {jobConfig = a} :: GetEarthObservationJobResponse)

-- | The name of the Earth Observation job.
getEarthObservationJobResponse_name :: Lens.Lens' GetEarthObservationJobResponse Prelude.Text
getEarthObservationJobResponse_name = Lens.lens (\GetEarthObservationJobResponse' {name} -> name) (\s@GetEarthObservationJobResponse' {} a -> s {name = a} :: GetEarthObservationJobResponse)

-- | The status of a previously initiated Earth Observation job.
getEarthObservationJobResponse_status :: Lens.Lens' GetEarthObservationJobResponse EarthObservationJobStatus
getEarthObservationJobResponse_status = Lens.lens (\GetEarthObservationJobResponse' {status} -> status) (\s@GetEarthObservationJobResponse' {} a -> s {status = a} :: GetEarthObservationJobResponse)

instance
  Prelude.NFData
    GetEarthObservationJobResponse
  where
  rnf GetEarthObservationJobResponse' {..} =
    Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf exportErrorDetails
      `Prelude.seq` Prelude.rnf exportStatus
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf outputBands
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf durationInSeconds
      `Prelude.seq` Prelude.rnf inputConfig
      `Prelude.seq` Prelude.rnf jobConfig
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
