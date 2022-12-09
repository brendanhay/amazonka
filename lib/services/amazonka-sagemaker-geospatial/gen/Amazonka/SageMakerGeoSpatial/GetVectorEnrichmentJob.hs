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
-- Module      : Amazonka.SageMakerGeoSpatial.GetVectorEnrichmentJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of a Vector Enrichment Job for a given job Amazon
-- Resource Name (ARN).
module Amazonka.SageMakerGeoSpatial.GetVectorEnrichmentJob
  ( -- * Creating a Request
    GetVectorEnrichmentJob (..),
    newGetVectorEnrichmentJob,

    -- * Request Lenses
    getVectorEnrichmentJob_arn,

    -- * Destructuring the Response
    GetVectorEnrichmentJobResponse (..),
    newGetVectorEnrichmentJobResponse,

    -- * Response Lenses
    getVectorEnrichmentJobResponse_errorDetails,
    getVectorEnrichmentJobResponse_exportErrorDetails,
    getVectorEnrichmentJobResponse_exportStatus,
    getVectorEnrichmentJobResponse_kmsKeyId,
    getVectorEnrichmentJobResponse_tags,
    getVectorEnrichmentJobResponse_httpStatus,
    getVectorEnrichmentJobResponse_arn,
    getVectorEnrichmentJobResponse_creationTime,
    getVectorEnrichmentJobResponse_durationInSeconds,
    getVectorEnrichmentJobResponse_executionRoleArn,
    getVectorEnrichmentJobResponse_inputConfig,
    getVectorEnrichmentJobResponse_jobConfig,
    getVectorEnrichmentJobResponse_name,
    getVectorEnrichmentJobResponse_status,
    getVectorEnrichmentJobResponse_type,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newGetVectorEnrichmentJob' smart constructor.
data GetVectorEnrichmentJob = GetVectorEnrichmentJob'
  { -- | The Amazon Resource Name (ARN) of the Vector Enrichment job.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVectorEnrichmentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getVectorEnrichmentJob_arn' - The Amazon Resource Name (ARN) of the Vector Enrichment job.
newGetVectorEnrichmentJob ::
  -- | 'arn'
  Prelude.Text ->
  GetVectorEnrichmentJob
newGetVectorEnrichmentJob pArn_ =
  GetVectorEnrichmentJob' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the Vector Enrichment job.
getVectorEnrichmentJob_arn :: Lens.Lens' GetVectorEnrichmentJob Prelude.Text
getVectorEnrichmentJob_arn = Lens.lens (\GetVectorEnrichmentJob' {arn} -> arn) (\s@GetVectorEnrichmentJob' {} a -> s {arn = a} :: GetVectorEnrichmentJob)

instance Core.AWSRequest GetVectorEnrichmentJob where
  type
    AWSResponse GetVectorEnrichmentJob =
      GetVectorEnrichmentJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVectorEnrichmentJobResponse'
            Prelude.<$> (x Data..?> "ErrorDetails")
            Prelude.<*> (x Data..?> "ExportErrorDetails")
            Prelude.<*> (x Data..?> "ExportStatus")
            Prelude.<*> (x Data..?> "KmsKeyId")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "DurationInSeconds")
            Prelude.<*> (x Data..:> "ExecutionRoleArn")
            Prelude.<*> (x Data..:> "InputConfig")
            Prelude.<*> (x Data..:> "JobConfig")
            Prelude.<*> (x Data..:> "Name")
            Prelude.<*> (x Data..:> "Status")
            Prelude.<*> (x Data..:> "Type")
      )

instance Prelude.Hashable GetVectorEnrichmentJob where
  hashWithSalt _salt GetVectorEnrichmentJob' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetVectorEnrichmentJob where
  rnf GetVectorEnrichmentJob' {..} = Prelude.rnf arn

instance Data.ToHeaders GetVectorEnrichmentJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetVectorEnrichmentJob where
  toPath GetVectorEnrichmentJob' {..} =
    Prelude.mconcat
      ["/vector-enrichment-jobs/", Data.toBS arn]

instance Data.ToQuery GetVectorEnrichmentJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVectorEnrichmentJobResponse' smart constructor.
data GetVectorEnrichmentJobResponse = GetVectorEnrichmentJobResponse'
  { -- | Details about the errors generated during the Vector Enrichment job.
    errorDetails :: Prelude.Maybe VectorEnrichmentJobErrorDetails,
    -- | Details about the errors generated during the ExportVectorEnrichmentJob.
    exportErrorDetails :: Prelude.Maybe VectorEnrichmentJobExportErrorDetails,
    -- | The export status of the Vector Enrichment job being initiated.
    exportStatus :: Prelude.Maybe VectorEnrichmentJobExportStatus,
    -- | The Amazon Key Management Service (KMS) key ID for server-side
    -- encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Each tag consists of a key and a value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Vector Enrichment job.
    arn :: Prelude.Text,
    -- | The creation time.
    creationTime :: Data.POSIX,
    -- | The duration of the Vector Enrichment job, in seconds.
    durationInSeconds :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for
    -- the job.
    executionRoleArn :: Prelude.Text,
    -- | Input configuration information for the Vector Enrichment job.
    inputConfig :: VectorEnrichmentJobInputConfig,
    -- | An object containing information about the job configuration.
    jobConfig :: VectorEnrichmentJobConfig,
    -- | The name of the Vector Enrichment job.
    name :: Prelude.Text,
    -- | The status of the initiated Vector Enrichment job.
    status :: VectorEnrichmentJobStatus,
    -- | The type of the Vector Enrichment job being initiated.
    type' :: VectorEnrichmentJobType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVectorEnrichmentJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorDetails', 'getVectorEnrichmentJobResponse_errorDetails' - Details about the errors generated during the Vector Enrichment job.
--
-- 'exportErrorDetails', 'getVectorEnrichmentJobResponse_exportErrorDetails' - Details about the errors generated during the ExportVectorEnrichmentJob.
--
-- 'exportStatus', 'getVectorEnrichmentJobResponse_exportStatus' - The export status of the Vector Enrichment job being initiated.
--
-- 'kmsKeyId', 'getVectorEnrichmentJobResponse_kmsKeyId' - The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
--
-- 'tags', 'getVectorEnrichmentJobResponse_tags' - Each tag consists of a key and a value.
--
-- 'httpStatus', 'getVectorEnrichmentJobResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getVectorEnrichmentJobResponse_arn' - The Amazon Resource Name (ARN) of the Vector Enrichment job.
--
-- 'creationTime', 'getVectorEnrichmentJobResponse_creationTime' - The creation time.
--
-- 'durationInSeconds', 'getVectorEnrichmentJobResponse_durationInSeconds' - The duration of the Vector Enrichment job, in seconds.
--
-- 'executionRoleArn', 'getVectorEnrichmentJobResponse_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
--
-- 'inputConfig', 'getVectorEnrichmentJobResponse_inputConfig' - Input configuration information for the Vector Enrichment job.
--
-- 'jobConfig', 'getVectorEnrichmentJobResponse_jobConfig' - An object containing information about the job configuration.
--
-- 'name', 'getVectorEnrichmentJobResponse_name' - The name of the Vector Enrichment job.
--
-- 'status', 'getVectorEnrichmentJobResponse_status' - The status of the initiated Vector Enrichment job.
--
-- 'type'', 'getVectorEnrichmentJobResponse_type' - The type of the Vector Enrichment job being initiated.
newGetVectorEnrichmentJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'durationInSeconds'
  Prelude.Int ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'inputConfig'
  VectorEnrichmentJobInputConfig ->
  -- | 'jobConfig'
  VectorEnrichmentJobConfig ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  VectorEnrichmentJobStatus ->
  -- | 'type''
  VectorEnrichmentJobType ->
  GetVectorEnrichmentJobResponse
newGetVectorEnrichmentJobResponse
  pHttpStatus_
  pArn_
  pCreationTime_
  pDurationInSeconds_
  pExecutionRoleArn_
  pInputConfig_
  pJobConfig_
  pName_
  pStatus_
  pType_ =
    GetVectorEnrichmentJobResponse'
      { errorDetails =
          Prelude.Nothing,
        exportErrorDetails = Prelude.Nothing,
        exportStatus = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        durationInSeconds = pDurationInSeconds_,
        executionRoleArn = pExecutionRoleArn_,
        inputConfig = pInputConfig_,
        jobConfig = pJobConfig_,
        name = pName_,
        status = pStatus_,
        type' = pType_
      }

-- | Details about the errors generated during the Vector Enrichment job.
getVectorEnrichmentJobResponse_errorDetails :: Lens.Lens' GetVectorEnrichmentJobResponse (Prelude.Maybe VectorEnrichmentJobErrorDetails)
getVectorEnrichmentJobResponse_errorDetails = Lens.lens (\GetVectorEnrichmentJobResponse' {errorDetails} -> errorDetails) (\s@GetVectorEnrichmentJobResponse' {} a -> s {errorDetails = a} :: GetVectorEnrichmentJobResponse)

-- | Details about the errors generated during the ExportVectorEnrichmentJob.
getVectorEnrichmentJobResponse_exportErrorDetails :: Lens.Lens' GetVectorEnrichmentJobResponse (Prelude.Maybe VectorEnrichmentJobExportErrorDetails)
getVectorEnrichmentJobResponse_exportErrorDetails = Lens.lens (\GetVectorEnrichmentJobResponse' {exportErrorDetails} -> exportErrorDetails) (\s@GetVectorEnrichmentJobResponse' {} a -> s {exportErrorDetails = a} :: GetVectorEnrichmentJobResponse)

-- | The export status of the Vector Enrichment job being initiated.
getVectorEnrichmentJobResponse_exportStatus :: Lens.Lens' GetVectorEnrichmentJobResponse (Prelude.Maybe VectorEnrichmentJobExportStatus)
getVectorEnrichmentJobResponse_exportStatus = Lens.lens (\GetVectorEnrichmentJobResponse' {exportStatus} -> exportStatus) (\s@GetVectorEnrichmentJobResponse' {} a -> s {exportStatus = a} :: GetVectorEnrichmentJobResponse)

-- | The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
getVectorEnrichmentJobResponse_kmsKeyId :: Lens.Lens' GetVectorEnrichmentJobResponse (Prelude.Maybe Prelude.Text)
getVectorEnrichmentJobResponse_kmsKeyId = Lens.lens (\GetVectorEnrichmentJobResponse' {kmsKeyId} -> kmsKeyId) (\s@GetVectorEnrichmentJobResponse' {} a -> s {kmsKeyId = a} :: GetVectorEnrichmentJobResponse)

-- | Each tag consists of a key and a value.
getVectorEnrichmentJobResponse_tags :: Lens.Lens' GetVectorEnrichmentJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getVectorEnrichmentJobResponse_tags = Lens.lens (\GetVectorEnrichmentJobResponse' {tags} -> tags) (\s@GetVectorEnrichmentJobResponse' {} a -> s {tags = a} :: GetVectorEnrichmentJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getVectorEnrichmentJobResponse_httpStatus :: Lens.Lens' GetVectorEnrichmentJobResponse Prelude.Int
getVectorEnrichmentJobResponse_httpStatus = Lens.lens (\GetVectorEnrichmentJobResponse' {httpStatus} -> httpStatus) (\s@GetVectorEnrichmentJobResponse' {} a -> s {httpStatus = a} :: GetVectorEnrichmentJobResponse)

-- | The Amazon Resource Name (ARN) of the Vector Enrichment job.
getVectorEnrichmentJobResponse_arn :: Lens.Lens' GetVectorEnrichmentJobResponse Prelude.Text
getVectorEnrichmentJobResponse_arn = Lens.lens (\GetVectorEnrichmentJobResponse' {arn} -> arn) (\s@GetVectorEnrichmentJobResponse' {} a -> s {arn = a} :: GetVectorEnrichmentJobResponse)

-- | The creation time.
getVectorEnrichmentJobResponse_creationTime :: Lens.Lens' GetVectorEnrichmentJobResponse Prelude.UTCTime
getVectorEnrichmentJobResponse_creationTime = Lens.lens (\GetVectorEnrichmentJobResponse' {creationTime} -> creationTime) (\s@GetVectorEnrichmentJobResponse' {} a -> s {creationTime = a} :: GetVectorEnrichmentJobResponse) Prelude.. Data._Time

-- | The duration of the Vector Enrichment job, in seconds.
getVectorEnrichmentJobResponse_durationInSeconds :: Lens.Lens' GetVectorEnrichmentJobResponse Prelude.Int
getVectorEnrichmentJobResponse_durationInSeconds = Lens.lens (\GetVectorEnrichmentJobResponse' {durationInSeconds} -> durationInSeconds) (\s@GetVectorEnrichmentJobResponse' {} a -> s {durationInSeconds = a} :: GetVectorEnrichmentJobResponse)

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
getVectorEnrichmentJobResponse_executionRoleArn :: Lens.Lens' GetVectorEnrichmentJobResponse Prelude.Text
getVectorEnrichmentJobResponse_executionRoleArn = Lens.lens (\GetVectorEnrichmentJobResponse' {executionRoleArn} -> executionRoleArn) (\s@GetVectorEnrichmentJobResponse' {} a -> s {executionRoleArn = a} :: GetVectorEnrichmentJobResponse)

-- | Input configuration information for the Vector Enrichment job.
getVectorEnrichmentJobResponse_inputConfig :: Lens.Lens' GetVectorEnrichmentJobResponse VectorEnrichmentJobInputConfig
getVectorEnrichmentJobResponse_inputConfig = Lens.lens (\GetVectorEnrichmentJobResponse' {inputConfig} -> inputConfig) (\s@GetVectorEnrichmentJobResponse' {} a -> s {inputConfig = a} :: GetVectorEnrichmentJobResponse)

-- | An object containing information about the job configuration.
getVectorEnrichmentJobResponse_jobConfig :: Lens.Lens' GetVectorEnrichmentJobResponse VectorEnrichmentJobConfig
getVectorEnrichmentJobResponse_jobConfig = Lens.lens (\GetVectorEnrichmentJobResponse' {jobConfig} -> jobConfig) (\s@GetVectorEnrichmentJobResponse' {} a -> s {jobConfig = a} :: GetVectorEnrichmentJobResponse)

-- | The name of the Vector Enrichment job.
getVectorEnrichmentJobResponse_name :: Lens.Lens' GetVectorEnrichmentJobResponse Prelude.Text
getVectorEnrichmentJobResponse_name = Lens.lens (\GetVectorEnrichmentJobResponse' {name} -> name) (\s@GetVectorEnrichmentJobResponse' {} a -> s {name = a} :: GetVectorEnrichmentJobResponse)

-- | The status of the initiated Vector Enrichment job.
getVectorEnrichmentJobResponse_status :: Lens.Lens' GetVectorEnrichmentJobResponse VectorEnrichmentJobStatus
getVectorEnrichmentJobResponse_status = Lens.lens (\GetVectorEnrichmentJobResponse' {status} -> status) (\s@GetVectorEnrichmentJobResponse' {} a -> s {status = a} :: GetVectorEnrichmentJobResponse)

-- | The type of the Vector Enrichment job being initiated.
getVectorEnrichmentJobResponse_type :: Lens.Lens' GetVectorEnrichmentJobResponse VectorEnrichmentJobType
getVectorEnrichmentJobResponse_type = Lens.lens (\GetVectorEnrichmentJobResponse' {type'} -> type') (\s@GetVectorEnrichmentJobResponse' {} a -> s {type' = a} :: GetVectorEnrichmentJobResponse)

instance
  Prelude.NFData
    GetVectorEnrichmentJobResponse
  where
  rnf GetVectorEnrichmentJobResponse' {..} =
    Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf exportErrorDetails
      `Prelude.seq` Prelude.rnf exportStatus
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf durationInSeconds
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf inputConfig
      `Prelude.seq` Prelude.rnf jobConfig
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
