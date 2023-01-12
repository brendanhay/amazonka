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
-- Module      : Amazonka.SageMakerGeoSpatial.StartVectorEnrichmentJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Vector Enrichment job for the supplied job type. Currently,
-- there are two supported job types: reverse geocoding and map matching.
module Amazonka.SageMakerGeoSpatial.StartVectorEnrichmentJob
  ( -- * Creating a Request
    StartVectorEnrichmentJob (..),
    newStartVectorEnrichmentJob,

    -- * Request Lenses
    startVectorEnrichmentJob_clientToken,
    startVectorEnrichmentJob_kmsKeyId,
    startVectorEnrichmentJob_tags,
    startVectorEnrichmentJob_executionRoleArn,
    startVectorEnrichmentJob_inputConfig,
    startVectorEnrichmentJob_jobConfig,
    startVectorEnrichmentJob_name,

    -- * Destructuring the Response
    StartVectorEnrichmentJobResponse (..),
    newStartVectorEnrichmentJobResponse,

    -- * Response Lenses
    startVectorEnrichmentJobResponse_kmsKeyId,
    startVectorEnrichmentJobResponse_tags,
    startVectorEnrichmentJobResponse_httpStatus,
    startVectorEnrichmentJobResponse_arn,
    startVectorEnrichmentJobResponse_creationTime,
    startVectorEnrichmentJobResponse_durationInSeconds,
    startVectorEnrichmentJobResponse_executionRoleArn,
    startVectorEnrichmentJobResponse_inputConfig,
    startVectorEnrichmentJobResponse_jobConfig,
    startVectorEnrichmentJobResponse_name,
    startVectorEnrichmentJobResponse_status,
    startVectorEnrichmentJobResponse_type,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newStartVectorEnrichmentJob' smart constructor.
data StartVectorEnrichmentJob = StartVectorEnrichmentJob'
  { -- | A unique token that guarantees that the call to this API is idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Key Management Service (KMS) key ID for server-side
    -- encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Each tag consists of a key and a value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for
    -- the job.
    executionRoleArn :: Prelude.Text,
    -- | Input configuration information for the Vector Enrichment job.
    inputConfig :: VectorEnrichmentJobInputConfig,
    -- | An object containing information about the job configuration.
    jobConfig :: VectorEnrichmentJobConfig,
    -- | The name of the Vector Enrichment job.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartVectorEnrichmentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startVectorEnrichmentJob_clientToken' - A unique token that guarantees that the call to this API is idempotent.
--
-- 'kmsKeyId', 'startVectorEnrichmentJob_kmsKeyId' - The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
--
-- 'tags', 'startVectorEnrichmentJob_tags' - Each tag consists of a key and a value.
--
-- 'executionRoleArn', 'startVectorEnrichmentJob_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
--
-- 'inputConfig', 'startVectorEnrichmentJob_inputConfig' - Input configuration information for the Vector Enrichment job.
--
-- 'jobConfig', 'startVectorEnrichmentJob_jobConfig' - An object containing information about the job configuration.
--
-- 'name', 'startVectorEnrichmentJob_name' - The name of the Vector Enrichment job.
newStartVectorEnrichmentJob ::
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'inputConfig'
  VectorEnrichmentJobInputConfig ->
  -- | 'jobConfig'
  VectorEnrichmentJobConfig ->
  -- | 'name'
  Prelude.Text ->
  StartVectorEnrichmentJob
newStartVectorEnrichmentJob
  pExecutionRoleArn_
  pInputConfig_
  pJobConfig_
  pName_ =
    StartVectorEnrichmentJob'
      { clientToken =
          Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        executionRoleArn = pExecutionRoleArn_,
        inputConfig = pInputConfig_,
        jobConfig = pJobConfig_,
        name = pName_
      }

-- | A unique token that guarantees that the call to this API is idempotent.
startVectorEnrichmentJob_clientToken :: Lens.Lens' StartVectorEnrichmentJob (Prelude.Maybe Prelude.Text)
startVectorEnrichmentJob_clientToken = Lens.lens (\StartVectorEnrichmentJob' {clientToken} -> clientToken) (\s@StartVectorEnrichmentJob' {} a -> s {clientToken = a} :: StartVectorEnrichmentJob)

-- | The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
startVectorEnrichmentJob_kmsKeyId :: Lens.Lens' StartVectorEnrichmentJob (Prelude.Maybe Prelude.Text)
startVectorEnrichmentJob_kmsKeyId = Lens.lens (\StartVectorEnrichmentJob' {kmsKeyId} -> kmsKeyId) (\s@StartVectorEnrichmentJob' {} a -> s {kmsKeyId = a} :: StartVectorEnrichmentJob)

-- | Each tag consists of a key and a value.
startVectorEnrichmentJob_tags :: Lens.Lens' StartVectorEnrichmentJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startVectorEnrichmentJob_tags = Lens.lens (\StartVectorEnrichmentJob' {tags} -> tags) (\s@StartVectorEnrichmentJob' {} a -> s {tags = a} :: StartVectorEnrichmentJob) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
startVectorEnrichmentJob_executionRoleArn :: Lens.Lens' StartVectorEnrichmentJob Prelude.Text
startVectorEnrichmentJob_executionRoleArn = Lens.lens (\StartVectorEnrichmentJob' {executionRoleArn} -> executionRoleArn) (\s@StartVectorEnrichmentJob' {} a -> s {executionRoleArn = a} :: StartVectorEnrichmentJob)

-- | Input configuration information for the Vector Enrichment job.
startVectorEnrichmentJob_inputConfig :: Lens.Lens' StartVectorEnrichmentJob VectorEnrichmentJobInputConfig
startVectorEnrichmentJob_inputConfig = Lens.lens (\StartVectorEnrichmentJob' {inputConfig} -> inputConfig) (\s@StartVectorEnrichmentJob' {} a -> s {inputConfig = a} :: StartVectorEnrichmentJob)

-- | An object containing information about the job configuration.
startVectorEnrichmentJob_jobConfig :: Lens.Lens' StartVectorEnrichmentJob VectorEnrichmentJobConfig
startVectorEnrichmentJob_jobConfig = Lens.lens (\StartVectorEnrichmentJob' {jobConfig} -> jobConfig) (\s@StartVectorEnrichmentJob' {} a -> s {jobConfig = a} :: StartVectorEnrichmentJob)

-- | The name of the Vector Enrichment job.
startVectorEnrichmentJob_name :: Lens.Lens' StartVectorEnrichmentJob Prelude.Text
startVectorEnrichmentJob_name = Lens.lens (\StartVectorEnrichmentJob' {name} -> name) (\s@StartVectorEnrichmentJob' {} a -> s {name = a} :: StartVectorEnrichmentJob)

instance Core.AWSRequest StartVectorEnrichmentJob where
  type
    AWSResponse StartVectorEnrichmentJob =
      StartVectorEnrichmentJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartVectorEnrichmentJobResponse'
            Prelude.<$> (x Data..?> "KmsKeyId")
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

instance Prelude.Hashable StartVectorEnrichmentJob where
  hashWithSalt _salt StartVectorEnrichmentJob' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` inputConfig
      `Prelude.hashWithSalt` jobConfig
      `Prelude.hashWithSalt` name

instance Prelude.NFData StartVectorEnrichmentJob where
  rnf StartVectorEnrichmentJob' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf inputConfig
      `Prelude.seq` Prelude.rnf jobConfig
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders StartVectorEnrichmentJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartVectorEnrichmentJob where
  toJSON StartVectorEnrichmentJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ExecutionRoleArn" Data..= executionRoleArn),
            Prelude.Just ("InputConfig" Data..= inputConfig),
            Prelude.Just ("JobConfig" Data..= jobConfig),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath StartVectorEnrichmentJob where
  toPath = Prelude.const "/vector-enrichment-jobs"

instance Data.ToQuery StartVectorEnrichmentJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartVectorEnrichmentJobResponse' smart constructor.
data StartVectorEnrichmentJobResponse = StartVectorEnrichmentJobResponse'
  { -- | The Amazon Key Management Service (KMS) key ID for server-side
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
    -- | Input configuration information for starting the Vector Enrichment job.
    inputConfig :: VectorEnrichmentJobInputConfig,
    -- | An object containing information about the job configuration.
    jobConfig :: VectorEnrichmentJobConfig,
    -- | The name of the Vector Enrichment job.
    name :: Prelude.Text,
    -- | The status of the Vector Enrichment job being started.
    status :: VectorEnrichmentJobStatus,
    -- | The type of the Vector Enrichment job.
    type' :: VectorEnrichmentJobType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartVectorEnrichmentJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'startVectorEnrichmentJobResponse_kmsKeyId' - The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
--
-- 'tags', 'startVectorEnrichmentJobResponse_tags' - Each tag consists of a key and a value.
--
-- 'httpStatus', 'startVectorEnrichmentJobResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'startVectorEnrichmentJobResponse_arn' - The Amazon Resource Name (ARN) of the Vector Enrichment job.
--
-- 'creationTime', 'startVectorEnrichmentJobResponse_creationTime' - The creation time.
--
-- 'durationInSeconds', 'startVectorEnrichmentJobResponse_durationInSeconds' - The duration of the Vector Enrichment job, in seconds.
--
-- 'executionRoleArn', 'startVectorEnrichmentJobResponse_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
--
-- 'inputConfig', 'startVectorEnrichmentJobResponse_inputConfig' - Input configuration information for starting the Vector Enrichment job.
--
-- 'jobConfig', 'startVectorEnrichmentJobResponse_jobConfig' - An object containing information about the job configuration.
--
-- 'name', 'startVectorEnrichmentJobResponse_name' - The name of the Vector Enrichment job.
--
-- 'status', 'startVectorEnrichmentJobResponse_status' - The status of the Vector Enrichment job being started.
--
-- 'type'', 'startVectorEnrichmentJobResponse_type' - The type of the Vector Enrichment job.
newStartVectorEnrichmentJobResponse ::
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
  StartVectorEnrichmentJobResponse
newStartVectorEnrichmentJobResponse
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
    StartVectorEnrichmentJobResponse'
      { kmsKeyId =
          Prelude.Nothing,
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

-- | The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
startVectorEnrichmentJobResponse_kmsKeyId :: Lens.Lens' StartVectorEnrichmentJobResponse (Prelude.Maybe Prelude.Text)
startVectorEnrichmentJobResponse_kmsKeyId = Lens.lens (\StartVectorEnrichmentJobResponse' {kmsKeyId} -> kmsKeyId) (\s@StartVectorEnrichmentJobResponse' {} a -> s {kmsKeyId = a} :: StartVectorEnrichmentJobResponse)

-- | Each tag consists of a key and a value.
startVectorEnrichmentJobResponse_tags :: Lens.Lens' StartVectorEnrichmentJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startVectorEnrichmentJobResponse_tags = Lens.lens (\StartVectorEnrichmentJobResponse' {tags} -> tags) (\s@StartVectorEnrichmentJobResponse' {} a -> s {tags = a} :: StartVectorEnrichmentJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startVectorEnrichmentJobResponse_httpStatus :: Lens.Lens' StartVectorEnrichmentJobResponse Prelude.Int
startVectorEnrichmentJobResponse_httpStatus = Lens.lens (\StartVectorEnrichmentJobResponse' {httpStatus} -> httpStatus) (\s@StartVectorEnrichmentJobResponse' {} a -> s {httpStatus = a} :: StartVectorEnrichmentJobResponse)

-- | The Amazon Resource Name (ARN) of the Vector Enrichment job.
startVectorEnrichmentJobResponse_arn :: Lens.Lens' StartVectorEnrichmentJobResponse Prelude.Text
startVectorEnrichmentJobResponse_arn = Lens.lens (\StartVectorEnrichmentJobResponse' {arn} -> arn) (\s@StartVectorEnrichmentJobResponse' {} a -> s {arn = a} :: StartVectorEnrichmentJobResponse)

-- | The creation time.
startVectorEnrichmentJobResponse_creationTime :: Lens.Lens' StartVectorEnrichmentJobResponse Prelude.UTCTime
startVectorEnrichmentJobResponse_creationTime = Lens.lens (\StartVectorEnrichmentJobResponse' {creationTime} -> creationTime) (\s@StartVectorEnrichmentJobResponse' {} a -> s {creationTime = a} :: StartVectorEnrichmentJobResponse) Prelude.. Data._Time

-- | The duration of the Vector Enrichment job, in seconds.
startVectorEnrichmentJobResponse_durationInSeconds :: Lens.Lens' StartVectorEnrichmentJobResponse Prelude.Int
startVectorEnrichmentJobResponse_durationInSeconds = Lens.lens (\StartVectorEnrichmentJobResponse' {durationInSeconds} -> durationInSeconds) (\s@StartVectorEnrichmentJobResponse' {} a -> s {durationInSeconds = a} :: StartVectorEnrichmentJobResponse)

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
startVectorEnrichmentJobResponse_executionRoleArn :: Lens.Lens' StartVectorEnrichmentJobResponse Prelude.Text
startVectorEnrichmentJobResponse_executionRoleArn = Lens.lens (\StartVectorEnrichmentJobResponse' {executionRoleArn} -> executionRoleArn) (\s@StartVectorEnrichmentJobResponse' {} a -> s {executionRoleArn = a} :: StartVectorEnrichmentJobResponse)

-- | Input configuration information for starting the Vector Enrichment job.
startVectorEnrichmentJobResponse_inputConfig :: Lens.Lens' StartVectorEnrichmentJobResponse VectorEnrichmentJobInputConfig
startVectorEnrichmentJobResponse_inputConfig = Lens.lens (\StartVectorEnrichmentJobResponse' {inputConfig} -> inputConfig) (\s@StartVectorEnrichmentJobResponse' {} a -> s {inputConfig = a} :: StartVectorEnrichmentJobResponse)

-- | An object containing information about the job configuration.
startVectorEnrichmentJobResponse_jobConfig :: Lens.Lens' StartVectorEnrichmentJobResponse VectorEnrichmentJobConfig
startVectorEnrichmentJobResponse_jobConfig = Lens.lens (\StartVectorEnrichmentJobResponse' {jobConfig} -> jobConfig) (\s@StartVectorEnrichmentJobResponse' {} a -> s {jobConfig = a} :: StartVectorEnrichmentJobResponse)

-- | The name of the Vector Enrichment job.
startVectorEnrichmentJobResponse_name :: Lens.Lens' StartVectorEnrichmentJobResponse Prelude.Text
startVectorEnrichmentJobResponse_name = Lens.lens (\StartVectorEnrichmentJobResponse' {name} -> name) (\s@StartVectorEnrichmentJobResponse' {} a -> s {name = a} :: StartVectorEnrichmentJobResponse)

-- | The status of the Vector Enrichment job being started.
startVectorEnrichmentJobResponse_status :: Lens.Lens' StartVectorEnrichmentJobResponse VectorEnrichmentJobStatus
startVectorEnrichmentJobResponse_status = Lens.lens (\StartVectorEnrichmentJobResponse' {status} -> status) (\s@StartVectorEnrichmentJobResponse' {} a -> s {status = a} :: StartVectorEnrichmentJobResponse)

-- | The type of the Vector Enrichment job.
startVectorEnrichmentJobResponse_type :: Lens.Lens' StartVectorEnrichmentJobResponse VectorEnrichmentJobType
startVectorEnrichmentJobResponse_type = Lens.lens (\StartVectorEnrichmentJobResponse' {type'} -> type') (\s@StartVectorEnrichmentJobResponse' {} a -> s {type' = a} :: StartVectorEnrichmentJobResponse)

instance
  Prelude.NFData
    StartVectorEnrichmentJobResponse
  where
  rnf StartVectorEnrichmentJobResponse' {..} =
    Prelude.rnf kmsKeyId
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
