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
-- Module      : Amazonka.SageMakerGeoSpatial.StartEarthObservationJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to create an Earth observation job.
module Amazonka.SageMakerGeoSpatial.StartEarthObservationJob
  ( -- * Creating a Request
    StartEarthObservationJob (..),
    newStartEarthObservationJob,

    -- * Request Lenses
    startEarthObservationJob_clientToken,
    startEarthObservationJob_kmsKeyId,
    startEarthObservationJob_tags,
    startEarthObservationJob_executionRoleArn,
    startEarthObservationJob_inputConfig,
    startEarthObservationJob_jobConfig,
    startEarthObservationJob_name,

    -- * Destructuring the Response
    StartEarthObservationJobResponse (..),
    newStartEarthObservationJobResponse,

    -- * Response Lenses
    startEarthObservationJobResponse_inputConfig,
    startEarthObservationJobResponse_kmsKeyId,
    startEarthObservationJobResponse_tags,
    startEarthObservationJobResponse_httpStatus,
    startEarthObservationJobResponse_arn,
    startEarthObservationJobResponse_creationTime,
    startEarthObservationJobResponse_durationInSeconds,
    startEarthObservationJobResponse_executionRoleArn,
    startEarthObservationJobResponse_jobConfig,
    startEarthObservationJobResponse_name,
    startEarthObservationJobResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newStartEarthObservationJob' smart constructor.
data StartEarthObservationJob = StartEarthObservationJob'
  { -- | A unique token that guarantees that the call to this API is idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Key Management Service key ID for server-side encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Each tag consists of a key and a value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for
    -- the job.
    executionRoleArn :: Prelude.Text,
    -- | Input configuration information for the Earth Observation job.
    inputConfig :: InputConfigInput,
    -- | An object containing information about the job configuration.
    jobConfig :: JobConfigInput,
    -- | The name of the Earth Observation job.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEarthObservationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startEarthObservationJob_clientToken' - A unique token that guarantees that the call to this API is idempotent.
--
-- 'kmsKeyId', 'startEarthObservationJob_kmsKeyId' - The Key Management Service key ID for server-side encryption.
--
-- 'tags', 'startEarthObservationJob_tags' - Each tag consists of a key and a value.
--
-- 'executionRoleArn', 'startEarthObservationJob_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
--
-- 'inputConfig', 'startEarthObservationJob_inputConfig' - Input configuration information for the Earth Observation job.
--
-- 'jobConfig', 'startEarthObservationJob_jobConfig' - An object containing information about the job configuration.
--
-- 'name', 'startEarthObservationJob_name' - The name of the Earth Observation job.
newStartEarthObservationJob ::
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'inputConfig'
  InputConfigInput ->
  -- | 'jobConfig'
  JobConfigInput ->
  -- | 'name'
  Prelude.Text ->
  StartEarthObservationJob
newStartEarthObservationJob
  pExecutionRoleArn_
  pInputConfig_
  pJobConfig_
  pName_ =
    StartEarthObservationJob'
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
startEarthObservationJob_clientToken :: Lens.Lens' StartEarthObservationJob (Prelude.Maybe Prelude.Text)
startEarthObservationJob_clientToken = Lens.lens (\StartEarthObservationJob' {clientToken} -> clientToken) (\s@StartEarthObservationJob' {} a -> s {clientToken = a} :: StartEarthObservationJob)

-- | The Key Management Service key ID for server-side encryption.
startEarthObservationJob_kmsKeyId :: Lens.Lens' StartEarthObservationJob (Prelude.Maybe Prelude.Text)
startEarthObservationJob_kmsKeyId = Lens.lens (\StartEarthObservationJob' {kmsKeyId} -> kmsKeyId) (\s@StartEarthObservationJob' {} a -> s {kmsKeyId = a} :: StartEarthObservationJob)

-- | Each tag consists of a key and a value.
startEarthObservationJob_tags :: Lens.Lens' StartEarthObservationJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startEarthObservationJob_tags = Lens.lens (\StartEarthObservationJob' {tags} -> tags) (\s@StartEarthObservationJob' {} a -> s {tags = a} :: StartEarthObservationJob) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
startEarthObservationJob_executionRoleArn :: Lens.Lens' StartEarthObservationJob Prelude.Text
startEarthObservationJob_executionRoleArn = Lens.lens (\StartEarthObservationJob' {executionRoleArn} -> executionRoleArn) (\s@StartEarthObservationJob' {} a -> s {executionRoleArn = a} :: StartEarthObservationJob)

-- | Input configuration information for the Earth Observation job.
startEarthObservationJob_inputConfig :: Lens.Lens' StartEarthObservationJob InputConfigInput
startEarthObservationJob_inputConfig = Lens.lens (\StartEarthObservationJob' {inputConfig} -> inputConfig) (\s@StartEarthObservationJob' {} a -> s {inputConfig = a} :: StartEarthObservationJob)

-- | An object containing information about the job configuration.
startEarthObservationJob_jobConfig :: Lens.Lens' StartEarthObservationJob JobConfigInput
startEarthObservationJob_jobConfig = Lens.lens (\StartEarthObservationJob' {jobConfig} -> jobConfig) (\s@StartEarthObservationJob' {} a -> s {jobConfig = a} :: StartEarthObservationJob)

-- | The name of the Earth Observation job.
startEarthObservationJob_name :: Lens.Lens' StartEarthObservationJob Prelude.Text
startEarthObservationJob_name = Lens.lens (\StartEarthObservationJob' {name} -> name) (\s@StartEarthObservationJob' {} a -> s {name = a} :: StartEarthObservationJob)

instance Core.AWSRequest StartEarthObservationJob where
  type
    AWSResponse StartEarthObservationJob =
      StartEarthObservationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartEarthObservationJobResponse'
            Prelude.<$> (x Data..?> "InputConfig")
            Prelude.<*> (x Data..?> "KmsKeyId")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "DurationInSeconds")
            Prelude.<*> (x Data..:> "ExecutionRoleArn")
            Prelude.<*> (x Data..:> "JobConfig")
            Prelude.<*> (x Data..:> "Name")
            Prelude.<*> (x Data..:> "Status")
      )

instance Prelude.Hashable StartEarthObservationJob where
  hashWithSalt _salt StartEarthObservationJob' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` inputConfig
      `Prelude.hashWithSalt` jobConfig
      `Prelude.hashWithSalt` name

instance Prelude.NFData StartEarthObservationJob where
  rnf StartEarthObservationJob' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf inputConfig
      `Prelude.seq` Prelude.rnf jobConfig
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders StartEarthObservationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartEarthObservationJob where
  toJSON StartEarthObservationJob' {..} =
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

instance Data.ToPath StartEarthObservationJob where
  toPath = Prelude.const "/earth-observation-jobs"

instance Data.ToQuery StartEarthObservationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartEarthObservationJobResponse' smart constructor.
data StartEarthObservationJobResponse = StartEarthObservationJobResponse'
  { -- | Input configuration information for the Earth Observation job.
    inputConfig :: Prelude.Maybe InputConfigOutput,
    -- | The Key Management Service key ID for server-side encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Each tag consists of a key and a value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Earth Observation job.
    arn :: Prelude.Text,
    -- | The creation time.
    creationTime :: Data.ISO8601,
    -- | The duration of the session, in seconds.
    durationInSeconds :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for
    -- the job.
    executionRoleArn :: Prelude.Text,
    -- | An object containing information about the job configuration.
    jobConfig :: JobConfigInput,
    -- | The name of the Earth Observation job.
    name :: Prelude.Text,
    -- | The status of the Earth Observation job.
    status :: EarthObservationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEarthObservationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputConfig', 'startEarthObservationJobResponse_inputConfig' - Input configuration information for the Earth Observation job.
--
-- 'kmsKeyId', 'startEarthObservationJobResponse_kmsKeyId' - The Key Management Service key ID for server-side encryption.
--
-- 'tags', 'startEarthObservationJobResponse_tags' - Each tag consists of a key and a value.
--
-- 'httpStatus', 'startEarthObservationJobResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'startEarthObservationJobResponse_arn' - The Amazon Resource Name (ARN) of the Earth Observation job.
--
-- 'creationTime', 'startEarthObservationJobResponse_creationTime' - The creation time.
--
-- 'durationInSeconds', 'startEarthObservationJobResponse_durationInSeconds' - The duration of the session, in seconds.
--
-- 'executionRoleArn', 'startEarthObservationJobResponse_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
--
-- 'jobConfig', 'startEarthObservationJobResponse_jobConfig' - An object containing information about the job configuration.
--
-- 'name', 'startEarthObservationJobResponse_name' - The name of the Earth Observation job.
--
-- 'status', 'startEarthObservationJobResponse_status' - The status of the Earth Observation job.
newStartEarthObservationJobResponse ::
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
  -- | 'jobConfig'
  JobConfigInput ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  EarthObservationJobStatus ->
  StartEarthObservationJobResponse
newStartEarthObservationJobResponse
  pHttpStatus_
  pArn_
  pCreationTime_
  pDurationInSeconds_
  pExecutionRoleArn_
  pJobConfig_
  pName_
  pStatus_ =
    StartEarthObservationJobResponse'
      { inputConfig =
          Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        durationInSeconds = pDurationInSeconds_,
        executionRoleArn = pExecutionRoleArn_,
        jobConfig = pJobConfig_,
        name = pName_,
        status = pStatus_
      }

-- | Input configuration information for the Earth Observation job.
startEarthObservationJobResponse_inputConfig :: Lens.Lens' StartEarthObservationJobResponse (Prelude.Maybe InputConfigOutput)
startEarthObservationJobResponse_inputConfig = Lens.lens (\StartEarthObservationJobResponse' {inputConfig} -> inputConfig) (\s@StartEarthObservationJobResponse' {} a -> s {inputConfig = a} :: StartEarthObservationJobResponse)

-- | The Key Management Service key ID for server-side encryption.
startEarthObservationJobResponse_kmsKeyId :: Lens.Lens' StartEarthObservationJobResponse (Prelude.Maybe Prelude.Text)
startEarthObservationJobResponse_kmsKeyId = Lens.lens (\StartEarthObservationJobResponse' {kmsKeyId} -> kmsKeyId) (\s@StartEarthObservationJobResponse' {} a -> s {kmsKeyId = a} :: StartEarthObservationJobResponse)

-- | Each tag consists of a key and a value.
startEarthObservationJobResponse_tags :: Lens.Lens' StartEarthObservationJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startEarthObservationJobResponse_tags = Lens.lens (\StartEarthObservationJobResponse' {tags} -> tags) (\s@StartEarthObservationJobResponse' {} a -> s {tags = a} :: StartEarthObservationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startEarthObservationJobResponse_httpStatus :: Lens.Lens' StartEarthObservationJobResponse Prelude.Int
startEarthObservationJobResponse_httpStatus = Lens.lens (\StartEarthObservationJobResponse' {httpStatus} -> httpStatus) (\s@StartEarthObservationJobResponse' {} a -> s {httpStatus = a} :: StartEarthObservationJobResponse)

-- | The Amazon Resource Name (ARN) of the Earth Observation job.
startEarthObservationJobResponse_arn :: Lens.Lens' StartEarthObservationJobResponse Prelude.Text
startEarthObservationJobResponse_arn = Lens.lens (\StartEarthObservationJobResponse' {arn} -> arn) (\s@StartEarthObservationJobResponse' {} a -> s {arn = a} :: StartEarthObservationJobResponse)

-- | The creation time.
startEarthObservationJobResponse_creationTime :: Lens.Lens' StartEarthObservationJobResponse Prelude.UTCTime
startEarthObservationJobResponse_creationTime = Lens.lens (\StartEarthObservationJobResponse' {creationTime} -> creationTime) (\s@StartEarthObservationJobResponse' {} a -> s {creationTime = a} :: StartEarthObservationJobResponse) Prelude.. Data._Time

-- | The duration of the session, in seconds.
startEarthObservationJobResponse_durationInSeconds :: Lens.Lens' StartEarthObservationJobResponse Prelude.Int
startEarthObservationJobResponse_durationInSeconds = Lens.lens (\StartEarthObservationJobResponse' {durationInSeconds} -> durationInSeconds) (\s@StartEarthObservationJobResponse' {} a -> s {durationInSeconds = a} :: StartEarthObservationJobResponse)

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
startEarthObservationJobResponse_executionRoleArn :: Lens.Lens' StartEarthObservationJobResponse Prelude.Text
startEarthObservationJobResponse_executionRoleArn = Lens.lens (\StartEarthObservationJobResponse' {executionRoleArn} -> executionRoleArn) (\s@StartEarthObservationJobResponse' {} a -> s {executionRoleArn = a} :: StartEarthObservationJobResponse)

-- | An object containing information about the job configuration.
startEarthObservationJobResponse_jobConfig :: Lens.Lens' StartEarthObservationJobResponse JobConfigInput
startEarthObservationJobResponse_jobConfig = Lens.lens (\StartEarthObservationJobResponse' {jobConfig} -> jobConfig) (\s@StartEarthObservationJobResponse' {} a -> s {jobConfig = a} :: StartEarthObservationJobResponse)

-- | The name of the Earth Observation job.
startEarthObservationJobResponse_name :: Lens.Lens' StartEarthObservationJobResponse Prelude.Text
startEarthObservationJobResponse_name = Lens.lens (\StartEarthObservationJobResponse' {name} -> name) (\s@StartEarthObservationJobResponse' {} a -> s {name = a} :: StartEarthObservationJobResponse)

-- | The status of the Earth Observation job.
startEarthObservationJobResponse_status :: Lens.Lens' StartEarthObservationJobResponse EarthObservationJobStatus
startEarthObservationJobResponse_status = Lens.lens (\StartEarthObservationJobResponse' {status} -> status) (\s@StartEarthObservationJobResponse' {} a -> s {status = a} :: StartEarthObservationJobResponse)

instance
  Prelude.NFData
    StartEarthObservationJobResponse
  where
  rnf StartEarthObservationJobResponse' {..} =
    Prelude.rnf inputConfig
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf durationInSeconds
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf jobConfig
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
