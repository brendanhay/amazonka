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
-- Module      : Amazonka.Braket.GetJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified Amazon Braket job.
module Amazonka.Braket.GetJob
  ( -- * Creating a Request
    GetJob (..),
    newGetJob,

    -- * Request Lenses
    getJob_jobArn,

    -- * Destructuring the Response
    GetJobResponse (..),
    newGetJobResponse,

    -- * Response Lenses
    getJobResponse_billableDuration,
    getJobResponse_checkpointConfig,
    getJobResponse_deviceConfig,
    getJobResponse_endedAt,
    getJobResponse_events,
    getJobResponse_failureReason,
    getJobResponse_hyperParameters,
    getJobResponse_inputDataConfig,
    getJobResponse_startedAt,
    getJobResponse_stoppingCondition,
    getJobResponse_tags,
    getJobResponse_httpStatus,
    getJobResponse_algorithmSpecification,
    getJobResponse_createdAt,
    getJobResponse_instanceConfig,
    getJobResponse_jobArn,
    getJobResponse_jobName,
    getJobResponse_outputDataConfig,
    getJobResponse_roleArn,
    getJobResponse_status,
  )
where

import Amazonka.Braket.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJob' smart constructor.
data GetJob = GetJob'
  { -- | The ARN of the job to retrieve.
    jobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobArn', 'getJob_jobArn' - The ARN of the job to retrieve.
newGetJob ::
  -- | 'jobArn'
  Prelude.Text ->
  GetJob
newGetJob pJobArn_ = GetJob' {jobArn = pJobArn_}

-- | The ARN of the job to retrieve.
getJob_jobArn :: Lens.Lens' GetJob Prelude.Text
getJob_jobArn = Lens.lens (\GetJob' {jobArn} -> jobArn) (\s@GetJob' {} a -> s {jobArn = a} :: GetJob)

instance Core.AWSRequest GetJob where
  type AWSResponse GetJob = GetJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobResponse'
            Prelude.<$> (x Data..?> "billableDuration")
            Prelude.<*> (x Data..?> "checkpointConfig")
            Prelude.<*> (x Data..?> "deviceConfig")
            Prelude.<*> (x Data..?> "endedAt")
            Prelude.<*> (x Data..?> "events" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "failureReason")
            Prelude.<*> ( x Data..?> "hyperParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "inputDataConfig"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "startedAt")
            Prelude.<*> (x Data..?> "stoppingCondition")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "algorithmSpecification")
            Prelude.<*> (x Data..:> "createdAt")
            Prelude.<*> (x Data..:> "instanceConfig")
            Prelude.<*> (x Data..:> "jobArn")
            Prelude.<*> (x Data..:> "jobName")
            Prelude.<*> (x Data..:> "outputDataConfig")
            Prelude.<*> (x Data..:> "roleArn")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable GetJob where
  hashWithSalt _salt GetJob' {..} =
    _salt `Prelude.hashWithSalt` jobArn

instance Prelude.NFData GetJob where
  rnf GetJob' {..} = Prelude.rnf jobArn

instance Data.ToHeaders GetJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetJob where
  toPath GetJob' {..} =
    Prelude.mconcat ["/job/", Data.toBS jobArn]

instance Data.ToQuery GetJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { -- | The billable time the Amazon Braket job used to complete.
    billableDuration :: Prelude.Maybe Prelude.Int,
    -- | Information about the output locations for job checkpoint data.
    checkpointConfig :: Prelude.Maybe JobCheckpointConfig,
    -- | The quantum processing unit (QPU) or simulator used to run the Amazon
    -- Braket job.
    deviceConfig :: Prelude.Maybe DeviceConfig,
    -- | The date and time that the Amazon Braket job ended.
    endedAt :: Prelude.Maybe Data.ISO8601,
    -- | Details about the type and time events occurred related to the Amazon
    -- Braket job.
    events :: Prelude.Maybe [JobEventDetails],
    -- | A description of the reason why an Amazon Braket job failed, if it
    -- failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Algorithm-specific parameters used by an Amazon Braket job that
    -- influence the quality of the traiing job. The values are set with a
    -- string of JSON key:value pairs, where the key is the name of the
    -- hyperparameter and the value is the value of th hyperparameter.
    hyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of parameters that specify the name and type of input data and
    -- where it is located.
    inputDataConfig :: Prelude.Maybe [InputFileConfig],
    -- | The date and time that the Amazon Braket job was started.
    startedAt :: Prelude.Maybe Data.ISO8601,
    -- | The user-defined criteria that specifies when to stop a job running.
    stoppingCondition :: Prelude.Maybe JobStoppingCondition,
    -- | A tag object that consists of a key and an optional value, used to
    -- manage metadata for Amazon Braket resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Definition of the Amazon Braket job created. Specifies the container
    -- image the job uses, information about the Python scripts used for entry
    -- and training, and the user-defined metrics used to evaluation the job.
    algorithmSpecification :: AlgorithmSpecification,
    -- | The date and time that the Amazon Braket job was created.
    createdAt :: Data.ISO8601,
    -- | The resource instances to use while running the hybrid job on Amazon
    -- Braket.
    instanceConfig :: InstanceConfig,
    -- | The ARN of the Amazon Braket job.
    jobArn :: Prelude.Text,
    -- | The name of the Amazon Braket job.
    jobName :: Prelude.Text,
    -- | The path to the S3 location where job artifacts are stored and the
    -- encryption key used to store them there.
    outputDataConfig :: JobOutputDataConfig,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon Braket can
    -- assume to perform tasks on behalf of a user. It can access user
    -- resources, run an Amazon Braket job container on behalf of user, and
    -- output resources to the s3 buckets of a user.
    roleArn :: Prelude.Text,
    -- | The status of the Amazon Braket job.
    status :: JobPrimaryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billableDuration', 'getJobResponse_billableDuration' - The billable time the Amazon Braket job used to complete.
--
-- 'checkpointConfig', 'getJobResponse_checkpointConfig' - Information about the output locations for job checkpoint data.
--
-- 'deviceConfig', 'getJobResponse_deviceConfig' - The quantum processing unit (QPU) or simulator used to run the Amazon
-- Braket job.
--
-- 'endedAt', 'getJobResponse_endedAt' - The date and time that the Amazon Braket job ended.
--
-- 'events', 'getJobResponse_events' - Details about the type and time events occurred related to the Amazon
-- Braket job.
--
-- 'failureReason', 'getJobResponse_failureReason' - A description of the reason why an Amazon Braket job failed, if it
-- failed.
--
-- 'hyperParameters', 'getJobResponse_hyperParameters' - Algorithm-specific parameters used by an Amazon Braket job that
-- influence the quality of the traiing job. The values are set with a
-- string of JSON key:value pairs, where the key is the name of the
-- hyperparameter and the value is the value of th hyperparameter.
--
-- 'inputDataConfig', 'getJobResponse_inputDataConfig' - A list of parameters that specify the name and type of input data and
-- where it is located.
--
-- 'startedAt', 'getJobResponse_startedAt' - The date and time that the Amazon Braket job was started.
--
-- 'stoppingCondition', 'getJobResponse_stoppingCondition' - The user-defined criteria that specifies when to stop a job running.
--
-- 'tags', 'getJobResponse_tags' - A tag object that consists of a key and an optional value, used to
-- manage metadata for Amazon Braket resources.
--
-- 'httpStatus', 'getJobResponse_httpStatus' - The response's http status code.
--
-- 'algorithmSpecification', 'getJobResponse_algorithmSpecification' - Definition of the Amazon Braket job created. Specifies the container
-- image the job uses, information about the Python scripts used for entry
-- and training, and the user-defined metrics used to evaluation the job.
--
-- 'createdAt', 'getJobResponse_createdAt' - The date and time that the Amazon Braket job was created.
--
-- 'instanceConfig', 'getJobResponse_instanceConfig' - The resource instances to use while running the hybrid job on Amazon
-- Braket.
--
-- 'jobArn', 'getJobResponse_jobArn' - The ARN of the Amazon Braket job.
--
-- 'jobName', 'getJobResponse_jobName' - The name of the Amazon Braket job.
--
-- 'outputDataConfig', 'getJobResponse_outputDataConfig' - The path to the S3 location where job artifacts are stored and the
-- encryption key used to store them there.
--
-- 'roleArn', 'getJobResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon Braket can
-- assume to perform tasks on behalf of a user. It can access user
-- resources, run an Amazon Braket job container on behalf of user, and
-- output resources to the s3 buckets of a user.
--
-- 'status', 'getJobResponse_status' - The status of the Amazon Braket job.
newGetJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'algorithmSpecification'
  AlgorithmSpecification ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'instanceConfig'
  InstanceConfig ->
  -- | 'jobArn'
  Prelude.Text ->
  -- | 'jobName'
  Prelude.Text ->
  -- | 'outputDataConfig'
  JobOutputDataConfig ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'status'
  JobPrimaryStatus ->
  GetJobResponse
newGetJobResponse
  pHttpStatus_
  pAlgorithmSpecification_
  pCreatedAt_
  pInstanceConfig_
  pJobArn_
  pJobName_
  pOutputDataConfig_
  pRoleArn_
  pStatus_ =
    GetJobResponse'
      { billableDuration = Prelude.Nothing,
        checkpointConfig = Prelude.Nothing,
        deviceConfig = Prelude.Nothing,
        endedAt = Prelude.Nothing,
        events = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        hyperParameters = Prelude.Nothing,
        inputDataConfig = Prelude.Nothing,
        startedAt = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        algorithmSpecification = pAlgorithmSpecification_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        instanceConfig = pInstanceConfig_,
        jobArn = pJobArn_,
        jobName = pJobName_,
        outputDataConfig = pOutputDataConfig_,
        roleArn = pRoleArn_,
        status = pStatus_
      }

-- | The billable time the Amazon Braket job used to complete.
getJobResponse_billableDuration :: Lens.Lens' GetJobResponse (Prelude.Maybe Prelude.Int)
getJobResponse_billableDuration = Lens.lens (\GetJobResponse' {billableDuration} -> billableDuration) (\s@GetJobResponse' {} a -> s {billableDuration = a} :: GetJobResponse)

-- | Information about the output locations for job checkpoint data.
getJobResponse_checkpointConfig :: Lens.Lens' GetJobResponse (Prelude.Maybe JobCheckpointConfig)
getJobResponse_checkpointConfig = Lens.lens (\GetJobResponse' {checkpointConfig} -> checkpointConfig) (\s@GetJobResponse' {} a -> s {checkpointConfig = a} :: GetJobResponse)

-- | The quantum processing unit (QPU) or simulator used to run the Amazon
-- Braket job.
getJobResponse_deviceConfig :: Lens.Lens' GetJobResponse (Prelude.Maybe DeviceConfig)
getJobResponse_deviceConfig = Lens.lens (\GetJobResponse' {deviceConfig} -> deviceConfig) (\s@GetJobResponse' {} a -> s {deviceConfig = a} :: GetJobResponse)

-- | The date and time that the Amazon Braket job ended.
getJobResponse_endedAt :: Lens.Lens' GetJobResponse (Prelude.Maybe Prelude.UTCTime)
getJobResponse_endedAt = Lens.lens (\GetJobResponse' {endedAt} -> endedAt) (\s@GetJobResponse' {} a -> s {endedAt = a} :: GetJobResponse) Prelude.. Lens.mapping Data._Time

-- | Details about the type and time events occurred related to the Amazon
-- Braket job.
getJobResponse_events :: Lens.Lens' GetJobResponse (Prelude.Maybe [JobEventDetails])
getJobResponse_events = Lens.lens (\GetJobResponse' {events} -> events) (\s@GetJobResponse' {} a -> s {events = a} :: GetJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | A description of the reason why an Amazon Braket job failed, if it
-- failed.
getJobResponse_failureReason :: Lens.Lens' GetJobResponse (Prelude.Maybe Prelude.Text)
getJobResponse_failureReason = Lens.lens (\GetJobResponse' {failureReason} -> failureReason) (\s@GetJobResponse' {} a -> s {failureReason = a} :: GetJobResponse)

-- | Algorithm-specific parameters used by an Amazon Braket job that
-- influence the quality of the traiing job. The values are set with a
-- string of JSON key:value pairs, where the key is the name of the
-- hyperparameter and the value is the value of th hyperparameter.
getJobResponse_hyperParameters :: Lens.Lens' GetJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getJobResponse_hyperParameters = Lens.lens (\GetJobResponse' {hyperParameters} -> hyperParameters) (\s@GetJobResponse' {} a -> s {hyperParameters = a} :: GetJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of parameters that specify the name and type of input data and
-- where it is located.
getJobResponse_inputDataConfig :: Lens.Lens' GetJobResponse (Prelude.Maybe [InputFileConfig])
getJobResponse_inputDataConfig = Lens.lens (\GetJobResponse' {inputDataConfig} -> inputDataConfig) (\s@GetJobResponse' {} a -> s {inputDataConfig = a} :: GetJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the Amazon Braket job was started.
getJobResponse_startedAt :: Lens.Lens' GetJobResponse (Prelude.Maybe Prelude.UTCTime)
getJobResponse_startedAt = Lens.lens (\GetJobResponse' {startedAt} -> startedAt) (\s@GetJobResponse' {} a -> s {startedAt = a} :: GetJobResponse) Prelude.. Lens.mapping Data._Time

-- | The user-defined criteria that specifies when to stop a job running.
getJobResponse_stoppingCondition :: Lens.Lens' GetJobResponse (Prelude.Maybe JobStoppingCondition)
getJobResponse_stoppingCondition = Lens.lens (\GetJobResponse' {stoppingCondition} -> stoppingCondition) (\s@GetJobResponse' {} a -> s {stoppingCondition = a} :: GetJobResponse)

-- | A tag object that consists of a key and an optional value, used to
-- manage metadata for Amazon Braket resources.
getJobResponse_tags :: Lens.Lens' GetJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getJobResponse_tags = Lens.lens (\GetJobResponse' {tags} -> tags) (\s@GetJobResponse' {} a -> s {tags = a} :: GetJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getJobResponse_httpStatus :: Lens.Lens' GetJobResponse Prelude.Int
getJobResponse_httpStatus = Lens.lens (\GetJobResponse' {httpStatus} -> httpStatus) (\s@GetJobResponse' {} a -> s {httpStatus = a} :: GetJobResponse)

-- | Definition of the Amazon Braket job created. Specifies the container
-- image the job uses, information about the Python scripts used for entry
-- and training, and the user-defined metrics used to evaluation the job.
getJobResponse_algorithmSpecification :: Lens.Lens' GetJobResponse AlgorithmSpecification
getJobResponse_algorithmSpecification = Lens.lens (\GetJobResponse' {algorithmSpecification} -> algorithmSpecification) (\s@GetJobResponse' {} a -> s {algorithmSpecification = a} :: GetJobResponse)

-- | The date and time that the Amazon Braket job was created.
getJobResponse_createdAt :: Lens.Lens' GetJobResponse Prelude.UTCTime
getJobResponse_createdAt = Lens.lens (\GetJobResponse' {createdAt} -> createdAt) (\s@GetJobResponse' {} a -> s {createdAt = a} :: GetJobResponse) Prelude.. Data._Time

-- | The resource instances to use while running the hybrid job on Amazon
-- Braket.
getJobResponse_instanceConfig :: Lens.Lens' GetJobResponse InstanceConfig
getJobResponse_instanceConfig = Lens.lens (\GetJobResponse' {instanceConfig} -> instanceConfig) (\s@GetJobResponse' {} a -> s {instanceConfig = a} :: GetJobResponse)

-- | The ARN of the Amazon Braket job.
getJobResponse_jobArn :: Lens.Lens' GetJobResponse Prelude.Text
getJobResponse_jobArn = Lens.lens (\GetJobResponse' {jobArn} -> jobArn) (\s@GetJobResponse' {} a -> s {jobArn = a} :: GetJobResponse)

-- | The name of the Amazon Braket job.
getJobResponse_jobName :: Lens.Lens' GetJobResponse Prelude.Text
getJobResponse_jobName = Lens.lens (\GetJobResponse' {jobName} -> jobName) (\s@GetJobResponse' {} a -> s {jobName = a} :: GetJobResponse)

-- | The path to the S3 location where job artifacts are stored and the
-- encryption key used to store them there.
getJobResponse_outputDataConfig :: Lens.Lens' GetJobResponse JobOutputDataConfig
getJobResponse_outputDataConfig = Lens.lens (\GetJobResponse' {outputDataConfig} -> outputDataConfig) (\s@GetJobResponse' {} a -> s {outputDataConfig = a} :: GetJobResponse)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon Braket can
-- assume to perform tasks on behalf of a user. It can access user
-- resources, run an Amazon Braket job container on behalf of user, and
-- output resources to the s3 buckets of a user.
getJobResponse_roleArn :: Lens.Lens' GetJobResponse Prelude.Text
getJobResponse_roleArn = Lens.lens (\GetJobResponse' {roleArn} -> roleArn) (\s@GetJobResponse' {} a -> s {roleArn = a} :: GetJobResponse)

-- | The status of the Amazon Braket job.
getJobResponse_status :: Lens.Lens' GetJobResponse JobPrimaryStatus
getJobResponse_status = Lens.lens (\GetJobResponse' {status} -> status) (\s@GetJobResponse' {} a -> s {status = a} :: GetJobResponse)

instance Prelude.NFData GetJobResponse where
  rnf GetJobResponse' {..} =
    Prelude.rnf billableDuration
      `Prelude.seq` Prelude.rnf checkpointConfig
      `Prelude.seq` Prelude.rnf deviceConfig
      `Prelude.seq` Prelude.rnf endedAt
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf hyperParameters
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf stoppingCondition
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf algorithmSpecification
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf instanceConfig
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
