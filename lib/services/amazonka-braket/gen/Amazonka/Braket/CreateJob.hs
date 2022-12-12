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
-- Module      : Amazonka.Braket.CreateJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Braket job.
module Amazonka.Braket.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_checkpointConfig,
    createJob_hyperParameters,
    createJob_inputDataConfig,
    createJob_stoppingCondition,
    createJob_tags,
    createJob_algorithmSpecification,
    createJob_clientToken,
    createJob_deviceConfig,
    createJob_instanceConfig,
    createJob_jobName,
    createJob_outputDataConfig,
    createJob_roleArn,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_httpStatus,
    createJobResponse_jobArn,
  )
where

import Amazonka.Braket.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | Information about the output locations for job checkpoint data.
    checkpointConfig :: Prelude.Maybe JobCheckpointConfig,
    -- | Algorithm-specific parameters used by an Amazon Braket job that
    -- influence the quality of the training job. The values are set with a
    -- string of JSON key:value pairs, where the key is the name of the
    -- hyperparameter and the value is the value of th hyperparameter.
    hyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of parameters that specify the name and type of input data and
    -- where it is located.
    inputDataConfig :: Prelude.Maybe [InputFileConfig],
    -- | The user-defined criteria that specifies when a job stops running.
    stoppingCondition :: Prelude.Maybe JobStoppingCondition,
    -- | A tag object that consists of a key and an optional value, used to
    -- manage metadata for Amazon Braket resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Definition of the Amazon Braket job to be created. Specifies the
    -- container image the job uses and information about the Python scripts
    -- used for entry and training.
    algorithmSpecification :: AlgorithmSpecification,
    -- | A unique token that guarantees that the call to this API is idempotent.
    clientToken :: Prelude.Text,
    -- | The quantum processing unit (QPU) or simulator used to create an Amazon
    -- Braket job.
    deviceConfig :: DeviceConfig,
    -- | Configuration of the resource instances to use while running the hybrid
    -- job on Amazon Braket.
    instanceConfig :: InstanceConfig,
    -- | The name of the Amazon Braket job.
    jobName :: Prelude.Text,
    -- | The path to the S3 location where you want to store job artifacts and
    -- the encryption key used to store them.
    outputDataConfig :: JobOutputDataConfig,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon Braket can
    -- assume to perform tasks on behalf of a user. It can access user
    -- resources, run an Amazon Braket job container on behalf of user, and
    -- output resources to the users\' s3 buckets.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkpointConfig', 'createJob_checkpointConfig' - Information about the output locations for job checkpoint data.
--
-- 'hyperParameters', 'createJob_hyperParameters' - Algorithm-specific parameters used by an Amazon Braket job that
-- influence the quality of the training job. The values are set with a
-- string of JSON key:value pairs, where the key is the name of the
-- hyperparameter and the value is the value of th hyperparameter.
--
-- 'inputDataConfig', 'createJob_inputDataConfig' - A list of parameters that specify the name and type of input data and
-- where it is located.
--
-- 'stoppingCondition', 'createJob_stoppingCondition' - The user-defined criteria that specifies when a job stops running.
--
-- 'tags', 'createJob_tags' - A tag object that consists of a key and an optional value, used to
-- manage metadata for Amazon Braket resources.
--
-- 'algorithmSpecification', 'createJob_algorithmSpecification' - Definition of the Amazon Braket job to be created. Specifies the
-- container image the job uses and information about the Python scripts
-- used for entry and training.
--
-- 'clientToken', 'createJob_clientToken' - A unique token that guarantees that the call to this API is idempotent.
--
-- 'deviceConfig', 'createJob_deviceConfig' - The quantum processing unit (QPU) or simulator used to create an Amazon
-- Braket job.
--
-- 'instanceConfig', 'createJob_instanceConfig' - Configuration of the resource instances to use while running the hybrid
-- job on Amazon Braket.
--
-- 'jobName', 'createJob_jobName' - The name of the Amazon Braket job.
--
-- 'outputDataConfig', 'createJob_outputDataConfig' - The path to the S3 location where you want to store job artifacts and
-- the encryption key used to store them.
--
-- 'roleArn', 'createJob_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon Braket can
-- assume to perform tasks on behalf of a user. It can access user
-- resources, run an Amazon Braket job container on behalf of user, and
-- output resources to the users\' s3 buckets.
newCreateJob ::
  -- | 'algorithmSpecification'
  AlgorithmSpecification ->
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'deviceConfig'
  DeviceConfig ->
  -- | 'instanceConfig'
  InstanceConfig ->
  -- | 'jobName'
  Prelude.Text ->
  -- | 'outputDataConfig'
  JobOutputDataConfig ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateJob
newCreateJob
  pAlgorithmSpecification_
  pClientToken_
  pDeviceConfig_
  pInstanceConfig_
  pJobName_
  pOutputDataConfig_
  pRoleArn_ =
    CreateJob'
      { checkpointConfig = Prelude.Nothing,
        hyperParameters = Prelude.Nothing,
        inputDataConfig = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        tags = Prelude.Nothing,
        algorithmSpecification = pAlgorithmSpecification_,
        clientToken = pClientToken_,
        deviceConfig = pDeviceConfig_,
        instanceConfig = pInstanceConfig_,
        jobName = pJobName_,
        outputDataConfig = pOutputDataConfig_,
        roleArn = pRoleArn_
      }

-- | Information about the output locations for job checkpoint data.
createJob_checkpointConfig :: Lens.Lens' CreateJob (Prelude.Maybe JobCheckpointConfig)
createJob_checkpointConfig = Lens.lens (\CreateJob' {checkpointConfig} -> checkpointConfig) (\s@CreateJob' {} a -> s {checkpointConfig = a} :: CreateJob)

-- | Algorithm-specific parameters used by an Amazon Braket job that
-- influence the quality of the training job. The values are set with a
-- string of JSON key:value pairs, where the key is the name of the
-- hyperparameter and the value is the value of th hyperparameter.
createJob_hyperParameters :: Lens.Lens' CreateJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createJob_hyperParameters = Lens.lens (\CreateJob' {hyperParameters} -> hyperParameters) (\s@CreateJob' {} a -> s {hyperParameters = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | A list of parameters that specify the name and type of input data and
-- where it is located.
createJob_inputDataConfig :: Lens.Lens' CreateJob (Prelude.Maybe [InputFileConfig])
createJob_inputDataConfig = Lens.lens (\CreateJob' {inputDataConfig} -> inputDataConfig) (\s@CreateJob' {} a -> s {inputDataConfig = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | The user-defined criteria that specifies when a job stops running.
createJob_stoppingCondition :: Lens.Lens' CreateJob (Prelude.Maybe JobStoppingCondition)
createJob_stoppingCondition = Lens.lens (\CreateJob' {stoppingCondition} -> stoppingCondition) (\s@CreateJob' {} a -> s {stoppingCondition = a} :: CreateJob)

-- | A tag object that consists of a key and an optional value, used to
-- manage metadata for Amazon Braket resources.
createJob_tags :: Lens.Lens' CreateJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createJob_tags = Lens.lens (\CreateJob' {tags} -> tags) (\s@CreateJob' {} a -> s {tags = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | Definition of the Amazon Braket job to be created. Specifies the
-- container image the job uses and information about the Python scripts
-- used for entry and training.
createJob_algorithmSpecification :: Lens.Lens' CreateJob AlgorithmSpecification
createJob_algorithmSpecification = Lens.lens (\CreateJob' {algorithmSpecification} -> algorithmSpecification) (\s@CreateJob' {} a -> s {algorithmSpecification = a} :: CreateJob)

-- | A unique token that guarantees that the call to this API is idempotent.
createJob_clientToken :: Lens.Lens' CreateJob Prelude.Text
createJob_clientToken = Lens.lens (\CreateJob' {clientToken} -> clientToken) (\s@CreateJob' {} a -> s {clientToken = a} :: CreateJob)

-- | The quantum processing unit (QPU) or simulator used to create an Amazon
-- Braket job.
createJob_deviceConfig :: Lens.Lens' CreateJob DeviceConfig
createJob_deviceConfig = Lens.lens (\CreateJob' {deviceConfig} -> deviceConfig) (\s@CreateJob' {} a -> s {deviceConfig = a} :: CreateJob)

-- | Configuration of the resource instances to use while running the hybrid
-- job on Amazon Braket.
createJob_instanceConfig :: Lens.Lens' CreateJob InstanceConfig
createJob_instanceConfig = Lens.lens (\CreateJob' {instanceConfig} -> instanceConfig) (\s@CreateJob' {} a -> s {instanceConfig = a} :: CreateJob)

-- | The name of the Amazon Braket job.
createJob_jobName :: Lens.Lens' CreateJob Prelude.Text
createJob_jobName = Lens.lens (\CreateJob' {jobName} -> jobName) (\s@CreateJob' {} a -> s {jobName = a} :: CreateJob)

-- | The path to the S3 location where you want to store job artifacts and
-- the encryption key used to store them.
createJob_outputDataConfig :: Lens.Lens' CreateJob JobOutputDataConfig
createJob_outputDataConfig = Lens.lens (\CreateJob' {outputDataConfig} -> outputDataConfig) (\s@CreateJob' {} a -> s {outputDataConfig = a} :: CreateJob)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon Braket can
-- assume to perform tasks on behalf of a user. It can access user
-- resources, run an Amazon Braket job container on behalf of user, and
-- output resources to the users\' s3 buckets.
createJob_roleArn :: Lens.Lens' CreateJob Prelude.Text
createJob_roleArn = Lens.lens (\CreateJob' {roleArn} -> roleArn) (\s@CreateJob' {} a -> s {roleArn = a} :: CreateJob)

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "jobArn")
      )

instance Prelude.Hashable CreateJob where
  hashWithSalt _salt CreateJob' {..} =
    _salt `Prelude.hashWithSalt` checkpointConfig
      `Prelude.hashWithSalt` hyperParameters
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` stoppingCondition
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` algorithmSpecification
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` deviceConfig
      `Prelude.hashWithSalt` instanceConfig
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateJob where
  rnf CreateJob' {..} =
    Prelude.rnf checkpointConfig
      `Prelude.seq` Prelude.rnf hyperParameters
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf stoppingCondition
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf algorithmSpecification
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf deviceConfig
      `Prelude.seq` Prelude.rnf instanceConfig
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("checkpointConfig" Data..=)
              Prelude.<$> checkpointConfig,
            ("hyperParameters" Data..=)
              Prelude.<$> hyperParameters,
            ("inputDataConfig" Data..=)
              Prelude.<$> inputDataConfig,
            ("stoppingCondition" Data..=)
              Prelude.<$> stoppingCondition,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "algorithmSpecification"
                  Data..= algorithmSpecification
              ),
            Prelude.Just ("clientToken" Data..= clientToken),
            Prelude.Just ("deviceConfig" Data..= deviceConfig),
            Prelude.Just
              ("instanceConfig" Data..= instanceConfig),
            Prelude.Just ("jobName" Data..= jobName),
            Prelude.Just
              ("outputDataConfig" Data..= outputDataConfig),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateJob where
  toPath = Prelude.const "/job"

instance Data.ToQuery CreateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the Amazon Braket job created.
    jobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createJobResponse_httpStatus' - The response's http status code.
--
-- 'jobArn', 'createJobResponse_jobArn' - The ARN of the Amazon Braket job created.
newCreateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobArn'
  Prelude.Text ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ pJobArn_ =
  CreateJobResponse'
    { httpStatus = pHttpStatus_,
      jobArn = pJobArn_
    }

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Prelude.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

-- | The ARN of the Amazon Braket job created.
createJobResponse_jobArn :: Lens.Lens' CreateJobResponse Prelude.Text
createJobResponse_jobArn = Lens.lens (\CreateJobResponse' {jobArn} -> jobArn) (\s@CreateJobResponse' {} a -> s {jobArn = a} :: CreateJobResponse)

instance Prelude.NFData CreateJobResponse where
  rnf CreateJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobArn
