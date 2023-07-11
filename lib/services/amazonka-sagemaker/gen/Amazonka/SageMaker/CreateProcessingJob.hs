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
-- Module      : Amazonka.SageMaker.CreateProcessingJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a processing job.
module Amazonka.SageMaker.CreateProcessingJob
  ( -- * Creating a Request
    CreateProcessingJob (..),
    newCreateProcessingJob,

    -- * Request Lenses
    createProcessingJob_environment,
    createProcessingJob_experimentConfig,
    createProcessingJob_networkConfig,
    createProcessingJob_processingInputs,
    createProcessingJob_processingOutputConfig,
    createProcessingJob_stoppingCondition,
    createProcessingJob_tags,
    createProcessingJob_processingJobName,
    createProcessingJob_processingResources,
    createProcessingJob_appSpecification,
    createProcessingJob_roleArn,

    -- * Destructuring the Response
    CreateProcessingJobResponse (..),
    newCreateProcessingJobResponse,

    -- * Response Lenses
    createProcessingJobResponse_httpStatus,
    createProcessingJobResponse_processingJobArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateProcessingJob' smart constructor.
data CreateProcessingJob = CreateProcessingJob'
  { -- | The environment variables to set in the Docker container. Up to 100 key
    -- and values entries in the map are supported.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | Networking options for a processing job, such as whether to allow
    -- inbound and outbound network calls to and from processing containers,
    -- and the VPC subnets and security groups to use for VPC-enabled
    -- processing jobs.
    networkConfig :: Prelude.Maybe NetworkConfig,
    -- | An array of inputs configuring the data to download into the processing
    -- container.
    processingInputs :: Prelude.Maybe [ProcessingInput],
    -- | Output configuration for the processing job.
    processingOutputConfig :: Prelude.Maybe ProcessingOutputConfig,
    -- | The time limit for how long the processing job is allowed to run.
    stoppingCondition :: Prelude.Maybe ProcessingStoppingCondition,
    -- | (Optional) An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /Amazon Web Services Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the processing job. The name must be unique within an Amazon
    -- Web Services Region in the Amazon Web Services account.
    processingJobName :: Prelude.Text,
    -- | Identifies the resources, ML compute instances, and ML storage volumes
    -- to deploy for a processing job. In distributed training, you specify
    -- more than one instance.
    processingResources :: ProcessingResources,
    -- | Configures the processing job to run a specified Docker container image.
    appSpecification :: AppSpecification,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProcessingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environment', 'createProcessingJob_environment' - The environment variables to set in the Docker container. Up to 100 key
-- and values entries in the map are supported.
--
-- 'experimentConfig', 'createProcessingJob_experimentConfig' - Undocumented member.
--
-- 'networkConfig', 'createProcessingJob_networkConfig' - Networking options for a processing job, such as whether to allow
-- inbound and outbound network calls to and from processing containers,
-- and the VPC subnets and security groups to use for VPC-enabled
-- processing jobs.
--
-- 'processingInputs', 'createProcessingJob_processingInputs' - An array of inputs configuring the data to download into the processing
-- container.
--
-- 'processingOutputConfig', 'createProcessingJob_processingOutputConfig' - Output configuration for the processing job.
--
-- 'stoppingCondition', 'createProcessingJob_stoppingCondition' - The time limit for how long the processing job is allowed to run.
--
-- 'tags', 'createProcessingJob_tags' - (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
--
-- 'processingJobName', 'createProcessingJob_processingJobName' - The name of the processing job. The name must be unique within an Amazon
-- Web Services Region in the Amazon Web Services account.
--
-- 'processingResources', 'createProcessingJob_processingResources' - Identifies the resources, ML compute instances, and ML storage volumes
-- to deploy for a processing job. In distributed training, you specify
-- more than one instance.
--
-- 'appSpecification', 'createProcessingJob_appSpecification' - Configures the processing job to run a specified Docker container image.
--
-- 'roleArn', 'createProcessingJob_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
newCreateProcessingJob ::
  -- | 'processingJobName'
  Prelude.Text ->
  -- | 'processingResources'
  ProcessingResources ->
  -- | 'appSpecification'
  AppSpecification ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateProcessingJob
newCreateProcessingJob
  pProcessingJobName_
  pProcessingResources_
  pAppSpecification_
  pRoleArn_ =
    CreateProcessingJob'
      { environment = Prelude.Nothing,
        experimentConfig = Prelude.Nothing,
        networkConfig = Prelude.Nothing,
        processingInputs = Prelude.Nothing,
        processingOutputConfig = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        tags = Prelude.Nothing,
        processingJobName = pProcessingJobName_,
        processingResources = pProcessingResources_,
        appSpecification = pAppSpecification_,
        roleArn = pRoleArn_
      }

-- | The environment variables to set in the Docker container. Up to 100 key
-- and values entries in the map are supported.
createProcessingJob_environment :: Lens.Lens' CreateProcessingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProcessingJob_environment = Lens.lens (\CreateProcessingJob' {environment} -> environment) (\s@CreateProcessingJob' {} a -> s {environment = a} :: CreateProcessingJob) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createProcessingJob_experimentConfig :: Lens.Lens' CreateProcessingJob (Prelude.Maybe ExperimentConfig)
createProcessingJob_experimentConfig = Lens.lens (\CreateProcessingJob' {experimentConfig} -> experimentConfig) (\s@CreateProcessingJob' {} a -> s {experimentConfig = a} :: CreateProcessingJob)

-- | Networking options for a processing job, such as whether to allow
-- inbound and outbound network calls to and from processing containers,
-- and the VPC subnets and security groups to use for VPC-enabled
-- processing jobs.
createProcessingJob_networkConfig :: Lens.Lens' CreateProcessingJob (Prelude.Maybe NetworkConfig)
createProcessingJob_networkConfig = Lens.lens (\CreateProcessingJob' {networkConfig} -> networkConfig) (\s@CreateProcessingJob' {} a -> s {networkConfig = a} :: CreateProcessingJob)

-- | An array of inputs configuring the data to download into the processing
-- container.
createProcessingJob_processingInputs :: Lens.Lens' CreateProcessingJob (Prelude.Maybe [ProcessingInput])
createProcessingJob_processingInputs = Lens.lens (\CreateProcessingJob' {processingInputs} -> processingInputs) (\s@CreateProcessingJob' {} a -> s {processingInputs = a} :: CreateProcessingJob) Prelude.. Lens.mapping Lens.coerced

-- | Output configuration for the processing job.
createProcessingJob_processingOutputConfig :: Lens.Lens' CreateProcessingJob (Prelude.Maybe ProcessingOutputConfig)
createProcessingJob_processingOutputConfig = Lens.lens (\CreateProcessingJob' {processingOutputConfig} -> processingOutputConfig) (\s@CreateProcessingJob' {} a -> s {processingOutputConfig = a} :: CreateProcessingJob)

-- | The time limit for how long the processing job is allowed to run.
createProcessingJob_stoppingCondition :: Lens.Lens' CreateProcessingJob (Prelude.Maybe ProcessingStoppingCondition)
createProcessingJob_stoppingCondition = Lens.lens (\CreateProcessingJob' {stoppingCondition} -> stoppingCondition) (\s@CreateProcessingJob' {} a -> s {stoppingCondition = a} :: CreateProcessingJob)

-- | (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
createProcessingJob_tags :: Lens.Lens' CreateProcessingJob (Prelude.Maybe [Tag])
createProcessingJob_tags = Lens.lens (\CreateProcessingJob' {tags} -> tags) (\s@CreateProcessingJob' {} a -> s {tags = a} :: CreateProcessingJob) Prelude.. Lens.mapping Lens.coerced

-- | The name of the processing job. The name must be unique within an Amazon
-- Web Services Region in the Amazon Web Services account.
createProcessingJob_processingJobName :: Lens.Lens' CreateProcessingJob Prelude.Text
createProcessingJob_processingJobName = Lens.lens (\CreateProcessingJob' {processingJobName} -> processingJobName) (\s@CreateProcessingJob' {} a -> s {processingJobName = a} :: CreateProcessingJob)

-- | Identifies the resources, ML compute instances, and ML storage volumes
-- to deploy for a processing job. In distributed training, you specify
-- more than one instance.
createProcessingJob_processingResources :: Lens.Lens' CreateProcessingJob ProcessingResources
createProcessingJob_processingResources = Lens.lens (\CreateProcessingJob' {processingResources} -> processingResources) (\s@CreateProcessingJob' {} a -> s {processingResources = a} :: CreateProcessingJob)

-- | Configures the processing job to run a specified Docker container image.
createProcessingJob_appSpecification :: Lens.Lens' CreateProcessingJob AppSpecification
createProcessingJob_appSpecification = Lens.lens (\CreateProcessingJob' {appSpecification} -> appSpecification) (\s@CreateProcessingJob' {} a -> s {appSpecification = a} :: CreateProcessingJob)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
createProcessingJob_roleArn :: Lens.Lens' CreateProcessingJob Prelude.Text
createProcessingJob_roleArn = Lens.lens (\CreateProcessingJob' {roleArn} -> roleArn) (\s@CreateProcessingJob' {} a -> s {roleArn = a} :: CreateProcessingJob)

instance Core.AWSRequest CreateProcessingJob where
  type
    AWSResponse CreateProcessingJob =
      CreateProcessingJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProcessingJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ProcessingJobArn")
      )

instance Prelude.Hashable CreateProcessingJob where
  hashWithSalt _salt CreateProcessingJob' {..} =
    _salt
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` experimentConfig
      `Prelude.hashWithSalt` networkConfig
      `Prelude.hashWithSalt` processingInputs
      `Prelude.hashWithSalt` processingOutputConfig
      `Prelude.hashWithSalt` stoppingCondition
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` processingJobName
      `Prelude.hashWithSalt` processingResources
      `Prelude.hashWithSalt` appSpecification
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateProcessingJob where
  rnf CreateProcessingJob' {..} =
    Prelude.rnf environment
      `Prelude.seq` Prelude.rnf experimentConfig
      `Prelude.seq` Prelude.rnf networkConfig
      `Prelude.seq` Prelude.rnf processingInputs
      `Prelude.seq` Prelude.rnf processingOutputConfig
      `Prelude.seq` Prelude.rnf stoppingCondition
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf processingJobName
      `Prelude.seq` Prelude.rnf processingResources
      `Prelude.seq` Prelude.rnf appSpecification
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreateProcessingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateProcessingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProcessingJob where
  toJSON CreateProcessingJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Environment" Data..=) Prelude.<$> environment,
            ("ExperimentConfig" Data..=)
              Prelude.<$> experimentConfig,
            ("NetworkConfig" Data..=) Prelude.<$> networkConfig,
            ("ProcessingInputs" Data..=)
              Prelude.<$> processingInputs,
            ("ProcessingOutputConfig" Data..=)
              Prelude.<$> processingOutputConfig,
            ("StoppingCondition" Data..=)
              Prelude.<$> stoppingCondition,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ProcessingJobName" Data..= processingJobName),
            Prelude.Just
              ("ProcessingResources" Data..= processingResources),
            Prelude.Just
              ("AppSpecification" Data..= appSpecification),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateProcessingJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateProcessingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProcessingJobResponse' smart constructor.
data CreateProcessingJobResponse = CreateProcessingJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the processing job.
    processingJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProcessingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createProcessingJobResponse_httpStatus' - The response's http status code.
--
-- 'processingJobArn', 'createProcessingJobResponse_processingJobArn' - The Amazon Resource Name (ARN) of the processing job.
newCreateProcessingJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'processingJobArn'
  Prelude.Text ->
  CreateProcessingJobResponse
newCreateProcessingJobResponse
  pHttpStatus_
  pProcessingJobArn_ =
    CreateProcessingJobResponse'
      { httpStatus =
          pHttpStatus_,
        processingJobArn = pProcessingJobArn_
      }

-- | The response's http status code.
createProcessingJobResponse_httpStatus :: Lens.Lens' CreateProcessingJobResponse Prelude.Int
createProcessingJobResponse_httpStatus = Lens.lens (\CreateProcessingJobResponse' {httpStatus} -> httpStatus) (\s@CreateProcessingJobResponse' {} a -> s {httpStatus = a} :: CreateProcessingJobResponse)

-- | The Amazon Resource Name (ARN) of the processing job.
createProcessingJobResponse_processingJobArn :: Lens.Lens' CreateProcessingJobResponse Prelude.Text
createProcessingJobResponse_processingJobArn = Lens.lens (\CreateProcessingJobResponse' {processingJobArn} -> processingJobArn) (\s@CreateProcessingJobResponse' {} a -> s {processingJobArn = a} :: CreateProcessingJobResponse)

instance Prelude.NFData CreateProcessingJobResponse where
  rnf CreateProcessingJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf processingJobArn
