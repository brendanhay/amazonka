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
-- Module      : Network.AWS.SageMaker.CreateProcessingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a processing job.
module Network.AWS.SageMaker.CreateProcessingJob
  ( -- * Creating a Request
    CreateProcessingJob (..),
    newCreateProcessingJob,

    -- * Request Lenses
    createProcessingJob_networkConfig,
    createProcessingJob_processingOutputConfig,
    createProcessingJob_experimentConfig,
    createProcessingJob_environment,
    createProcessingJob_tags,
    createProcessingJob_processingInputs,
    createProcessingJob_stoppingCondition,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateProcessingJob' smart constructor.
data CreateProcessingJob = CreateProcessingJob'
  { -- | Networking options for a processing job, such as whether to allow
    -- inbound and outbound network calls to and from processing containers,
    -- and the VPC subnets and security groups to use for VPC-enabled
    -- processing jobs.
    networkConfig :: Prelude.Maybe NetworkConfig,
    -- | Output configuration for the processing job.
    processingOutputConfig :: Prelude.Maybe ProcessingOutputConfig,
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | The environment variables to set in the Docker container. Up to 100 key
    -- and values entries in the map are supported.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | (Optional) An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | An array of inputs configuring the data to download into the processing
    -- container.
    processingInputs :: Prelude.Maybe [ProcessingInput],
    -- | The time limit for how long the processing job is allowed to run.
    stoppingCondition :: Prelude.Maybe ProcessingStoppingCondition,
    -- | The name of the processing job. The name must be unique within an AWS
    -- Region in the AWS account.
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
-- 'networkConfig', 'createProcessingJob_networkConfig' - Networking options for a processing job, such as whether to allow
-- inbound and outbound network calls to and from processing containers,
-- and the VPC subnets and security groups to use for VPC-enabled
-- processing jobs.
--
-- 'processingOutputConfig', 'createProcessingJob_processingOutputConfig' - Output configuration for the processing job.
--
-- 'experimentConfig', 'createProcessingJob_experimentConfig' - Undocumented member.
--
-- 'environment', 'createProcessingJob_environment' - The environment variables to set in the Docker container. Up to 100 key
-- and values entries in the map are supported.
--
-- 'tags', 'createProcessingJob_tags' - (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- 'processingInputs', 'createProcessingJob_processingInputs' - An array of inputs configuring the data to download into the processing
-- container.
--
-- 'stoppingCondition', 'createProcessingJob_stoppingCondition' - The time limit for how long the processing job is allowed to run.
--
-- 'processingJobName', 'createProcessingJob_processingJobName' - The name of the processing job. The name must be unique within an AWS
-- Region in the AWS account.
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
      { networkConfig =
          Prelude.Nothing,
        processingOutputConfig = Prelude.Nothing,
        experimentConfig = Prelude.Nothing,
        environment = Prelude.Nothing,
        tags = Prelude.Nothing,
        processingInputs = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        processingJobName = pProcessingJobName_,
        processingResources = pProcessingResources_,
        appSpecification = pAppSpecification_,
        roleArn = pRoleArn_
      }

-- | Networking options for a processing job, such as whether to allow
-- inbound and outbound network calls to and from processing containers,
-- and the VPC subnets and security groups to use for VPC-enabled
-- processing jobs.
createProcessingJob_networkConfig :: Lens.Lens' CreateProcessingJob (Prelude.Maybe NetworkConfig)
createProcessingJob_networkConfig = Lens.lens (\CreateProcessingJob' {networkConfig} -> networkConfig) (\s@CreateProcessingJob' {} a -> s {networkConfig = a} :: CreateProcessingJob)

-- | Output configuration for the processing job.
createProcessingJob_processingOutputConfig :: Lens.Lens' CreateProcessingJob (Prelude.Maybe ProcessingOutputConfig)
createProcessingJob_processingOutputConfig = Lens.lens (\CreateProcessingJob' {processingOutputConfig} -> processingOutputConfig) (\s@CreateProcessingJob' {} a -> s {processingOutputConfig = a} :: CreateProcessingJob)

-- | Undocumented member.
createProcessingJob_experimentConfig :: Lens.Lens' CreateProcessingJob (Prelude.Maybe ExperimentConfig)
createProcessingJob_experimentConfig = Lens.lens (\CreateProcessingJob' {experimentConfig} -> experimentConfig) (\s@CreateProcessingJob' {} a -> s {experimentConfig = a} :: CreateProcessingJob)

-- | The environment variables to set in the Docker container. Up to 100 key
-- and values entries in the map are supported.
createProcessingJob_environment :: Lens.Lens' CreateProcessingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProcessingJob_environment = Lens.lens (\CreateProcessingJob' {environment} -> environment) (\s@CreateProcessingJob' {} a -> s {environment = a} :: CreateProcessingJob) Prelude.. Lens.mapping Lens._Coerce

-- | (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
createProcessingJob_tags :: Lens.Lens' CreateProcessingJob (Prelude.Maybe [Tag])
createProcessingJob_tags = Lens.lens (\CreateProcessingJob' {tags} -> tags) (\s@CreateProcessingJob' {} a -> s {tags = a} :: CreateProcessingJob) Prelude.. Lens.mapping Lens._Coerce

-- | An array of inputs configuring the data to download into the processing
-- container.
createProcessingJob_processingInputs :: Lens.Lens' CreateProcessingJob (Prelude.Maybe [ProcessingInput])
createProcessingJob_processingInputs = Lens.lens (\CreateProcessingJob' {processingInputs} -> processingInputs) (\s@CreateProcessingJob' {} a -> s {processingInputs = a} :: CreateProcessingJob) Prelude.. Lens.mapping Lens._Coerce

-- | The time limit for how long the processing job is allowed to run.
createProcessingJob_stoppingCondition :: Lens.Lens' CreateProcessingJob (Prelude.Maybe ProcessingStoppingCondition)
createProcessingJob_stoppingCondition = Lens.lens (\CreateProcessingJob' {stoppingCondition} -> stoppingCondition) (\s@CreateProcessingJob' {} a -> s {stoppingCondition = a} :: CreateProcessingJob)

-- | The name of the processing job. The name must be unique within an AWS
-- Region in the AWS account.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProcessingJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ProcessingJobArn")
      )

instance Prelude.Hashable CreateProcessingJob

instance Prelude.NFData CreateProcessingJob

instance Core.ToHeaders CreateProcessingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateProcessingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateProcessingJob where
  toJSON CreateProcessingJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NetworkConfig" Core..=) Prelude.<$> networkConfig,
            ("ProcessingOutputConfig" Core..=)
              Prelude.<$> processingOutputConfig,
            ("ExperimentConfig" Core..=)
              Prelude.<$> experimentConfig,
            ("Environment" Core..=) Prelude.<$> environment,
            ("Tags" Core..=) Prelude.<$> tags,
            ("ProcessingInputs" Core..=)
              Prelude.<$> processingInputs,
            ("StoppingCondition" Core..=)
              Prelude.<$> stoppingCondition,
            Prelude.Just
              ("ProcessingJobName" Core..= processingJobName),
            Prelude.Just
              ("ProcessingResources" Core..= processingResources),
            Prelude.Just
              ("AppSpecification" Core..= appSpecification),
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateProcessingJob where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateProcessingJob where
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

instance Prelude.NFData CreateProcessingJobResponse
