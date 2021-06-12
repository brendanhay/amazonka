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
-- Module      : Network.AWS.SageMaker.DescribeModelExplainabilityJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a model explainability job definition.
module Network.AWS.SageMaker.DescribeModelExplainabilityJobDefinition
  ( -- * Creating a Request
    DescribeModelExplainabilityJobDefinition (..),
    newDescribeModelExplainabilityJobDefinition,

    -- * Request Lenses
    describeModelExplainabilityJobDefinition_jobDefinitionName,

    -- * Destructuring the Response
    DescribeModelExplainabilityJobDefinitionResponse (..),
    newDescribeModelExplainabilityJobDefinitionResponse,

    -- * Response Lenses
    describeModelExplainabilityJobDefinitionResponse_networkConfig,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityBaselineConfig,
    describeModelExplainabilityJobDefinitionResponse_stoppingCondition,
    describeModelExplainabilityJobDefinitionResponse_httpStatus,
    describeModelExplainabilityJobDefinitionResponse_jobDefinitionArn,
    describeModelExplainabilityJobDefinitionResponse_jobDefinitionName,
    describeModelExplainabilityJobDefinitionResponse_creationTime,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityAppSpecification,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobInput,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobOutputConfig,
    describeModelExplainabilityJobDefinitionResponse_jobResources,
    describeModelExplainabilityJobDefinitionResponse_roleArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeModelExplainabilityJobDefinition' smart constructor.
data DescribeModelExplainabilityJobDefinition = DescribeModelExplainabilityJobDefinition'
  { -- | The name of the model explainability job definition. The name must be
    -- unique within an AWS Region in the AWS account.
    jobDefinitionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeModelExplainabilityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinitionName', 'describeModelExplainabilityJobDefinition_jobDefinitionName' - The name of the model explainability job definition. The name must be
-- unique within an AWS Region in the AWS account.
newDescribeModelExplainabilityJobDefinition ::
  -- | 'jobDefinitionName'
  Core.Text ->
  DescribeModelExplainabilityJobDefinition
newDescribeModelExplainabilityJobDefinition
  pJobDefinitionName_ =
    DescribeModelExplainabilityJobDefinition'
      { jobDefinitionName =
          pJobDefinitionName_
      }

-- | The name of the model explainability job definition. The name must be
-- unique within an AWS Region in the AWS account.
describeModelExplainabilityJobDefinition_jobDefinitionName :: Lens.Lens' DescribeModelExplainabilityJobDefinition Core.Text
describeModelExplainabilityJobDefinition_jobDefinitionName = Lens.lens (\DescribeModelExplainabilityJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeModelExplainabilityJobDefinition' {} a -> s {jobDefinitionName = a} :: DescribeModelExplainabilityJobDefinition)

instance
  Core.AWSRequest
    DescribeModelExplainabilityJobDefinition
  where
  type
    AWSResponse
      DescribeModelExplainabilityJobDefinition =
      DescribeModelExplainabilityJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelExplainabilityJobDefinitionResponse'
            Core.<$> (x Core..?> "NetworkConfig")
              Core.<*> (x Core..?> "ModelExplainabilityBaselineConfig")
              Core.<*> (x Core..?> "StoppingCondition")
              Core.<*> (Core.pure (Core.fromEnum s))
              Core.<*> (x Core..:> "JobDefinitionArn")
              Core.<*> (x Core..:> "JobDefinitionName")
              Core.<*> (x Core..:> "CreationTime")
              Core.<*> (x Core..:> "ModelExplainabilityAppSpecification")
              Core.<*> (x Core..:> "ModelExplainabilityJobInput")
              Core.<*> (x Core..:> "ModelExplainabilityJobOutputConfig")
              Core.<*> (x Core..:> "JobResources")
              Core.<*> (x Core..:> "RoleArn")
      )

instance
  Core.Hashable
    DescribeModelExplainabilityJobDefinition

instance
  Core.NFData
    DescribeModelExplainabilityJobDefinition

instance
  Core.ToHeaders
    DescribeModelExplainabilityJobDefinition
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeModelExplainabilityJobDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeModelExplainabilityJobDefinition
  where
  toJSON DescribeModelExplainabilityJobDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("JobDefinitionName" Core..= jobDefinitionName)
          ]
      )

instance
  Core.ToPath
    DescribeModelExplainabilityJobDefinition
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeModelExplainabilityJobDefinition
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeModelExplainabilityJobDefinitionResponse' smart constructor.
data DescribeModelExplainabilityJobDefinitionResponse = DescribeModelExplainabilityJobDefinitionResponse'
  { -- | Networking options for a model explainability job.
    networkConfig :: Core.Maybe MonitoringNetworkConfig,
    -- | The baseline configuration for a model explainability job.
    modelExplainabilityBaselineConfig :: Core.Maybe ModelExplainabilityBaselineConfig,
    stoppingCondition :: Core.Maybe MonitoringStoppingCondition,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the model explainability job.
    jobDefinitionArn :: Core.Text,
    -- | The name of the explainability job definition. The name must be unique
    -- within an AWS Region in the AWS account.
    jobDefinitionName :: Core.Text,
    -- | The time at which the model explainability job was created.
    creationTime :: Core.POSIX,
    -- | Configures the model explainability job to run a specified Docker
    -- container image.
    modelExplainabilityAppSpecification :: ModelExplainabilityAppSpecification,
    -- | Inputs for the model explainability job.
    modelExplainabilityJobInput :: ModelExplainabilityJobInput,
    modelExplainabilityJobOutputConfig :: MonitoringOutputConfig,
    jobResources :: MonitoringResources,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that has read permission to the input data location and write
    -- permission to the output data location in Amazon S3.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeModelExplainabilityJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfig', 'describeModelExplainabilityJobDefinitionResponse_networkConfig' - Networking options for a model explainability job.
--
-- 'modelExplainabilityBaselineConfig', 'describeModelExplainabilityJobDefinitionResponse_modelExplainabilityBaselineConfig' - The baseline configuration for a model explainability job.
--
-- 'stoppingCondition', 'describeModelExplainabilityJobDefinitionResponse_stoppingCondition' - Undocumented member.
--
-- 'httpStatus', 'describeModelExplainabilityJobDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionArn', 'describeModelExplainabilityJobDefinitionResponse_jobDefinitionArn' - The Amazon Resource Name (ARN) of the model explainability job.
--
-- 'jobDefinitionName', 'describeModelExplainabilityJobDefinitionResponse_jobDefinitionName' - The name of the explainability job definition. The name must be unique
-- within an AWS Region in the AWS account.
--
-- 'creationTime', 'describeModelExplainabilityJobDefinitionResponse_creationTime' - The time at which the model explainability job was created.
--
-- 'modelExplainabilityAppSpecification', 'describeModelExplainabilityJobDefinitionResponse_modelExplainabilityAppSpecification' - Configures the model explainability job to run a specified Docker
-- container image.
--
-- 'modelExplainabilityJobInput', 'describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobInput' - Inputs for the model explainability job.
--
-- 'modelExplainabilityJobOutputConfig', 'describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobOutputConfig' - Undocumented member.
--
-- 'jobResources', 'describeModelExplainabilityJobDefinitionResponse_jobResources' - Undocumented member.
--
-- 'roleArn', 'describeModelExplainabilityJobDefinitionResponse_roleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that has read permission to the input data location and write
-- permission to the output data location in Amazon S3.
newDescribeModelExplainabilityJobDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'jobDefinitionArn'
  Core.Text ->
  -- | 'jobDefinitionName'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'modelExplainabilityAppSpecification'
  ModelExplainabilityAppSpecification ->
  -- | 'modelExplainabilityJobInput'
  ModelExplainabilityJobInput ->
  -- | 'modelExplainabilityJobOutputConfig'
  MonitoringOutputConfig ->
  -- | 'jobResources'
  MonitoringResources ->
  -- | 'roleArn'
  Core.Text ->
  DescribeModelExplainabilityJobDefinitionResponse
newDescribeModelExplainabilityJobDefinitionResponse
  pHttpStatus_
  pJobDefinitionArn_
  pJobDefinitionName_
  pCreationTime_
  pModelExplainabilityAppSpecification_
  pModelExplainabilityJobInput_
  pModelExplainabilityJobOutputConfig_
  pJobResources_
  pRoleArn_ =
    DescribeModelExplainabilityJobDefinitionResponse'
      { networkConfig =
          Core.Nothing,
        modelExplainabilityBaselineConfig =
          Core.Nothing,
        stoppingCondition =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        jobDefinitionArn =
          pJobDefinitionArn_,
        jobDefinitionName =
          pJobDefinitionName_,
        creationTime =
          Core._Time
            Lens.# pCreationTime_,
        modelExplainabilityAppSpecification =
          pModelExplainabilityAppSpecification_,
        modelExplainabilityJobInput =
          pModelExplainabilityJobInput_,
        modelExplainabilityJobOutputConfig =
          pModelExplainabilityJobOutputConfig_,
        jobResources =
          pJobResources_,
        roleArn = pRoleArn_
      }

-- | Networking options for a model explainability job.
describeModelExplainabilityJobDefinitionResponse_networkConfig :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse (Core.Maybe MonitoringNetworkConfig)
describeModelExplainabilityJobDefinitionResponse_networkConfig = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {networkConfig} -> networkConfig) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {networkConfig = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | The baseline configuration for a model explainability job.
describeModelExplainabilityJobDefinitionResponse_modelExplainabilityBaselineConfig :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse (Core.Maybe ModelExplainabilityBaselineConfig)
describeModelExplainabilityJobDefinitionResponse_modelExplainabilityBaselineConfig = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {modelExplainabilityBaselineConfig} -> modelExplainabilityBaselineConfig) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {modelExplainabilityBaselineConfig = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | Undocumented member.
describeModelExplainabilityJobDefinitionResponse_stoppingCondition :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse (Core.Maybe MonitoringStoppingCondition)
describeModelExplainabilityJobDefinitionResponse_stoppingCondition = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {stoppingCondition = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | The response's http status code.
describeModelExplainabilityJobDefinitionResponse_httpStatus :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse Core.Int
describeModelExplainabilityJobDefinitionResponse_httpStatus = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {httpStatus = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the model explainability job.
describeModelExplainabilityJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse Core.Text
describeModelExplainabilityJobDefinitionResponse_jobDefinitionArn = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | The name of the explainability job definition. The name must be unique
-- within an AWS Region in the AWS account.
describeModelExplainabilityJobDefinitionResponse_jobDefinitionName :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse Core.Text
describeModelExplainabilityJobDefinitionResponse_jobDefinitionName = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {jobDefinitionName = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | The time at which the model explainability job was created.
describeModelExplainabilityJobDefinitionResponse_creationTime :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse Core.UTCTime
describeModelExplainabilityJobDefinitionResponse_creationTime = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {creationTime} -> creationTime) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {creationTime = a} :: DescribeModelExplainabilityJobDefinitionResponse) Core.. Core._Time

-- | Configures the model explainability job to run a specified Docker
-- container image.
describeModelExplainabilityJobDefinitionResponse_modelExplainabilityAppSpecification :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse ModelExplainabilityAppSpecification
describeModelExplainabilityJobDefinitionResponse_modelExplainabilityAppSpecification = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {modelExplainabilityAppSpecification} -> modelExplainabilityAppSpecification) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {modelExplainabilityAppSpecification = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | Inputs for the model explainability job.
describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobInput :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse ModelExplainabilityJobInput
describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobInput = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {modelExplainabilityJobInput} -> modelExplainabilityJobInput) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {modelExplainabilityJobInput = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | Undocumented member.
describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobOutputConfig :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse MonitoringOutputConfig
describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobOutputConfig = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {modelExplainabilityJobOutputConfig} -> modelExplainabilityJobOutputConfig) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {modelExplainabilityJobOutputConfig = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | Undocumented member.
describeModelExplainabilityJobDefinitionResponse_jobResources :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse MonitoringResources
describeModelExplainabilityJobDefinitionResponse_jobResources = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {jobResources} -> jobResources) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {jobResources = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that has read permission to the input data location and write
-- permission to the output data location in Amazon S3.
describeModelExplainabilityJobDefinitionResponse_roleArn :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse Core.Text
describeModelExplainabilityJobDefinitionResponse_roleArn = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {roleArn} -> roleArn) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {roleArn = a} :: DescribeModelExplainabilityJobDefinitionResponse)

instance
  Core.NFData
    DescribeModelExplainabilityJobDefinitionResponse
