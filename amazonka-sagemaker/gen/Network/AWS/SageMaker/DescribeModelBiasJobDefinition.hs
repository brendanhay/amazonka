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
-- Module      : Network.AWS.SageMaker.DescribeModelBiasJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a model bias job definition.
module Network.AWS.SageMaker.DescribeModelBiasJobDefinition
  ( -- * Creating a Request
    DescribeModelBiasJobDefinition (..),
    newDescribeModelBiasJobDefinition,

    -- * Request Lenses
    describeModelBiasJobDefinition_jobDefinitionName,

    -- * Destructuring the Response
    DescribeModelBiasJobDefinitionResponse (..),
    newDescribeModelBiasJobDefinitionResponse,

    -- * Response Lenses
    describeModelBiasJobDefinitionResponse_networkConfig,
    describeModelBiasJobDefinitionResponse_modelBiasBaselineConfig,
    describeModelBiasJobDefinitionResponse_stoppingCondition,
    describeModelBiasJobDefinitionResponse_httpStatus,
    describeModelBiasJobDefinitionResponse_jobDefinitionArn,
    describeModelBiasJobDefinitionResponse_jobDefinitionName,
    describeModelBiasJobDefinitionResponse_creationTime,
    describeModelBiasJobDefinitionResponse_modelBiasAppSpecification,
    describeModelBiasJobDefinitionResponse_modelBiasJobInput,
    describeModelBiasJobDefinitionResponse_modelBiasJobOutputConfig,
    describeModelBiasJobDefinitionResponse_jobResources,
    describeModelBiasJobDefinitionResponse_roleArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeModelBiasJobDefinition' smart constructor.
data DescribeModelBiasJobDefinition = DescribeModelBiasJobDefinition'
  { -- | The name of the model bias job definition. The name must be unique
    -- within an AWS Region in the AWS account.
    jobDefinitionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeModelBiasJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinitionName', 'describeModelBiasJobDefinition_jobDefinitionName' - The name of the model bias job definition. The name must be unique
-- within an AWS Region in the AWS account.
newDescribeModelBiasJobDefinition ::
  -- | 'jobDefinitionName'
  Core.Text ->
  DescribeModelBiasJobDefinition
newDescribeModelBiasJobDefinition pJobDefinitionName_ =
  DescribeModelBiasJobDefinition'
    { jobDefinitionName =
        pJobDefinitionName_
    }

-- | The name of the model bias job definition. The name must be unique
-- within an AWS Region in the AWS account.
describeModelBiasJobDefinition_jobDefinitionName :: Lens.Lens' DescribeModelBiasJobDefinition Core.Text
describeModelBiasJobDefinition_jobDefinitionName = Lens.lens (\DescribeModelBiasJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeModelBiasJobDefinition' {} a -> s {jobDefinitionName = a} :: DescribeModelBiasJobDefinition)

instance
  Core.AWSRequest
    DescribeModelBiasJobDefinition
  where
  type
    AWSResponse DescribeModelBiasJobDefinition =
      DescribeModelBiasJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelBiasJobDefinitionResponse'
            Core.<$> (x Core..?> "NetworkConfig")
            Core.<*> (x Core..?> "ModelBiasBaselineConfig")
            Core.<*> (x Core..?> "StoppingCondition")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "JobDefinitionArn")
            Core.<*> (x Core..:> "JobDefinitionName")
            Core.<*> (x Core..:> "CreationTime")
            Core.<*> (x Core..:> "ModelBiasAppSpecification")
            Core.<*> (x Core..:> "ModelBiasJobInput")
            Core.<*> (x Core..:> "ModelBiasJobOutputConfig")
            Core.<*> (x Core..:> "JobResources")
            Core.<*> (x Core..:> "RoleArn")
      )

instance Core.Hashable DescribeModelBiasJobDefinition

instance Core.NFData DescribeModelBiasJobDefinition

instance
  Core.ToHeaders
    DescribeModelBiasJobDefinition
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeModelBiasJobDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeModelBiasJobDefinition where
  toJSON DescribeModelBiasJobDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("JobDefinitionName" Core..= jobDefinitionName)
          ]
      )

instance Core.ToPath DescribeModelBiasJobDefinition where
  toPath = Core.const "/"

instance Core.ToQuery DescribeModelBiasJobDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeModelBiasJobDefinitionResponse' smart constructor.
data DescribeModelBiasJobDefinitionResponse = DescribeModelBiasJobDefinitionResponse'
  { -- | Networking options for a model bias job.
    networkConfig :: Core.Maybe MonitoringNetworkConfig,
    -- | The baseline configuration for a model bias job.
    modelBiasBaselineConfig :: Core.Maybe ModelBiasBaselineConfig,
    stoppingCondition :: Core.Maybe MonitoringStoppingCondition,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the model bias job.
    jobDefinitionArn :: Core.Text,
    -- | The name of the bias job definition. The name must be unique within an
    -- AWS Region in the AWS account.
    jobDefinitionName :: Core.Text,
    -- | The time at which the model bias job was created.
    creationTime :: Core.POSIX,
    -- | Configures the model bias job to run a specified Docker container image.
    modelBiasAppSpecification :: ModelBiasAppSpecification,
    -- | Inputs for the model bias job.
    modelBiasJobInput :: ModelBiasJobInput,
    modelBiasJobOutputConfig :: MonitoringOutputConfig,
    jobResources :: MonitoringResources,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that has read permission to the input data location and write
    -- permission to the output data location in Amazon S3.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeModelBiasJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfig', 'describeModelBiasJobDefinitionResponse_networkConfig' - Networking options for a model bias job.
--
-- 'modelBiasBaselineConfig', 'describeModelBiasJobDefinitionResponse_modelBiasBaselineConfig' - The baseline configuration for a model bias job.
--
-- 'stoppingCondition', 'describeModelBiasJobDefinitionResponse_stoppingCondition' - Undocumented member.
--
-- 'httpStatus', 'describeModelBiasJobDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionArn', 'describeModelBiasJobDefinitionResponse_jobDefinitionArn' - The Amazon Resource Name (ARN) of the model bias job.
--
-- 'jobDefinitionName', 'describeModelBiasJobDefinitionResponse_jobDefinitionName' - The name of the bias job definition. The name must be unique within an
-- AWS Region in the AWS account.
--
-- 'creationTime', 'describeModelBiasJobDefinitionResponse_creationTime' - The time at which the model bias job was created.
--
-- 'modelBiasAppSpecification', 'describeModelBiasJobDefinitionResponse_modelBiasAppSpecification' - Configures the model bias job to run a specified Docker container image.
--
-- 'modelBiasJobInput', 'describeModelBiasJobDefinitionResponse_modelBiasJobInput' - Inputs for the model bias job.
--
-- 'modelBiasJobOutputConfig', 'describeModelBiasJobDefinitionResponse_modelBiasJobOutputConfig' - Undocumented member.
--
-- 'jobResources', 'describeModelBiasJobDefinitionResponse_jobResources' - Undocumented member.
--
-- 'roleArn', 'describeModelBiasJobDefinitionResponse_roleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that has read permission to the input data location and write
-- permission to the output data location in Amazon S3.
newDescribeModelBiasJobDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'jobDefinitionArn'
  Core.Text ->
  -- | 'jobDefinitionName'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'modelBiasAppSpecification'
  ModelBiasAppSpecification ->
  -- | 'modelBiasJobInput'
  ModelBiasJobInput ->
  -- | 'modelBiasJobOutputConfig'
  MonitoringOutputConfig ->
  -- | 'jobResources'
  MonitoringResources ->
  -- | 'roleArn'
  Core.Text ->
  DescribeModelBiasJobDefinitionResponse
newDescribeModelBiasJobDefinitionResponse
  pHttpStatus_
  pJobDefinitionArn_
  pJobDefinitionName_
  pCreationTime_
  pModelBiasAppSpecification_
  pModelBiasJobInput_
  pModelBiasJobOutputConfig_
  pJobResources_
  pRoleArn_ =
    DescribeModelBiasJobDefinitionResponse'
      { networkConfig =
          Core.Nothing,
        modelBiasBaselineConfig =
          Core.Nothing,
        stoppingCondition = Core.Nothing,
        httpStatus = pHttpStatus_,
        jobDefinitionArn =
          pJobDefinitionArn_,
        jobDefinitionName =
          pJobDefinitionName_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        modelBiasAppSpecification =
          pModelBiasAppSpecification_,
        modelBiasJobInput =
          pModelBiasJobInput_,
        modelBiasJobOutputConfig =
          pModelBiasJobOutputConfig_,
        jobResources = pJobResources_,
        roleArn = pRoleArn_
      }

-- | Networking options for a model bias job.
describeModelBiasJobDefinitionResponse_networkConfig :: Lens.Lens' DescribeModelBiasJobDefinitionResponse (Core.Maybe MonitoringNetworkConfig)
describeModelBiasJobDefinitionResponse_networkConfig = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {networkConfig} -> networkConfig) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {networkConfig = a} :: DescribeModelBiasJobDefinitionResponse)

-- | The baseline configuration for a model bias job.
describeModelBiasJobDefinitionResponse_modelBiasBaselineConfig :: Lens.Lens' DescribeModelBiasJobDefinitionResponse (Core.Maybe ModelBiasBaselineConfig)
describeModelBiasJobDefinitionResponse_modelBiasBaselineConfig = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {modelBiasBaselineConfig} -> modelBiasBaselineConfig) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {modelBiasBaselineConfig = a} :: DescribeModelBiasJobDefinitionResponse)

-- | Undocumented member.
describeModelBiasJobDefinitionResponse_stoppingCondition :: Lens.Lens' DescribeModelBiasJobDefinitionResponse (Core.Maybe MonitoringStoppingCondition)
describeModelBiasJobDefinitionResponse_stoppingCondition = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {stoppingCondition = a} :: DescribeModelBiasJobDefinitionResponse)

-- | The response's http status code.
describeModelBiasJobDefinitionResponse_httpStatus :: Lens.Lens' DescribeModelBiasJobDefinitionResponse Core.Int
describeModelBiasJobDefinitionResponse_httpStatus = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {httpStatus = a} :: DescribeModelBiasJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the model bias job.
describeModelBiasJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' DescribeModelBiasJobDefinitionResponse Core.Text
describeModelBiasJobDefinitionResponse_jobDefinitionArn = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: DescribeModelBiasJobDefinitionResponse)

-- | The name of the bias job definition. The name must be unique within an
-- AWS Region in the AWS account.
describeModelBiasJobDefinitionResponse_jobDefinitionName :: Lens.Lens' DescribeModelBiasJobDefinitionResponse Core.Text
describeModelBiasJobDefinitionResponse_jobDefinitionName = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {jobDefinitionName = a} :: DescribeModelBiasJobDefinitionResponse)

-- | The time at which the model bias job was created.
describeModelBiasJobDefinitionResponse_creationTime :: Lens.Lens' DescribeModelBiasJobDefinitionResponse Core.UTCTime
describeModelBiasJobDefinitionResponse_creationTime = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {creationTime} -> creationTime) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {creationTime = a} :: DescribeModelBiasJobDefinitionResponse) Core.. Core._Time

-- | Configures the model bias job to run a specified Docker container image.
describeModelBiasJobDefinitionResponse_modelBiasAppSpecification :: Lens.Lens' DescribeModelBiasJobDefinitionResponse ModelBiasAppSpecification
describeModelBiasJobDefinitionResponse_modelBiasAppSpecification = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {modelBiasAppSpecification} -> modelBiasAppSpecification) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {modelBiasAppSpecification = a} :: DescribeModelBiasJobDefinitionResponse)

-- | Inputs for the model bias job.
describeModelBiasJobDefinitionResponse_modelBiasJobInput :: Lens.Lens' DescribeModelBiasJobDefinitionResponse ModelBiasJobInput
describeModelBiasJobDefinitionResponse_modelBiasJobInput = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {modelBiasJobInput} -> modelBiasJobInput) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {modelBiasJobInput = a} :: DescribeModelBiasJobDefinitionResponse)

-- | Undocumented member.
describeModelBiasJobDefinitionResponse_modelBiasJobOutputConfig :: Lens.Lens' DescribeModelBiasJobDefinitionResponse MonitoringOutputConfig
describeModelBiasJobDefinitionResponse_modelBiasJobOutputConfig = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {modelBiasJobOutputConfig} -> modelBiasJobOutputConfig) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {modelBiasJobOutputConfig = a} :: DescribeModelBiasJobDefinitionResponse)

-- | Undocumented member.
describeModelBiasJobDefinitionResponse_jobResources :: Lens.Lens' DescribeModelBiasJobDefinitionResponse MonitoringResources
describeModelBiasJobDefinitionResponse_jobResources = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {jobResources} -> jobResources) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {jobResources = a} :: DescribeModelBiasJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that has read permission to the input data location and write
-- permission to the output data location in Amazon S3.
describeModelBiasJobDefinitionResponse_roleArn :: Lens.Lens' DescribeModelBiasJobDefinitionResponse Core.Text
describeModelBiasJobDefinitionResponse_roleArn = Lens.lens (\DescribeModelBiasJobDefinitionResponse' {roleArn} -> roleArn) (\s@DescribeModelBiasJobDefinitionResponse' {} a -> s {roleArn = a} :: DescribeModelBiasJobDefinitionResponse)

instance
  Core.NFData
    DescribeModelBiasJobDefinitionResponse
