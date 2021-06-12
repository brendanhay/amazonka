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
-- Module      : Network.AWS.SageMaker.DescribeModelQualityJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a model quality job definition.
module Network.AWS.SageMaker.DescribeModelQualityJobDefinition
  ( -- * Creating a Request
    DescribeModelQualityJobDefinition (..),
    newDescribeModelQualityJobDefinition,

    -- * Request Lenses
    describeModelQualityJobDefinition_jobDefinitionName,

    -- * Destructuring the Response
    DescribeModelQualityJobDefinitionResponse (..),
    newDescribeModelQualityJobDefinitionResponse,

    -- * Response Lenses
    describeModelQualityJobDefinitionResponse_networkConfig,
    describeModelQualityJobDefinitionResponse_modelQualityBaselineConfig,
    describeModelQualityJobDefinitionResponse_stoppingCondition,
    describeModelQualityJobDefinitionResponse_httpStatus,
    describeModelQualityJobDefinitionResponse_jobDefinitionArn,
    describeModelQualityJobDefinitionResponse_jobDefinitionName,
    describeModelQualityJobDefinitionResponse_creationTime,
    describeModelQualityJobDefinitionResponse_modelQualityAppSpecification,
    describeModelQualityJobDefinitionResponse_modelQualityJobInput,
    describeModelQualityJobDefinitionResponse_modelQualityJobOutputConfig,
    describeModelQualityJobDefinitionResponse_jobResources,
    describeModelQualityJobDefinitionResponse_roleArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeModelQualityJobDefinition' smart constructor.
data DescribeModelQualityJobDefinition = DescribeModelQualityJobDefinition'
  { -- | The name of the model quality job. The name must be unique within an AWS
    -- Region in the AWS account.
    jobDefinitionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeModelQualityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinitionName', 'describeModelQualityJobDefinition_jobDefinitionName' - The name of the model quality job. The name must be unique within an AWS
-- Region in the AWS account.
newDescribeModelQualityJobDefinition ::
  -- | 'jobDefinitionName'
  Core.Text ->
  DescribeModelQualityJobDefinition
newDescribeModelQualityJobDefinition
  pJobDefinitionName_ =
    DescribeModelQualityJobDefinition'
      { jobDefinitionName =
          pJobDefinitionName_
      }

-- | The name of the model quality job. The name must be unique within an AWS
-- Region in the AWS account.
describeModelQualityJobDefinition_jobDefinitionName :: Lens.Lens' DescribeModelQualityJobDefinition Core.Text
describeModelQualityJobDefinition_jobDefinitionName = Lens.lens (\DescribeModelQualityJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeModelQualityJobDefinition' {} a -> s {jobDefinitionName = a} :: DescribeModelQualityJobDefinition)

instance
  Core.AWSRequest
    DescribeModelQualityJobDefinition
  where
  type
    AWSResponse DescribeModelQualityJobDefinition =
      DescribeModelQualityJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelQualityJobDefinitionResponse'
            Core.<$> (x Core..?> "NetworkConfig")
            Core.<*> (x Core..?> "ModelQualityBaselineConfig")
            Core.<*> (x Core..?> "StoppingCondition")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "JobDefinitionArn")
            Core.<*> (x Core..:> "JobDefinitionName")
            Core.<*> (x Core..:> "CreationTime")
            Core.<*> (x Core..:> "ModelQualityAppSpecification")
            Core.<*> (x Core..:> "ModelQualityJobInput")
            Core.<*> (x Core..:> "ModelQualityJobOutputConfig")
            Core.<*> (x Core..:> "JobResources")
            Core.<*> (x Core..:> "RoleArn")
      )

instance
  Core.Hashable
    DescribeModelQualityJobDefinition

instance
  Core.NFData
    DescribeModelQualityJobDefinition

instance
  Core.ToHeaders
    DescribeModelQualityJobDefinition
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeModelQualityJobDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeModelQualityJobDefinition
  where
  toJSON DescribeModelQualityJobDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("JobDefinitionName" Core..= jobDefinitionName)
          ]
      )

instance
  Core.ToPath
    DescribeModelQualityJobDefinition
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeModelQualityJobDefinition
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeModelQualityJobDefinitionResponse' smart constructor.
data DescribeModelQualityJobDefinitionResponse = DescribeModelQualityJobDefinitionResponse'
  { -- | Networking options for a model quality job.
    networkConfig :: Core.Maybe MonitoringNetworkConfig,
    -- | The baseline configuration for a model quality job.
    modelQualityBaselineConfig :: Core.Maybe ModelQualityBaselineConfig,
    stoppingCondition :: Core.Maybe MonitoringStoppingCondition,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the model quality job.
    jobDefinitionArn :: Core.Text,
    -- | The name of the quality job definition. The name must be unique within
    -- an AWS Region in the AWS account.
    jobDefinitionName :: Core.Text,
    -- | The time at which the model quality job was created.
    creationTime :: Core.POSIX,
    -- | Configures the model quality job to run a specified Docker container
    -- image.
    modelQualityAppSpecification :: ModelQualityAppSpecification,
    -- | Inputs for the model quality job.
    modelQualityJobInput :: ModelQualityJobInput,
    modelQualityJobOutputConfig :: MonitoringOutputConfig,
    jobResources :: MonitoringResources,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeModelQualityJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfig', 'describeModelQualityJobDefinitionResponse_networkConfig' - Networking options for a model quality job.
--
-- 'modelQualityBaselineConfig', 'describeModelQualityJobDefinitionResponse_modelQualityBaselineConfig' - The baseline configuration for a model quality job.
--
-- 'stoppingCondition', 'describeModelQualityJobDefinitionResponse_stoppingCondition' - Undocumented member.
--
-- 'httpStatus', 'describeModelQualityJobDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionArn', 'describeModelQualityJobDefinitionResponse_jobDefinitionArn' - The Amazon Resource Name (ARN) of the model quality job.
--
-- 'jobDefinitionName', 'describeModelQualityJobDefinitionResponse_jobDefinitionName' - The name of the quality job definition. The name must be unique within
-- an AWS Region in the AWS account.
--
-- 'creationTime', 'describeModelQualityJobDefinitionResponse_creationTime' - The time at which the model quality job was created.
--
-- 'modelQualityAppSpecification', 'describeModelQualityJobDefinitionResponse_modelQualityAppSpecification' - Configures the model quality job to run a specified Docker container
-- image.
--
-- 'modelQualityJobInput', 'describeModelQualityJobDefinitionResponse_modelQualityJobInput' - Inputs for the model quality job.
--
-- 'modelQualityJobOutputConfig', 'describeModelQualityJobDefinitionResponse_modelQualityJobOutputConfig' - Undocumented member.
--
-- 'jobResources', 'describeModelQualityJobDefinitionResponse_jobResources' - Undocumented member.
--
-- 'roleArn', 'describeModelQualityJobDefinitionResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
newDescribeModelQualityJobDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'jobDefinitionArn'
  Core.Text ->
  -- | 'jobDefinitionName'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'modelQualityAppSpecification'
  ModelQualityAppSpecification ->
  -- | 'modelQualityJobInput'
  ModelQualityJobInput ->
  -- | 'modelQualityJobOutputConfig'
  MonitoringOutputConfig ->
  -- | 'jobResources'
  MonitoringResources ->
  -- | 'roleArn'
  Core.Text ->
  DescribeModelQualityJobDefinitionResponse
newDescribeModelQualityJobDefinitionResponse
  pHttpStatus_
  pJobDefinitionArn_
  pJobDefinitionName_
  pCreationTime_
  pModelQualityAppSpecification_
  pModelQualityJobInput_
  pModelQualityJobOutputConfig_
  pJobResources_
  pRoleArn_ =
    DescribeModelQualityJobDefinitionResponse'
      { networkConfig =
          Core.Nothing,
        modelQualityBaselineConfig =
          Core.Nothing,
        stoppingCondition = Core.Nothing,
        httpStatus = pHttpStatus_,
        jobDefinitionArn =
          pJobDefinitionArn_,
        jobDefinitionName =
          pJobDefinitionName_,
        creationTime =
          Core._Time
            Lens.# pCreationTime_,
        modelQualityAppSpecification =
          pModelQualityAppSpecification_,
        modelQualityJobInput =
          pModelQualityJobInput_,
        modelQualityJobOutputConfig =
          pModelQualityJobOutputConfig_,
        jobResources = pJobResources_,
        roleArn = pRoleArn_
      }

-- | Networking options for a model quality job.
describeModelQualityJobDefinitionResponse_networkConfig :: Lens.Lens' DescribeModelQualityJobDefinitionResponse (Core.Maybe MonitoringNetworkConfig)
describeModelQualityJobDefinitionResponse_networkConfig = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {networkConfig} -> networkConfig) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {networkConfig = a} :: DescribeModelQualityJobDefinitionResponse)

-- | The baseline configuration for a model quality job.
describeModelQualityJobDefinitionResponse_modelQualityBaselineConfig :: Lens.Lens' DescribeModelQualityJobDefinitionResponse (Core.Maybe ModelQualityBaselineConfig)
describeModelQualityJobDefinitionResponse_modelQualityBaselineConfig = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {modelQualityBaselineConfig} -> modelQualityBaselineConfig) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {modelQualityBaselineConfig = a} :: DescribeModelQualityJobDefinitionResponse)

-- | Undocumented member.
describeModelQualityJobDefinitionResponse_stoppingCondition :: Lens.Lens' DescribeModelQualityJobDefinitionResponse (Core.Maybe MonitoringStoppingCondition)
describeModelQualityJobDefinitionResponse_stoppingCondition = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {stoppingCondition = a} :: DescribeModelQualityJobDefinitionResponse)

-- | The response's http status code.
describeModelQualityJobDefinitionResponse_httpStatus :: Lens.Lens' DescribeModelQualityJobDefinitionResponse Core.Int
describeModelQualityJobDefinitionResponse_httpStatus = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {httpStatus = a} :: DescribeModelQualityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the model quality job.
describeModelQualityJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' DescribeModelQualityJobDefinitionResponse Core.Text
describeModelQualityJobDefinitionResponse_jobDefinitionArn = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: DescribeModelQualityJobDefinitionResponse)

-- | The name of the quality job definition. The name must be unique within
-- an AWS Region in the AWS account.
describeModelQualityJobDefinitionResponse_jobDefinitionName :: Lens.Lens' DescribeModelQualityJobDefinitionResponse Core.Text
describeModelQualityJobDefinitionResponse_jobDefinitionName = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {jobDefinitionName = a} :: DescribeModelQualityJobDefinitionResponse)

-- | The time at which the model quality job was created.
describeModelQualityJobDefinitionResponse_creationTime :: Lens.Lens' DescribeModelQualityJobDefinitionResponse Core.UTCTime
describeModelQualityJobDefinitionResponse_creationTime = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {creationTime} -> creationTime) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {creationTime = a} :: DescribeModelQualityJobDefinitionResponse) Core.. Core._Time

-- | Configures the model quality job to run a specified Docker container
-- image.
describeModelQualityJobDefinitionResponse_modelQualityAppSpecification :: Lens.Lens' DescribeModelQualityJobDefinitionResponse ModelQualityAppSpecification
describeModelQualityJobDefinitionResponse_modelQualityAppSpecification = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {modelQualityAppSpecification} -> modelQualityAppSpecification) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {modelQualityAppSpecification = a} :: DescribeModelQualityJobDefinitionResponse)

-- | Inputs for the model quality job.
describeModelQualityJobDefinitionResponse_modelQualityJobInput :: Lens.Lens' DescribeModelQualityJobDefinitionResponse ModelQualityJobInput
describeModelQualityJobDefinitionResponse_modelQualityJobInput = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {modelQualityJobInput} -> modelQualityJobInput) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {modelQualityJobInput = a} :: DescribeModelQualityJobDefinitionResponse)

-- | Undocumented member.
describeModelQualityJobDefinitionResponse_modelQualityJobOutputConfig :: Lens.Lens' DescribeModelQualityJobDefinitionResponse MonitoringOutputConfig
describeModelQualityJobDefinitionResponse_modelQualityJobOutputConfig = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {modelQualityJobOutputConfig} -> modelQualityJobOutputConfig) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {modelQualityJobOutputConfig = a} :: DescribeModelQualityJobDefinitionResponse)

-- | Undocumented member.
describeModelQualityJobDefinitionResponse_jobResources :: Lens.Lens' DescribeModelQualityJobDefinitionResponse MonitoringResources
describeModelQualityJobDefinitionResponse_jobResources = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {jobResources} -> jobResources) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {jobResources = a} :: DescribeModelQualityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
describeModelQualityJobDefinitionResponse_roleArn :: Lens.Lens' DescribeModelQualityJobDefinitionResponse Core.Text
describeModelQualityJobDefinitionResponse_roleArn = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {roleArn} -> roleArn) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {roleArn = a} :: DescribeModelQualityJobDefinitionResponse)

instance
  Core.NFData
    DescribeModelQualityJobDefinitionResponse
