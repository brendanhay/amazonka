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
-- Module      : Network.AWS.SageMaker.CreateModelQualityJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a definition for a job that monitors model quality and drift.
-- For information about model monitor, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
module Network.AWS.SageMaker.CreateModelQualityJobDefinition
  ( -- * Creating a Request
    CreateModelQualityJobDefinition (..),
    newCreateModelQualityJobDefinition,

    -- * Request Lenses
    createModelQualityJobDefinition_networkConfig,
    createModelQualityJobDefinition_modelQualityBaselineConfig,
    createModelQualityJobDefinition_tags,
    createModelQualityJobDefinition_stoppingCondition,
    createModelQualityJobDefinition_jobDefinitionName,
    createModelQualityJobDefinition_modelQualityAppSpecification,
    createModelQualityJobDefinition_modelQualityJobInput,
    createModelQualityJobDefinition_modelQualityJobOutputConfig,
    createModelQualityJobDefinition_jobResources,
    createModelQualityJobDefinition_roleArn,

    -- * Destructuring the Response
    CreateModelQualityJobDefinitionResponse (..),
    newCreateModelQualityJobDefinitionResponse,

    -- * Response Lenses
    createModelQualityJobDefinitionResponse_httpStatus,
    createModelQualityJobDefinitionResponse_jobDefinitionArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateModelQualityJobDefinition' smart constructor.
data CreateModelQualityJobDefinition = CreateModelQualityJobDefinition'
  { -- | Specifies the network configuration for the monitoring job.
    networkConfig :: Core.Maybe MonitoringNetworkConfig,
    -- | Specifies the constraints and baselines for the monitoring job.
    modelQualityBaselineConfig :: Core.Maybe ModelQualityBaselineConfig,
    -- | (Optional) An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/.
    tags :: Core.Maybe [Tag],
    stoppingCondition :: Core.Maybe MonitoringStoppingCondition,
    -- | The name of the monitoring job definition.
    jobDefinitionName :: Core.Text,
    -- | The container that runs the monitoring job.
    modelQualityAppSpecification :: ModelQualityAppSpecification,
    -- | A list of the inputs that are monitored. Currently endpoints are
    -- supported.
    modelQualityJobInput :: ModelQualityJobInput,
    modelQualityJobOutputConfig :: MonitoringOutputConfig,
    jobResources :: MonitoringResources,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateModelQualityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfig', 'createModelQualityJobDefinition_networkConfig' - Specifies the network configuration for the monitoring job.
--
-- 'modelQualityBaselineConfig', 'createModelQualityJobDefinition_modelQualityBaselineConfig' - Specifies the constraints and baselines for the monitoring job.
--
-- 'tags', 'createModelQualityJobDefinition_tags' - (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- 'stoppingCondition', 'createModelQualityJobDefinition_stoppingCondition' - Undocumented member.
--
-- 'jobDefinitionName', 'createModelQualityJobDefinition_jobDefinitionName' - The name of the monitoring job definition.
--
-- 'modelQualityAppSpecification', 'createModelQualityJobDefinition_modelQualityAppSpecification' - The container that runs the monitoring job.
--
-- 'modelQualityJobInput', 'createModelQualityJobDefinition_modelQualityJobInput' - A list of the inputs that are monitored. Currently endpoints are
-- supported.
--
-- 'modelQualityJobOutputConfig', 'createModelQualityJobDefinition_modelQualityJobOutputConfig' - Undocumented member.
--
-- 'jobResources', 'createModelQualityJobDefinition_jobResources' - Undocumented member.
--
-- 'roleArn', 'createModelQualityJobDefinition_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
newCreateModelQualityJobDefinition ::
  -- | 'jobDefinitionName'
  Core.Text ->
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
  CreateModelQualityJobDefinition
newCreateModelQualityJobDefinition
  pJobDefinitionName_
  pModelQualityAppSpecification_
  pModelQualityJobInput_
  pModelQualityJobOutputConfig_
  pJobResources_
  pRoleArn_ =
    CreateModelQualityJobDefinition'
      { networkConfig =
          Core.Nothing,
        modelQualityBaselineConfig = Core.Nothing,
        tags = Core.Nothing,
        stoppingCondition = Core.Nothing,
        jobDefinitionName = pJobDefinitionName_,
        modelQualityAppSpecification =
          pModelQualityAppSpecification_,
        modelQualityJobInput =
          pModelQualityJobInput_,
        modelQualityJobOutputConfig =
          pModelQualityJobOutputConfig_,
        jobResources = pJobResources_,
        roleArn = pRoleArn_
      }

-- | Specifies the network configuration for the monitoring job.
createModelQualityJobDefinition_networkConfig :: Lens.Lens' CreateModelQualityJobDefinition (Core.Maybe MonitoringNetworkConfig)
createModelQualityJobDefinition_networkConfig = Lens.lens (\CreateModelQualityJobDefinition' {networkConfig} -> networkConfig) (\s@CreateModelQualityJobDefinition' {} a -> s {networkConfig = a} :: CreateModelQualityJobDefinition)

-- | Specifies the constraints and baselines for the monitoring job.
createModelQualityJobDefinition_modelQualityBaselineConfig :: Lens.Lens' CreateModelQualityJobDefinition (Core.Maybe ModelQualityBaselineConfig)
createModelQualityJobDefinition_modelQualityBaselineConfig = Lens.lens (\CreateModelQualityJobDefinition' {modelQualityBaselineConfig} -> modelQualityBaselineConfig) (\s@CreateModelQualityJobDefinition' {} a -> s {modelQualityBaselineConfig = a} :: CreateModelQualityJobDefinition)

-- | (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
createModelQualityJobDefinition_tags :: Lens.Lens' CreateModelQualityJobDefinition (Core.Maybe [Tag])
createModelQualityJobDefinition_tags = Lens.lens (\CreateModelQualityJobDefinition' {tags} -> tags) (\s@CreateModelQualityJobDefinition' {} a -> s {tags = a} :: CreateModelQualityJobDefinition) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
createModelQualityJobDefinition_stoppingCondition :: Lens.Lens' CreateModelQualityJobDefinition (Core.Maybe MonitoringStoppingCondition)
createModelQualityJobDefinition_stoppingCondition = Lens.lens (\CreateModelQualityJobDefinition' {stoppingCondition} -> stoppingCondition) (\s@CreateModelQualityJobDefinition' {} a -> s {stoppingCondition = a} :: CreateModelQualityJobDefinition)

-- | The name of the monitoring job definition.
createModelQualityJobDefinition_jobDefinitionName :: Lens.Lens' CreateModelQualityJobDefinition Core.Text
createModelQualityJobDefinition_jobDefinitionName = Lens.lens (\CreateModelQualityJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@CreateModelQualityJobDefinition' {} a -> s {jobDefinitionName = a} :: CreateModelQualityJobDefinition)

-- | The container that runs the monitoring job.
createModelQualityJobDefinition_modelQualityAppSpecification :: Lens.Lens' CreateModelQualityJobDefinition ModelQualityAppSpecification
createModelQualityJobDefinition_modelQualityAppSpecification = Lens.lens (\CreateModelQualityJobDefinition' {modelQualityAppSpecification} -> modelQualityAppSpecification) (\s@CreateModelQualityJobDefinition' {} a -> s {modelQualityAppSpecification = a} :: CreateModelQualityJobDefinition)

-- | A list of the inputs that are monitored. Currently endpoints are
-- supported.
createModelQualityJobDefinition_modelQualityJobInput :: Lens.Lens' CreateModelQualityJobDefinition ModelQualityJobInput
createModelQualityJobDefinition_modelQualityJobInput = Lens.lens (\CreateModelQualityJobDefinition' {modelQualityJobInput} -> modelQualityJobInput) (\s@CreateModelQualityJobDefinition' {} a -> s {modelQualityJobInput = a} :: CreateModelQualityJobDefinition)

-- | Undocumented member.
createModelQualityJobDefinition_modelQualityJobOutputConfig :: Lens.Lens' CreateModelQualityJobDefinition MonitoringOutputConfig
createModelQualityJobDefinition_modelQualityJobOutputConfig = Lens.lens (\CreateModelQualityJobDefinition' {modelQualityJobOutputConfig} -> modelQualityJobOutputConfig) (\s@CreateModelQualityJobDefinition' {} a -> s {modelQualityJobOutputConfig = a} :: CreateModelQualityJobDefinition)

-- | Undocumented member.
createModelQualityJobDefinition_jobResources :: Lens.Lens' CreateModelQualityJobDefinition MonitoringResources
createModelQualityJobDefinition_jobResources = Lens.lens (\CreateModelQualityJobDefinition' {jobResources} -> jobResources) (\s@CreateModelQualityJobDefinition' {} a -> s {jobResources = a} :: CreateModelQualityJobDefinition)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
createModelQualityJobDefinition_roleArn :: Lens.Lens' CreateModelQualityJobDefinition Core.Text
createModelQualityJobDefinition_roleArn = Lens.lens (\CreateModelQualityJobDefinition' {roleArn} -> roleArn) (\s@CreateModelQualityJobDefinition' {} a -> s {roleArn = a} :: CreateModelQualityJobDefinition)

instance
  Core.AWSRequest
    CreateModelQualityJobDefinition
  where
  type
    AWSResponse CreateModelQualityJobDefinition =
      CreateModelQualityJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelQualityJobDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "JobDefinitionArn")
      )

instance
  Core.Hashable
    CreateModelQualityJobDefinition

instance Core.NFData CreateModelQualityJobDefinition

instance
  Core.ToHeaders
    CreateModelQualityJobDefinition
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateModelQualityJobDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateModelQualityJobDefinition where
  toJSON CreateModelQualityJobDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NetworkConfig" Core..=) Core.<$> networkConfig,
            ("ModelQualityBaselineConfig" Core..=)
              Core.<$> modelQualityBaselineConfig,
            ("Tags" Core..=) Core.<$> tags,
            ("StoppingCondition" Core..=)
              Core.<$> stoppingCondition,
            Core.Just
              ("JobDefinitionName" Core..= jobDefinitionName),
            Core.Just
              ( "ModelQualityAppSpecification"
                  Core..= modelQualityAppSpecification
              ),
            Core.Just
              ( "ModelQualityJobInput"
                  Core..= modelQualityJobInput
              ),
            Core.Just
              ( "ModelQualityJobOutputConfig"
                  Core..= modelQualityJobOutputConfig
              ),
            Core.Just ("JobResources" Core..= jobResources),
            Core.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateModelQualityJobDefinition where
  toPath = Core.const "/"

instance Core.ToQuery CreateModelQualityJobDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateModelQualityJobDefinitionResponse' smart constructor.
data CreateModelQualityJobDefinitionResponse = CreateModelQualityJobDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the model quality monitoring job.
    jobDefinitionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateModelQualityJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createModelQualityJobDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionArn', 'createModelQualityJobDefinitionResponse_jobDefinitionArn' - The Amazon Resource Name (ARN) of the model quality monitoring job.
newCreateModelQualityJobDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'jobDefinitionArn'
  Core.Text ->
  CreateModelQualityJobDefinitionResponse
newCreateModelQualityJobDefinitionResponse
  pHttpStatus_
  pJobDefinitionArn_ =
    CreateModelQualityJobDefinitionResponse'
      { httpStatus =
          pHttpStatus_,
        jobDefinitionArn =
          pJobDefinitionArn_
      }

-- | The response's http status code.
createModelQualityJobDefinitionResponse_httpStatus :: Lens.Lens' CreateModelQualityJobDefinitionResponse Core.Int
createModelQualityJobDefinitionResponse_httpStatus = Lens.lens (\CreateModelQualityJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateModelQualityJobDefinitionResponse' {} a -> s {httpStatus = a} :: CreateModelQualityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the model quality monitoring job.
createModelQualityJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' CreateModelQualityJobDefinitionResponse Core.Text
createModelQualityJobDefinitionResponse_jobDefinitionArn = Lens.lens (\CreateModelQualityJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@CreateModelQualityJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: CreateModelQualityJobDefinitionResponse)

instance
  Core.NFData
    CreateModelQualityJobDefinitionResponse
