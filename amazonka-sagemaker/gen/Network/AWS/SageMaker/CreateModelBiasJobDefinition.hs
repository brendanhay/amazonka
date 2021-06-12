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
-- Module      : Network.AWS.SageMaker.CreateModelBiasJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the definition for a model bias job.
module Network.AWS.SageMaker.CreateModelBiasJobDefinition
  ( -- * Creating a Request
    CreateModelBiasJobDefinition (..),
    newCreateModelBiasJobDefinition,

    -- * Request Lenses
    createModelBiasJobDefinition_networkConfig,
    createModelBiasJobDefinition_modelBiasBaselineConfig,
    createModelBiasJobDefinition_tags,
    createModelBiasJobDefinition_stoppingCondition,
    createModelBiasJobDefinition_jobDefinitionName,
    createModelBiasJobDefinition_modelBiasAppSpecification,
    createModelBiasJobDefinition_modelBiasJobInput,
    createModelBiasJobDefinition_modelBiasJobOutputConfig,
    createModelBiasJobDefinition_jobResources,
    createModelBiasJobDefinition_roleArn,

    -- * Destructuring the Response
    CreateModelBiasJobDefinitionResponse (..),
    newCreateModelBiasJobDefinitionResponse,

    -- * Response Lenses
    createModelBiasJobDefinitionResponse_httpStatus,
    createModelBiasJobDefinitionResponse_jobDefinitionArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateModelBiasJobDefinition' smart constructor.
data CreateModelBiasJobDefinition = CreateModelBiasJobDefinition'
  { -- | Networking options for a model bias job.
    networkConfig :: Core.Maybe MonitoringNetworkConfig,
    -- | The baseline configuration for a model bias job.
    modelBiasBaselineConfig :: Core.Maybe ModelBiasBaselineConfig,
    -- | (Optional) An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/.
    tags :: Core.Maybe [Tag],
    stoppingCondition :: Core.Maybe MonitoringStoppingCondition,
    -- | The name of the bias job definition. The name must be unique within an
    -- AWS Region in the AWS account.
    jobDefinitionName :: Core.Text,
    -- | Configures the model bias job to run a specified Docker container image.
    modelBiasAppSpecification :: ModelBiasAppSpecification,
    -- | Inputs for the model bias job.
    modelBiasJobInput :: ModelBiasJobInput,
    modelBiasJobOutputConfig :: MonitoringOutputConfig,
    jobResources :: MonitoringResources,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateModelBiasJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfig', 'createModelBiasJobDefinition_networkConfig' - Networking options for a model bias job.
--
-- 'modelBiasBaselineConfig', 'createModelBiasJobDefinition_modelBiasBaselineConfig' - The baseline configuration for a model bias job.
--
-- 'tags', 'createModelBiasJobDefinition_tags' - (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- 'stoppingCondition', 'createModelBiasJobDefinition_stoppingCondition' - Undocumented member.
--
-- 'jobDefinitionName', 'createModelBiasJobDefinition_jobDefinitionName' - The name of the bias job definition. The name must be unique within an
-- AWS Region in the AWS account.
--
-- 'modelBiasAppSpecification', 'createModelBiasJobDefinition_modelBiasAppSpecification' - Configures the model bias job to run a specified Docker container image.
--
-- 'modelBiasJobInput', 'createModelBiasJobDefinition_modelBiasJobInput' - Inputs for the model bias job.
--
-- 'modelBiasJobOutputConfig', 'createModelBiasJobDefinition_modelBiasJobOutputConfig' - Undocumented member.
--
-- 'jobResources', 'createModelBiasJobDefinition_jobResources' - Undocumented member.
--
-- 'roleArn', 'createModelBiasJobDefinition_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
newCreateModelBiasJobDefinition ::
  -- | 'jobDefinitionName'
  Core.Text ->
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
  CreateModelBiasJobDefinition
newCreateModelBiasJobDefinition
  pJobDefinitionName_
  pModelBiasAppSpecification_
  pModelBiasJobInput_
  pModelBiasJobOutputConfig_
  pJobResources_
  pRoleArn_ =
    CreateModelBiasJobDefinition'
      { networkConfig =
          Core.Nothing,
        modelBiasBaselineConfig = Core.Nothing,
        tags = Core.Nothing,
        stoppingCondition = Core.Nothing,
        jobDefinitionName = pJobDefinitionName_,
        modelBiasAppSpecification =
          pModelBiasAppSpecification_,
        modelBiasJobInput = pModelBiasJobInput_,
        modelBiasJobOutputConfig =
          pModelBiasJobOutputConfig_,
        jobResources = pJobResources_,
        roleArn = pRoleArn_
      }

-- | Networking options for a model bias job.
createModelBiasJobDefinition_networkConfig :: Lens.Lens' CreateModelBiasJobDefinition (Core.Maybe MonitoringNetworkConfig)
createModelBiasJobDefinition_networkConfig = Lens.lens (\CreateModelBiasJobDefinition' {networkConfig} -> networkConfig) (\s@CreateModelBiasJobDefinition' {} a -> s {networkConfig = a} :: CreateModelBiasJobDefinition)

-- | The baseline configuration for a model bias job.
createModelBiasJobDefinition_modelBiasBaselineConfig :: Lens.Lens' CreateModelBiasJobDefinition (Core.Maybe ModelBiasBaselineConfig)
createModelBiasJobDefinition_modelBiasBaselineConfig = Lens.lens (\CreateModelBiasJobDefinition' {modelBiasBaselineConfig} -> modelBiasBaselineConfig) (\s@CreateModelBiasJobDefinition' {} a -> s {modelBiasBaselineConfig = a} :: CreateModelBiasJobDefinition)

-- | (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
createModelBiasJobDefinition_tags :: Lens.Lens' CreateModelBiasJobDefinition (Core.Maybe [Tag])
createModelBiasJobDefinition_tags = Lens.lens (\CreateModelBiasJobDefinition' {tags} -> tags) (\s@CreateModelBiasJobDefinition' {} a -> s {tags = a} :: CreateModelBiasJobDefinition) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
createModelBiasJobDefinition_stoppingCondition :: Lens.Lens' CreateModelBiasJobDefinition (Core.Maybe MonitoringStoppingCondition)
createModelBiasJobDefinition_stoppingCondition = Lens.lens (\CreateModelBiasJobDefinition' {stoppingCondition} -> stoppingCondition) (\s@CreateModelBiasJobDefinition' {} a -> s {stoppingCondition = a} :: CreateModelBiasJobDefinition)

-- | The name of the bias job definition. The name must be unique within an
-- AWS Region in the AWS account.
createModelBiasJobDefinition_jobDefinitionName :: Lens.Lens' CreateModelBiasJobDefinition Core.Text
createModelBiasJobDefinition_jobDefinitionName = Lens.lens (\CreateModelBiasJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@CreateModelBiasJobDefinition' {} a -> s {jobDefinitionName = a} :: CreateModelBiasJobDefinition)

-- | Configures the model bias job to run a specified Docker container image.
createModelBiasJobDefinition_modelBiasAppSpecification :: Lens.Lens' CreateModelBiasJobDefinition ModelBiasAppSpecification
createModelBiasJobDefinition_modelBiasAppSpecification = Lens.lens (\CreateModelBiasJobDefinition' {modelBiasAppSpecification} -> modelBiasAppSpecification) (\s@CreateModelBiasJobDefinition' {} a -> s {modelBiasAppSpecification = a} :: CreateModelBiasJobDefinition)

-- | Inputs for the model bias job.
createModelBiasJobDefinition_modelBiasJobInput :: Lens.Lens' CreateModelBiasJobDefinition ModelBiasJobInput
createModelBiasJobDefinition_modelBiasJobInput = Lens.lens (\CreateModelBiasJobDefinition' {modelBiasJobInput} -> modelBiasJobInput) (\s@CreateModelBiasJobDefinition' {} a -> s {modelBiasJobInput = a} :: CreateModelBiasJobDefinition)

-- | Undocumented member.
createModelBiasJobDefinition_modelBiasJobOutputConfig :: Lens.Lens' CreateModelBiasJobDefinition MonitoringOutputConfig
createModelBiasJobDefinition_modelBiasJobOutputConfig = Lens.lens (\CreateModelBiasJobDefinition' {modelBiasJobOutputConfig} -> modelBiasJobOutputConfig) (\s@CreateModelBiasJobDefinition' {} a -> s {modelBiasJobOutputConfig = a} :: CreateModelBiasJobDefinition)

-- | Undocumented member.
createModelBiasJobDefinition_jobResources :: Lens.Lens' CreateModelBiasJobDefinition MonitoringResources
createModelBiasJobDefinition_jobResources = Lens.lens (\CreateModelBiasJobDefinition' {jobResources} -> jobResources) (\s@CreateModelBiasJobDefinition' {} a -> s {jobResources = a} :: CreateModelBiasJobDefinition)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
createModelBiasJobDefinition_roleArn :: Lens.Lens' CreateModelBiasJobDefinition Core.Text
createModelBiasJobDefinition_roleArn = Lens.lens (\CreateModelBiasJobDefinition' {roleArn} -> roleArn) (\s@CreateModelBiasJobDefinition' {} a -> s {roleArn = a} :: CreateModelBiasJobDefinition)

instance Core.AWSRequest CreateModelBiasJobDefinition where
  type
    AWSResponse CreateModelBiasJobDefinition =
      CreateModelBiasJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelBiasJobDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "JobDefinitionArn")
      )

instance Core.Hashable CreateModelBiasJobDefinition

instance Core.NFData CreateModelBiasJobDefinition

instance Core.ToHeaders CreateModelBiasJobDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateModelBiasJobDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateModelBiasJobDefinition where
  toJSON CreateModelBiasJobDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NetworkConfig" Core..=) Core.<$> networkConfig,
            ("ModelBiasBaselineConfig" Core..=)
              Core.<$> modelBiasBaselineConfig,
            ("Tags" Core..=) Core.<$> tags,
            ("StoppingCondition" Core..=)
              Core.<$> stoppingCondition,
            Core.Just
              ("JobDefinitionName" Core..= jobDefinitionName),
            Core.Just
              ( "ModelBiasAppSpecification"
                  Core..= modelBiasAppSpecification
              ),
            Core.Just
              ("ModelBiasJobInput" Core..= modelBiasJobInput),
            Core.Just
              ( "ModelBiasJobOutputConfig"
                  Core..= modelBiasJobOutputConfig
              ),
            Core.Just ("JobResources" Core..= jobResources),
            Core.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateModelBiasJobDefinition where
  toPath = Core.const "/"

instance Core.ToQuery CreateModelBiasJobDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateModelBiasJobDefinitionResponse' smart constructor.
data CreateModelBiasJobDefinitionResponse = CreateModelBiasJobDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the model bias job.
    jobDefinitionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateModelBiasJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createModelBiasJobDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionArn', 'createModelBiasJobDefinitionResponse_jobDefinitionArn' - The Amazon Resource Name (ARN) of the model bias job.
newCreateModelBiasJobDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'jobDefinitionArn'
  Core.Text ->
  CreateModelBiasJobDefinitionResponse
newCreateModelBiasJobDefinitionResponse
  pHttpStatus_
  pJobDefinitionArn_ =
    CreateModelBiasJobDefinitionResponse'
      { httpStatus =
          pHttpStatus_,
        jobDefinitionArn = pJobDefinitionArn_
      }

-- | The response's http status code.
createModelBiasJobDefinitionResponse_httpStatus :: Lens.Lens' CreateModelBiasJobDefinitionResponse Core.Int
createModelBiasJobDefinitionResponse_httpStatus = Lens.lens (\CreateModelBiasJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateModelBiasJobDefinitionResponse' {} a -> s {httpStatus = a} :: CreateModelBiasJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the model bias job.
createModelBiasJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' CreateModelBiasJobDefinitionResponse Core.Text
createModelBiasJobDefinitionResponse_jobDefinitionArn = Lens.lens (\CreateModelBiasJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@CreateModelBiasJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: CreateModelBiasJobDefinitionResponse)

instance
  Core.NFData
    CreateModelBiasJobDefinitionResponse
