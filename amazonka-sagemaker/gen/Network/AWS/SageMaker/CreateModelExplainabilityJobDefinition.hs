{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.CreateModelExplainabilityJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the definition for a model explainability job.
module Network.AWS.SageMaker.CreateModelExplainabilityJobDefinition
  ( -- * Creating a Request
    CreateModelExplainabilityJobDefinition (..),
    newCreateModelExplainabilityJobDefinition,

    -- * Request Lenses
    createModelExplainabilityJobDefinition_networkConfig,
    createModelExplainabilityJobDefinition_modelExplainabilityBaselineConfig,
    createModelExplainabilityJobDefinition_tags,
    createModelExplainabilityJobDefinition_stoppingCondition,
    createModelExplainabilityJobDefinition_jobDefinitionName,
    createModelExplainabilityJobDefinition_modelExplainabilityAppSpecification,
    createModelExplainabilityJobDefinition_modelExplainabilityJobInput,
    createModelExplainabilityJobDefinition_modelExplainabilityJobOutputConfig,
    createModelExplainabilityJobDefinition_jobResources,
    createModelExplainabilityJobDefinition_roleArn,

    -- * Destructuring the Response
    CreateModelExplainabilityJobDefinitionResponse (..),
    newCreateModelExplainabilityJobDefinitionResponse,

    -- * Response Lenses
    createModelExplainabilityJobDefinitionResponse_httpStatus,
    createModelExplainabilityJobDefinitionResponse_jobDefinitionArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateModelExplainabilityJobDefinition' smart constructor.
data CreateModelExplainabilityJobDefinition = CreateModelExplainabilityJobDefinition'
  { -- | Networking options for a model explainability job.
    networkConfig :: Prelude.Maybe MonitoringNetworkConfig,
    -- | The baseline configuration for a model explainability job.
    modelExplainabilityBaselineConfig :: Prelude.Maybe ModelExplainabilityBaselineConfig,
    -- | (Optional) An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    stoppingCondition :: Prelude.Maybe MonitoringStoppingCondition,
    -- | The name of the model explainability job definition. The name must be
    -- unique within an AWS Region in the AWS account.
    jobDefinitionName :: Prelude.Text,
    -- | Configures the model explainability job to run a specified Docker
    -- container image.
    modelExplainabilityAppSpecification :: ModelExplainabilityAppSpecification,
    -- | Inputs for the model explainability job.
    modelExplainabilityJobInput :: ModelExplainabilityJobInput,
    modelExplainabilityJobOutputConfig :: MonitoringOutputConfig,
    jobResources :: MonitoringResources,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateModelExplainabilityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfig', 'createModelExplainabilityJobDefinition_networkConfig' - Networking options for a model explainability job.
--
-- 'modelExplainabilityBaselineConfig', 'createModelExplainabilityJobDefinition_modelExplainabilityBaselineConfig' - The baseline configuration for a model explainability job.
--
-- 'tags', 'createModelExplainabilityJobDefinition_tags' - (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- 'stoppingCondition', 'createModelExplainabilityJobDefinition_stoppingCondition' - Undocumented member.
--
-- 'jobDefinitionName', 'createModelExplainabilityJobDefinition_jobDefinitionName' - The name of the model explainability job definition. The name must be
-- unique within an AWS Region in the AWS account.
--
-- 'modelExplainabilityAppSpecification', 'createModelExplainabilityJobDefinition_modelExplainabilityAppSpecification' - Configures the model explainability job to run a specified Docker
-- container image.
--
-- 'modelExplainabilityJobInput', 'createModelExplainabilityJobDefinition_modelExplainabilityJobInput' - Inputs for the model explainability job.
--
-- 'modelExplainabilityJobOutputConfig', 'createModelExplainabilityJobDefinition_modelExplainabilityJobOutputConfig' - Undocumented member.
--
-- 'jobResources', 'createModelExplainabilityJobDefinition_jobResources' - Undocumented member.
--
-- 'roleArn', 'createModelExplainabilityJobDefinition_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
newCreateModelExplainabilityJobDefinition ::
  -- | 'jobDefinitionName'
  Prelude.Text ->
  -- | 'modelExplainabilityAppSpecification'
  ModelExplainabilityAppSpecification ->
  -- | 'modelExplainabilityJobInput'
  ModelExplainabilityJobInput ->
  -- | 'modelExplainabilityJobOutputConfig'
  MonitoringOutputConfig ->
  -- | 'jobResources'
  MonitoringResources ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateModelExplainabilityJobDefinition
newCreateModelExplainabilityJobDefinition
  pJobDefinitionName_
  pModelExplainabilityAppSpecification_
  pModelExplainabilityJobInput_
  pModelExplainabilityJobOutputConfig_
  pJobResources_
  pRoleArn_ =
    CreateModelExplainabilityJobDefinition'
      { networkConfig =
          Prelude.Nothing,
        modelExplainabilityBaselineConfig =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        jobDefinitionName =
          pJobDefinitionName_,
        modelExplainabilityAppSpecification =
          pModelExplainabilityAppSpecification_,
        modelExplainabilityJobInput =
          pModelExplainabilityJobInput_,
        modelExplainabilityJobOutputConfig =
          pModelExplainabilityJobOutputConfig_,
        jobResources = pJobResources_,
        roleArn = pRoleArn_
      }

-- | Networking options for a model explainability job.
createModelExplainabilityJobDefinition_networkConfig :: Lens.Lens' CreateModelExplainabilityJobDefinition (Prelude.Maybe MonitoringNetworkConfig)
createModelExplainabilityJobDefinition_networkConfig = Lens.lens (\CreateModelExplainabilityJobDefinition' {networkConfig} -> networkConfig) (\s@CreateModelExplainabilityJobDefinition' {} a -> s {networkConfig = a} :: CreateModelExplainabilityJobDefinition)

-- | The baseline configuration for a model explainability job.
createModelExplainabilityJobDefinition_modelExplainabilityBaselineConfig :: Lens.Lens' CreateModelExplainabilityJobDefinition (Prelude.Maybe ModelExplainabilityBaselineConfig)
createModelExplainabilityJobDefinition_modelExplainabilityBaselineConfig = Lens.lens (\CreateModelExplainabilityJobDefinition' {modelExplainabilityBaselineConfig} -> modelExplainabilityBaselineConfig) (\s@CreateModelExplainabilityJobDefinition' {} a -> s {modelExplainabilityBaselineConfig = a} :: CreateModelExplainabilityJobDefinition)

-- | (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
createModelExplainabilityJobDefinition_tags :: Lens.Lens' CreateModelExplainabilityJobDefinition (Prelude.Maybe [Tag])
createModelExplainabilityJobDefinition_tags = Lens.lens (\CreateModelExplainabilityJobDefinition' {tags} -> tags) (\s@CreateModelExplainabilityJobDefinition' {} a -> s {tags = a} :: CreateModelExplainabilityJobDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
createModelExplainabilityJobDefinition_stoppingCondition :: Lens.Lens' CreateModelExplainabilityJobDefinition (Prelude.Maybe MonitoringStoppingCondition)
createModelExplainabilityJobDefinition_stoppingCondition = Lens.lens (\CreateModelExplainabilityJobDefinition' {stoppingCondition} -> stoppingCondition) (\s@CreateModelExplainabilityJobDefinition' {} a -> s {stoppingCondition = a} :: CreateModelExplainabilityJobDefinition)

-- | The name of the model explainability job definition. The name must be
-- unique within an AWS Region in the AWS account.
createModelExplainabilityJobDefinition_jobDefinitionName :: Lens.Lens' CreateModelExplainabilityJobDefinition Prelude.Text
createModelExplainabilityJobDefinition_jobDefinitionName = Lens.lens (\CreateModelExplainabilityJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@CreateModelExplainabilityJobDefinition' {} a -> s {jobDefinitionName = a} :: CreateModelExplainabilityJobDefinition)

-- | Configures the model explainability job to run a specified Docker
-- container image.
createModelExplainabilityJobDefinition_modelExplainabilityAppSpecification :: Lens.Lens' CreateModelExplainabilityJobDefinition ModelExplainabilityAppSpecification
createModelExplainabilityJobDefinition_modelExplainabilityAppSpecification = Lens.lens (\CreateModelExplainabilityJobDefinition' {modelExplainabilityAppSpecification} -> modelExplainabilityAppSpecification) (\s@CreateModelExplainabilityJobDefinition' {} a -> s {modelExplainabilityAppSpecification = a} :: CreateModelExplainabilityJobDefinition)

-- | Inputs for the model explainability job.
createModelExplainabilityJobDefinition_modelExplainabilityJobInput :: Lens.Lens' CreateModelExplainabilityJobDefinition ModelExplainabilityJobInput
createModelExplainabilityJobDefinition_modelExplainabilityJobInput = Lens.lens (\CreateModelExplainabilityJobDefinition' {modelExplainabilityJobInput} -> modelExplainabilityJobInput) (\s@CreateModelExplainabilityJobDefinition' {} a -> s {modelExplainabilityJobInput = a} :: CreateModelExplainabilityJobDefinition)

-- | Undocumented member.
createModelExplainabilityJobDefinition_modelExplainabilityJobOutputConfig :: Lens.Lens' CreateModelExplainabilityJobDefinition MonitoringOutputConfig
createModelExplainabilityJobDefinition_modelExplainabilityJobOutputConfig = Lens.lens (\CreateModelExplainabilityJobDefinition' {modelExplainabilityJobOutputConfig} -> modelExplainabilityJobOutputConfig) (\s@CreateModelExplainabilityJobDefinition' {} a -> s {modelExplainabilityJobOutputConfig = a} :: CreateModelExplainabilityJobDefinition)

-- | Undocumented member.
createModelExplainabilityJobDefinition_jobResources :: Lens.Lens' CreateModelExplainabilityJobDefinition MonitoringResources
createModelExplainabilityJobDefinition_jobResources = Lens.lens (\CreateModelExplainabilityJobDefinition' {jobResources} -> jobResources) (\s@CreateModelExplainabilityJobDefinition' {} a -> s {jobResources = a} :: CreateModelExplainabilityJobDefinition)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
createModelExplainabilityJobDefinition_roleArn :: Lens.Lens' CreateModelExplainabilityJobDefinition Prelude.Text
createModelExplainabilityJobDefinition_roleArn = Lens.lens (\CreateModelExplainabilityJobDefinition' {roleArn} -> roleArn) (\s@CreateModelExplainabilityJobDefinition' {} a -> s {roleArn = a} :: CreateModelExplainabilityJobDefinition)

instance
  Prelude.AWSRequest
    CreateModelExplainabilityJobDefinition
  where
  type
    Rs CreateModelExplainabilityJobDefinition =
      CreateModelExplainabilityJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelExplainabilityJobDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Prelude..:> "JobDefinitionArn")
      )

instance
  Prelude.Hashable
    CreateModelExplainabilityJobDefinition

instance
  Prelude.NFData
    CreateModelExplainabilityJobDefinition

instance
  Prelude.ToHeaders
    CreateModelExplainabilityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.CreateModelExplainabilityJobDefinition" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    CreateModelExplainabilityJobDefinition
  where
  toJSON CreateModelExplainabilityJobDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NetworkConfig" Prelude..=)
              Prelude.<$> networkConfig,
            ("ModelExplainabilityBaselineConfig" Prelude..=)
              Prelude.<$> modelExplainabilityBaselineConfig,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("StoppingCondition" Prelude..=)
              Prelude.<$> stoppingCondition,
            Prelude.Just
              ("JobDefinitionName" Prelude..= jobDefinitionName),
            Prelude.Just
              ( "ModelExplainabilityAppSpecification"
                  Prelude..= modelExplainabilityAppSpecification
              ),
            Prelude.Just
              ( "ModelExplainabilityJobInput"
                  Prelude..= modelExplainabilityJobInput
              ),
            Prelude.Just
              ( "ModelExplainabilityJobOutputConfig"
                  Prelude..= modelExplainabilityJobOutputConfig
              ),
            Prelude.Just
              ("JobResources" Prelude..= jobResources),
            Prelude.Just ("RoleArn" Prelude..= roleArn)
          ]
      )

instance
  Prelude.ToPath
    CreateModelExplainabilityJobDefinition
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CreateModelExplainabilityJobDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateModelExplainabilityJobDefinitionResponse' smart constructor.
data CreateModelExplainabilityJobDefinitionResponse = CreateModelExplainabilityJobDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the model explainability job.
    jobDefinitionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateModelExplainabilityJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createModelExplainabilityJobDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionArn', 'createModelExplainabilityJobDefinitionResponse_jobDefinitionArn' - The Amazon Resource Name (ARN) of the model explainability job.
newCreateModelExplainabilityJobDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobDefinitionArn'
  Prelude.Text ->
  CreateModelExplainabilityJobDefinitionResponse
newCreateModelExplainabilityJobDefinitionResponse
  pHttpStatus_
  pJobDefinitionArn_ =
    CreateModelExplainabilityJobDefinitionResponse'
      { httpStatus =
          pHttpStatus_,
        jobDefinitionArn =
          pJobDefinitionArn_
      }

-- | The response's http status code.
createModelExplainabilityJobDefinitionResponse_httpStatus :: Lens.Lens' CreateModelExplainabilityJobDefinitionResponse Prelude.Int
createModelExplainabilityJobDefinitionResponse_httpStatus = Lens.lens (\CreateModelExplainabilityJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateModelExplainabilityJobDefinitionResponse' {} a -> s {httpStatus = a} :: CreateModelExplainabilityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the model explainability job.
createModelExplainabilityJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' CreateModelExplainabilityJobDefinitionResponse Prelude.Text
createModelExplainabilityJobDefinitionResponse_jobDefinitionArn = Lens.lens (\CreateModelExplainabilityJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@CreateModelExplainabilityJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: CreateModelExplainabilityJobDefinitionResponse)

instance
  Prelude.NFData
    CreateModelExplainabilityJobDefinitionResponse
