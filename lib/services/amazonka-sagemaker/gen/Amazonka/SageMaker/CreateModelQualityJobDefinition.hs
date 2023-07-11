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
-- Module      : Amazonka.SageMaker.CreateModelQualityJobDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a definition for a job that monitors model quality and drift.
-- For information about model monitor, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
module Amazonka.SageMaker.CreateModelQualityJobDefinition
  ( -- * Creating a Request
    CreateModelQualityJobDefinition (..),
    newCreateModelQualityJobDefinition,

    -- * Request Lenses
    createModelQualityJobDefinition_modelQualityBaselineConfig,
    createModelQualityJobDefinition_networkConfig,
    createModelQualityJobDefinition_stoppingCondition,
    createModelQualityJobDefinition_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateModelQualityJobDefinition' smart constructor.
data CreateModelQualityJobDefinition = CreateModelQualityJobDefinition'
  { -- | Specifies the constraints and baselines for the monitoring job.
    modelQualityBaselineConfig :: Prelude.Maybe ModelQualityBaselineConfig,
    -- | Specifies the network configuration for the monitoring job.
    networkConfig :: Prelude.Maybe MonitoringNetworkConfig,
    stoppingCondition :: Prelude.Maybe MonitoringStoppingCondition,
    -- | (Optional) An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /Amazon Web Services Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the monitoring job definition.
    jobDefinitionName :: Prelude.Text,
    -- | The container that runs the monitoring job.
    modelQualityAppSpecification :: ModelQualityAppSpecification,
    -- | A list of the inputs that are monitored. Currently endpoints are
    -- supported.
    modelQualityJobInput :: ModelQualityJobInput,
    modelQualityJobOutputConfig :: MonitoringOutputConfig,
    jobResources :: MonitoringResources,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelQualityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelQualityBaselineConfig', 'createModelQualityJobDefinition_modelQualityBaselineConfig' - Specifies the constraints and baselines for the monitoring job.
--
-- 'networkConfig', 'createModelQualityJobDefinition_networkConfig' - Specifies the network configuration for the monitoring job.
--
-- 'stoppingCondition', 'createModelQualityJobDefinition_stoppingCondition' - Undocumented member.
--
-- 'tags', 'createModelQualityJobDefinition_tags' - (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
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
  Prelude.Text ->
  -- | 'modelQualityAppSpecification'
  ModelQualityAppSpecification ->
  -- | 'modelQualityJobInput'
  ModelQualityJobInput ->
  -- | 'modelQualityJobOutputConfig'
  MonitoringOutputConfig ->
  -- | 'jobResources'
  MonitoringResources ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateModelQualityJobDefinition
newCreateModelQualityJobDefinition
  pJobDefinitionName_
  pModelQualityAppSpecification_
  pModelQualityJobInput_
  pModelQualityJobOutputConfig_
  pJobResources_
  pRoleArn_ =
    CreateModelQualityJobDefinition'
      { modelQualityBaselineConfig =
          Prelude.Nothing,
        networkConfig = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        tags = Prelude.Nothing,
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

-- | Specifies the constraints and baselines for the monitoring job.
createModelQualityJobDefinition_modelQualityBaselineConfig :: Lens.Lens' CreateModelQualityJobDefinition (Prelude.Maybe ModelQualityBaselineConfig)
createModelQualityJobDefinition_modelQualityBaselineConfig = Lens.lens (\CreateModelQualityJobDefinition' {modelQualityBaselineConfig} -> modelQualityBaselineConfig) (\s@CreateModelQualityJobDefinition' {} a -> s {modelQualityBaselineConfig = a} :: CreateModelQualityJobDefinition)

-- | Specifies the network configuration for the monitoring job.
createModelQualityJobDefinition_networkConfig :: Lens.Lens' CreateModelQualityJobDefinition (Prelude.Maybe MonitoringNetworkConfig)
createModelQualityJobDefinition_networkConfig = Lens.lens (\CreateModelQualityJobDefinition' {networkConfig} -> networkConfig) (\s@CreateModelQualityJobDefinition' {} a -> s {networkConfig = a} :: CreateModelQualityJobDefinition)

-- | Undocumented member.
createModelQualityJobDefinition_stoppingCondition :: Lens.Lens' CreateModelQualityJobDefinition (Prelude.Maybe MonitoringStoppingCondition)
createModelQualityJobDefinition_stoppingCondition = Lens.lens (\CreateModelQualityJobDefinition' {stoppingCondition} -> stoppingCondition) (\s@CreateModelQualityJobDefinition' {} a -> s {stoppingCondition = a} :: CreateModelQualityJobDefinition)

-- | (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
createModelQualityJobDefinition_tags :: Lens.Lens' CreateModelQualityJobDefinition (Prelude.Maybe [Tag])
createModelQualityJobDefinition_tags = Lens.lens (\CreateModelQualityJobDefinition' {tags} -> tags) (\s@CreateModelQualityJobDefinition' {} a -> s {tags = a} :: CreateModelQualityJobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The name of the monitoring job definition.
createModelQualityJobDefinition_jobDefinitionName :: Lens.Lens' CreateModelQualityJobDefinition Prelude.Text
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
createModelQualityJobDefinition_roleArn :: Lens.Lens' CreateModelQualityJobDefinition Prelude.Text
createModelQualityJobDefinition_roleArn = Lens.lens (\CreateModelQualityJobDefinition' {roleArn} -> roleArn) (\s@CreateModelQualityJobDefinition' {} a -> s {roleArn = a} :: CreateModelQualityJobDefinition)

instance
  Core.AWSRequest
    CreateModelQualityJobDefinition
  where
  type
    AWSResponse CreateModelQualityJobDefinition =
      CreateModelQualityJobDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelQualityJobDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "JobDefinitionArn")
      )

instance
  Prelude.Hashable
    CreateModelQualityJobDefinition
  where
  hashWithSalt
    _salt
    CreateModelQualityJobDefinition' {..} =
      _salt
        `Prelude.hashWithSalt` modelQualityBaselineConfig
        `Prelude.hashWithSalt` networkConfig
        `Prelude.hashWithSalt` stoppingCondition
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` jobDefinitionName
        `Prelude.hashWithSalt` modelQualityAppSpecification
        `Prelude.hashWithSalt` modelQualityJobInput
        `Prelude.hashWithSalt` modelQualityJobOutputConfig
        `Prelude.hashWithSalt` jobResources
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    CreateModelQualityJobDefinition
  where
  rnf CreateModelQualityJobDefinition' {..} =
    Prelude.rnf modelQualityBaselineConfig
      `Prelude.seq` Prelude.rnf networkConfig
      `Prelude.seq` Prelude.rnf stoppingCondition
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf jobDefinitionName
      `Prelude.seq` Prelude.rnf modelQualityAppSpecification
      `Prelude.seq` Prelude.rnf modelQualityJobInput
      `Prelude.seq` Prelude.rnf modelQualityJobOutputConfig
      `Prelude.seq` Prelude.rnf jobResources
      `Prelude.seq` Prelude.rnf roleArn

instance
  Data.ToHeaders
    CreateModelQualityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateModelQualityJobDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateModelQualityJobDefinition where
  toJSON CreateModelQualityJobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ModelQualityBaselineConfig" Data..=)
              Prelude.<$> modelQualityBaselineConfig,
            ("NetworkConfig" Data..=) Prelude.<$> networkConfig,
            ("StoppingCondition" Data..=)
              Prelude.<$> stoppingCondition,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("JobDefinitionName" Data..= jobDefinitionName),
            Prelude.Just
              ( "ModelQualityAppSpecification"
                  Data..= modelQualityAppSpecification
              ),
            Prelude.Just
              ( "ModelQualityJobInput"
                  Data..= modelQualityJobInput
              ),
            Prelude.Just
              ( "ModelQualityJobOutputConfig"
                  Data..= modelQualityJobOutputConfig
              ),
            Prelude.Just ("JobResources" Data..= jobResources),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateModelQualityJobDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateModelQualityJobDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateModelQualityJobDefinitionResponse' smart constructor.
data CreateModelQualityJobDefinitionResponse = CreateModelQualityJobDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the model quality monitoring job.
    jobDefinitionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'jobDefinitionArn'
  Prelude.Text ->
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
createModelQualityJobDefinitionResponse_httpStatus :: Lens.Lens' CreateModelQualityJobDefinitionResponse Prelude.Int
createModelQualityJobDefinitionResponse_httpStatus = Lens.lens (\CreateModelQualityJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateModelQualityJobDefinitionResponse' {} a -> s {httpStatus = a} :: CreateModelQualityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the model quality monitoring job.
createModelQualityJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' CreateModelQualityJobDefinitionResponse Prelude.Text
createModelQualityJobDefinitionResponse_jobDefinitionArn = Lens.lens (\CreateModelQualityJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@CreateModelQualityJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: CreateModelQualityJobDefinitionResponse)

instance
  Prelude.NFData
    CreateModelQualityJobDefinitionResponse
  where
  rnf CreateModelQualityJobDefinitionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobDefinitionArn
