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
-- Module      : Network.AWS.SageMaker.CreateDataQualityJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a definition for a job that monitors data quality and drift. For
-- information about model monitor, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
module Network.AWS.SageMaker.CreateDataQualityJobDefinition
  ( -- * Creating a Request
    CreateDataQualityJobDefinition (..),
    newCreateDataQualityJobDefinition,

    -- * Request Lenses
    createDataQualityJobDefinition_networkConfig,
    createDataQualityJobDefinition_dataQualityBaselineConfig,
    createDataQualityJobDefinition_tags,
    createDataQualityJobDefinition_stoppingCondition,
    createDataQualityJobDefinition_jobDefinitionName,
    createDataQualityJobDefinition_dataQualityAppSpecification,
    createDataQualityJobDefinition_dataQualityJobInput,
    createDataQualityJobDefinition_dataQualityJobOutputConfig,
    createDataQualityJobDefinition_jobResources,
    createDataQualityJobDefinition_roleArn,

    -- * Destructuring the Response
    CreateDataQualityJobDefinitionResponse (..),
    newCreateDataQualityJobDefinitionResponse,

    -- * Response Lenses
    createDataQualityJobDefinitionResponse_httpStatus,
    createDataQualityJobDefinitionResponse_jobDefinitionArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateDataQualityJobDefinition' smart constructor.
data CreateDataQualityJobDefinition = CreateDataQualityJobDefinition'
  { -- | Specifies networking configuration for the monitoring job.
    networkConfig :: Prelude.Maybe MonitoringNetworkConfig,
    -- | Configures the constraints and baselines for the monitoring job.
    dataQualityBaselineConfig :: Prelude.Maybe DataQualityBaselineConfig,
    -- | (Optional) An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    stoppingCondition :: Prelude.Maybe MonitoringStoppingCondition,
    -- | The name for the monitoring job definition.
    jobDefinitionName :: Prelude.Text,
    -- | Specifies the container that runs the monitoring job.
    dataQualityAppSpecification :: DataQualityAppSpecification,
    -- | A list of inputs for the monitoring job. Currently endpoints are
    -- supported as monitoring inputs.
    dataQualityJobInput :: DataQualityJobInput,
    dataQualityJobOutputConfig :: MonitoringOutputConfig,
    jobResources :: MonitoringResources,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDataQualityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfig', 'createDataQualityJobDefinition_networkConfig' - Specifies networking configuration for the monitoring job.
--
-- 'dataQualityBaselineConfig', 'createDataQualityJobDefinition_dataQualityBaselineConfig' - Configures the constraints and baselines for the monitoring job.
--
-- 'tags', 'createDataQualityJobDefinition_tags' - (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- 'stoppingCondition', 'createDataQualityJobDefinition_stoppingCondition' - Undocumented member.
--
-- 'jobDefinitionName', 'createDataQualityJobDefinition_jobDefinitionName' - The name for the monitoring job definition.
--
-- 'dataQualityAppSpecification', 'createDataQualityJobDefinition_dataQualityAppSpecification' - Specifies the container that runs the monitoring job.
--
-- 'dataQualityJobInput', 'createDataQualityJobDefinition_dataQualityJobInput' - A list of inputs for the monitoring job. Currently endpoints are
-- supported as monitoring inputs.
--
-- 'dataQualityJobOutputConfig', 'createDataQualityJobDefinition_dataQualityJobOutputConfig' - Undocumented member.
--
-- 'jobResources', 'createDataQualityJobDefinition_jobResources' - Undocumented member.
--
-- 'roleArn', 'createDataQualityJobDefinition_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
newCreateDataQualityJobDefinition ::
  -- | 'jobDefinitionName'
  Prelude.Text ->
  -- | 'dataQualityAppSpecification'
  DataQualityAppSpecification ->
  -- | 'dataQualityJobInput'
  DataQualityJobInput ->
  -- | 'dataQualityJobOutputConfig'
  MonitoringOutputConfig ->
  -- | 'jobResources'
  MonitoringResources ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateDataQualityJobDefinition
newCreateDataQualityJobDefinition
  pJobDefinitionName_
  pDataQualityAppSpecification_
  pDataQualityJobInput_
  pDataQualityJobOutputConfig_
  pJobResources_
  pRoleArn_ =
    CreateDataQualityJobDefinition'
      { networkConfig =
          Prelude.Nothing,
        dataQualityBaselineConfig = Prelude.Nothing,
        tags = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        jobDefinitionName = pJobDefinitionName_,
        dataQualityAppSpecification =
          pDataQualityAppSpecification_,
        dataQualityJobInput = pDataQualityJobInput_,
        dataQualityJobOutputConfig =
          pDataQualityJobOutputConfig_,
        jobResources = pJobResources_,
        roleArn = pRoleArn_
      }

-- | Specifies networking configuration for the monitoring job.
createDataQualityJobDefinition_networkConfig :: Lens.Lens' CreateDataQualityJobDefinition (Prelude.Maybe MonitoringNetworkConfig)
createDataQualityJobDefinition_networkConfig = Lens.lens (\CreateDataQualityJobDefinition' {networkConfig} -> networkConfig) (\s@CreateDataQualityJobDefinition' {} a -> s {networkConfig = a} :: CreateDataQualityJobDefinition)

-- | Configures the constraints and baselines for the monitoring job.
createDataQualityJobDefinition_dataQualityBaselineConfig :: Lens.Lens' CreateDataQualityJobDefinition (Prelude.Maybe DataQualityBaselineConfig)
createDataQualityJobDefinition_dataQualityBaselineConfig = Lens.lens (\CreateDataQualityJobDefinition' {dataQualityBaselineConfig} -> dataQualityBaselineConfig) (\s@CreateDataQualityJobDefinition' {} a -> s {dataQualityBaselineConfig = a} :: CreateDataQualityJobDefinition)

-- | (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
createDataQualityJobDefinition_tags :: Lens.Lens' CreateDataQualityJobDefinition (Prelude.Maybe [Tag])
createDataQualityJobDefinition_tags = Lens.lens (\CreateDataQualityJobDefinition' {tags} -> tags) (\s@CreateDataQualityJobDefinition' {} a -> s {tags = a} :: CreateDataQualityJobDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
createDataQualityJobDefinition_stoppingCondition :: Lens.Lens' CreateDataQualityJobDefinition (Prelude.Maybe MonitoringStoppingCondition)
createDataQualityJobDefinition_stoppingCondition = Lens.lens (\CreateDataQualityJobDefinition' {stoppingCondition} -> stoppingCondition) (\s@CreateDataQualityJobDefinition' {} a -> s {stoppingCondition = a} :: CreateDataQualityJobDefinition)

-- | The name for the monitoring job definition.
createDataQualityJobDefinition_jobDefinitionName :: Lens.Lens' CreateDataQualityJobDefinition Prelude.Text
createDataQualityJobDefinition_jobDefinitionName = Lens.lens (\CreateDataQualityJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@CreateDataQualityJobDefinition' {} a -> s {jobDefinitionName = a} :: CreateDataQualityJobDefinition)

-- | Specifies the container that runs the monitoring job.
createDataQualityJobDefinition_dataQualityAppSpecification :: Lens.Lens' CreateDataQualityJobDefinition DataQualityAppSpecification
createDataQualityJobDefinition_dataQualityAppSpecification = Lens.lens (\CreateDataQualityJobDefinition' {dataQualityAppSpecification} -> dataQualityAppSpecification) (\s@CreateDataQualityJobDefinition' {} a -> s {dataQualityAppSpecification = a} :: CreateDataQualityJobDefinition)

-- | A list of inputs for the monitoring job. Currently endpoints are
-- supported as monitoring inputs.
createDataQualityJobDefinition_dataQualityJobInput :: Lens.Lens' CreateDataQualityJobDefinition DataQualityJobInput
createDataQualityJobDefinition_dataQualityJobInput = Lens.lens (\CreateDataQualityJobDefinition' {dataQualityJobInput} -> dataQualityJobInput) (\s@CreateDataQualityJobDefinition' {} a -> s {dataQualityJobInput = a} :: CreateDataQualityJobDefinition)

-- | Undocumented member.
createDataQualityJobDefinition_dataQualityJobOutputConfig :: Lens.Lens' CreateDataQualityJobDefinition MonitoringOutputConfig
createDataQualityJobDefinition_dataQualityJobOutputConfig = Lens.lens (\CreateDataQualityJobDefinition' {dataQualityJobOutputConfig} -> dataQualityJobOutputConfig) (\s@CreateDataQualityJobDefinition' {} a -> s {dataQualityJobOutputConfig = a} :: CreateDataQualityJobDefinition)

-- | Undocumented member.
createDataQualityJobDefinition_jobResources :: Lens.Lens' CreateDataQualityJobDefinition MonitoringResources
createDataQualityJobDefinition_jobResources = Lens.lens (\CreateDataQualityJobDefinition' {jobResources} -> jobResources) (\s@CreateDataQualityJobDefinition' {} a -> s {jobResources = a} :: CreateDataQualityJobDefinition)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
createDataQualityJobDefinition_roleArn :: Lens.Lens' CreateDataQualityJobDefinition Prelude.Text
createDataQualityJobDefinition_roleArn = Lens.lens (\CreateDataQualityJobDefinition' {roleArn} -> roleArn) (\s@CreateDataQualityJobDefinition' {} a -> s {roleArn = a} :: CreateDataQualityJobDefinition)

instance
  Prelude.AWSRequest
    CreateDataQualityJobDefinition
  where
  type
    Rs CreateDataQualityJobDefinition =
      CreateDataQualityJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataQualityJobDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "JobDefinitionArn")
      )

instance
  Prelude.Hashable
    CreateDataQualityJobDefinition

instance
  Prelude.NFData
    CreateDataQualityJobDefinition

instance
  Prelude.ToHeaders
    CreateDataQualityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.CreateDataQualityJobDefinition" ::
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
    CreateDataQualityJobDefinition
  where
  toJSON CreateDataQualityJobDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NetworkConfig" Prelude..=)
              Prelude.<$> networkConfig,
            ("DataQualityBaselineConfig" Prelude..=)
              Prelude.<$> dataQualityBaselineConfig,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("StoppingCondition" Prelude..=)
              Prelude.<$> stoppingCondition,
            Prelude.Just
              ("JobDefinitionName" Prelude..= jobDefinitionName),
            Prelude.Just
              ( "DataQualityAppSpecification"
                  Prelude..= dataQualityAppSpecification
              ),
            Prelude.Just
              ( "DataQualityJobInput"
                  Prelude..= dataQualityJobInput
              ),
            Prelude.Just
              ( "DataQualityJobOutputConfig"
                  Prelude..= dataQualityJobOutputConfig
              ),
            Prelude.Just
              ("JobResources" Prelude..= jobResources),
            Prelude.Just ("RoleArn" Prelude..= roleArn)
          ]
      )

instance
  Prelude.ToPath
    CreateDataQualityJobDefinition
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CreateDataQualityJobDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataQualityJobDefinitionResponse' smart constructor.
data CreateDataQualityJobDefinitionResponse = CreateDataQualityJobDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the job definition.
    jobDefinitionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDataQualityJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDataQualityJobDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionArn', 'createDataQualityJobDefinitionResponse_jobDefinitionArn' - The Amazon Resource Name (ARN) of the job definition.
newCreateDataQualityJobDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobDefinitionArn'
  Prelude.Text ->
  CreateDataQualityJobDefinitionResponse
newCreateDataQualityJobDefinitionResponse
  pHttpStatus_
  pJobDefinitionArn_ =
    CreateDataQualityJobDefinitionResponse'
      { httpStatus =
          pHttpStatus_,
        jobDefinitionArn =
          pJobDefinitionArn_
      }

-- | The response's http status code.
createDataQualityJobDefinitionResponse_httpStatus :: Lens.Lens' CreateDataQualityJobDefinitionResponse Prelude.Int
createDataQualityJobDefinitionResponse_httpStatus = Lens.lens (\CreateDataQualityJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateDataQualityJobDefinitionResponse' {} a -> s {httpStatus = a} :: CreateDataQualityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the job definition.
createDataQualityJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' CreateDataQualityJobDefinitionResponse Prelude.Text
createDataQualityJobDefinitionResponse_jobDefinitionArn = Lens.lens (\CreateDataQualityJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@CreateDataQualityJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: CreateDataQualityJobDefinitionResponse)

instance
  Prelude.NFData
    CreateDataQualityJobDefinitionResponse
