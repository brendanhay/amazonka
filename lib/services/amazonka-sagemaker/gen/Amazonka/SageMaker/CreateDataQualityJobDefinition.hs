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
-- Module      : Amazonka.SageMaker.CreateDataQualityJobDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a definition for a job that monitors data quality and drift. For
-- information about model monitor, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
module Amazonka.SageMaker.CreateDataQualityJobDefinition
  ( -- * Creating a Request
    CreateDataQualityJobDefinition (..),
    newCreateDataQualityJobDefinition,

    -- * Request Lenses
    createDataQualityJobDefinition_dataQualityBaselineConfig,
    createDataQualityJobDefinition_networkConfig,
    createDataQualityJobDefinition_stoppingCondition,
    createDataQualityJobDefinition_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateDataQualityJobDefinition' smart constructor.
data CreateDataQualityJobDefinition = CreateDataQualityJobDefinition'
  { -- | Configures the constraints and baselines for the monitoring job.
    dataQualityBaselineConfig :: Prelude.Maybe DataQualityBaselineConfig,
    -- | Specifies networking configuration for the monitoring job.
    networkConfig :: Prelude.Maybe MonitoringNetworkConfig,
    stoppingCondition :: Prelude.Maybe MonitoringStoppingCondition,
    -- | (Optional) An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /Amazon Web Services Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataQualityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataQualityBaselineConfig', 'createDataQualityJobDefinition_dataQualityBaselineConfig' - Configures the constraints and baselines for the monitoring job.
--
-- 'networkConfig', 'createDataQualityJobDefinition_networkConfig' - Specifies networking configuration for the monitoring job.
--
-- 'stoppingCondition', 'createDataQualityJobDefinition_stoppingCondition' - Undocumented member.
--
-- 'tags', 'createDataQualityJobDefinition_tags' - (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
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
      { dataQualityBaselineConfig =
          Prelude.Nothing,
        networkConfig = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        tags = Prelude.Nothing,
        jobDefinitionName = pJobDefinitionName_,
        dataQualityAppSpecification =
          pDataQualityAppSpecification_,
        dataQualityJobInput = pDataQualityJobInput_,
        dataQualityJobOutputConfig =
          pDataQualityJobOutputConfig_,
        jobResources = pJobResources_,
        roleArn = pRoleArn_
      }

-- | Configures the constraints and baselines for the monitoring job.
createDataQualityJobDefinition_dataQualityBaselineConfig :: Lens.Lens' CreateDataQualityJobDefinition (Prelude.Maybe DataQualityBaselineConfig)
createDataQualityJobDefinition_dataQualityBaselineConfig = Lens.lens (\CreateDataQualityJobDefinition' {dataQualityBaselineConfig} -> dataQualityBaselineConfig) (\s@CreateDataQualityJobDefinition' {} a -> s {dataQualityBaselineConfig = a} :: CreateDataQualityJobDefinition)

-- | Specifies networking configuration for the monitoring job.
createDataQualityJobDefinition_networkConfig :: Lens.Lens' CreateDataQualityJobDefinition (Prelude.Maybe MonitoringNetworkConfig)
createDataQualityJobDefinition_networkConfig = Lens.lens (\CreateDataQualityJobDefinition' {networkConfig} -> networkConfig) (\s@CreateDataQualityJobDefinition' {} a -> s {networkConfig = a} :: CreateDataQualityJobDefinition)

-- | Undocumented member.
createDataQualityJobDefinition_stoppingCondition :: Lens.Lens' CreateDataQualityJobDefinition (Prelude.Maybe MonitoringStoppingCondition)
createDataQualityJobDefinition_stoppingCondition = Lens.lens (\CreateDataQualityJobDefinition' {stoppingCondition} -> stoppingCondition) (\s@CreateDataQualityJobDefinition' {} a -> s {stoppingCondition = a} :: CreateDataQualityJobDefinition)

-- | (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
createDataQualityJobDefinition_tags :: Lens.Lens' CreateDataQualityJobDefinition (Prelude.Maybe [Tag])
createDataQualityJobDefinition_tags = Lens.lens (\CreateDataQualityJobDefinition' {tags} -> tags) (\s@CreateDataQualityJobDefinition' {} a -> s {tags = a} :: CreateDataQualityJobDefinition) Prelude.. Lens.mapping Lens.coerced

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
  Core.AWSRequest
    CreateDataQualityJobDefinition
  where
  type
    AWSResponse CreateDataQualityJobDefinition =
      CreateDataQualityJobDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataQualityJobDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "JobDefinitionArn")
      )

instance
  Prelude.Hashable
    CreateDataQualityJobDefinition
  where
  hashWithSalt
    _salt
    CreateDataQualityJobDefinition' {..} =
      _salt
        `Prelude.hashWithSalt` dataQualityBaselineConfig
        `Prelude.hashWithSalt` networkConfig
        `Prelude.hashWithSalt` stoppingCondition
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` jobDefinitionName
        `Prelude.hashWithSalt` dataQualityAppSpecification
        `Prelude.hashWithSalt` dataQualityJobInput
        `Prelude.hashWithSalt` dataQualityJobOutputConfig
        `Prelude.hashWithSalt` jobResources
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    CreateDataQualityJobDefinition
  where
  rnf CreateDataQualityJobDefinition' {..} =
    Prelude.rnf dataQualityBaselineConfig `Prelude.seq`
      Prelude.rnf networkConfig `Prelude.seq`
        Prelude.rnf stoppingCondition `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf jobDefinitionName `Prelude.seq`
              Prelude.rnf dataQualityAppSpecification `Prelude.seq`
                Prelude.rnf dataQualityJobInput `Prelude.seq`
                  Prelude.rnf dataQualityJobOutputConfig `Prelude.seq`
                    Prelude.rnf jobResources `Prelude.seq`
                      Prelude.rnf roleArn

instance
  Data.ToHeaders
    CreateDataQualityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateDataQualityJobDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataQualityJobDefinition where
  toJSON CreateDataQualityJobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataQualityBaselineConfig" Data..=)
              Prelude.<$> dataQualityBaselineConfig,
            ("NetworkConfig" Data..=) Prelude.<$> networkConfig,
            ("StoppingCondition" Data..=)
              Prelude.<$> stoppingCondition,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("JobDefinitionName" Data..= jobDefinitionName),
            Prelude.Just
              ( "DataQualityAppSpecification"
                  Data..= dataQualityAppSpecification
              ),
            Prelude.Just
              ("DataQualityJobInput" Data..= dataQualityJobInput),
            Prelude.Just
              ( "DataQualityJobOutputConfig"
                  Data..= dataQualityJobOutputConfig
              ),
            Prelude.Just ("JobResources" Data..= jobResources),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateDataQualityJobDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDataQualityJobDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataQualityJobDefinitionResponse' smart constructor.
data CreateDataQualityJobDefinitionResponse = CreateDataQualityJobDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the job definition.
    jobDefinitionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf CreateDataQualityJobDefinitionResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf jobDefinitionArn
