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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeModelExplainabilityJobDefinition' smart constructor.
data DescribeModelExplainabilityJobDefinition = DescribeModelExplainabilityJobDefinition'
  { -- | The name of the model explainability job definition. The name must be
    -- unique within an Amazon Web Services Region in the Amazon Web Services
    -- account.
    jobDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelExplainabilityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinitionName', 'describeModelExplainabilityJobDefinition_jobDefinitionName' - The name of the model explainability job definition. The name must be
-- unique within an Amazon Web Services Region in the Amazon Web Services
-- account.
newDescribeModelExplainabilityJobDefinition ::
  -- | 'jobDefinitionName'
  Prelude.Text ->
  DescribeModelExplainabilityJobDefinition
newDescribeModelExplainabilityJobDefinition
  pJobDefinitionName_ =
    DescribeModelExplainabilityJobDefinition'
      { jobDefinitionName =
          pJobDefinitionName_
      }

-- | The name of the model explainability job definition. The name must be
-- unique within an Amazon Web Services Region in the Amazon Web Services
-- account.
describeModelExplainabilityJobDefinition_jobDefinitionName :: Lens.Lens' DescribeModelExplainabilityJobDefinition Prelude.Text
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
            Prelude.<$> (x Core..?> "NetworkConfig")
              Prelude.<*> (x Core..?> "ModelExplainabilityBaselineConfig")
              Prelude.<*> (x Core..?> "StoppingCondition")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Core..:> "JobDefinitionArn")
              Prelude.<*> (x Core..:> "JobDefinitionName")
              Prelude.<*> (x Core..:> "CreationTime")
              Prelude.<*> (x Core..:> "ModelExplainabilityAppSpecification")
              Prelude.<*> (x Core..:> "ModelExplainabilityJobInput")
              Prelude.<*> (x Core..:> "ModelExplainabilityJobOutputConfig")
              Prelude.<*> (x Core..:> "JobResources")
              Prelude.<*> (x Core..:> "RoleArn")
      )

instance
  Prelude.Hashable
    DescribeModelExplainabilityJobDefinition

instance
  Prelude.NFData
    DescribeModelExplainabilityJobDefinition

instance
  Core.ToHeaders
    DescribeModelExplainabilityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeModelExplainabilityJobDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeModelExplainabilityJobDefinition
  where
  toJSON DescribeModelExplainabilityJobDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("JobDefinitionName" Core..= jobDefinitionName)
          ]
      )

instance
  Core.ToPath
    DescribeModelExplainabilityJobDefinition
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeModelExplainabilityJobDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelExplainabilityJobDefinitionResponse' smart constructor.
data DescribeModelExplainabilityJobDefinitionResponse = DescribeModelExplainabilityJobDefinitionResponse'
  { -- | Networking options for a model explainability job.
    networkConfig :: Prelude.Maybe MonitoringNetworkConfig,
    -- | The baseline configuration for a model explainability job.
    modelExplainabilityBaselineConfig :: Prelude.Maybe ModelExplainabilityBaselineConfig,
    stoppingCondition :: Prelude.Maybe MonitoringStoppingCondition,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the model explainability job.
    jobDefinitionArn :: Prelude.Text,
    -- | The name of the explainability job definition. The name must be unique
    -- within an Amazon Web Services Region in the Amazon Web Services account.
    jobDefinitionName :: Prelude.Text,
    -- | The time at which the model explainability job was created.
    creationTime :: Core.POSIX,
    -- | Configures the model explainability job to run a specified Docker
    -- container image.
    modelExplainabilityAppSpecification :: ModelExplainabilityAppSpecification,
    -- | Inputs for the model explainability job.
    modelExplainabilityJobInput :: ModelExplainabilityJobInput,
    modelExplainabilityJobOutputConfig :: MonitoringOutputConfig,
    jobResources :: MonitoringResources,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
    -- Access Management (IAM) role that has read permission to the input data
    -- location and write permission to the output data location in Amazon S3.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- within an Amazon Web Services Region in the Amazon Web Services account.
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
-- 'roleArn', 'describeModelExplainabilityJobDefinitionResponse_roleArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role that has read permission to the input data
-- location and write permission to the output data location in Amazon S3.
newDescribeModelExplainabilityJobDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobDefinitionArn'
  Prelude.Text ->
  -- | 'jobDefinitionName'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
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
          Prelude.Nothing,
        modelExplainabilityBaselineConfig =
          Prelude.Nothing,
        stoppingCondition =
          Prelude.Nothing,
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
describeModelExplainabilityJobDefinitionResponse_networkConfig :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse (Prelude.Maybe MonitoringNetworkConfig)
describeModelExplainabilityJobDefinitionResponse_networkConfig = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {networkConfig} -> networkConfig) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {networkConfig = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | The baseline configuration for a model explainability job.
describeModelExplainabilityJobDefinitionResponse_modelExplainabilityBaselineConfig :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse (Prelude.Maybe ModelExplainabilityBaselineConfig)
describeModelExplainabilityJobDefinitionResponse_modelExplainabilityBaselineConfig = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {modelExplainabilityBaselineConfig} -> modelExplainabilityBaselineConfig) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {modelExplainabilityBaselineConfig = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | Undocumented member.
describeModelExplainabilityJobDefinitionResponse_stoppingCondition :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse (Prelude.Maybe MonitoringStoppingCondition)
describeModelExplainabilityJobDefinitionResponse_stoppingCondition = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {stoppingCondition = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | The response's http status code.
describeModelExplainabilityJobDefinitionResponse_httpStatus :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse Prelude.Int
describeModelExplainabilityJobDefinitionResponse_httpStatus = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {httpStatus = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the model explainability job.
describeModelExplainabilityJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse Prelude.Text
describeModelExplainabilityJobDefinitionResponse_jobDefinitionArn = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | The name of the explainability job definition. The name must be unique
-- within an Amazon Web Services Region in the Amazon Web Services account.
describeModelExplainabilityJobDefinitionResponse_jobDefinitionName :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse Prelude.Text
describeModelExplainabilityJobDefinitionResponse_jobDefinitionName = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {jobDefinitionName = a} :: DescribeModelExplainabilityJobDefinitionResponse)

-- | The time at which the model explainability job was created.
describeModelExplainabilityJobDefinitionResponse_creationTime :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse Prelude.UTCTime
describeModelExplainabilityJobDefinitionResponse_creationTime = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {creationTime} -> creationTime) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {creationTime = a} :: DescribeModelExplainabilityJobDefinitionResponse) Prelude.. Core._Time

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

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role that has read permission to the input data
-- location and write permission to the output data location in Amazon S3.
describeModelExplainabilityJobDefinitionResponse_roleArn :: Lens.Lens' DescribeModelExplainabilityJobDefinitionResponse Prelude.Text
describeModelExplainabilityJobDefinitionResponse_roleArn = Lens.lens (\DescribeModelExplainabilityJobDefinitionResponse' {roleArn} -> roleArn) (\s@DescribeModelExplainabilityJobDefinitionResponse' {} a -> s {roleArn = a} :: DescribeModelExplainabilityJobDefinitionResponse)

instance
  Prelude.NFData
    DescribeModelExplainabilityJobDefinitionResponse
