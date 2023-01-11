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
-- Module      : Amazonka.SageMaker.DescribeModelQualityJobDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a model quality job definition.
module Amazonka.SageMaker.DescribeModelQualityJobDefinition
  ( -- * Creating a Request
    DescribeModelQualityJobDefinition (..),
    newDescribeModelQualityJobDefinition,

    -- * Request Lenses
    describeModelQualityJobDefinition_jobDefinitionName,

    -- * Destructuring the Response
    DescribeModelQualityJobDefinitionResponse (..),
    newDescribeModelQualityJobDefinitionResponse,

    -- * Response Lenses
    describeModelQualityJobDefinitionResponse_modelQualityBaselineConfig,
    describeModelQualityJobDefinitionResponse_networkConfig,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeModelQualityJobDefinition' smart constructor.
data DescribeModelQualityJobDefinition = DescribeModelQualityJobDefinition'
  { -- | The name of the model quality job. The name must be unique within an
    -- Amazon Web Services Region in the Amazon Web Services account.
    jobDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelQualityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinitionName', 'describeModelQualityJobDefinition_jobDefinitionName' - The name of the model quality job. The name must be unique within an
-- Amazon Web Services Region in the Amazon Web Services account.
newDescribeModelQualityJobDefinition ::
  -- | 'jobDefinitionName'
  Prelude.Text ->
  DescribeModelQualityJobDefinition
newDescribeModelQualityJobDefinition
  pJobDefinitionName_ =
    DescribeModelQualityJobDefinition'
      { jobDefinitionName =
          pJobDefinitionName_
      }

-- | The name of the model quality job. The name must be unique within an
-- Amazon Web Services Region in the Amazon Web Services account.
describeModelQualityJobDefinition_jobDefinitionName :: Lens.Lens' DescribeModelQualityJobDefinition Prelude.Text
describeModelQualityJobDefinition_jobDefinitionName = Lens.lens (\DescribeModelQualityJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeModelQualityJobDefinition' {} a -> s {jobDefinitionName = a} :: DescribeModelQualityJobDefinition)

instance
  Core.AWSRequest
    DescribeModelQualityJobDefinition
  where
  type
    AWSResponse DescribeModelQualityJobDefinition =
      DescribeModelQualityJobDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelQualityJobDefinitionResponse'
            Prelude.<$> (x Data..?> "ModelQualityBaselineConfig")
              Prelude.<*> (x Data..?> "NetworkConfig")
              Prelude.<*> (x Data..?> "StoppingCondition")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "JobDefinitionArn")
              Prelude.<*> (x Data..:> "JobDefinitionName")
              Prelude.<*> (x Data..:> "CreationTime")
              Prelude.<*> (x Data..:> "ModelQualityAppSpecification")
              Prelude.<*> (x Data..:> "ModelQualityJobInput")
              Prelude.<*> (x Data..:> "ModelQualityJobOutputConfig")
              Prelude.<*> (x Data..:> "JobResources")
              Prelude.<*> (x Data..:> "RoleArn")
      )

instance
  Prelude.Hashable
    DescribeModelQualityJobDefinition
  where
  hashWithSalt
    _salt
    DescribeModelQualityJobDefinition' {..} =
      _salt `Prelude.hashWithSalt` jobDefinitionName

instance
  Prelude.NFData
    DescribeModelQualityJobDefinition
  where
  rnf DescribeModelQualityJobDefinition' {..} =
    Prelude.rnf jobDefinitionName

instance
  Data.ToHeaders
    DescribeModelQualityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeModelQualityJobDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeModelQualityJobDefinition
  where
  toJSON DescribeModelQualityJobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("JobDefinitionName" Data..= jobDefinitionName)
          ]
      )

instance
  Data.ToPath
    DescribeModelQualityJobDefinition
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeModelQualityJobDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelQualityJobDefinitionResponse' smart constructor.
data DescribeModelQualityJobDefinitionResponse = DescribeModelQualityJobDefinitionResponse'
  { -- | The baseline configuration for a model quality job.
    modelQualityBaselineConfig :: Prelude.Maybe ModelQualityBaselineConfig,
    -- | Networking options for a model quality job.
    networkConfig :: Prelude.Maybe MonitoringNetworkConfig,
    stoppingCondition :: Prelude.Maybe MonitoringStoppingCondition,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the model quality job.
    jobDefinitionArn :: Prelude.Text,
    -- | The name of the quality job definition. The name must be unique within
    -- an Amazon Web Services Region in the Amazon Web Services account.
    jobDefinitionName :: Prelude.Text,
    -- | The time at which the model quality job was created.
    creationTime :: Data.POSIX,
    -- | Configures the model quality job to run a specified Docker container
    -- image.
    modelQualityAppSpecification :: ModelQualityAppSpecification,
    -- | Inputs for the model quality job.
    modelQualityJobInput :: ModelQualityJobInput,
    modelQualityJobOutputConfig :: MonitoringOutputConfig,
    jobResources :: MonitoringResources,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelQualityJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelQualityBaselineConfig', 'describeModelQualityJobDefinitionResponse_modelQualityBaselineConfig' - The baseline configuration for a model quality job.
--
-- 'networkConfig', 'describeModelQualityJobDefinitionResponse_networkConfig' - Networking options for a model quality job.
--
-- 'stoppingCondition', 'describeModelQualityJobDefinitionResponse_stoppingCondition' - Undocumented member.
--
-- 'httpStatus', 'describeModelQualityJobDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionArn', 'describeModelQualityJobDefinitionResponse_jobDefinitionArn' - The Amazon Resource Name (ARN) of the model quality job.
--
-- 'jobDefinitionName', 'describeModelQualityJobDefinitionResponse_jobDefinitionName' - The name of the quality job definition. The name must be unique within
-- an Amazon Web Services Region in the Amazon Web Services account.
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
  Prelude.Int ->
  -- | 'jobDefinitionArn'
  Prelude.Text ->
  -- | 'jobDefinitionName'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
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
      { modelQualityBaselineConfig =
          Prelude.Nothing,
        networkConfig = Prelude.Nothing,
        stoppingCondition =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        jobDefinitionArn =
          pJobDefinitionArn_,
        jobDefinitionName =
          pJobDefinitionName_,
        creationTime =
          Data._Time
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

-- | The baseline configuration for a model quality job.
describeModelQualityJobDefinitionResponse_modelQualityBaselineConfig :: Lens.Lens' DescribeModelQualityJobDefinitionResponse (Prelude.Maybe ModelQualityBaselineConfig)
describeModelQualityJobDefinitionResponse_modelQualityBaselineConfig = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {modelQualityBaselineConfig} -> modelQualityBaselineConfig) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {modelQualityBaselineConfig = a} :: DescribeModelQualityJobDefinitionResponse)

-- | Networking options for a model quality job.
describeModelQualityJobDefinitionResponse_networkConfig :: Lens.Lens' DescribeModelQualityJobDefinitionResponse (Prelude.Maybe MonitoringNetworkConfig)
describeModelQualityJobDefinitionResponse_networkConfig = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {networkConfig} -> networkConfig) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {networkConfig = a} :: DescribeModelQualityJobDefinitionResponse)

-- | Undocumented member.
describeModelQualityJobDefinitionResponse_stoppingCondition :: Lens.Lens' DescribeModelQualityJobDefinitionResponse (Prelude.Maybe MonitoringStoppingCondition)
describeModelQualityJobDefinitionResponse_stoppingCondition = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {stoppingCondition = a} :: DescribeModelQualityJobDefinitionResponse)

-- | The response's http status code.
describeModelQualityJobDefinitionResponse_httpStatus :: Lens.Lens' DescribeModelQualityJobDefinitionResponse Prelude.Int
describeModelQualityJobDefinitionResponse_httpStatus = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {httpStatus = a} :: DescribeModelQualityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the model quality job.
describeModelQualityJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' DescribeModelQualityJobDefinitionResponse Prelude.Text
describeModelQualityJobDefinitionResponse_jobDefinitionArn = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: DescribeModelQualityJobDefinitionResponse)

-- | The name of the quality job definition. The name must be unique within
-- an Amazon Web Services Region in the Amazon Web Services account.
describeModelQualityJobDefinitionResponse_jobDefinitionName :: Lens.Lens' DescribeModelQualityJobDefinitionResponse Prelude.Text
describeModelQualityJobDefinitionResponse_jobDefinitionName = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {jobDefinitionName = a} :: DescribeModelQualityJobDefinitionResponse)

-- | The time at which the model quality job was created.
describeModelQualityJobDefinitionResponse_creationTime :: Lens.Lens' DescribeModelQualityJobDefinitionResponse Prelude.UTCTime
describeModelQualityJobDefinitionResponse_creationTime = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {creationTime} -> creationTime) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {creationTime = a} :: DescribeModelQualityJobDefinitionResponse) Prelude.. Data._Time

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
describeModelQualityJobDefinitionResponse_roleArn :: Lens.Lens' DescribeModelQualityJobDefinitionResponse Prelude.Text
describeModelQualityJobDefinitionResponse_roleArn = Lens.lens (\DescribeModelQualityJobDefinitionResponse' {roleArn} -> roleArn) (\s@DescribeModelQualityJobDefinitionResponse' {} a -> s {roleArn = a} :: DescribeModelQualityJobDefinitionResponse)

instance
  Prelude.NFData
    DescribeModelQualityJobDefinitionResponse
  where
  rnf DescribeModelQualityJobDefinitionResponse' {..} =
    Prelude.rnf modelQualityBaselineConfig
      `Prelude.seq` Prelude.rnf networkConfig
      `Prelude.seq` Prelude.rnf stoppingCondition
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobDefinitionArn
      `Prelude.seq` Prelude.rnf jobDefinitionName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf modelQualityAppSpecification
      `Prelude.seq` Prelude.rnf modelQualityJobInput
      `Prelude.seq` Prelude.rnf modelQualityJobOutputConfig
      `Prelude.seq` Prelude.rnf jobResources
      `Prelude.seq` Prelude.rnf roleArn
