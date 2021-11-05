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
-- Module      : Amazonka.SageMaker.DescribeDataQualityJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a data quality monitoring job definition.
module Amazonka.SageMaker.DescribeDataQualityJobDefinition
  ( -- * Creating a Request
    DescribeDataQualityJobDefinition (..),
    newDescribeDataQualityJobDefinition,

    -- * Request Lenses
    describeDataQualityJobDefinition_jobDefinitionName,

    -- * Destructuring the Response
    DescribeDataQualityJobDefinitionResponse (..),
    newDescribeDataQualityJobDefinitionResponse,

    -- * Response Lenses
    describeDataQualityJobDefinitionResponse_dataQualityBaselineConfig,
    describeDataQualityJobDefinitionResponse_stoppingCondition,
    describeDataQualityJobDefinitionResponse_networkConfig,
    describeDataQualityJobDefinitionResponse_httpStatus,
    describeDataQualityJobDefinitionResponse_jobDefinitionArn,
    describeDataQualityJobDefinitionResponse_jobDefinitionName,
    describeDataQualityJobDefinitionResponse_creationTime,
    describeDataQualityJobDefinitionResponse_dataQualityAppSpecification,
    describeDataQualityJobDefinitionResponse_dataQualityJobInput,
    describeDataQualityJobDefinitionResponse_dataQualityJobOutputConfig,
    describeDataQualityJobDefinitionResponse_jobResources,
    describeDataQualityJobDefinitionResponse_roleArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeDataQualityJobDefinition' smart constructor.
data DescribeDataQualityJobDefinition = DescribeDataQualityJobDefinition'
  { -- | The name of the data quality monitoring job definition to describe.
    jobDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataQualityJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinitionName', 'describeDataQualityJobDefinition_jobDefinitionName' - The name of the data quality monitoring job definition to describe.
newDescribeDataQualityJobDefinition ::
  -- | 'jobDefinitionName'
  Prelude.Text ->
  DescribeDataQualityJobDefinition
newDescribeDataQualityJobDefinition
  pJobDefinitionName_ =
    DescribeDataQualityJobDefinition'
      { jobDefinitionName =
          pJobDefinitionName_
      }

-- | The name of the data quality monitoring job definition to describe.
describeDataQualityJobDefinition_jobDefinitionName :: Lens.Lens' DescribeDataQualityJobDefinition Prelude.Text
describeDataQualityJobDefinition_jobDefinitionName = Lens.lens (\DescribeDataQualityJobDefinition' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeDataQualityJobDefinition' {} a -> s {jobDefinitionName = a} :: DescribeDataQualityJobDefinition)

instance
  Core.AWSRequest
    DescribeDataQualityJobDefinition
  where
  type
    AWSResponse DescribeDataQualityJobDefinition =
      DescribeDataQualityJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataQualityJobDefinitionResponse'
            Prelude.<$> (x Core..?> "DataQualityBaselineConfig")
            Prelude.<*> (x Core..?> "StoppingCondition")
            Prelude.<*> (x Core..?> "NetworkConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "JobDefinitionArn")
            Prelude.<*> (x Core..:> "JobDefinitionName")
            Prelude.<*> (x Core..:> "CreationTime")
            Prelude.<*> (x Core..:> "DataQualityAppSpecification")
            Prelude.<*> (x Core..:> "DataQualityJobInput")
            Prelude.<*> (x Core..:> "DataQualityJobOutputConfig")
            Prelude.<*> (x Core..:> "JobResources")
            Prelude.<*> (x Core..:> "RoleArn")
      )

instance
  Prelude.Hashable
    DescribeDataQualityJobDefinition

instance
  Prelude.NFData
    DescribeDataQualityJobDefinition

instance
  Core.ToHeaders
    DescribeDataQualityJobDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeDataQualityJobDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDataQualityJobDefinition where
  toJSON DescribeDataQualityJobDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("JobDefinitionName" Core..= jobDefinitionName)
          ]
      )

instance Core.ToPath DescribeDataQualityJobDefinition where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeDataQualityJobDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataQualityJobDefinitionResponse' smart constructor.
data DescribeDataQualityJobDefinitionResponse = DescribeDataQualityJobDefinitionResponse'
  { -- | The constraints and baselines for the data quality monitoring job
    -- definition.
    dataQualityBaselineConfig :: Prelude.Maybe DataQualityBaselineConfig,
    stoppingCondition :: Prelude.Maybe MonitoringStoppingCondition,
    -- | The networking configuration for the data quality monitoring job.
    networkConfig :: Prelude.Maybe MonitoringNetworkConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the data quality monitoring job
    -- definition.
    jobDefinitionArn :: Prelude.Text,
    -- | The name of the data quality monitoring job definition.
    jobDefinitionName :: Prelude.Text,
    -- | The time that the data quality monitoring job definition was created.
    creationTime :: Core.POSIX,
    -- | Information about the container that runs the data quality monitoring
    -- job.
    dataQualityAppSpecification :: DataQualityAppSpecification,
    -- | The list of inputs for the data quality monitoring job. Currently
    -- endpoints are supported.
    dataQualityJobInput :: DataQualityJobInput,
    dataQualityJobOutputConfig :: MonitoringOutputConfig,
    jobResources :: MonitoringResources,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataQualityJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataQualityBaselineConfig', 'describeDataQualityJobDefinitionResponse_dataQualityBaselineConfig' - The constraints and baselines for the data quality monitoring job
-- definition.
--
-- 'stoppingCondition', 'describeDataQualityJobDefinitionResponse_stoppingCondition' - Undocumented member.
--
-- 'networkConfig', 'describeDataQualityJobDefinitionResponse_networkConfig' - The networking configuration for the data quality monitoring job.
--
-- 'httpStatus', 'describeDataQualityJobDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionArn', 'describeDataQualityJobDefinitionResponse_jobDefinitionArn' - The Amazon Resource Name (ARN) of the data quality monitoring job
-- definition.
--
-- 'jobDefinitionName', 'describeDataQualityJobDefinitionResponse_jobDefinitionName' - The name of the data quality monitoring job definition.
--
-- 'creationTime', 'describeDataQualityJobDefinitionResponse_creationTime' - The time that the data quality monitoring job definition was created.
--
-- 'dataQualityAppSpecification', 'describeDataQualityJobDefinitionResponse_dataQualityAppSpecification' - Information about the container that runs the data quality monitoring
-- job.
--
-- 'dataQualityJobInput', 'describeDataQualityJobDefinitionResponse_dataQualityJobInput' - The list of inputs for the data quality monitoring job. Currently
-- endpoints are supported.
--
-- 'dataQualityJobOutputConfig', 'describeDataQualityJobDefinitionResponse_dataQualityJobOutputConfig' - Undocumented member.
--
-- 'jobResources', 'describeDataQualityJobDefinitionResponse_jobResources' - Undocumented member.
--
-- 'roleArn', 'describeDataQualityJobDefinitionResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
newDescribeDataQualityJobDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobDefinitionArn'
  Prelude.Text ->
  -- | 'jobDefinitionName'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
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
  DescribeDataQualityJobDefinitionResponse
newDescribeDataQualityJobDefinitionResponse
  pHttpStatus_
  pJobDefinitionArn_
  pJobDefinitionName_
  pCreationTime_
  pDataQualityAppSpecification_
  pDataQualityJobInput_
  pDataQualityJobOutputConfig_
  pJobResources_
  pRoleArn_ =
    DescribeDataQualityJobDefinitionResponse'
      { dataQualityBaselineConfig =
          Prelude.Nothing,
        stoppingCondition =
          Prelude.Nothing,
        networkConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        jobDefinitionArn =
          pJobDefinitionArn_,
        jobDefinitionName =
          pJobDefinitionName_,
        creationTime =
          Core._Time
            Lens.# pCreationTime_,
        dataQualityAppSpecification =
          pDataQualityAppSpecification_,
        dataQualityJobInput =
          pDataQualityJobInput_,
        dataQualityJobOutputConfig =
          pDataQualityJobOutputConfig_,
        jobResources = pJobResources_,
        roleArn = pRoleArn_
      }

-- | The constraints and baselines for the data quality monitoring job
-- definition.
describeDataQualityJobDefinitionResponse_dataQualityBaselineConfig :: Lens.Lens' DescribeDataQualityJobDefinitionResponse (Prelude.Maybe DataQualityBaselineConfig)
describeDataQualityJobDefinitionResponse_dataQualityBaselineConfig = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {dataQualityBaselineConfig} -> dataQualityBaselineConfig) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {dataQualityBaselineConfig = a} :: DescribeDataQualityJobDefinitionResponse)

-- | Undocumented member.
describeDataQualityJobDefinitionResponse_stoppingCondition :: Lens.Lens' DescribeDataQualityJobDefinitionResponse (Prelude.Maybe MonitoringStoppingCondition)
describeDataQualityJobDefinitionResponse_stoppingCondition = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {stoppingCondition = a} :: DescribeDataQualityJobDefinitionResponse)

-- | The networking configuration for the data quality monitoring job.
describeDataQualityJobDefinitionResponse_networkConfig :: Lens.Lens' DescribeDataQualityJobDefinitionResponse (Prelude.Maybe MonitoringNetworkConfig)
describeDataQualityJobDefinitionResponse_networkConfig = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {networkConfig} -> networkConfig) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {networkConfig = a} :: DescribeDataQualityJobDefinitionResponse)

-- | The response's http status code.
describeDataQualityJobDefinitionResponse_httpStatus :: Lens.Lens' DescribeDataQualityJobDefinitionResponse Prelude.Int
describeDataQualityJobDefinitionResponse_httpStatus = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {httpStatus = a} :: DescribeDataQualityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the data quality monitoring job
-- definition.
describeDataQualityJobDefinitionResponse_jobDefinitionArn :: Lens.Lens' DescribeDataQualityJobDefinitionResponse Prelude.Text
describeDataQualityJobDefinitionResponse_jobDefinitionArn = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {jobDefinitionArn} -> jobDefinitionArn) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {jobDefinitionArn = a} :: DescribeDataQualityJobDefinitionResponse)

-- | The name of the data quality monitoring job definition.
describeDataQualityJobDefinitionResponse_jobDefinitionName :: Lens.Lens' DescribeDataQualityJobDefinitionResponse Prelude.Text
describeDataQualityJobDefinitionResponse_jobDefinitionName = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {jobDefinitionName = a} :: DescribeDataQualityJobDefinitionResponse)

-- | The time that the data quality monitoring job definition was created.
describeDataQualityJobDefinitionResponse_creationTime :: Lens.Lens' DescribeDataQualityJobDefinitionResponse Prelude.UTCTime
describeDataQualityJobDefinitionResponse_creationTime = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {creationTime} -> creationTime) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {creationTime = a} :: DescribeDataQualityJobDefinitionResponse) Prelude.. Core._Time

-- | Information about the container that runs the data quality monitoring
-- job.
describeDataQualityJobDefinitionResponse_dataQualityAppSpecification :: Lens.Lens' DescribeDataQualityJobDefinitionResponse DataQualityAppSpecification
describeDataQualityJobDefinitionResponse_dataQualityAppSpecification = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {dataQualityAppSpecification} -> dataQualityAppSpecification) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {dataQualityAppSpecification = a} :: DescribeDataQualityJobDefinitionResponse)

-- | The list of inputs for the data quality monitoring job. Currently
-- endpoints are supported.
describeDataQualityJobDefinitionResponse_dataQualityJobInput :: Lens.Lens' DescribeDataQualityJobDefinitionResponse DataQualityJobInput
describeDataQualityJobDefinitionResponse_dataQualityJobInput = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {dataQualityJobInput} -> dataQualityJobInput) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {dataQualityJobInput = a} :: DescribeDataQualityJobDefinitionResponse)

-- | Undocumented member.
describeDataQualityJobDefinitionResponse_dataQualityJobOutputConfig :: Lens.Lens' DescribeDataQualityJobDefinitionResponse MonitoringOutputConfig
describeDataQualityJobDefinitionResponse_dataQualityJobOutputConfig = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {dataQualityJobOutputConfig} -> dataQualityJobOutputConfig) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {dataQualityJobOutputConfig = a} :: DescribeDataQualityJobDefinitionResponse)

-- | Undocumented member.
describeDataQualityJobDefinitionResponse_jobResources :: Lens.Lens' DescribeDataQualityJobDefinitionResponse MonitoringResources
describeDataQualityJobDefinitionResponse_jobResources = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {jobResources} -> jobResources) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {jobResources = a} :: DescribeDataQualityJobDefinitionResponse)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
describeDataQualityJobDefinitionResponse_roleArn :: Lens.Lens' DescribeDataQualityJobDefinitionResponse Prelude.Text
describeDataQualityJobDefinitionResponse_roleArn = Lens.lens (\DescribeDataQualityJobDefinitionResponse' {roleArn} -> roleArn) (\s@DescribeDataQualityJobDefinitionResponse' {} a -> s {roleArn = a} :: DescribeDataQualityJobDefinitionResponse)

instance
  Prelude.NFData
    DescribeDataQualityJobDefinitionResponse
