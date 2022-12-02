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
-- Module      : Amazonka.Synthetics.UpdateCanary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of a canary that has already been created.
--
-- You can\'t use this operation to update the tags of an existing canary.
-- To change the tags of an existing canary, use
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_TagResource.html TagResource>.
module Amazonka.Synthetics.UpdateCanary
  ( -- * Creating a Request
    UpdateCanary (..),
    newUpdateCanary,

    -- * Request Lenses
    updateCanary_schedule,
    updateCanary_code,
    updateCanary_vpcConfig,
    updateCanary_visualReference,
    updateCanary_artifactConfig,
    updateCanary_failureRetentionPeriodInDays,
    updateCanary_successRetentionPeriodInDays,
    updateCanary_executionRoleArn,
    updateCanary_runConfig,
    updateCanary_artifactS3Location,
    updateCanary_runtimeVersion,
    updateCanary_name,

    -- * Destructuring the Response
    UpdateCanaryResponse (..),
    newUpdateCanaryResponse,

    -- * Response Lenses
    updateCanaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newUpdateCanary' smart constructor.
data UpdateCanary = UpdateCanary'
  { -- | A structure that contains information about how often the canary is to
    -- run, and when these runs are to stop.
    schedule :: Prelude.Maybe CanaryScheduleInput,
    -- | A structure that includes the entry point from which the canary should
    -- start running your script. If the script is stored in an S3 bucket, the
    -- bucket name, key, and version are also included.
    code :: Prelude.Maybe CanaryCodeInput,
    -- | If this canary is to test an endpoint in a VPC, this structure contains
    -- information about the subnet and security groups of the VPC endpoint.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_VPC.html Running a Canary in a VPC>.
    vpcConfig :: Prelude.Maybe VpcConfigInput,
    -- | Defines the screenshots to use as the baseline for comparisons during
    -- visual monitoring comparisons during future runs of this canary. If you
    -- omit this parameter, no changes are made to any baseline screenshots
    -- that the canary might be using already.
    --
    -- Visual monitoring is supported only on canaries running the
    -- __syn-puppeteer-node-3.2__ runtime or later. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Library_SyntheticsLogger_VisualTesting.html Visual monitoring>
    -- and
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Blueprints_VisualTesting.html Visual monitoring blueprint>
    visualReference :: Prelude.Maybe VisualReferenceInput,
    -- | A structure that contains the configuration for canary artifacts,
    -- including the encryption-at-rest settings for artifacts that the canary
    -- uploads to Amazon S3.
    artifactConfig :: Prelude.Maybe ArtifactConfigInput,
    -- | The number of days to retain data about failed runs of this canary.
    failureRetentionPeriodInDays :: Prelude.Maybe Prelude.Natural,
    -- | The number of days to retain data about successful runs of this canary.
    successRetentionPeriodInDays :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the IAM role to be used to run the canary. This role must
    -- already exist, and must include @lambda.amazonaws.com@ as a principal in
    -- the trust policy. The role must also have the following permissions:
    --
    -- -   @s3:PutObject@
    --
    -- -   @s3:GetBucketLocation@
    --
    -- -   @s3:ListAllMyBuckets@
    --
    -- -   @cloudwatch:PutMetricData@
    --
    -- -   @logs:CreateLogGroup@
    --
    -- -   @logs:CreateLogStream@
    --
    -- -   @logs:CreateLogStream@
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains the timeout value that is used for each
    -- individual run of the canary.
    --
    -- The environment variables keys and values are not encrypted. Do not
    -- store sensitive information in this field.
    runConfig :: Prelude.Maybe CanaryRunConfigInput,
    -- | The location in Amazon S3 where Synthetics stores artifacts from the
    -- test runs of this canary. Artifacts include the log file, screenshots,
    -- and HAR files. The name of the S3 bucket can\'t include a period (.).
    artifactS3Location :: Prelude.Maybe Prelude.Text,
    -- | Specifies the runtime version to use for the canary. For a list of valid
    -- runtime versions and for more information about runtime versions, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Library.html Canary Runtime Versions>.
    runtimeVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the canary that you want to update. To find the names of
    -- your canaries, use
    -- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html DescribeCanaries>.
    --
    -- You cannot change the name of a canary that has already been created.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCanary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schedule', 'updateCanary_schedule' - A structure that contains information about how often the canary is to
-- run, and when these runs are to stop.
--
-- 'code', 'updateCanary_code' - A structure that includes the entry point from which the canary should
-- start running your script. If the script is stored in an S3 bucket, the
-- bucket name, key, and version are also included.
--
-- 'vpcConfig', 'updateCanary_vpcConfig' - If this canary is to test an endpoint in a VPC, this structure contains
-- information about the subnet and security groups of the VPC endpoint.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_VPC.html Running a Canary in a VPC>.
--
-- 'visualReference', 'updateCanary_visualReference' - Defines the screenshots to use as the baseline for comparisons during
-- visual monitoring comparisons during future runs of this canary. If you
-- omit this parameter, no changes are made to any baseline screenshots
-- that the canary might be using already.
--
-- Visual monitoring is supported only on canaries running the
-- __syn-puppeteer-node-3.2__ runtime or later. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Library_SyntheticsLogger_VisualTesting.html Visual monitoring>
-- and
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Blueprints_VisualTesting.html Visual monitoring blueprint>
--
-- 'artifactConfig', 'updateCanary_artifactConfig' - A structure that contains the configuration for canary artifacts,
-- including the encryption-at-rest settings for artifacts that the canary
-- uploads to Amazon S3.
--
-- 'failureRetentionPeriodInDays', 'updateCanary_failureRetentionPeriodInDays' - The number of days to retain data about failed runs of this canary.
--
-- 'successRetentionPeriodInDays', 'updateCanary_successRetentionPeriodInDays' - The number of days to retain data about successful runs of this canary.
--
-- 'executionRoleArn', 'updateCanary_executionRoleArn' - The ARN of the IAM role to be used to run the canary. This role must
-- already exist, and must include @lambda.amazonaws.com@ as a principal in
-- the trust policy. The role must also have the following permissions:
--
-- -   @s3:PutObject@
--
-- -   @s3:GetBucketLocation@
--
-- -   @s3:ListAllMyBuckets@
--
-- -   @cloudwatch:PutMetricData@
--
-- -   @logs:CreateLogGroup@
--
-- -   @logs:CreateLogStream@
--
-- -   @logs:CreateLogStream@
--
-- 'runConfig', 'updateCanary_runConfig' - A structure that contains the timeout value that is used for each
-- individual run of the canary.
--
-- The environment variables keys and values are not encrypted. Do not
-- store sensitive information in this field.
--
-- 'artifactS3Location', 'updateCanary_artifactS3Location' - The location in Amazon S3 where Synthetics stores artifacts from the
-- test runs of this canary. Artifacts include the log file, screenshots,
-- and HAR files. The name of the S3 bucket can\'t include a period (.).
--
-- 'runtimeVersion', 'updateCanary_runtimeVersion' - Specifies the runtime version to use for the canary. For a list of valid
-- runtime versions and for more information about runtime versions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Library.html Canary Runtime Versions>.
--
-- 'name', 'updateCanary_name' - The name of the canary that you want to update. To find the names of
-- your canaries, use
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html DescribeCanaries>.
--
-- You cannot change the name of a canary that has already been created.
newUpdateCanary ::
  -- | 'name'
  Prelude.Text ->
  UpdateCanary
newUpdateCanary pName_ =
  UpdateCanary'
    { schedule = Prelude.Nothing,
      code = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      visualReference = Prelude.Nothing,
      artifactConfig = Prelude.Nothing,
      failureRetentionPeriodInDays = Prelude.Nothing,
      successRetentionPeriodInDays = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      runConfig = Prelude.Nothing,
      artifactS3Location = Prelude.Nothing,
      runtimeVersion = Prelude.Nothing,
      name = pName_
    }

-- | A structure that contains information about how often the canary is to
-- run, and when these runs are to stop.
updateCanary_schedule :: Lens.Lens' UpdateCanary (Prelude.Maybe CanaryScheduleInput)
updateCanary_schedule = Lens.lens (\UpdateCanary' {schedule} -> schedule) (\s@UpdateCanary' {} a -> s {schedule = a} :: UpdateCanary)

-- | A structure that includes the entry point from which the canary should
-- start running your script. If the script is stored in an S3 bucket, the
-- bucket name, key, and version are also included.
updateCanary_code :: Lens.Lens' UpdateCanary (Prelude.Maybe CanaryCodeInput)
updateCanary_code = Lens.lens (\UpdateCanary' {code} -> code) (\s@UpdateCanary' {} a -> s {code = a} :: UpdateCanary)

-- | If this canary is to test an endpoint in a VPC, this structure contains
-- information about the subnet and security groups of the VPC endpoint.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_VPC.html Running a Canary in a VPC>.
updateCanary_vpcConfig :: Lens.Lens' UpdateCanary (Prelude.Maybe VpcConfigInput)
updateCanary_vpcConfig = Lens.lens (\UpdateCanary' {vpcConfig} -> vpcConfig) (\s@UpdateCanary' {} a -> s {vpcConfig = a} :: UpdateCanary)

-- | Defines the screenshots to use as the baseline for comparisons during
-- visual monitoring comparisons during future runs of this canary. If you
-- omit this parameter, no changes are made to any baseline screenshots
-- that the canary might be using already.
--
-- Visual monitoring is supported only on canaries running the
-- __syn-puppeteer-node-3.2__ runtime or later. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Library_SyntheticsLogger_VisualTesting.html Visual monitoring>
-- and
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Blueprints_VisualTesting.html Visual monitoring blueprint>
updateCanary_visualReference :: Lens.Lens' UpdateCanary (Prelude.Maybe VisualReferenceInput)
updateCanary_visualReference = Lens.lens (\UpdateCanary' {visualReference} -> visualReference) (\s@UpdateCanary' {} a -> s {visualReference = a} :: UpdateCanary)

-- | A structure that contains the configuration for canary artifacts,
-- including the encryption-at-rest settings for artifacts that the canary
-- uploads to Amazon S3.
updateCanary_artifactConfig :: Lens.Lens' UpdateCanary (Prelude.Maybe ArtifactConfigInput)
updateCanary_artifactConfig = Lens.lens (\UpdateCanary' {artifactConfig} -> artifactConfig) (\s@UpdateCanary' {} a -> s {artifactConfig = a} :: UpdateCanary)

-- | The number of days to retain data about failed runs of this canary.
updateCanary_failureRetentionPeriodInDays :: Lens.Lens' UpdateCanary (Prelude.Maybe Prelude.Natural)
updateCanary_failureRetentionPeriodInDays = Lens.lens (\UpdateCanary' {failureRetentionPeriodInDays} -> failureRetentionPeriodInDays) (\s@UpdateCanary' {} a -> s {failureRetentionPeriodInDays = a} :: UpdateCanary)

-- | The number of days to retain data about successful runs of this canary.
updateCanary_successRetentionPeriodInDays :: Lens.Lens' UpdateCanary (Prelude.Maybe Prelude.Natural)
updateCanary_successRetentionPeriodInDays = Lens.lens (\UpdateCanary' {successRetentionPeriodInDays} -> successRetentionPeriodInDays) (\s@UpdateCanary' {} a -> s {successRetentionPeriodInDays = a} :: UpdateCanary)

-- | The ARN of the IAM role to be used to run the canary. This role must
-- already exist, and must include @lambda.amazonaws.com@ as a principal in
-- the trust policy. The role must also have the following permissions:
--
-- -   @s3:PutObject@
--
-- -   @s3:GetBucketLocation@
--
-- -   @s3:ListAllMyBuckets@
--
-- -   @cloudwatch:PutMetricData@
--
-- -   @logs:CreateLogGroup@
--
-- -   @logs:CreateLogStream@
--
-- -   @logs:CreateLogStream@
updateCanary_executionRoleArn :: Lens.Lens' UpdateCanary (Prelude.Maybe Prelude.Text)
updateCanary_executionRoleArn = Lens.lens (\UpdateCanary' {executionRoleArn} -> executionRoleArn) (\s@UpdateCanary' {} a -> s {executionRoleArn = a} :: UpdateCanary)

-- | A structure that contains the timeout value that is used for each
-- individual run of the canary.
--
-- The environment variables keys and values are not encrypted. Do not
-- store sensitive information in this field.
updateCanary_runConfig :: Lens.Lens' UpdateCanary (Prelude.Maybe CanaryRunConfigInput)
updateCanary_runConfig = Lens.lens (\UpdateCanary' {runConfig} -> runConfig) (\s@UpdateCanary' {} a -> s {runConfig = a} :: UpdateCanary)

-- | The location in Amazon S3 where Synthetics stores artifacts from the
-- test runs of this canary. Artifacts include the log file, screenshots,
-- and HAR files. The name of the S3 bucket can\'t include a period (.).
updateCanary_artifactS3Location :: Lens.Lens' UpdateCanary (Prelude.Maybe Prelude.Text)
updateCanary_artifactS3Location = Lens.lens (\UpdateCanary' {artifactS3Location} -> artifactS3Location) (\s@UpdateCanary' {} a -> s {artifactS3Location = a} :: UpdateCanary)

-- | Specifies the runtime version to use for the canary. For a list of valid
-- runtime versions and for more information about runtime versions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Library.html Canary Runtime Versions>.
updateCanary_runtimeVersion :: Lens.Lens' UpdateCanary (Prelude.Maybe Prelude.Text)
updateCanary_runtimeVersion = Lens.lens (\UpdateCanary' {runtimeVersion} -> runtimeVersion) (\s@UpdateCanary' {} a -> s {runtimeVersion = a} :: UpdateCanary)

-- | The name of the canary that you want to update. To find the names of
-- your canaries, use
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html DescribeCanaries>.
--
-- You cannot change the name of a canary that has already been created.
updateCanary_name :: Lens.Lens' UpdateCanary Prelude.Text
updateCanary_name = Lens.lens (\UpdateCanary' {name} -> name) (\s@UpdateCanary' {} a -> s {name = a} :: UpdateCanary)

instance Core.AWSRequest UpdateCanary where
  type AWSResponse UpdateCanary = UpdateCanaryResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCanaryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCanary where
  hashWithSalt _salt UpdateCanary' {..} =
    _salt `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` visualReference
      `Prelude.hashWithSalt` artifactConfig
      `Prelude.hashWithSalt` failureRetentionPeriodInDays
      `Prelude.hashWithSalt` successRetentionPeriodInDays
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` runConfig
      `Prelude.hashWithSalt` artifactS3Location
      `Prelude.hashWithSalt` runtimeVersion
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateCanary where
  rnf UpdateCanary' {..} =
    Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf visualReference
      `Prelude.seq` Prelude.rnf artifactConfig
      `Prelude.seq` Prelude.rnf failureRetentionPeriodInDays
      `Prelude.seq` Prelude.rnf successRetentionPeriodInDays
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf runConfig
      `Prelude.seq` Prelude.rnf artifactS3Location
      `Prelude.seq` Prelude.rnf runtimeVersion
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateCanary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCanary where
  toJSON UpdateCanary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Schedule" Data..=) Prelude.<$> schedule,
            ("Code" Data..=) Prelude.<$> code,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            ("VisualReference" Data..=)
              Prelude.<$> visualReference,
            ("ArtifactConfig" Data..=)
              Prelude.<$> artifactConfig,
            ("FailureRetentionPeriodInDays" Data..=)
              Prelude.<$> failureRetentionPeriodInDays,
            ("SuccessRetentionPeriodInDays" Data..=)
              Prelude.<$> successRetentionPeriodInDays,
            ("ExecutionRoleArn" Data..=)
              Prelude.<$> executionRoleArn,
            ("RunConfig" Data..=) Prelude.<$> runConfig,
            ("ArtifactS3Location" Data..=)
              Prelude.<$> artifactS3Location,
            ("RuntimeVersion" Data..=)
              Prelude.<$> runtimeVersion
          ]
      )

instance Data.ToPath UpdateCanary where
  toPath UpdateCanary' {..} =
    Prelude.mconcat ["/canary/", Data.toBS name]

instance Data.ToQuery UpdateCanary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCanaryResponse' smart constructor.
data UpdateCanaryResponse = UpdateCanaryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCanaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCanaryResponse_httpStatus' - The response's http status code.
newUpdateCanaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCanaryResponse
newUpdateCanaryResponse pHttpStatus_ =
  UpdateCanaryResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateCanaryResponse_httpStatus :: Lens.Lens' UpdateCanaryResponse Prelude.Int
updateCanaryResponse_httpStatus = Lens.lens (\UpdateCanaryResponse' {httpStatus} -> httpStatus) (\s@UpdateCanaryResponse' {} a -> s {httpStatus = a} :: UpdateCanaryResponse)

instance Prelude.NFData UpdateCanaryResponse where
  rnf UpdateCanaryResponse' {..} =
    Prelude.rnf httpStatus
