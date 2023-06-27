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
-- Module      : Amazonka.SageMaker.CreateAutoMLJobV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Autopilot job also referred to as Autopilot experiment or
-- AutoML job V2.
--
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateAutoMLJobV2.html CreateAutoMLJobV2>
-- and
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeAutoMLJobV2.html DescribeAutoMLJobV2>
-- are new versions of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateAutoMLJob.html CreateAutoMLJob>
-- and
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeAutoMLJob.html DescribeAutoMLJob>
-- which offer backward compatibility.
--
-- @CreateAutoMLJobV2@ can manage tabular problem types identical to those
-- of its previous version @CreateAutoMLJob@, as well as non-tabular
-- problem types such as image or text classification.
--
-- Find guidelines about how to migrate a @CreateAutoMLJob@ to
-- @CreateAutoMLJobV2@ in
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-automate-model-development-create-experiment-api.html#autopilot-create-experiment-api-migrate-v1-v2 Migrate a CreateAutoMLJob to CreateAutoMLJobV2>.
--
-- For the list of available problem types supported by
-- @CreateAutoMLJobV2@, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLProblemTypeConfig.html AutoMLProblemTypeConfig>.
--
-- You can find the best-performing model after you run an AutoML job V2 by
-- calling
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeAutoMLJobV2.html DescribeAutoMLJobV2>.
module Amazonka.SageMaker.CreateAutoMLJobV2
  ( -- * Creating a Request
    CreateAutoMLJobV2 (..),
    newCreateAutoMLJobV2,

    -- * Request Lenses
    createAutoMLJobV2_autoMLJobObjective,
    createAutoMLJobV2_dataSplitConfig,
    createAutoMLJobV2_modelDeployConfig,
    createAutoMLJobV2_securityConfig,
    createAutoMLJobV2_tags,
    createAutoMLJobV2_autoMLJobName,
    createAutoMLJobV2_autoMLJobInputDataConfig,
    createAutoMLJobV2_outputDataConfig,
    createAutoMLJobV2_autoMLProblemTypeConfig,
    createAutoMLJobV2_roleArn,

    -- * Destructuring the Response
    CreateAutoMLJobV2Response (..),
    newCreateAutoMLJobV2Response,

    -- * Response Lenses
    createAutoMLJobV2Response_httpStatus,
    createAutoMLJobV2Response_autoMLJobArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateAutoMLJobV2' smart constructor.
data CreateAutoMLJobV2 = CreateAutoMLJobV2'
  { -- | Specifies a metric to minimize or maximize as the objective of a job. If
    -- not specified, the default objective metric depends on the problem type.
    -- For the list of default values per problem type, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLJobObjective.html AutoMLJobObjective>.
    --
    -- For tabular problem types, you must either provide both the
    -- @AutoMLJobObjective@ and indicate the type of supervised learning
    -- problem in @AutoMLProblemTypeConfig@ (@TabularJobConfig.ProblemType@),
    -- or none at all.
    autoMLJobObjective :: Prelude.Maybe AutoMLJobObjective,
    -- | This structure specifies how to split the data into train and validation
    -- datasets.
    --
    -- The validation and training datasets must contain the same headers. For
    -- jobs created by calling @CreateAutoMLJob@, the validation dataset must
    -- be less than 2 GB in size.
    dataSplitConfig :: Prelude.Maybe AutoMLDataSplitConfig,
    -- | Specifies how to generate the endpoint name for an automatic one-click
    -- Autopilot model deployment.
    modelDeployConfig :: Prelude.Maybe ModelDeployConfig,
    -- | The security configuration for traffic encryption or Amazon VPC
    -- settings.
    securityConfig :: Prelude.Maybe AutoMLSecurityConfig,
    -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, such as by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web ServicesResources>.
    -- Tag keys must be unique per resource.
    tags :: Prelude.Maybe [Tag],
    -- | Identifies an Autopilot job. The name must be unique to your account and
    -- is case insensitive.
    autoMLJobName :: Prelude.Text,
    -- | An array of channel objects describing the input data and their
    -- location. Each channel is a named input source. Similar to the
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateAutoMLJob.html#sagemaker-CreateAutoMLJob-request-InputDataConfig InputDataConfig>
    -- attribute in the @CreateAutoMLJob@ input parameters. The supported
    -- formats depend on the problem type:
    --
    -- -   For Tabular problem types: @S3Prefix@, @ManifestFile@.
    --
    -- -   For ImageClassification: @S3Prefix@, @ManifestFile@,
    --     @AugmentedManifestFile@.
    --
    -- -   For TextClassification: @S3Prefix@.
    autoMLJobInputDataConfig :: Prelude.NonEmpty AutoMLJobChannel,
    -- | Provides information about encryption and the Amazon S3 output path
    -- needed to store artifacts from an AutoML job.
    outputDataConfig :: AutoMLOutputDataConfig,
    -- | Defines the configuration settings of one of the supported problem
    -- types.
    autoMLProblemTypeConfig :: AutoMLProblemTypeConfig,
    -- | The ARN of the role that is used to access the data.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAutoMLJobV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoMLJobObjective', 'createAutoMLJobV2_autoMLJobObjective' - Specifies a metric to minimize or maximize as the objective of a job. If
-- not specified, the default objective metric depends on the problem type.
-- For the list of default values per problem type, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLJobObjective.html AutoMLJobObjective>.
--
-- For tabular problem types, you must either provide both the
-- @AutoMLJobObjective@ and indicate the type of supervised learning
-- problem in @AutoMLProblemTypeConfig@ (@TabularJobConfig.ProblemType@),
-- or none at all.
--
-- 'dataSplitConfig', 'createAutoMLJobV2_dataSplitConfig' - This structure specifies how to split the data into train and validation
-- datasets.
--
-- The validation and training datasets must contain the same headers. For
-- jobs created by calling @CreateAutoMLJob@, the validation dataset must
-- be less than 2 GB in size.
--
-- 'modelDeployConfig', 'createAutoMLJobV2_modelDeployConfig' - Specifies how to generate the endpoint name for an automatic one-click
-- Autopilot model deployment.
--
-- 'securityConfig', 'createAutoMLJobV2_securityConfig' - The security configuration for traffic encryption or Amazon VPC
-- settings.
--
-- 'tags', 'createAutoMLJobV2_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, such as by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web ServicesResources>.
-- Tag keys must be unique per resource.
--
-- 'autoMLJobName', 'createAutoMLJobV2_autoMLJobName' - Identifies an Autopilot job. The name must be unique to your account and
-- is case insensitive.
--
-- 'autoMLJobInputDataConfig', 'createAutoMLJobV2_autoMLJobInputDataConfig' - An array of channel objects describing the input data and their
-- location. Each channel is a named input source. Similar to the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateAutoMLJob.html#sagemaker-CreateAutoMLJob-request-InputDataConfig InputDataConfig>
-- attribute in the @CreateAutoMLJob@ input parameters. The supported
-- formats depend on the problem type:
--
-- -   For Tabular problem types: @S3Prefix@, @ManifestFile@.
--
-- -   For ImageClassification: @S3Prefix@, @ManifestFile@,
--     @AugmentedManifestFile@.
--
-- -   For TextClassification: @S3Prefix@.
--
-- 'outputDataConfig', 'createAutoMLJobV2_outputDataConfig' - Provides information about encryption and the Amazon S3 output path
-- needed to store artifacts from an AutoML job.
--
-- 'autoMLProblemTypeConfig', 'createAutoMLJobV2_autoMLProblemTypeConfig' - Defines the configuration settings of one of the supported problem
-- types.
--
-- 'roleArn', 'createAutoMLJobV2_roleArn' - The ARN of the role that is used to access the data.
newCreateAutoMLJobV2 ::
  -- | 'autoMLJobName'
  Prelude.Text ->
  -- | 'autoMLJobInputDataConfig'
  Prelude.NonEmpty AutoMLJobChannel ->
  -- | 'outputDataConfig'
  AutoMLOutputDataConfig ->
  -- | 'autoMLProblemTypeConfig'
  AutoMLProblemTypeConfig ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateAutoMLJobV2
newCreateAutoMLJobV2
  pAutoMLJobName_
  pAutoMLJobInputDataConfig_
  pOutputDataConfig_
  pAutoMLProblemTypeConfig_
  pRoleArn_ =
    CreateAutoMLJobV2'
      { autoMLJobObjective =
          Prelude.Nothing,
        dataSplitConfig = Prelude.Nothing,
        modelDeployConfig = Prelude.Nothing,
        securityConfig = Prelude.Nothing,
        tags = Prelude.Nothing,
        autoMLJobName = pAutoMLJobName_,
        autoMLJobInputDataConfig =
          Lens.coerced Lens.# pAutoMLJobInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        autoMLProblemTypeConfig = pAutoMLProblemTypeConfig_,
        roleArn = pRoleArn_
      }

-- | Specifies a metric to minimize or maximize as the objective of a job. If
-- not specified, the default objective metric depends on the problem type.
-- For the list of default values per problem type, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLJobObjective.html AutoMLJobObjective>.
--
-- For tabular problem types, you must either provide both the
-- @AutoMLJobObjective@ and indicate the type of supervised learning
-- problem in @AutoMLProblemTypeConfig@ (@TabularJobConfig.ProblemType@),
-- or none at all.
createAutoMLJobV2_autoMLJobObjective :: Lens.Lens' CreateAutoMLJobV2 (Prelude.Maybe AutoMLJobObjective)
createAutoMLJobV2_autoMLJobObjective = Lens.lens (\CreateAutoMLJobV2' {autoMLJobObjective} -> autoMLJobObjective) (\s@CreateAutoMLJobV2' {} a -> s {autoMLJobObjective = a} :: CreateAutoMLJobV2)

-- | This structure specifies how to split the data into train and validation
-- datasets.
--
-- The validation and training datasets must contain the same headers. For
-- jobs created by calling @CreateAutoMLJob@, the validation dataset must
-- be less than 2 GB in size.
createAutoMLJobV2_dataSplitConfig :: Lens.Lens' CreateAutoMLJobV2 (Prelude.Maybe AutoMLDataSplitConfig)
createAutoMLJobV2_dataSplitConfig = Lens.lens (\CreateAutoMLJobV2' {dataSplitConfig} -> dataSplitConfig) (\s@CreateAutoMLJobV2' {} a -> s {dataSplitConfig = a} :: CreateAutoMLJobV2)

-- | Specifies how to generate the endpoint name for an automatic one-click
-- Autopilot model deployment.
createAutoMLJobV2_modelDeployConfig :: Lens.Lens' CreateAutoMLJobV2 (Prelude.Maybe ModelDeployConfig)
createAutoMLJobV2_modelDeployConfig = Lens.lens (\CreateAutoMLJobV2' {modelDeployConfig} -> modelDeployConfig) (\s@CreateAutoMLJobV2' {} a -> s {modelDeployConfig = a} :: CreateAutoMLJobV2)

-- | The security configuration for traffic encryption or Amazon VPC
-- settings.
createAutoMLJobV2_securityConfig :: Lens.Lens' CreateAutoMLJobV2 (Prelude.Maybe AutoMLSecurityConfig)
createAutoMLJobV2_securityConfig = Lens.lens (\CreateAutoMLJobV2' {securityConfig} -> securityConfig) (\s@CreateAutoMLJobV2' {} a -> s {securityConfig = a} :: CreateAutoMLJobV2)

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, such as by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web ServicesResources>.
-- Tag keys must be unique per resource.
createAutoMLJobV2_tags :: Lens.Lens' CreateAutoMLJobV2 (Prelude.Maybe [Tag])
createAutoMLJobV2_tags = Lens.lens (\CreateAutoMLJobV2' {tags} -> tags) (\s@CreateAutoMLJobV2' {} a -> s {tags = a} :: CreateAutoMLJobV2) Prelude.. Lens.mapping Lens.coerced

-- | Identifies an Autopilot job. The name must be unique to your account and
-- is case insensitive.
createAutoMLJobV2_autoMLJobName :: Lens.Lens' CreateAutoMLJobV2 Prelude.Text
createAutoMLJobV2_autoMLJobName = Lens.lens (\CreateAutoMLJobV2' {autoMLJobName} -> autoMLJobName) (\s@CreateAutoMLJobV2' {} a -> s {autoMLJobName = a} :: CreateAutoMLJobV2)

-- | An array of channel objects describing the input data and their
-- location. Each channel is a named input source. Similar to the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateAutoMLJob.html#sagemaker-CreateAutoMLJob-request-InputDataConfig InputDataConfig>
-- attribute in the @CreateAutoMLJob@ input parameters. The supported
-- formats depend on the problem type:
--
-- -   For Tabular problem types: @S3Prefix@, @ManifestFile@.
--
-- -   For ImageClassification: @S3Prefix@, @ManifestFile@,
--     @AugmentedManifestFile@.
--
-- -   For TextClassification: @S3Prefix@.
createAutoMLJobV2_autoMLJobInputDataConfig :: Lens.Lens' CreateAutoMLJobV2 (Prelude.NonEmpty AutoMLJobChannel)
createAutoMLJobV2_autoMLJobInputDataConfig = Lens.lens (\CreateAutoMLJobV2' {autoMLJobInputDataConfig} -> autoMLJobInputDataConfig) (\s@CreateAutoMLJobV2' {} a -> s {autoMLJobInputDataConfig = a} :: CreateAutoMLJobV2) Prelude.. Lens.coerced

-- | Provides information about encryption and the Amazon S3 output path
-- needed to store artifacts from an AutoML job.
createAutoMLJobV2_outputDataConfig :: Lens.Lens' CreateAutoMLJobV2 AutoMLOutputDataConfig
createAutoMLJobV2_outputDataConfig = Lens.lens (\CreateAutoMLJobV2' {outputDataConfig} -> outputDataConfig) (\s@CreateAutoMLJobV2' {} a -> s {outputDataConfig = a} :: CreateAutoMLJobV2)

-- | Defines the configuration settings of one of the supported problem
-- types.
createAutoMLJobV2_autoMLProblemTypeConfig :: Lens.Lens' CreateAutoMLJobV2 AutoMLProblemTypeConfig
createAutoMLJobV2_autoMLProblemTypeConfig = Lens.lens (\CreateAutoMLJobV2' {autoMLProblemTypeConfig} -> autoMLProblemTypeConfig) (\s@CreateAutoMLJobV2' {} a -> s {autoMLProblemTypeConfig = a} :: CreateAutoMLJobV2)

-- | The ARN of the role that is used to access the data.
createAutoMLJobV2_roleArn :: Lens.Lens' CreateAutoMLJobV2 Prelude.Text
createAutoMLJobV2_roleArn = Lens.lens (\CreateAutoMLJobV2' {roleArn} -> roleArn) (\s@CreateAutoMLJobV2' {} a -> s {roleArn = a} :: CreateAutoMLJobV2)

instance Core.AWSRequest CreateAutoMLJobV2 where
  type
    AWSResponse CreateAutoMLJobV2 =
      CreateAutoMLJobV2Response
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAutoMLJobV2Response'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "AutoMLJobArn")
      )

instance Prelude.Hashable CreateAutoMLJobV2 where
  hashWithSalt _salt CreateAutoMLJobV2' {..} =
    _salt
      `Prelude.hashWithSalt` autoMLJobObjective
      `Prelude.hashWithSalt` dataSplitConfig
      `Prelude.hashWithSalt` modelDeployConfig
      `Prelude.hashWithSalt` securityConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` autoMLJobName
      `Prelude.hashWithSalt` autoMLJobInputDataConfig
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` autoMLProblemTypeConfig
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateAutoMLJobV2 where
  rnf CreateAutoMLJobV2' {..} =
    Prelude.rnf autoMLJobObjective
      `Prelude.seq` Prelude.rnf dataSplitConfig
      `Prelude.seq` Prelude.rnf modelDeployConfig
      `Prelude.seq` Prelude.rnf securityConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf autoMLJobName
      `Prelude.seq` Prelude.rnf autoMLJobInputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf autoMLProblemTypeConfig
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreateAutoMLJobV2 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateAutoMLJobV2" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAutoMLJobV2 where
  toJSON CreateAutoMLJobV2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoMLJobObjective" Data..=)
              Prelude.<$> autoMLJobObjective,
            ("DataSplitConfig" Data..=)
              Prelude.<$> dataSplitConfig,
            ("ModelDeployConfig" Data..=)
              Prelude.<$> modelDeployConfig,
            ("SecurityConfig" Data..=)
              Prelude.<$> securityConfig,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("AutoMLJobName" Data..= autoMLJobName),
            Prelude.Just
              ( "AutoMLJobInputDataConfig"
                  Data..= autoMLJobInputDataConfig
              ),
            Prelude.Just
              ("OutputDataConfig" Data..= outputDataConfig),
            Prelude.Just
              ( "AutoMLProblemTypeConfig"
                  Data..= autoMLProblemTypeConfig
              ),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateAutoMLJobV2 where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAutoMLJobV2 where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAutoMLJobV2Response' smart constructor.
data CreateAutoMLJobV2Response = CreateAutoMLJobV2Response'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique ARN assigned to the AutoMLJob when it is created.
    autoMLJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAutoMLJobV2Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAutoMLJobV2Response_httpStatus' - The response's http status code.
--
-- 'autoMLJobArn', 'createAutoMLJobV2Response_autoMLJobArn' - The unique ARN assigned to the AutoMLJob when it is created.
newCreateAutoMLJobV2Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'autoMLJobArn'
  Prelude.Text ->
  CreateAutoMLJobV2Response
newCreateAutoMLJobV2Response
  pHttpStatus_
  pAutoMLJobArn_ =
    CreateAutoMLJobV2Response'
      { httpStatus =
          pHttpStatus_,
        autoMLJobArn = pAutoMLJobArn_
      }

-- | The response's http status code.
createAutoMLJobV2Response_httpStatus :: Lens.Lens' CreateAutoMLJobV2Response Prelude.Int
createAutoMLJobV2Response_httpStatus = Lens.lens (\CreateAutoMLJobV2Response' {httpStatus} -> httpStatus) (\s@CreateAutoMLJobV2Response' {} a -> s {httpStatus = a} :: CreateAutoMLJobV2Response)

-- | The unique ARN assigned to the AutoMLJob when it is created.
createAutoMLJobV2Response_autoMLJobArn :: Lens.Lens' CreateAutoMLJobV2Response Prelude.Text
createAutoMLJobV2Response_autoMLJobArn = Lens.lens (\CreateAutoMLJobV2Response' {autoMLJobArn} -> autoMLJobArn) (\s@CreateAutoMLJobV2Response' {} a -> s {autoMLJobArn = a} :: CreateAutoMLJobV2Response)

instance Prelude.NFData CreateAutoMLJobV2Response where
  rnf CreateAutoMLJobV2Response' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf autoMLJobArn
