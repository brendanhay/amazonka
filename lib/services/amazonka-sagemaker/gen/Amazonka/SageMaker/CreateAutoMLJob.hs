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
-- Module      : Amazonka.SageMaker.CreateAutoMLJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Autopilot job.
--
-- Find the best-performing model after you run an Autopilot job by calling
-- .
--
-- For information about how to use Autopilot, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-automate-model-development.html Automate Model Development with Amazon SageMaker Autopilot>.
module Amazonka.SageMaker.CreateAutoMLJob
  ( -- * Creating a Request
    CreateAutoMLJob (..),
    newCreateAutoMLJob,

    -- * Request Lenses
    createAutoMLJob_tags,
    createAutoMLJob_autoMLJobConfig,
    createAutoMLJob_autoMLJobObjective,
    createAutoMLJob_modelDeployConfig,
    createAutoMLJob_problemType,
    createAutoMLJob_generateCandidateDefinitionsOnly,
    createAutoMLJob_autoMLJobName,
    createAutoMLJob_inputDataConfig,
    createAutoMLJob_outputDataConfig,
    createAutoMLJob_roleArn,

    -- * Destructuring the Response
    CreateAutoMLJobResponse (..),
    newCreateAutoMLJobResponse,

    -- * Response Lenses
    createAutoMLJobResponse_httpStatus,
    createAutoMLJobResponse_autoMLJobArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateAutoMLJob' smart constructor.
data CreateAutoMLJob = CreateAutoMLJob'
  { -- | Each tag consists of a key and an optional value. Tag keys must be
    -- unique per resource.
    tags :: Prelude.Maybe [Tag],
    -- | A collection of settings used to configure an AutoML job.
    autoMLJobConfig :: Prelude.Maybe AutoMLJobConfig,
    -- | Defines the objective metric used to measure the predictive quality of
    -- an AutoML job. You provide an AutoMLJobObjective$MetricName and
    -- Autopilot infers whether to minimize or maximize it.
    autoMLJobObjective :: Prelude.Maybe AutoMLJobObjective,
    -- | Specifies how to generate the endpoint name for an automatic one-click
    -- Autopilot model deployment.
    modelDeployConfig :: Prelude.Maybe ModelDeployConfig,
    -- | Defines the type of supervised learning available for the candidates.
    -- For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-automate-model-development-problem-types.html Amazon SageMaker Autopilot problem types and algorithm support>.
    problemType :: Prelude.Maybe ProblemType,
    -- | Generates possible candidates without training the models. A candidate
    -- is a combination of data preprocessors, algorithms, and algorithm
    -- parameter settings.
    generateCandidateDefinitionsOnly :: Prelude.Maybe Prelude.Bool,
    -- | Identifies an Autopilot job. The name must be unique to your account and
    -- is case-insensitive.
    autoMLJobName :: Prelude.Text,
    -- | An array of channel objects that describes the input data and its
    -- location. Each channel is a named input source. Similar to
    -- @InputDataConfig@ supported by . Format(s) supported: CSV, Parquet. A
    -- minimum of 500 rows is required for the training dataset. There is not a
    -- minimum number of rows required for the validation dataset.
    inputDataConfig :: Prelude.NonEmpty AutoMLChannel,
    -- | Provides information about encryption and the Amazon S3 output path
    -- needed to store artifacts from an AutoML job. Format(s) supported: CSV.
    outputDataConfig :: AutoMLOutputDataConfig,
    -- | The ARN of the role that is used to access the data.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAutoMLJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAutoMLJob_tags' - Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource.
--
-- 'autoMLJobConfig', 'createAutoMLJob_autoMLJobConfig' - A collection of settings used to configure an AutoML job.
--
-- 'autoMLJobObjective', 'createAutoMLJob_autoMLJobObjective' - Defines the objective metric used to measure the predictive quality of
-- an AutoML job. You provide an AutoMLJobObjective$MetricName and
-- Autopilot infers whether to minimize or maximize it.
--
-- 'modelDeployConfig', 'createAutoMLJob_modelDeployConfig' - Specifies how to generate the endpoint name for an automatic one-click
-- Autopilot model deployment.
--
-- 'problemType', 'createAutoMLJob_problemType' - Defines the type of supervised learning available for the candidates.
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-automate-model-development-problem-types.html Amazon SageMaker Autopilot problem types and algorithm support>.
--
-- 'generateCandidateDefinitionsOnly', 'createAutoMLJob_generateCandidateDefinitionsOnly' - Generates possible candidates without training the models. A candidate
-- is a combination of data preprocessors, algorithms, and algorithm
-- parameter settings.
--
-- 'autoMLJobName', 'createAutoMLJob_autoMLJobName' - Identifies an Autopilot job. The name must be unique to your account and
-- is case-insensitive.
--
-- 'inputDataConfig', 'createAutoMLJob_inputDataConfig' - An array of channel objects that describes the input data and its
-- location. Each channel is a named input source. Similar to
-- @InputDataConfig@ supported by . Format(s) supported: CSV, Parquet. A
-- minimum of 500 rows is required for the training dataset. There is not a
-- minimum number of rows required for the validation dataset.
--
-- 'outputDataConfig', 'createAutoMLJob_outputDataConfig' - Provides information about encryption and the Amazon S3 output path
-- needed to store artifacts from an AutoML job. Format(s) supported: CSV.
--
-- 'roleArn', 'createAutoMLJob_roleArn' - The ARN of the role that is used to access the data.
newCreateAutoMLJob ::
  -- | 'autoMLJobName'
  Prelude.Text ->
  -- | 'inputDataConfig'
  Prelude.NonEmpty AutoMLChannel ->
  -- | 'outputDataConfig'
  AutoMLOutputDataConfig ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateAutoMLJob
newCreateAutoMLJob
  pAutoMLJobName_
  pInputDataConfig_
  pOutputDataConfig_
  pRoleArn_ =
    CreateAutoMLJob'
      { tags = Prelude.Nothing,
        autoMLJobConfig = Prelude.Nothing,
        autoMLJobObjective = Prelude.Nothing,
        modelDeployConfig = Prelude.Nothing,
        problemType = Prelude.Nothing,
        generateCandidateDefinitionsOnly = Prelude.Nothing,
        autoMLJobName = pAutoMLJobName_,
        inputDataConfig =
          Lens.coerced Lens.# pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        roleArn = pRoleArn_
      }

-- | Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource.
createAutoMLJob_tags :: Lens.Lens' CreateAutoMLJob (Prelude.Maybe [Tag])
createAutoMLJob_tags = Lens.lens (\CreateAutoMLJob' {tags} -> tags) (\s@CreateAutoMLJob' {} a -> s {tags = a} :: CreateAutoMLJob) Prelude.. Lens.mapping Lens.coerced

-- | A collection of settings used to configure an AutoML job.
createAutoMLJob_autoMLJobConfig :: Lens.Lens' CreateAutoMLJob (Prelude.Maybe AutoMLJobConfig)
createAutoMLJob_autoMLJobConfig = Lens.lens (\CreateAutoMLJob' {autoMLJobConfig} -> autoMLJobConfig) (\s@CreateAutoMLJob' {} a -> s {autoMLJobConfig = a} :: CreateAutoMLJob)

-- | Defines the objective metric used to measure the predictive quality of
-- an AutoML job. You provide an AutoMLJobObjective$MetricName and
-- Autopilot infers whether to minimize or maximize it.
createAutoMLJob_autoMLJobObjective :: Lens.Lens' CreateAutoMLJob (Prelude.Maybe AutoMLJobObjective)
createAutoMLJob_autoMLJobObjective = Lens.lens (\CreateAutoMLJob' {autoMLJobObjective} -> autoMLJobObjective) (\s@CreateAutoMLJob' {} a -> s {autoMLJobObjective = a} :: CreateAutoMLJob)

-- | Specifies how to generate the endpoint name for an automatic one-click
-- Autopilot model deployment.
createAutoMLJob_modelDeployConfig :: Lens.Lens' CreateAutoMLJob (Prelude.Maybe ModelDeployConfig)
createAutoMLJob_modelDeployConfig = Lens.lens (\CreateAutoMLJob' {modelDeployConfig} -> modelDeployConfig) (\s@CreateAutoMLJob' {} a -> s {modelDeployConfig = a} :: CreateAutoMLJob)

-- | Defines the type of supervised learning available for the candidates.
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-automate-model-development-problem-types.html Amazon SageMaker Autopilot problem types and algorithm support>.
createAutoMLJob_problemType :: Lens.Lens' CreateAutoMLJob (Prelude.Maybe ProblemType)
createAutoMLJob_problemType = Lens.lens (\CreateAutoMLJob' {problemType} -> problemType) (\s@CreateAutoMLJob' {} a -> s {problemType = a} :: CreateAutoMLJob)

-- | Generates possible candidates without training the models. A candidate
-- is a combination of data preprocessors, algorithms, and algorithm
-- parameter settings.
createAutoMLJob_generateCandidateDefinitionsOnly :: Lens.Lens' CreateAutoMLJob (Prelude.Maybe Prelude.Bool)
createAutoMLJob_generateCandidateDefinitionsOnly = Lens.lens (\CreateAutoMLJob' {generateCandidateDefinitionsOnly} -> generateCandidateDefinitionsOnly) (\s@CreateAutoMLJob' {} a -> s {generateCandidateDefinitionsOnly = a} :: CreateAutoMLJob)

-- | Identifies an Autopilot job. The name must be unique to your account and
-- is case-insensitive.
createAutoMLJob_autoMLJobName :: Lens.Lens' CreateAutoMLJob Prelude.Text
createAutoMLJob_autoMLJobName = Lens.lens (\CreateAutoMLJob' {autoMLJobName} -> autoMLJobName) (\s@CreateAutoMLJob' {} a -> s {autoMLJobName = a} :: CreateAutoMLJob)

-- | An array of channel objects that describes the input data and its
-- location. Each channel is a named input source. Similar to
-- @InputDataConfig@ supported by . Format(s) supported: CSV, Parquet. A
-- minimum of 500 rows is required for the training dataset. There is not a
-- minimum number of rows required for the validation dataset.
createAutoMLJob_inputDataConfig :: Lens.Lens' CreateAutoMLJob (Prelude.NonEmpty AutoMLChannel)
createAutoMLJob_inputDataConfig = Lens.lens (\CreateAutoMLJob' {inputDataConfig} -> inputDataConfig) (\s@CreateAutoMLJob' {} a -> s {inputDataConfig = a} :: CreateAutoMLJob) Prelude.. Lens.coerced

-- | Provides information about encryption and the Amazon S3 output path
-- needed to store artifacts from an AutoML job. Format(s) supported: CSV.
createAutoMLJob_outputDataConfig :: Lens.Lens' CreateAutoMLJob AutoMLOutputDataConfig
createAutoMLJob_outputDataConfig = Lens.lens (\CreateAutoMLJob' {outputDataConfig} -> outputDataConfig) (\s@CreateAutoMLJob' {} a -> s {outputDataConfig = a} :: CreateAutoMLJob)

-- | The ARN of the role that is used to access the data.
createAutoMLJob_roleArn :: Lens.Lens' CreateAutoMLJob Prelude.Text
createAutoMLJob_roleArn = Lens.lens (\CreateAutoMLJob' {roleArn} -> roleArn) (\s@CreateAutoMLJob' {} a -> s {roleArn = a} :: CreateAutoMLJob)

instance Core.AWSRequest CreateAutoMLJob where
  type
    AWSResponse CreateAutoMLJob =
      CreateAutoMLJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAutoMLJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "AutoMLJobArn")
      )

instance Prelude.Hashable CreateAutoMLJob where
  hashWithSalt _salt CreateAutoMLJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` autoMLJobConfig
      `Prelude.hashWithSalt` autoMLJobObjective
      `Prelude.hashWithSalt` modelDeployConfig
      `Prelude.hashWithSalt` problemType
      `Prelude.hashWithSalt` generateCandidateDefinitionsOnly
      `Prelude.hashWithSalt` autoMLJobName
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateAutoMLJob where
  rnf CreateAutoMLJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf autoMLJobConfig
      `Prelude.seq` Prelude.rnf autoMLJobObjective
      `Prelude.seq` Prelude.rnf modelDeployConfig
      `Prelude.seq` Prelude.rnf problemType
      `Prelude.seq` Prelude.rnf generateCandidateDefinitionsOnly
      `Prelude.seq` Prelude.rnf autoMLJobName
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf roleArn

instance Core.ToHeaders CreateAutoMLJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateAutoMLJob" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAutoMLJob where
  toJSON CreateAutoMLJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("AutoMLJobConfig" Core..=)
              Prelude.<$> autoMLJobConfig,
            ("AutoMLJobObjective" Core..=)
              Prelude.<$> autoMLJobObjective,
            ("ModelDeployConfig" Core..=)
              Prelude.<$> modelDeployConfig,
            ("ProblemType" Core..=) Prelude.<$> problemType,
            ("GenerateCandidateDefinitionsOnly" Core..=)
              Prelude.<$> generateCandidateDefinitionsOnly,
            Prelude.Just ("AutoMLJobName" Core..= autoMLJobName),
            Prelude.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateAutoMLJob where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateAutoMLJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAutoMLJobResponse' smart constructor.
data CreateAutoMLJobResponse = CreateAutoMLJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique ARN assigned to the AutoML job when it is created.
    autoMLJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAutoMLJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAutoMLJobResponse_httpStatus' - The response's http status code.
--
-- 'autoMLJobArn', 'createAutoMLJobResponse_autoMLJobArn' - The unique ARN assigned to the AutoML job when it is created.
newCreateAutoMLJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'autoMLJobArn'
  Prelude.Text ->
  CreateAutoMLJobResponse
newCreateAutoMLJobResponse
  pHttpStatus_
  pAutoMLJobArn_ =
    CreateAutoMLJobResponse'
      { httpStatus = pHttpStatus_,
        autoMLJobArn = pAutoMLJobArn_
      }

-- | The response's http status code.
createAutoMLJobResponse_httpStatus :: Lens.Lens' CreateAutoMLJobResponse Prelude.Int
createAutoMLJobResponse_httpStatus = Lens.lens (\CreateAutoMLJobResponse' {httpStatus} -> httpStatus) (\s@CreateAutoMLJobResponse' {} a -> s {httpStatus = a} :: CreateAutoMLJobResponse)

-- | The unique ARN assigned to the AutoML job when it is created.
createAutoMLJobResponse_autoMLJobArn :: Lens.Lens' CreateAutoMLJobResponse Prelude.Text
createAutoMLJobResponse_autoMLJobArn = Lens.lens (\CreateAutoMLJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@CreateAutoMLJobResponse' {} a -> s {autoMLJobArn = a} :: CreateAutoMLJobResponse)

instance Prelude.NFData CreateAutoMLJobResponse where
  rnf CreateAutoMLJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf autoMLJobArn
