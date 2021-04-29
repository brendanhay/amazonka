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
-- Module      : Network.AWS.SageMaker.CreateAutoMLJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Autopilot job.
--
-- Find the best performing model after you run an Autopilot job by calling
-- . Deploy that model by following the steps described in
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ex1-deploy-model.html Step 6.1: Deploy the Model to Amazon SageMaker Hosting Services>.
--
-- For information about how to use Autopilot, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-automate-model-development.html Automate Model Development with Amazon SageMaker Autopilot>.
module Network.AWS.SageMaker.CreateAutoMLJob
  ( -- * Creating a Request
    CreateAutoMLJob (..),
    newCreateAutoMLJob,

    -- * Request Lenses
    createAutoMLJob_generateCandidateDefinitionsOnly,
    createAutoMLJob_tags,
    createAutoMLJob_autoMLJobObjective,
    createAutoMLJob_autoMLJobConfig,
    createAutoMLJob_problemType,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateAutoMLJob' smart constructor.
data CreateAutoMLJob = CreateAutoMLJob'
  { -- | Generates possible candidates without training a model. A candidate is a
    -- combination of data preprocessors, algorithms, and algorithm parameter
    -- settings.
    generateCandidateDefinitionsOnly :: Prelude.Maybe Prelude.Bool,
    -- | Each tag consists of a key and an optional value. Tag keys must be
    -- unique per resource.
    tags :: Prelude.Maybe [Tag],
    -- | Defines the objective of a an AutoML job. You provide a
    -- AutoMLJobObjective$MetricName and Autopilot infers whether to minimize
    -- or maximize it. If a metric is not specified, the most commonly used
    -- ObjectiveMetric for problem type is automaically selected.
    autoMLJobObjective :: Prelude.Maybe AutoMLJobObjective,
    -- | Contains CompletionCriteria and SecurityConfig.
    autoMLJobConfig :: Prelude.Maybe AutoMLJobConfig,
    -- | Defines the kind of preprocessing and algorithms intended for the
    -- candidates. Options include: BinaryClassification,
    -- MulticlassClassification, and Regression.
    problemType :: Prelude.Maybe ProblemType,
    -- | Identifies an Autopilot job. Must be unique to your account and is
    -- case-insensitive.
    autoMLJobName :: Prelude.Text,
    -- | Similar to InputDataConfig supported by Tuning. Format(s) supported:
    -- CSV. Minimum of 500 rows.
    inputDataConfig :: Prelude.NonEmpty AutoMLChannel,
    -- | Similar to OutputDataConfig supported by Tuning. Format(s) supported:
    -- CSV.
    outputDataConfig :: AutoMLOutputDataConfig,
    -- | The ARN of the role that is used to access the data.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAutoMLJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generateCandidateDefinitionsOnly', 'createAutoMLJob_generateCandidateDefinitionsOnly' - Generates possible candidates without training a model. A candidate is a
-- combination of data preprocessors, algorithms, and algorithm parameter
-- settings.
--
-- 'tags', 'createAutoMLJob_tags' - Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource.
--
-- 'autoMLJobObjective', 'createAutoMLJob_autoMLJobObjective' - Defines the objective of a an AutoML job. You provide a
-- AutoMLJobObjective$MetricName and Autopilot infers whether to minimize
-- or maximize it. If a metric is not specified, the most commonly used
-- ObjectiveMetric for problem type is automaically selected.
--
-- 'autoMLJobConfig', 'createAutoMLJob_autoMLJobConfig' - Contains CompletionCriteria and SecurityConfig.
--
-- 'problemType', 'createAutoMLJob_problemType' - Defines the kind of preprocessing and algorithms intended for the
-- candidates. Options include: BinaryClassification,
-- MulticlassClassification, and Regression.
--
-- 'autoMLJobName', 'createAutoMLJob_autoMLJobName' - Identifies an Autopilot job. Must be unique to your account and is
-- case-insensitive.
--
-- 'inputDataConfig', 'createAutoMLJob_inputDataConfig' - Similar to InputDataConfig supported by Tuning. Format(s) supported:
-- CSV. Minimum of 500 rows.
--
-- 'outputDataConfig', 'createAutoMLJob_outputDataConfig' - Similar to OutputDataConfig supported by Tuning. Format(s) supported:
-- CSV.
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
      { generateCandidateDefinitionsOnly =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        autoMLJobObjective = Prelude.Nothing,
        autoMLJobConfig = Prelude.Nothing,
        problemType = Prelude.Nothing,
        autoMLJobName = pAutoMLJobName_,
        inputDataConfig =
          Prelude._Coerce Lens.# pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        roleArn = pRoleArn_
      }

-- | Generates possible candidates without training a model. A candidate is a
-- combination of data preprocessors, algorithms, and algorithm parameter
-- settings.
createAutoMLJob_generateCandidateDefinitionsOnly :: Lens.Lens' CreateAutoMLJob (Prelude.Maybe Prelude.Bool)
createAutoMLJob_generateCandidateDefinitionsOnly = Lens.lens (\CreateAutoMLJob' {generateCandidateDefinitionsOnly} -> generateCandidateDefinitionsOnly) (\s@CreateAutoMLJob' {} a -> s {generateCandidateDefinitionsOnly = a} :: CreateAutoMLJob)

-- | Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource.
createAutoMLJob_tags :: Lens.Lens' CreateAutoMLJob (Prelude.Maybe [Tag])
createAutoMLJob_tags = Lens.lens (\CreateAutoMLJob' {tags} -> tags) (\s@CreateAutoMLJob' {} a -> s {tags = a} :: CreateAutoMLJob) Prelude.. Lens.mapping Prelude._Coerce

-- | Defines the objective of a an AutoML job. You provide a
-- AutoMLJobObjective$MetricName and Autopilot infers whether to minimize
-- or maximize it. If a metric is not specified, the most commonly used
-- ObjectiveMetric for problem type is automaically selected.
createAutoMLJob_autoMLJobObjective :: Lens.Lens' CreateAutoMLJob (Prelude.Maybe AutoMLJobObjective)
createAutoMLJob_autoMLJobObjective = Lens.lens (\CreateAutoMLJob' {autoMLJobObjective} -> autoMLJobObjective) (\s@CreateAutoMLJob' {} a -> s {autoMLJobObjective = a} :: CreateAutoMLJob)

-- | Contains CompletionCriteria and SecurityConfig.
createAutoMLJob_autoMLJobConfig :: Lens.Lens' CreateAutoMLJob (Prelude.Maybe AutoMLJobConfig)
createAutoMLJob_autoMLJobConfig = Lens.lens (\CreateAutoMLJob' {autoMLJobConfig} -> autoMLJobConfig) (\s@CreateAutoMLJob' {} a -> s {autoMLJobConfig = a} :: CreateAutoMLJob)

-- | Defines the kind of preprocessing and algorithms intended for the
-- candidates. Options include: BinaryClassification,
-- MulticlassClassification, and Regression.
createAutoMLJob_problemType :: Lens.Lens' CreateAutoMLJob (Prelude.Maybe ProblemType)
createAutoMLJob_problemType = Lens.lens (\CreateAutoMLJob' {problemType} -> problemType) (\s@CreateAutoMLJob' {} a -> s {problemType = a} :: CreateAutoMLJob)

-- | Identifies an Autopilot job. Must be unique to your account and is
-- case-insensitive.
createAutoMLJob_autoMLJobName :: Lens.Lens' CreateAutoMLJob Prelude.Text
createAutoMLJob_autoMLJobName = Lens.lens (\CreateAutoMLJob' {autoMLJobName} -> autoMLJobName) (\s@CreateAutoMLJob' {} a -> s {autoMLJobName = a} :: CreateAutoMLJob)

-- | Similar to InputDataConfig supported by Tuning. Format(s) supported:
-- CSV. Minimum of 500 rows.
createAutoMLJob_inputDataConfig :: Lens.Lens' CreateAutoMLJob (Prelude.NonEmpty AutoMLChannel)
createAutoMLJob_inputDataConfig = Lens.lens (\CreateAutoMLJob' {inputDataConfig} -> inputDataConfig) (\s@CreateAutoMLJob' {} a -> s {inputDataConfig = a} :: CreateAutoMLJob) Prelude.. Prelude._Coerce

-- | Similar to OutputDataConfig supported by Tuning. Format(s) supported:
-- CSV.
createAutoMLJob_outputDataConfig :: Lens.Lens' CreateAutoMLJob AutoMLOutputDataConfig
createAutoMLJob_outputDataConfig = Lens.lens (\CreateAutoMLJob' {outputDataConfig} -> outputDataConfig) (\s@CreateAutoMLJob' {} a -> s {outputDataConfig = a} :: CreateAutoMLJob)

-- | The ARN of the role that is used to access the data.
createAutoMLJob_roleArn :: Lens.Lens' CreateAutoMLJob Prelude.Text
createAutoMLJob_roleArn = Lens.lens (\CreateAutoMLJob' {roleArn} -> roleArn) (\s@CreateAutoMLJob' {} a -> s {roleArn = a} :: CreateAutoMLJob)

instance Prelude.AWSRequest CreateAutoMLJob where
  type Rs CreateAutoMLJob = CreateAutoMLJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAutoMLJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "AutoMLJobArn")
      )

instance Prelude.Hashable CreateAutoMLJob

instance Prelude.NFData CreateAutoMLJob

instance Prelude.ToHeaders CreateAutoMLJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.CreateAutoMLJob" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateAutoMLJob where
  toJSON CreateAutoMLJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GenerateCandidateDefinitionsOnly" Prelude..=)
              Prelude.<$> generateCandidateDefinitionsOnly,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("AutoMLJobObjective" Prelude..=)
              Prelude.<$> autoMLJobObjective,
            ("AutoMLJobConfig" Prelude..=)
              Prelude.<$> autoMLJobConfig,
            ("ProblemType" Prelude..=) Prelude.<$> problemType,
            Prelude.Just
              ("AutoMLJobName" Prelude..= autoMLJobName),
            Prelude.Just
              ("InputDataConfig" Prelude..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Prelude..= outputDataConfig),
            Prelude.Just ("RoleArn" Prelude..= roleArn)
          ]
      )

instance Prelude.ToPath CreateAutoMLJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateAutoMLJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAutoMLJobResponse' smart constructor.
data CreateAutoMLJobResponse = CreateAutoMLJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | When a job is created, it is assigned a unique ARN.
    autoMLJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'autoMLJobArn', 'createAutoMLJobResponse_autoMLJobArn' - When a job is created, it is assigned a unique ARN.
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

-- | When a job is created, it is assigned a unique ARN.
createAutoMLJobResponse_autoMLJobArn :: Lens.Lens' CreateAutoMLJobResponse Prelude.Text
createAutoMLJobResponse_autoMLJobArn = Lens.lens (\CreateAutoMLJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@CreateAutoMLJobResponse' {} a -> s {autoMLJobArn = a} :: CreateAutoMLJobResponse)

instance Prelude.NFData CreateAutoMLJobResponse
