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
-- Module      : Network.AWS.SageMaker.CreateHyperParameterTuningJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a hyperparameter tuning job. A hyperparameter tuning job finds
-- the best version of a model by running many training jobs on your
-- dataset using the algorithm you choose and values for hyperparameters
-- within ranges that you specify. It then chooses the hyperparameter
-- values that result in a model that performs the best, as measured by an
-- objective metric that you choose.
module Network.AWS.SageMaker.CreateHyperParameterTuningJob
  ( -- * Creating a Request
    CreateHyperParameterTuningJob (..),
    newCreateHyperParameterTuningJob,

    -- * Request Lenses
    createHyperParameterTuningJob_warmStartConfig,
    createHyperParameterTuningJob_tags,
    createHyperParameterTuningJob_trainingJobDefinitions,
    createHyperParameterTuningJob_trainingJobDefinition,
    createHyperParameterTuningJob_hyperParameterTuningJobName,
    createHyperParameterTuningJob_hyperParameterTuningJobConfig,

    -- * Destructuring the Response
    CreateHyperParameterTuningJobResponse (..),
    newCreateHyperParameterTuningJobResponse,

    -- * Response Lenses
    createHyperParameterTuningJobResponse_httpStatus,
    createHyperParameterTuningJobResponse_hyperParameterTuningJobArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateHyperParameterTuningJob' smart constructor.
data CreateHyperParameterTuningJob = CreateHyperParameterTuningJob'
  { -- | Specifies the configuration for starting the hyperparameter tuning job
    -- using one or more previous tuning jobs as a starting point. The results
    -- of previous tuning jobs are used to inform which combinations of
    -- hyperparameters to search over in the new tuning job.
    --
    -- All training jobs launched by the new hyperparameter tuning job are
    -- evaluated by using the objective metric. If you specify
    -- @IDENTICAL_DATA_AND_ALGORITHM@ as the @WarmStartType@ value for the warm
    -- start configuration, the training job that performs the best in the new
    -- tuning job is compared to the best training jobs from the parent tuning
    -- jobs. From these, the training job that performs the best as measured by
    -- the objective metric is returned as the overall best training job.
    --
    -- All training jobs launched by parent hyperparameter tuning jobs and the
    -- new hyperparameter tuning jobs count against the limit of training jobs
    -- for the tuning job.
    warmStartConfig :: Prelude.Maybe HyperParameterTuningJobWarmStartConfig,
    -- | An array of key-value pairs. You can use tags to categorize your AWS
    -- resources in different ways, for example, by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
    --
    -- Tags that you specify for the tuning job are also added to all training
    -- jobs that the tuning job launches.
    tags :: Prelude.Maybe [Tag],
    -- | A list of the HyperParameterTrainingJobDefinition objects launched for
    -- this tuning job.
    trainingJobDefinitions :: Prelude.Maybe (Prelude.NonEmpty HyperParameterTrainingJobDefinition),
    -- | The HyperParameterTrainingJobDefinition object that describes the
    -- training jobs that this tuning job launches, including static
    -- hyperparameters, input data configuration, output data configuration,
    -- resource configuration, and stopping condition.
    trainingJobDefinition :: Prelude.Maybe HyperParameterTrainingJobDefinition,
    -- | The name of the tuning job. This name is the prefix for the names of all
    -- training jobs that this tuning job launches. The name must be unique
    -- within the same AWS account and AWS Region. The name must have 1 to 32
    -- characters. Valid characters are a-z, A-Z, 0-9, and : + = \@ _ % -
    -- (hyphen). The name is not case sensitive.
    hyperParameterTuningJobName :: Prelude.Text,
    -- | The HyperParameterTuningJobConfig object that describes the tuning job,
    -- including the search strategy, the objective metric used to evaluate
    -- training jobs, ranges of parameters to search, and resource limits for
    -- the tuning job. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works>.
    hyperParameterTuningJobConfig :: HyperParameterTuningJobConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHyperParameterTuningJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warmStartConfig', 'createHyperParameterTuningJob_warmStartConfig' - Specifies the configuration for starting the hyperparameter tuning job
-- using one or more previous tuning jobs as a starting point. The results
-- of previous tuning jobs are used to inform which combinations of
-- hyperparameters to search over in the new tuning job.
--
-- All training jobs launched by the new hyperparameter tuning job are
-- evaluated by using the objective metric. If you specify
-- @IDENTICAL_DATA_AND_ALGORITHM@ as the @WarmStartType@ value for the warm
-- start configuration, the training job that performs the best in the new
-- tuning job is compared to the best training jobs from the parent tuning
-- jobs. From these, the training job that performs the best as measured by
-- the objective metric is returned as the overall best training job.
--
-- All training jobs launched by parent hyperparameter tuning jobs and the
-- new hyperparameter tuning jobs count against the limit of training jobs
-- for the tuning job.
--
-- 'tags', 'createHyperParameterTuningJob_tags' - An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
--
-- Tags that you specify for the tuning job are also added to all training
-- jobs that the tuning job launches.
--
-- 'trainingJobDefinitions', 'createHyperParameterTuningJob_trainingJobDefinitions' - A list of the HyperParameterTrainingJobDefinition objects launched for
-- this tuning job.
--
-- 'trainingJobDefinition', 'createHyperParameterTuningJob_trainingJobDefinition' - The HyperParameterTrainingJobDefinition object that describes the
-- training jobs that this tuning job launches, including static
-- hyperparameters, input data configuration, output data configuration,
-- resource configuration, and stopping condition.
--
-- 'hyperParameterTuningJobName', 'createHyperParameterTuningJob_hyperParameterTuningJobName' - The name of the tuning job. This name is the prefix for the names of all
-- training jobs that this tuning job launches. The name must be unique
-- within the same AWS account and AWS Region. The name must have 1 to 32
-- characters. Valid characters are a-z, A-Z, 0-9, and : + = \@ _ % -
-- (hyphen). The name is not case sensitive.
--
-- 'hyperParameterTuningJobConfig', 'createHyperParameterTuningJob_hyperParameterTuningJobConfig' - The HyperParameterTuningJobConfig object that describes the tuning job,
-- including the search strategy, the objective metric used to evaluate
-- training jobs, ranges of parameters to search, and resource limits for
-- the tuning job. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works>.
newCreateHyperParameterTuningJob ::
  -- | 'hyperParameterTuningJobName'
  Prelude.Text ->
  -- | 'hyperParameterTuningJobConfig'
  HyperParameterTuningJobConfig ->
  CreateHyperParameterTuningJob
newCreateHyperParameterTuningJob
  pHyperParameterTuningJobName_
  pHyperParameterTuningJobConfig_ =
    CreateHyperParameterTuningJob'
      { warmStartConfig =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        trainingJobDefinitions = Prelude.Nothing,
        trainingJobDefinition = Prelude.Nothing,
        hyperParameterTuningJobName =
          pHyperParameterTuningJobName_,
        hyperParameterTuningJobConfig =
          pHyperParameterTuningJobConfig_
      }

-- | Specifies the configuration for starting the hyperparameter tuning job
-- using one or more previous tuning jobs as a starting point. The results
-- of previous tuning jobs are used to inform which combinations of
-- hyperparameters to search over in the new tuning job.
--
-- All training jobs launched by the new hyperparameter tuning job are
-- evaluated by using the objective metric. If you specify
-- @IDENTICAL_DATA_AND_ALGORITHM@ as the @WarmStartType@ value for the warm
-- start configuration, the training job that performs the best in the new
-- tuning job is compared to the best training jobs from the parent tuning
-- jobs. From these, the training job that performs the best as measured by
-- the objective metric is returned as the overall best training job.
--
-- All training jobs launched by parent hyperparameter tuning jobs and the
-- new hyperparameter tuning jobs count against the limit of training jobs
-- for the tuning job.
createHyperParameterTuningJob_warmStartConfig :: Lens.Lens' CreateHyperParameterTuningJob (Prelude.Maybe HyperParameterTuningJobWarmStartConfig)
createHyperParameterTuningJob_warmStartConfig = Lens.lens (\CreateHyperParameterTuningJob' {warmStartConfig} -> warmStartConfig) (\s@CreateHyperParameterTuningJob' {} a -> s {warmStartConfig = a} :: CreateHyperParameterTuningJob)

-- | An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
--
-- Tags that you specify for the tuning job are also added to all training
-- jobs that the tuning job launches.
createHyperParameterTuningJob_tags :: Lens.Lens' CreateHyperParameterTuningJob (Prelude.Maybe [Tag])
createHyperParameterTuningJob_tags = Lens.lens (\CreateHyperParameterTuningJob' {tags} -> tags) (\s@CreateHyperParameterTuningJob' {} a -> s {tags = a} :: CreateHyperParameterTuningJob) Prelude.. Lens.mapping Lens._Coerce

-- | A list of the HyperParameterTrainingJobDefinition objects launched for
-- this tuning job.
createHyperParameterTuningJob_trainingJobDefinitions :: Lens.Lens' CreateHyperParameterTuningJob (Prelude.Maybe (Prelude.NonEmpty HyperParameterTrainingJobDefinition))
createHyperParameterTuningJob_trainingJobDefinitions = Lens.lens (\CreateHyperParameterTuningJob' {trainingJobDefinitions} -> trainingJobDefinitions) (\s@CreateHyperParameterTuningJob' {} a -> s {trainingJobDefinitions = a} :: CreateHyperParameterTuningJob) Prelude.. Lens.mapping Lens._Coerce

-- | The HyperParameterTrainingJobDefinition object that describes the
-- training jobs that this tuning job launches, including static
-- hyperparameters, input data configuration, output data configuration,
-- resource configuration, and stopping condition.
createHyperParameterTuningJob_trainingJobDefinition :: Lens.Lens' CreateHyperParameterTuningJob (Prelude.Maybe HyperParameterTrainingJobDefinition)
createHyperParameterTuningJob_trainingJobDefinition = Lens.lens (\CreateHyperParameterTuningJob' {trainingJobDefinition} -> trainingJobDefinition) (\s@CreateHyperParameterTuningJob' {} a -> s {trainingJobDefinition = a} :: CreateHyperParameterTuningJob)

-- | The name of the tuning job. This name is the prefix for the names of all
-- training jobs that this tuning job launches. The name must be unique
-- within the same AWS account and AWS Region. The name must have 1 to 32
-- characters. Valid characters are a-z, A-Z, 0-9, and : + = \@ _ % -
-- (hyphen). The name is not case sensitive.
createHyperParameterTuningJob_hyperParameterTuningJobName :: Lens.Lens' CreateHyperParameterTuningJob Prelude.Text
createHyperParameterTuningJob_hyperParameterTuningJobName = Lens.lens (\CreateHyperParameterTuningJob' {hyperParameterTuningJobName} -> hyperParameterTuningJobName) (\s@CreateHyperParameterTuningJob' {} a -> s {hyperParameterTuningJobName = a} :: CreateHyperParameterTuningJob)

-- | The HyperParameterTuningJobConfig object that describes the tuning job,
-- including the search strategy, the objective metric used to evaluate
-- training jobs, ranges of parameters to search, and resource limits for
-- the tuning job. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works>.
createHyperParameterTuningJob_hyperParameterTuningJobConfig :: Lens.Lens' CreateHyperParameterTuningJob HyperParameterTuningJobConfig
createHyperParameterTuningJob_hyperParameterTuningJobConfig = Lens.lens (\CreateHyperParameterTuningJob' {hyperParameterTuningJobConfig} -> hyperParameterTuningJobConfig) (\s@CreateHyperParameterTuningJob' {} a -> s {hyperParameterTuningJobConfig = a} :: CreateHyperParameterTuningJob)

instance
  Core.AWSRequest
    CreateHyperParameterTuningJob
  where
  type
    AWSResponse CreateHyperParameterTuningJob =
      CreateHyperParameterTuningJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHyperParameterTuningJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "HyperParameterTuningJobArn")
      )

instance
  Prelude.Hashable
    CreateHyperParameterTuningJob

instance Prelude.NFData CreateHyperParameterTuningJob

instance Core.ToHeaders CreateHyperParameterTuningJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateHyperParameterTuningJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateHyperParameterTuningJob where
  toJSON CreateHyperParameterTuningJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WarmStartConfig" Core..=)
              Prelude.<$> warmStartConfig,
            ("Tags" Core..=) Prelude.<$> tags,
            ("TrainingJobDefinitions" Core..=)
              Prelude.<$> trainingJobDefinitions,
            ("TrainingJobDefinition" Core..=)
              Prelude.<$> trainingJobDefinition,
            Prelude.Just
              ( "HyperParameterTuningJobName"
                  Core..= hyperParameterTuningJobName
              ),
            Prelude.Just
              ( "HyperParameterTuningJobConfig"
                  Core..= hyperParameterTuningJobConfig
              )
          ]
      )

instance Core.ToPath CreateHyperParameterTuningJob where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateHyperParameterTuningJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateHyperParameterTuningJobResponse' smart constructor.
data CreateHyperParameterTuningJobResponse = CreateHyperParameterTuningJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the tuning job. Amazon SageMaker
    -- assigns an ARN to a hyperparameter tuning job when you create it.
    hyperParameterTuningJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHyperParameterTuningJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createHyperParameterTuningJobResponse_httpStatus' - The response's http status code.
--
-- 'hyperParameterTuningJobArn', 'createHyperParameterTuningJobResponse_hyperParameterTuningJobArn' - The Amazon Resource Name (ARN) of the tuning job. Amazon SageMaker
-- assigns an ARN to a hyperparameter tuning job when you create it.
newCreateHyperParameterTuningJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hyperParameterTuningJobArn'
  Prelude.Text ->
  CreateHyperParameterTuningJobResponse
newCreateHyperParameterTuningJobResponse
  pHttpStatus_
  pHyperParameterTuningJobArn_ =
    CreateHyperParameterTuningJobResponse'
      { httpStatus =
          pHttpStatus_,
        hyperParameterTuningJobArn =
          pHyperParameterTuningJobArn_
      }

-- | The response's http status code.
createHyperParameterTuningJobResponse_httpStatus :: Lens.Lens' CreateHyperParameterTuningJobResponse Prelude.Int
createHyperParameterTuningJobResponse_httpStatus = Lens.lens (\CreateHyperParameterTuningJobResponse' {httpStatus} -> httpStatus) (\s@CreateHyperParameterTuningJobResponse' {} a -> s {httpStatus = a} :: CreateHyperParameterTuningJobResponse)

-- | The Amazon Resource Name (ARN) of the tuning job. Amazon SageMaker
-- assigns an ARN to a hyperparameter tuning job when you create it.
createHyperParameterTuningJobResponse_hyperParameterTuningJobArn :: Lens.Lens' CreateHyperParameterTuningJobResponse Prelude.Text
createHyperParameterTuningJobResponse_hyperParameterTuningJobArn = Lens.lens (\CreateHyperParameterTuningJobResponse' {hyperParameterTuningJobArn} -> hyperParameterTuningJobArn) (\s@CreateHyperParameterTuningJobResponse' {} a -> s {hyperParameterTuningJobArn = a} :: CreateHyperParameterTuningJobResponse)

instance
  Prelude.NFData
    CreateHyperParameterTuningJobResponse
