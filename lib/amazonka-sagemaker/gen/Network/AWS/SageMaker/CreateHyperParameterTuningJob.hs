{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateHyperParameterTuningJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a hyperparameter tuning job. A hyperparameter tuning job finds the best version of a model by running many training jobs on your dataset using the algorithm you choose and values for hyperparameters within ranges that you specify. It then chooses the hyperparameter values that result in a model that performs the best, as measured by an objective metric that you choose.
module Network.AWS.SageMaker.CreateHyperParameterTuningJob
  ( -- * Creating a request
    CreateHyperParameterTuningJob (..),
    mkCreateHyperParameterTuningJob,

    -- ** Request lenses
    chptjTrainingJobDefinition,
    chptjHyperParameterTuningJobName,
    chptjHyperParameterTuningJobConfig,
    chptjWarmStartConfig,
    chptjTags,
    chptjTrainingJobDefinitions,

    -- * Destructuring the response
    CreateHyperParameterTuningJobResponse (..),
    mkCreateHyperParameterTuningJobResponse,

    -- ** Response lenses
    chptjrsHyperParameterTuningJobARN,
    chptjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateHyperParameterTuningJob' smart constructor.
data CreateHyperParameterTuningJob = CreateHyperParameterTuningJob'
  { -- | The 'HyperParameterTrainingJobDefinition' object that describes the training jobs that this tuning job launches, including static hyperparameters, input data configuration, output data configuration, resource configuration, and stopping condition.
    trainingJobDefinition :: Lude.Maybe HyperParameterTrainingJobDefinition,
    -- | The name of the tuning job. This name is the prefix for the names of all training jobs that this tuning job launches. The name must be unique within the same AWS account and AWS Region. The name must have 1 to 32 characters. Valid characters are a-z, A-Z, 0-9, and : + = @ _ % - (hyphen). The name is not case sensitive.
    hyperParameterTuningJobName :: Lude.Text,
    -- | The 'HyperParameterTuningJobConfig' object that describes the tuning job, including the search strategy, the objective metric used to evaluate training jobs, ranges of parameters to search, and resource limits for the tuning job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works> .
    hyperParameterTuningJobConfig :: HyperParameterTuningJobConfig,
    -- | Specifies the configuration for starting the hyperparameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
    --
    -- All training jobs launched by the new hyperparameter tuning job are evaluated by using the objective metric. If you specify @IDENTICAL_DATA_AND_ALGORITHM@ as the @WarmStartType@ value for the warm start configuration, the training job that performs the best in the new tuning job is compared to the best training jobs from the parent tuning jobs. From these, the training job that performs the best as measured by the objective metric is returned as the overall best training job.
    warmStartConfig :: Lude.Maybe HyperParameterTuningJobWarmStartConfig,
    -- | An array of key-value pairs. You can use tags to categorize your AWS resources in different ways, for example, by purpose, owner, or environment. For more information, see <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies> .
    --
    -- Tags that you specify for the tuning job are also added to all training jobs that the tuning job launches.
    tags :: Lude.Maybe [Tag],
    -- | A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
    trainingJobDefinitions :: Lude.Maybe (Lude.NonEmpty HyperParameterTrainingJobDefinition)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHyperParameterTuningJob' with the minimum fields required to make a request.
--
-- * 'trainingJobDefinition' - The 'HyperParameterTrainingJobDefinition' object that describes the training jobs that this tuning job launches, including static hyperparameters, input data configuration, output data configuration, resource configuration, and stopping condition.
-- * 'hyperParameterTuningJobName' - The name of the tuning job. This name is the prefix for the names of all training jobs that this tuning job launches. The name must be unique within the same AWS account and AWS Region. The name must have 1 to 32 characters. Valid characters are a-z, A-Z, 0-9, and : + = @ _ % - (hyphen). The name is not case sensitive.
-- * 'hyperParameterTuningJobConfig' - The 'HyperParameterTuningJobConfig' object that describes the tuning job, including the search strategy, the objective metric used to evaluate training jobs, ranges of parameters to search, and resource limits for the tuning job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works> .
-- * 'warmStartConfig' - Specifies the configuration for starting the hyperparameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
--
-- All training jobs launched by the new hyperparameter tuning job are evaluated by using the objective metric. If you specify @IDENTICAL_DATA_AND_ALGORITHM@ as the @WarmStartType@ value for the warm start configuration, the training job that performs the best in the new tuning job is compared to the best training jobs from the parent tuning jobs. From these, the training job that performs the best as measured by the objective metric is returned as the overall best training job.
-- * 'tags' - An array of key-value pairs. You can use tags to categorize your AWS resources in different ways, for example, by purpose, owner, or environment. For more information, see <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies> .
--
-- Tags that you specify for the tuning job are also added to all training jobs that the tuning job launches.
-- * 'trainingJobDefinitions' - A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
mkCreateHyperParameterTuningJob ::
  -- | 'hyperParameterTuningJobName'
  Lude.Text ->
  -- | 'hyperParameterTuningJobConfig'
  HyperParameterTuningJobConfig ->
  CreateHyperParameterTuningJob
mkCreateHyperParameterTuningJob
  pHyperParameterTuningJobName_
  pHyperParameterTuningJobConfig_ =
    CreateHyperParameterTuningJob'
      { trainingJobDefinition =
          Lude.Nothing,
        hyperParameterTuningJobName = pHyperParameterTuningJobName_,
        hyperParameterTuningJobConfig = pHyperParameterTuningJobConfig_,
        warmStartConfig = Lude.Nothing,
        tags = Lude.Nothing,
        trainingJobDefinitions = Lude.Nothing
      }

-- | The 'HyperParameterTrainingJobDefinition' object that describes the training jobs that this tuning job launches, including static hyperparameters, input data configuration, output data configuration, resource configuration, and stopping condition.
--
-- /Note:/ Consider using 'trainingJobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjTrainingJobDefinition :: Lens.Lens' CreateHyperParameterTuningJob (Lude.Maybe HyperParameterTrainingJobDefinition)
chptjTrainingJobDefinition = Lens.lens (trainingJobDefinition :: CreateHyperParameterTuningJob -> Lude.Maybe HyperParameterTrainingJobDefinition) (\s a -> s {trainingJobDefinition = a} :: CreateHyperParameterTuningJob)
{-# DEPRECATED chptjTrainingJobDefinition "Use generic-lens or generic-optics with 'trainingJobDefinition' instead." #-}

-- | The name of the tuning job. This name is the prefix for the names of all training jobs that this tuning job launches. The name must be unique within the same AWS account and AWS Region. The name must have 1 to 32 characters. Valid characters are a-z, A-Z, 0-9, and : + = @ _ % - (hyphen). The name is not case sensitive.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjHyperParameterTuningJobName :: Lens.Lens' CreateHyperParameterTuningJob Lude.Text
chptjHyperParameterTuningJobName = Lens.lens (hyperParameterTuningJobName :: CreateHyperParameterTuningJob -> Lude.Text) (\s a -> s {hyperParameterTuningJobName = a} :: CreateHyperParameterTuningJob)
{-# DEPRECATED chptjHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

-- | The 'HyperParameterTuningJobConfig' object that describes the tuning job, including the search strategy, the objective metric used to evaluate training jobs, ranges of parameters to search, and resource limits for the tuning job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works> .
--
-- /Note:/ Consider using 'hyperParameterTuningJobConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjHyperParameterTuningJobConfig :: Lens.Lens' CreateHyperParameterTuningJob HyperParameterTuningJobConfig
chptjHyperParameterTuningJobConfig = Lens.lens (hyperParameterTuningJobConfig :: CreateHyperParameterTuningJob -> HyperParameterTuningJobConfig) (\s a -> s {hyperParameterTuningJobConfig = a} :: CreateHyperParameterTuningJob)
{-# DEPRECATED chptjHyperParameterTuningJobConfig "Use generic-lens or generic-optics with 'hyperParameterTuningJobConfig' instead." #-}

-- | Specifies the configuration for starting the hyperparameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
--
-- All training jobs launched by the new hyperparameter tuning job are evaluated by using the objective metric. If you specify @IDENTICAL_DATA_AND_ALGORITHM@ as the @WarmStartType@ value for the warm start configuration, the training job that performs the best in the new tuning job is compared to the best training jobs from the parent tuning jobs. From these, the training job that performs the best as measured by the objective metric is returned as the overall best training job.
--
-- /Note:/ Consider using 'warmStartConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjWarmStartConfig :: Lens.Lens' CreateHyperParameterTuningJob (Lude.Maybe HyperParameterTuningJobWarmStartConfig)
chptjWarmStartConfig = Lens.lens (warmStartConfig :: CreateHyperParameterTuningJob -> Lude.Maybe HyperParameterTuningJobWarmStartConfig) (\s a -> s {warmStartConfig = a} :: CreateHyperParameterTuningJob)
{-# DEPRECATED chptjWarmStartConfig "Use generic-lens or generic-optics with 'warmStartConfig' instead." #-}

-- | An array of key-value pairs. You can use tags to categorize your AWS resources in different ways, for example, by purpose, owner, or environment. For more information, see <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies> .
--
-- Tags that you specify for the tuning job are also added to all training jobs that the tuning job launches.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjTags :: Lens.Lens' CreateHyperParameterTuningJob (Lude.Maybe [Tag])
chptjTags = Lens.lens (tags :: CreateHyperParameterTuningJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateHyperParameterTuningJob)
{-# DEPRECATED chptjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
--
-- /Note:/ Consider using 'trainingJobDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjTrainingJobDefinitions :: Lens.Lens' CreateHyperParameterTuningJob (Lude.Maybe (Lude.NonEmpty HyperParameterTrainingJobDefinition))
chptjTrainingJobDefinitions = Lens.lens (trainingJobDefinitions :: CreateHyperParameterTuningJob -> Lude.Maybe (Lude.NonEmpty HyperParameterTrainingJobDefinition)) (\s a -> s {trainingJobDefinitions = a} :: CreateHyperParameterTuningJob)
{-# DEPRECATED chptjTrainingJobDefinitions "Use generic-lens or generic-optics with 'trainingJobDefinitions' instead." #-}

instance Lude.AWSRequest CreateHyperParameterTuningJob where
  type
    Rs CreateHyperParameterTuningJob =
      CreateHyperParameterTuningJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateHyperParameterTuningJobResponse'
            Lude.<$> (x Lude..:> "HyperParameterTuningJobArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateHyperParameterTuningJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateHyperParameterTuningJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateHyperParameterTuningJob where
  toJSON CreateHyperParameterTuningJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TrainingJobDefinition" Lude..=) Lude.<$> trainingJobDefinition,
            Lude.Just
              ( "HyperParameterTuningJobName"
                  Lude..= hyperParameterTuningJobName
              ),
            Lude.Just
              ( "HyperParameterTuningJobConfig"
                  Lude..= hyperParameterTuningJobConfig
              ),
            ("WarmStartConfig" Lude..=) Lude.<$> warmStartConfig,
            ("Tags" Lude..=) Lude.<$> tags,
            ("TrainingJobDefinitions" Lude..=)
              Lude.<$> trainingJobDefinitions
          ]
      )

instance Lude.ToPath CreateHyperParameterTuningJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateHyperParameterTuningJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateHyperParameterTuningJobResponse' smart constructor.
data CreateHyperParameterTuningJobResponse = CreateHyperParameterTuningJobResponse'
  { -- | The Amazon Resource Name (ARN) of the tuning job. Amazon SageMaker assigns an ARN to a hyperparameter tuning job when you create it.
    hyperParameterTuningJobARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHyperParameterTuningJobResponse' with the minimum fields required to make a request.
--
-- * 'hyperParameterTuningJobARN' - The Amazon Resource Name (ARN) of the tuning job. Amazon SageMaker assigns an ARN to a hyperparameter tuning job when you create it.
-- * 'responseStatus' - The response status code.
mkCreateHyperParameterTuningJobResponse ::
  -- | 'hyperParameterTuningJobARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateHyperParameterTuningJobResponse
mkCreateHyperParameterTuningJobResponse
  pHyperParameterTuningJobARN_
  pResponseStatus_ =
    CreateHyperParameterTuningJobResponse'
      { hyperParameterTuningJobARN =
          pHyperParameterTuningJobARN_,
        responseStatus = pResponseStatus_
      }

-- | The Amazon Resource Name (ARN) of the tuning job. Amazon SageMaker assigns an ARN to a hyperparameter tuning job when you create it.
--
-- /Note:/ Consider using 'hyperParameterTuningJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjrsHyperParameterTuningJobARN :: Lens.Lens' CreateHyperParameterTuningJobResponse Lude.Text
chptjrsHyperParameterTuningJobARN = Lens.lens (hyperParameterTuningJobARN :: CreateHyperParameterTuningJobResponse -> Lude.Text) (\s a -> s {hyperParameterTuningJobARN = a} :: CreateHyperParameterTuningJobResponse)
{-# DEPRECATED chptjrsHyperParameterTuningJobARN "Use generic-lens or generic-optics with 'hyperParameterTuningJobARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjrsResponseStatus :: Lens.Lens' CreateHyperParameterTuningJobResponse Lude.Int
chptjrsResponseStatus = Lens.lens (responseStatus :: CreateHyperParameterTuningJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateHyperParameterTuningJobResponse)
{-# DEPRECATED chptjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
