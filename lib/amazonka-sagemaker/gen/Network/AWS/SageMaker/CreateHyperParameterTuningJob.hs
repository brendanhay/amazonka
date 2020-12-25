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
    chptjHyperParameterTuningJobName,
    chptjHyperParameterTuningJobConfig,
    chptjTags,
    chptjTrainingJobDefinition,
    chptjTrainingJobDefinitions,
    chptjWarmStartConfig,

    -- * Destructuring the response
    CreateHyperParameterTuningJobResponse (..),
    mkCreateHyperParameterTuningJobResponse,

    -- ** Response lenses
    chptjrrsHyperParameterTuningJobArn,
    chptjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateHyperParameterTuningJob' smart constructor.
data CreateHyperParameterTuningJob = CreateHyperParameterTuningJob'
  { -- | The name of the tuning job. This name is the prefix for the names of all training jobs that this tuning job launches. The name must be unique within the same AWS account and AWS Region. The name must have 1 to 32 characters. Valid characters are a-z, A-Z, 0-9, and : + = @ _ % - (hyphen). The name is not case sensitive.
    hyperParameterTuningJobName :: Types.HyperParameterTuningJobName,
    -- | The 'HyperParameterTuningJobConfig' object that describes the tuning job, including the search strategy, the objective metric used to evaluate training jobs, ranges of parameters to search, and resource limits for the tuning job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works> .
    hyperParameterTuningJobConfig :: Types.HyperParameterTuningJobConfig,
    -- | An array of key-value pairs. You can use tags to categorize your AWS resources in different ways, for example, by purpose, owner, or environment. For more information, see <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies> .
    --
    -- Tags that you specify for the tuning job are also added to all training jobs that the tuning job launches.
    tags :: Core.Maybe [Types.Tag],
    -- | The 'HyperParameterTrainingJobDefinition' object that describes the training jobs that this tuning job launches, including static hyperparameters, input data configuration, output data configuration, resource configuration, and stopping condition.
    trainingJobDefinition :: Core.Maybe Types.HyperParameterTrainingJobDefinition,
    -- | A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
    trainingJobDefinitions :: Core.Maybe (Core.NonEmpty Types.HyperParameterTrainingJobDefinition),
    -- | Specifies the configuration for starting the hyperparameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
    --
    -- All training jobs launched by the new hyperparameter tuning job are evaluated by using the objective metric. If you specify @IDENTICAL_DATA_AND_ALGORITHM@ as the @WarmStartType@ value for the warm start configuration, the training job that performs the best in the new tuning job is compared to the best training jobs from the parent tuning jobs. From these, the training job that performs the best as measured by the objective metric is returned as the overall best training job.
    warmStartConfig :: Core.Maybe Types.HyperParameterTuningJobWarmStartConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHyperParameterTuningJob' value with any optional fields omitted.
mkCreateHyperParameterTuningJob ::
  -- | 'hyperParameterTuningJobName'
  Types.HyperParameterTuningJobName ->
  -- | 'hyperParameterTuningJobConfig'
  Types.HyperParameterTuningJobConfig ->
  CreateHyperParameterTuningJob
mkCreateHyperParameterTuningJob
  hyperParameterTuningJobName
  hyperParameterTuningJobConfig =
    CreateHyperParameterTuningJob'
      { hyperParameterTuningJobName,
        hyperParameterTuningJobConfig,
        tags = Core.Nothing,
        trainingJobDefinition = Core.Nothing,
        trainingJobDefinitions = Core.Nothing,
        warmStartConfig = Core.Nothing
      }

-- | The name of the tuning job. This name is the prefix for the names of all training jobs that this tuning job launches. The name must be unique within the same AWS account and AWS Region. The name must have 1 to 32 characters. Valid characters are a-z, A-Z, 0-9, and : + = @ _ % - (hyphen). The name is not case sensitive.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjHyperParameterTuningJobName :: Lens.Lens' CreateHyperParameterTuningJob Types.HyperParameterTuningJobName
chptjHyperParameterTuningJobName = Lens.field @"hyperParameterTuningJobName"
{-# DEPRECATED chptjHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

-- | The 'HyperParameterTuningJobConfig' object that describes the tuning job, including the search strategy, the objective metric used to evaluate training jobs, ranges of parameters to search, and resource limits for the tuning job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works> .
--
-- /Note:/ Consider using 'hyperParameterTuningJobConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjHyperParameterTuningJobConfig :: Lens.Lens' CreateHyperParameterTuningJob Types.HyperParameterTuningJobConfig
chptjHyperParameterTuningJobConfig = Lens.field @"hyperParameterTuningJobConfig"
{-# DEPRECATED chptjHyperParameterTuningJobConfig "Use generic-lens or generic-optics with 'hyperParameterTuningJobConfig' instead." #-}

-- | An array of key-value pairs. You can use tags to categorize your AWS resources in different ways, for example, by purpose, owner, or environment. For more information, see <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies> .
--
-- Tags that you specify for the tuning job are also added to all training jobs that the tuning job launches.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjTags :: Lens.Lens' CreateHyperParameterTuningJob (Core.Maybe [Types.Tag])
chptjTags = Lens.field @"tags"
{-# DEPRECATED chptjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The 'HyperParameterTrainingJobDefinition' object that describes the training jobs that this tuning job launches, including static hyperparameters, input data configuration, output data configuration, resource configuration, and stopping condition.
--
-- /Note:/ Consider using 'trainingJobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjTrainingJobDefinition :: Lens.Lens' CreateHyperParameterTuningJob (Core.Maybe Types.HyperParameterTrainingJobDefinition)
chptjTrainingJobDefinition = Lens.field @"trainingJobDefinition"
{-# DEPRECATED chptjTrainingJobDefinition "Use generic-lens or generic-optics with 'trainingJobDefinition' instead." #-}

-- | A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
--
-- /Note:/ Consider using 'trainingJobDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjTrainingJobDefinitions :: Lens.Lens' CreateHyperParameterTuningJob (Core.Maybe (Core.NonEmpty Types.HyperParameterTrainingJobDefinition))
chptjTrainingJobDefinitions = Lens.field @"trainingJobDefinitions"
{-# DEPRECATED chptjTrainingJobDefinitions "Use generic-lens or generic-optics with 'trainingJobDefinitions' instead." #-}

-- | Specifies the configuration for starting the hyperparameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
--
-- All training jobs launched by the new hyperparameter tuning job are evaluated by using the objective metric. If you specify @IDENTICAL_DATA_AND_ALGORITHM@ as the @WarmStartType@ value for the warm start configuration, the training job that performs the best in the new tuning job is compared to the best training jobs from the parent tuning jobs. From these, the training job that performs the best as measured by the objective metric is returned as the overall best training job.
--
-- /Note:/ Consider using 'warmStartConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjWarmStartConfig :: Lens.Lens' CreateHyperParameterTuningJob (Core.Maybe Types.HyperParameterTuningJobWarmStartConfig)
chptjWarmStartConfig = Lens.field @"warmStartConfig"
{-# DEPRECATED chptjWarmStartConfig "Use generic-lens or generic-optics with 'warmStartConfig' instead." #-}

instance Core.FromJSON CreateHyperParameterTuningJob where
  toJSON CreateHyperParameterTuningJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "HyperParameterTuningJobName"
                  Core..= hyperParameterTuningJobName
              ),
            Core.Just
              ( "HyperParameterTuningJobConfig"
                  Core..= hyperParameterTuningJobConfig
              ),
            ("Tags" Core..=) Core.<$> tags,
            ("TrainingJobDefinition" Core..=) Core.<$> trainingJobDefinition,
            ("TrainingJobDefinitions" Core..=) Core.<$> trainingJobDefinitions,
            ("WarmStartConfig" Core..=) Core.<$> warmStartConfig
          ]
      )

instance Core.AWSRequest CreateHyperParameterTuningJob where
  type
    Rs CreateHyperParameterTuningJob =
      CreateHyperParameterTuningJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SageMaker.CreateHyperParameterTuningJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHyperParameterTuningJobResponse'
            Core.<$> (x Core..: "HyperParameterTuningJobArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateHyperParameterTuningJobResponse' smart constructor.
data CreateHyperParameterTuningJobResponse = CreateHyperParameterTuningJobResponse'
  { -- | The Amazon Resource Name (ARN) of the tuning job. Amazon SageMaker assigns an ARN to a hyperparameter tuning job when you create it.
    hyperParameterTuningJobArn :: Types.HyperParameterTuningJobArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHyperParameterTuningJobResponse' value with any optional fields omitted.
mkCreateHyperParameterTuningJobResponse ::
  -- | 'hyperParameterTuningJobArn'
  Types.HyperParameterTuningJobArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateHyperParameterTuningJobResponse
mkCreateHyperParameterTuningJobResponse
  hyperParameterTuningJobArn
  responseStatus =
    CreateHyperParameterTuningJobResponse'
      { hyperParameterTuningJobArn,
        responseStatus
      }

-- | The Amazon Resource Name (ARN) of the tuning job. Amazon SageMaker assigns an ARN to a hyperparameter tuning job when you create it.
--
-- /Note:/ Consider using 'hyperParameterTuningJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjrrsHyperParameterTuningJobArn :: Lens.Lens' CreateHyperParameterTuningJobResponse Types.HyperParameterTuningJobArn
chptjrrsHyperParameterTuningJobArn = Lens.field @"hyperParameterTuningJobArn"
{-# DEPRECATED chptjrrsHyperParameterTuningJobArn "Use generic-lens or generic-optics with 'hyperParameterTuningJobArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chptjrrsResponseStatus :: Lens.Lens' CreateHyperParameterTuningJobResponse Core.Int
chptjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED chptjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
