{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeHyperParameterTuningJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description of a hyperparameter tuning job.
module Network.AWS.SageMaker.DescribeHyperParameterTuningJob
  ( -- * Creating a request
    DescribeHyperParameterTuningJob (..),
    mkDescribeHyperParameterTuningJob,

    -- ** Request lenses
    dhptjHyperParameterTuningJobName,

    -- * Destructuring the response
    DescribeHyperParameterTuningJobResponse (..),
    mkDescribeHyperParameterTuningJobResponse,

    -- ** Response lenses
    dhptjrrsHyperParameterTuningJobName,
    dhptjrrsHyperParameterTuningJobArn,
    dhptjrrsHyperParameterTuningJobConfig,
    dhptjrrsHyperParameterTuningJobStatus,
    dhptjrrsCreationTime,
    dhptjrrsTrainingJobStatusCounters,
    dhptjrrsObjectiveStatusCounters,
    dhptjrrsBestTrainingJob,
    dhptjrrsFailureReason,
    dhptjrrsHyperParameterTuningEndTime,
    dhptjrrsLastModifiedTime,
    dhptjrrsOverallBestTrainingJob,
    dhptjrrsTrainingJobDefinition,
    dhptjrrsTrainingJobDefinitions,
    dhptjrrsWarmStartConfig,
    dhptjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeHyperParameterTuningJob' smart constructor.
newtype DescribeHyperParameterTuningJob = DescribeHyperParameterTuningJob'
  { -- | The name of the tuning job.
    hyperParameterTuningJobName :: Types.HyperParameterTuningJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHyperParameterTuningJob' value with any optional fields omitted.
mkDescribeHyperParameterTuningJob ::
  -- | 'hyperParameterTuningJobName'
  Types.HyperParameterTuningJobName ->
  DescribeHyperParameterTuningJob
mkDescribeHyperParameterTuningJob hyperParameterTuningJobName =
  DescribeHyperParameterTuningJob' {hyperParameterTuningJobName}

-- | The name of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjHyperParameterTuningJobName :: Lens.Lens' DescribeHyperParameterTuningJob Types.HyperParameterTuningJobName
dhptjHyperParameterTuningJobName = Lens.field @"hyperParameterTuningJobName"
{-# DEPRECATED dhptjHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

instance Core.FromJSON DescribeHyperParameterTuningJob where
  toJSON DescribeHyperParameterTuningJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "HyperParameterTuningJobName"
                  Core..= hyperParameterTuningJobName
              )
          ]
      )

instance Core.AWSRequest DescribeHyperParameterTuningJob where
  type
    Rs DescribeHyperParameterTuningJob =
      DescribeHyperParameterTuningJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SageMaker.DescribeHyperParameterTuningJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHyperParameterTuningJobResponse'
            Core.<$> (x Core..: "HyperParameterTuningJobName")
            Core.<*> (x Core..: "HyperParameterTuningJobArn")
            Core.<*> (x Core..: "HyperParameterTuningJobConfig")
            Core.<*> (x Core..: "HyperParameterTuningJobStatus")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "TrainingJobStatusCounters")
            Core.<*> (x Core..: "ObjectiveStatusCounters")
            Core.<*> (x Core..:? "BestTrainingJob")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "HyperParameterTuningEndTime")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "OverallBestTrainingJob")
            Core.<*> (x Core..:? "TrainingJobDefinition")
            Core.<*> (x Core..:? "TrainingJobDefinitions")
            Core.<*> (x Core..:? "WarmStartConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeHyperParameterTuningJobResponse' smart constructor.
data DescribeHyperParameterTuningJobResponse = DescribeHyperParameterTuningJobResponse'
  { -- | The name of the tuning job.
    hyperParameterTuningJobName :: Types.HyperParameterTuningJobName,
    -- | The Amazon Resource Name (ARN) of the tuning job.
    hyperParameterTuningJobArn :: Types.HyperParameterTuningJobArn,
    -- | The 'HyperParameterTuningJobConfig' object that specifies the configuration of the tuning job.
    hyperParameterTuningJobConfig :: Types.HyperParameterTuningJobConfig,
    -- | The status of the tuning job: InProgress, Completed, Failed, Stopping, or Stopped.
    hyperParameterTuningJobStatus :: Types.HyperParameterTuningJobStatus,
    -- | The date and time that the tuning job started.
    creationTime :: Core.NominalDiffTime,
    -- | The 'TrainingJobStatusCounters' object that specifies the number of training jobs, categorized by status, that this tuning job launched.
    trainingJobStatusCounters :: Types.TrainingJobStatusCounters,
    -- | The 'ObjectiveStatusCounters' object that specifies the number of training jobs, categorized by the status of their final objective metric, that this tuning job launched.
    objectiveStatusCounters :: Types.ObjectiveStatusCounters,
    -- | A 'TrainingJobSummary' object that describes the training job that completed with the best current 'HyperParameterTuningJobObjective' .
    bestTrainingJob :: Core.Maybe Types.HyperParameterTrainingJobSummary,
    -- | If the tuning job failed, the reason it failed.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The date and time that the tuning job ended.
    hyperParameterTuningEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time that the status of the tuning job was modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | If the hyperparameter tuning job is an warm start tuning job with a @WarmStartType@ of @IDENTICAL_DATA_AND_ALGORITHM@ , this is the 'TrainingJobSummary' for the training job with the best objective metric value of all training jobs launched by this tuning job and all parent jobs specified for the warm start tuning job.
    overallBestTrainingJob :: Core.Maybe Types.HyperParameterTrainingJobSummary,
    -- | The 'HyperParameterTrainingJobDefinition' object that specifies the definition of the training jobs that this tuning job launches.
    trainingJobDefinition :: Core.Maybe Types.HyperParameterTrainingJobDefinition,
    -- | A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
    trainingJobDefinitions :: Core.Maybe (Core.NonEmpty Types.HyperParameterTrainingJobDefinition),
    -- | The configuration for starting the hyperparameter parameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
    warmStartConfig :: Core.Maybe Types.HyperParameterTuningJobWarmStartConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeHyperParameterTuningJobResponse' value with any optional fields omitted.
mkDescribeHyperParameterTuningJobResponse ::
  -- | 'hyperParameterTuningJobName'
  Types.HyperParameterTuningJobName ->
  -- | 'hyperParameterTuningJobArn'
  Types.HyperParameterTuningJobArn ->
  -- | 'hyperParameterTuningJobConfig'
  Types.HyperParameterTuningJobConfig ->
  -- | 'hyperParameterTuningJobStatus'
  Types.HyperParameterTuningJobStatus ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'trainingJobStatusCounters'
  Types.TrainingJobStatusCounters ->
  -- | 'objectiveStatusCounters'
  Types.ObjectiveStatusCounters ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeHyperParameterTuningJobResponse
mkDescribeHyperParameterTuningJobResponse
  hyperParameterTuningJobName
  hyperParameterTuningJobArn
  hyperParameterTuningJobConfig
  hyperParameterTuningJobStatus
  creationTime
  trainingJobStatusCounters
  objectiveStatusCounters
  responseStatus =
    DescribeHyperParameterTuningJobResponse'
      { hyperParameterTuningJobName,
        hyperParameterTuningJobArn,
        hyperParameterTuningJobConfig,
        hyperParameterTuningJobStatus,
        creationTime,
        trainingJobStatusCounters,
        objectiveStatusCounters,
        bestTrainingJob = Core.Nothing,
        failureReason = Core.Nothing,
        hyperParameterTuningEndTime = Core.Nothing,
        lastModifiedTime = Core.Nothing,
        overallBestTrainingJob = Core.Nothing,
        trainingJobDefinition = Core.Nothing,
        trainingJobDefinitions = Core.Nothing,
        warmStartConfig = Core.Nothing,
        responseStatus
      }

-- | The name of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsHyperParameterTuningJobName :: Lens.Lens' DescribeHyperParameterTuningJobResponse Types.HyperParameterTuningJobName
dhptjrrsHyperParameterTuningJobName = Lens.field @"hyperParameterTuningJobName"
{-# DEPRECATED dhptjrrsHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

-- | The Amazon Resource Name (ARN) of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsHyperParameterTuningJobArn :: Lens.Lens' DescribeHyperParameterTuningJobResponse Types.HyperParameterTuningJobArn
dhptjrrsHyperParameterTuningJobArn = Lens.field @"hyperParameterTuningJobArn"
{-# DEPRECATED dhptjrrsHyperParameterTuningJobArn "Use generic-lens or generic-optics with 'hyperParameterTuningJobArn' instead." #-}

-- | The 'HyperParameterTuningJobConfig' object that specifies the configuration of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsHyperParameterTuningJobConfig :: Lens.Lens' DescribeHyperParameterTuningJobResponse Types.HyperParameterTuningJobConfig
dhptjrrsHyperParameterTuningJobConfig = Lens.field @"hyperParameterTuningJobConfig"
{-# DEPRECATED dhptjrrsHyperParameterTuningJobConfig "Use generic-lens or generic-optics with 'hyperParameterTuningJobConfig' instead." #-}

-- | The status of the tuning job: InProgress, Completed, Failed, Stopping, or Stopped.
--
-- /Note:/ Consider using 'hyperParameterTuningJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsHyperParameterTuningJobStatus :: Lens.Lens' DescribeHyperParameterTuningJobResponse Types.HyperParameterTuningJobStatus
dhptjrrsHyperParameterTuningJobStatus = Lens.field @"hyperParameterTuningJobStatus"
{-# DEPRECATED dhptjrrsHyperParameterTuningJobStatus "Use generic-lens or generic-optics with 'hyperParameterTuningJobStatus' instead." #-}

-- | The date and time that the tuning job started.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsCreationTime :: Lens.Lens' DescribeHyperParameterTuningJobResponse Core.NominalDiffTime
dhptjrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dhptjrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The 'TrainingJobStatusCounters' object that specifies the number of training jobs, categorized by status, that this tuning job launched.
--
-- /Note:/ Consider using 'trainingJobStatusCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsTrainingJobStatusCounters :: Lens.Lens' DescribeHyperParameterTuningJobResponse Types.TrainingJobStatusCounters
dhptjrrsTrainingJobStatusCounters = Lens.field @"trainingJobStatusCounters"
{-# DEPRECATED dhptjrrsTrainingJobStatusCounters "Use generic-lens or generic-optics with 'trainingJobStatusCounters' instead." #-}

-- | The 'ObjectiveStatusCounters' object that specifies the number of training jobs, categorized by the status of their final objective metric, that this tuning job launched.
--
-- /Note:/ Consider using 'objectiveStatusCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsObjectiveStatusCounters :: Lens.Lens' DescribeHyperParameterTuningJobResponse Types.ObjectiveStatusCounters
dhptjrrsObjectiveStatusCounters = Lens.field @"objectiveStatusCounters"
{-# DEPRECATED dhptjrrsObjectiveStatusCounters "Use generic-lens or generic-optics with 'objectiveStatusCounters' instead." #-}

-- | A 'TrainingJobSummary' object that describes the training job that completed with the best current 'HyperParameterTuningJobObjective' .
--
-- /Note:/ Consider using 'bestTrainingJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsBestTrainingJob :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Core.Maybe Types.HyperParameterTrainingJobSummary)
dhptjrrsBestTrainingJob = Lens.field @"bestTrainingJob"
{-# DEPRECATED dhptjrrsBestTrainingJob "Use generic-lens or generic-optics with 'bestTrainingJob' instead." #-}

-- | If the tuning job failed, the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsFailureReason :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Core.Maybe Types.FailureReason)
dhptjrrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED dhptjrrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The date and time that the tuning job ended.
--
-- /Note:/ Consider using 'hyperParameterTuningEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsHyperParameterTuningEndTime :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Core.Maybe Core.NominalDiffTime)
dhptjrrsHyperParameterTuningEndTime = Lens.field @"hyperParameterTuningEndTime"
{-# DEPRECATED dhptjrrsHyperParameterTuningEndTime "Use generic-lens or generic-optics with 'hyperParameterTuningEndTime' instead." #-}

-- | The date and time that the status of the tuning job was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsLastModifiedTime :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Core.Maybe Core.NominalDiffTime)
dhptjrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED dhptjrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | If the hyperparameter tuning job is an warm start tuning job with a @WarmStartType@ of @IDENTICAL_DATA_AND_ALGORITHM@ , this is the 'TrainingJobSummary' for the training job with the best objective metric value of all training jobs launched by this tuning job and all parent jobs specified for the warm start tuning job.
--
-- /Note:/ Consider using 'overallBestTrainingJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsOverallBestTrainingJob :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Core.Maybe Types.HyperParameterTrainingJobSummary)
dhptjrrsOverallBestTrainingJob = Lens.field @"overallBestTrainingJob"
{-# DEPRECATED dhptjrrsOverallBestTrainingJob "Use generic-lens or generic-optics with 'overallBestTrainingJob' instead." #-}

-- | The 'HyperParameterTrainingJobDefinition' object that specifies the definition of the training jobs that this tuning job launches.
--
-- /Note:/ Consider using 'trainingJobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsTrainingJobDefinition :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Core.Maybe Types.HyperParameterTrainingJobDefinition)
dhptjrrsTrainingJobDefinition = Lens.field @"trainingJobDefinition"
{-# DEPRECATED dhptjrrsTrainingJobDefinition "Use generic-lens or generic-optics with 'trainingJobDefinition' instead." #-}

-- | A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
--
-- /Note:/ Consider using 'trainingJobDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsTrainingJobDefinitions :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Core.Maybe (Core.NonEmpty Types.HyperParameterTrainingJobDefinition))
dhptjrrsTrainingJobDefinitions = Lens.field @"trainingJobDefinitions"
{-# DEPRECATED dhptjrrsTrainingJobDefinitions "Use generic-lens or generic-optics with 'trainingJobDefinitions' instead." #-}

-- | The configuration for starting the hyperparameter parameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
--
-- /Note:/ Consider using 'warmStartConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsWarmStartConfig :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Core.Maybe Types.HyperParameterTuningJobWarmStartConfig)
dhptjrrsWarmStartConfig = Lens.field @"warmStartConfig"
{-# DEPRECATED dhptjrrsWarmStartConfig "Use generic-lens or generic-optics with 'warmStartConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrrsResponseStatus :: Lens.Lens' DescribeHyperParameterTuningJobResponse Core.Int
dhptjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dhptjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
