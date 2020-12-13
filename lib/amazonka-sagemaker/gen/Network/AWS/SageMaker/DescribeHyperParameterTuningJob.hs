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
    dhptjrsCreationTime,
    dhptjrsTrainingJobStatusCounters,
    dhptjrsFailureReason,
    dhptjrsObjectiveStatusCounters,
    dhptjrsHyperParameterTuningJobARN,
    dhptjrsTrainingJobDefinition,
    dhptjrsHyperParameterTuningJobName,
    dhptjrsLastModifiedTime,
    dhptjrsBestTrainingJob,
    dhptjrsHyperParameterTuningEndTime,
    dhptjrsHyperParameterTuningJobConfig,
    dhptjrsOverallBestTrainingJob,
    dhptjrsWarmStartConfig,
    dhptjrsHyperParameterTuningJobStatus,
    dhptjrsTrainingJobDefinitions,
    dhptjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeHyperParameterTuningJob' smart constructor.
newtype DescribeHyperParameterTuningJob = DescribeHyperParameterTuningJob'
  { -- | The name of the tuning job.
    hyperParameterTuningJobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHyperParameterTuningJob' with the minimum fields required to make a request.
--
-- * 'hyperParameterTuningJobName' - The name of the tuning job.
mkDescribeHyperParameterTuningJob ::
  -- | 'hyperParameterTuningJobName'
  Lude.Text ->
  DescribeHyperParameterTuningJob
mkDescribeHyperParameterTuningJob pHyperParameterTuningJobName_ =
  DescribeHyperParameterTuningJob'
    { hyperParameterTuningJobName =
        pHyperParameterTuningJobName_
    }

-- | The name of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjHyperParameterTuningJobName :: Lens.Lens' DescribeHyperParameterTuningJob Lude.Text
dhptjHyperParameterTuningJobName = Lens.lens (hyperParameterTuningJobName :: DescribeHyperParameterTuningJob -> Lude.Text) (\s a -> s {hyperParameterTuningJobName = a} :: DescribeHyperParameterTuningJob)
{-# DEPRECATED dhptjHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

instance Lude.AWSRequest DescribeHyperParameterTuningJob where
  type
    Rs DescribeHyperParameterTuningJob =
      DescribeHyperParameterTuningJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeHyperParameterTuningJobResponse'
            Lude.<$> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "TrainingJobStatusCounters")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..:> "ObjectiveStatusCounters")
            Lude.<*> (x Lude..:> "HyperParameterTuningJobArn")
            Lude.<*> (x Lude..?> "TrainingJobDefinition")
            Lude.<*> (x Lude..:> "HyperParameterTuningJobName")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "BestTrainingJob")
            Lude.<*> (x Lude..?> "HyperParameterTuningEndTime")
            Lude.<*> (x Lude..:> "HyperParameterTuningJobConfig")
            Lude.<*> (x Lude..?> "OverallBestTrainingJob")
            Lude.<*> (x Lude..?> "WarmStartConfig")
            Lude.<*> (x Lude..:> "HyperParameterTuningJobStatus")
            Lude.<*> (x Lude..?> "TrainingJobDefinitions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeHyperParameterTuningJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeHyperParameterTuningJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeHyperParameterTuningJob where
  toJSON DescribeHyperParameterTuningJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "HyperParameterTuningJobName"
                  Lude..= hyperParameterTuningJobName
              )
          ]
      )

instance Lude.ToPath DescribeHyperParameterTuningJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHyperParameterTuningJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeHyperParameterTuningJobResponse' smart constructor.
data DescribeHyperParameterTuningJobResponse = DescribeHyperParameterTuningJobResponse'
  { -- | The date and time that the tuning job started.
    creationTime :: Lude.Timestamp,
    -- | The 'TrainingJobStatusCounters' object that specifies the number of training jobs, categorized by status, that this tuning job launched.
    trainingJobStatusCounters :: TrainingJobStatusCounters,
    -- | If the tuning job failed, the reason it failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The 'ObjectiveStatusCounters' object that specifies the number of training jobs, categorized by the status of their final objective metric, that this tuning job launched.
    objectiveStatusCounters :: ObjectiveStatusCounters,
    -- | The Amazon Resource Name (ARN) of the tuning job.
    hyperParameterTuningJobARN :: Lude.Text,
    -- | The 'HyperParameterTrainingJobDefinition' object that specifies the definition of the training jobs that this tuning job launches.
    trainingJobDefinition :: Lude.Maybe HyperParameterTrainingJobDefinition,
    -- | The name of the tuning job.
    hyperParameterTuningJobName :: Lude.Text,
    -- | The date and time that the status of the tuning job was modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | A 'TrainingJobSummary' object that describes the training job that completed with the best current 'HyperParameterTuningJobObjective' .
    bestTrainingJob :: Lude.Maybe HyperParameterTrainingJobSummary,
    -- | The date and time that the tuning job ended.
    hyperParameterTuningEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The 'HyperParameterTuningJobConfig' object that specifies the configuration of the tuning job.
    hyperParameterTuningJobConfig :: HyperParameterTuningJobConfig,
    -- | If the hyperparameter tuning job is an warm start tuning job with a @WarmStartType@ of @IDENTICAL_DATA_AND_ALGORITHM@ , this is the 'TrainingJobSummary' for the training job with the best objective metric value of all training jobs launched by this tuning job and all parent jobs specified for the warm start tuning job.
    overallBestTrainingJob :: Lude.Maybe HyperParameterTrainingJobSummary,
    -- | The configuration for starting the hyperparameter parameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
    warmStartConfig :: Lude.Maybe HyperParameterTuningJobWarmStartConfig,
    -- | The status of the tuning job: InProgress, Completed, Failed, Stopping, or Stopped.
    hyperParameterTuningJobStatus :: HyperParameterTuningJobStatus,
    -- | A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
    trainingJobDefinitions :: Lude.Maybe (Lude.NonEmpty HyperParameterTrainingJobDefinition),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHyperParameterTuningJobResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time that the tuning job started.
-- * 'trainingJobStatusCounters' - The 'TrainingJobStatusCounters' object that specifies the number of training jobs, categorized by status, that this tuning job launched.
-- * 'failureReason' - If the tuning job failed, the reason it failed.
-- * 'objectiveStatusCounters' - The 'ObjectiveStatusCounters' object that specifies the number of training jobs, categorized by the status of their final objective metric, that this tuning job launched.
-- * 'hyperParameterTuningJobARN' - The Amazon Resource Name (ARN) of the tuning job.
-- * 'trainingJobDefinition' - The 'HyperParameterTrainingJobDefinition' object that specifies the definition of the training jobs that this tuning job launches.
-- * 'hyperParameterTuningJobName' - The name of the tuning job.
-- * 'lastModifiedTime' - The date and time that the status of the tuning job was modified.
-- * 'bestTrainingJob' - A 'TrainingJobSummary' object that describes the training job that completed with the best current 'HyperParameterTuningJobObjective' .
-- * 'hyperParameterTuningEndTime' - The date and time that the tuning job ended.
-- * 'hyperParameterTuningJobConfig' - The 'HyperParameterTuningJobConfig' object that specifies the configuration of the tuning job.
-- * 'overallBestTrainingJob' - If the hyperparameter tuning job is an warm start tuning job with a @WarmStartType@ of @IDENTICAL_DATA_AND_ALGORITHM@ , this is the 'TrainingJobSummary' for the training job with the best objective metric value of all training jobs launched by this tuning job and all parent jobs specified for the warm start tuning job.
-- * 'warmStartConfig' - The configuration for starting the hyperparameter parameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
-- * 'hyperParameterTuningJobStatus' - The status of the tuning job: InProgress, Completed, Failed, Stopping, or Stopped.
-- * 'trainingJobDefinitions' - A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
-- * 'responseStatus' - The response status code.
mkDescribeHyperParameterTuningJobResponse ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'trainingJobStatusCounters'
  TrainingJobStatusCounters ->
  -- | 'objectiveStatusCounters'
  ObjectiveStatusCounters ->
  -- | 'hyperParameterTuningJobARN'
  Lude.Text ->
  -- | 'hyperParameterTuningJobName'
  Lude.Text ->
  -- | 'hyperParameterTuningJobConfig'
  HyperParameterTuningJobConfig ->
  -- | 'hyperParameterTuningJobStatus'
  HyperParameterTuningJobStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHyperParameterTuningJobResponse
mkDescribeHyperParameterTuningJobResponse
  pCreationTime_
  pTrainingJobStatusCounters_
  pObjectiveStatusCounters_
  pHyperParameterTuningJobARN_
  pHyperParameterTuningJobName_
  pHyperParameterTuningJobConfig_
  pHyperParameterTuningJobStatus_
  pResponseStatus_ =
    DescribeHyperParameterTuningJobResponse'
      { creationTime =
          pCreationTime_,
        trainingJobStatusCounters =
          pTrainingJobStatusCounters_,
        failureReason = Lude.Nothing,
        objectiveStatusCounters = pObjectiveStatusCounters_,
        hyperParameterTuningJobARN =
          pHyperParameterTuningJobARN_,
        trainingJobDefinition = Lude.Nothing,
        hyperParameterTuningJobName =
          pHyperParameterTuningJobName_,
        lastModifiedTime = Lude.Nothing,
        bestTrainingJob = Lude.Nothing,
        hyperParameterTuningEndTime = Lude.Nothing,
        hyperParameterTuningJobConfig =
          pHyperParameterTuningJobConfig_,
        overallBestTrainingJob = Lude.Nothing,
        warmStartConfig = Lude.Nothing,
        hyperParameterTuningJobStatus =
          pHyperParameterTuningJobStatus_,
        trainingJobDefinitions = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The date and time that the tuning job started.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsCreationTime :: Lens.Lens' DescribeHyperParameterTuningJobResponse Lude.Timestamp
dhptjrsCreationTime = Lens.lens (creationTime :: DescribeHyperParameterTuningJobResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The 'TrainingJobStatusCounters' object that specifies the number of training jobs, categorized by status, that this tuning job launched.
--
-- /Note:/ Consider using 'trainingJobStatusCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsTrainingJobStatusCounters :: Lens.Lens' DescribeHyperParameterTuningJobResponse TrainingJobStatusCounters
dhptjrsTrainingJobStatusCounters = Lens.lens (trainingJobStatusCounters :: DescribeHyperParameterTuningJobResponse -> TrainingJobStatusCounters) (\s a -> s {trainingJobStatusCounters = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsTrainingJobStatusCounters "Use generic-lens or generic-optics with 'trainingJobStatusCounters' instead." #-}

-- | If the tuning job failed, the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsFailureReason :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Lude.Maybe Lude.Text)
dhptjrsFailureReason = Lens.lens (failureReason :: DescribeHyperParameterTuningJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The 'ObjectiveStatusCounters' object that specifies the number of training jobs, categorized by the status of their final objective metric, that this tuning job launched.
--
-- /Note:/ Consider using 'objectiveStatusCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsObjectiveStatusCounters :: Lens.Lens' DescribeHyperParameterTuningJobResponse ObjectiveStatusCounters
dhptjrsObjectiveStatusCounters = Lens.lens (objectiveStatusCounters :: DescribeHyperParameterTuningJobResponse -> ObjectiveStatusCounters) (\s a -> s {objectiveStatusCounters = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsObjectiveStatusCounters "Use generic-lens or generic-optics with 'objectiveStatusCounters' instead." #-}

-- | The Amazon Resource Name (ARN) of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsHyperParameterTuningJobARN :: Lens.Lens' DescribeHyperParameterTuningJobResponse Lude.Text
dhptjrsHyperParameterTuningJobARN = Lens.lens (hyperParameterTuningJobARN :: DescribeHyperParameterTuningJobResponse -> Lude.Text) (\s a -> s {hyperParameterTuningJobARN = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsHyperParameterTuningJobARN "Use generic-lens or generic-optics with 'hyperParameterTuningJobARN' instead." #-}

-- | The 'HyperParameterTrainingJobDefinition' object that specifies the definition of the training jobs that this tuning job launches.
--
-- /Note:/ Consider using 'trainingJobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsTrainingJobDefinition :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Lude.Maybe HyperParameterTrainingJobDefinition)
dhptjrsTrainingJobDefinition = Lens.lens (trainingJobDefinition :: DescribeHyperParameterTuningJobResponse -> Lude.Maybe HyperParameterTrainingJobDefinition) (\s a -> s {trainingJobDefinition = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsTrainingJobDefinition "Use generic-lens or generic-optics with 'trainingJobDefinition' instead." #-}

-- | The name of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsHyperParameterTuningJobName :: Lens.Lens' DescribeHyperParameterTuningJobResponse Lude.Text
dhptjrsHyperParameterTuningJobName = Lens.lens (hyperParameterTuningJobName :: DescribeHyperParameterTuningJobResponse -> Lude.Text) (\s a -> s {hyperParameterTuningJobName = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

-- | The date and time that the status of the tuning job was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsLastModifiedTime :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Lude.Maybe Lude.Timestamp)
dhptjrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeHyperParameterTuningJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | A 'TrainingJobSummary' object that describes the training job that completed with the best current 'HyperParameterTuningJobObjective' .
--
-- /Note:/ Consider using 'bestTrainingJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsBestTrainingJob :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Lude.Maybe HyperParameterTrainingJobSummary)
dhptjrsBestTrainingJob = Lens.lens (bestTrainingJob :: DescribeHyperParameterTuningJobResponse -> Lude.Maybe HyperParameterTrainingJobSummary) (\s a -> s {bestTrainingJob = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsBestTrainingJob "Use generic-lens or generic-optics with 'bestTrainingJob' instead." #-}

-- | The date and time that the tuning job ended.
--
-- /Note:/ Consider using 'hyperParameterTuningEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsHyperParameterTuningEndTime :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Lude.Maybe Lude.Timestamp)
dhptjrsHyperParameterTuningEndTime = Lens.lens (hyperParameterTuningEndTime :: DescribeHyperParameterTuningJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {hyperParameterTuningEndTime = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsHyperParameterTuningEndTime "Use generic-lens or generic-optics with 'hyperParameterTuningEndTime' instead." #-}

-- | The 'HyperParameterTuningJobConfig' object that specifies the configuration of the tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsHyperParameterTuningJobConfig :: Lens.Lens' DescribeHyperParameterTuningJobResponse HyperParameterTuningJobConfig
dhptjrsHyperParameterTuningJobConfig = Lens.lens (hyperParameterTuningJobConfig :: DescribeHyperParameterTuningJobResponse -> HyperParameterTuningJobConfig) (\s a -> s {hyperParameterTuningJobConfig = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsHyperParameterTuningJobConfig "Use generic-lens or generic-optics with 'hyperParameterTuningJobConfig' instead." #-}

-- | If the hyperparameter tuning job is an warm start tuning job with a @WarmStartType@ of @IDENTICAL_DATA_AND_ALGORITHM@ , this is the 'TrainingJobSummary' for the training job with the best objective metric value of all training jobs launched by this tuning job and all parent jobs specified for the warm start tuning job.
--
-- /Note:/ Consider using 'overallBestTrainingJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsOverallBestTrainingJob :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Lude.Maybe HyperParameterTrainingJobSummary)
dhptjrsOverallBestTrainingJob = Lens.lens (overallBestTrainingJob :: DescribeHyperParameterTuningJobResponse -> Lude.Maybe HyperParameterTrainingJobSummary) (\s a -> s {overallBestTrainingJob = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsOverallBestTrainingJob "Use generic-lens or generic-optics with 'overallBestTrainingJob' instead." #-}

-- | The configuration for starting the hyperparameter parameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
--
-- /Note:/ Consider using 'warmStartConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsWarmStartConfig :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Lude.Maybe HyperParameterTuningJobWarmStartConfig)
dhptjrsWarmStartConfig = Lens.lens (warmStartConfig :: DescribeHyperParameterTuningJobResponse -> Lude.Maybe HyperParameterTuningJobWarmStartConfig) (\s a -> s {warmStartConfig = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsWarmStartConfig "Use generic-lens or generic-optics with 'warmStartConfig' instead." #-}

-- | The status of the tuning job: InProgress, Completed, Failed, Stopping, or Stopped.
--
-- /Note:/ Consider using 'hyperParameterTuningJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsHyperParameterTuningJobStatus :: Lens.Lens' DescribeHyperParameterTuningJobResponse HyperParameterTuningJobStatus
dhptjrsHyperParameterTuningJobStatus = Lens.lens (hyperParameterTuningJobStatus :: DescribeHyperParameterTuningJobResponse -> HyperParameterTuningJobStatus) (\s a -> s {hyperParameterTuningJobStatus = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsHyperParameterTuningJobStatus "Use generic-lens or generic-optics with 'hyperParameterTuningJobStatus' instead." #-}

-- | A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
--
-- /Note:/ Consider using 'trainingJobDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsTrainingJobDefinitions :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Lude.Maybe (Lude.NonEmpty HyperParameterTrainingJobDefinition))
dhptjrsTrainingJobDefinitions = Lens.lens (trainingJobDefinitions :: DescribeHyperParameterTuningJobResponse -> Lude.Maybe (Lude.NonEmpty HyperParameterTrainingJobDefinition)) (\s a -> s {trainingJobDefinitions = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsTrainingJobDefinitions "Use generic-lens or generic-optics with 'trainingJobDefinitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhptjrsResponseStatus :: Lens.Lens' DescribeHyperParameterTuningJobResponse Lude.Int
dhptjrsResponseStatus = Lens.lens (responseStatus :: DescribeHyperParameterTuningJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHyperParameterTuningJobResponse)
{-# DEPRECATED dhptjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
