{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJob
  ( ProcessingJob (..),

    -- * Smart constructor
    mkProcessingJob,

    -- * Lenses
    pjCreationTime,
    pjFailureReason,
    pjMonitoringScheduleARN,
    pjAppSpecification,
    pjProcessingResources,
    pjEnvironment,
    pjProcessingJobName,
    pjStoppingCondition,
    pjExperimentConfig,
    pjLastModifiedTime,
    pjProcessingInputs,
    pjNetworkConfig,
    pjAutoMLJobARN,
    pjTrainingJobARN,
    pjProcessingJobStatus,
    pjExitMessage,
    pjProcessingOutputConfig,
    pjProcessingStartTime,
    pjProcessingEndTime,
    pjTags,
    pjProcessingJobARN,
    pjRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AppSpecification
import Network.AWS.SageMaker.Types.ExperimentConfig
import Network.AWS.SageMaker.Types.NetworkConfig
import Network.AWS.SageMaker.Types.ProcessingInput
import Network.AWS.SageMaker.Types.ProcessingJobStatus
import Network.AWS.SageMaker.Types.ProcessingOutputConfig
import Network.AWS.SageMaker.Types.ProcessingResources
import Network.AWS.SageMaker.Types.ProcessingStoppingCondition
import Network.AWS.SageMaker.Types.Tag

-- | An Amazon SageMaker processing job that is used to analyze data and evaluate models. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/processing-job.html Process Data and Evaluate Models> .
--
-- /See:/ 'mkProcessingJob' smart constructor.
data ProcessingJob = ProcessingJob'
  { -- | The time the processing job was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The ARN of a monitoring schedule for an endpoint associated with this processing job.
    monitoringScheduleARN :: Lude.Maybe Lude.Text,
    appSpecification :: Lude.Maybe AppSpecification,
    processingResources :: Lude.Maybe ProcessingResources,
    -- | Sets the environment variables in the Docker container.
    environment :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The name of the processing job.
    processingJobName :: Lude.Maybe Lude.Text,
    stoppingCondition :: Lude.Maybe ProcessingStoppingCondition,
    experimentConfig :: Lude.Maybe ExperimentConfig,
    -- | The time the processing job was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
    processingInputs :: Lude.Maybe [ProcessingInput],
    networkConfig :: Lude.Maybe NetworkConfig,
    -- | The Amazon Resource Name (ARN) of the AutoML job associated with this processing job.
    autoMLJobARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the training job associated with this processing job.
    trainingJobARN :: Lude.Maybe Lude.Text,
    -- | The status of the processing job.
    processingJobStatus :: Lude.Maybe ProcessingJobStatus,
    -- | A string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
    exitMessage :: Lude.Maybe Lude.Text,
    processingOutputConfig :: Lude.Maybe ProcessingOutputConfig,
    -- | The time that the processing job started.
    processingStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The time that the processing job ended.
    processingEndTime :: Lude.Maybe Lude.Timestamp,
    -- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Lude.Maybe [Tag],
    -- | The ARN of the processing job.
    processingJobARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the role used to create the processing job.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessingJob' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time the processing job was created.
-- * 'failureReason' - A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
-- * 'monitoringScheduleARN' - The ARN of a monitoring schedule for an endpoint associated with this processing job.
-- * 'appSpecification' -
-- * 'processingResources' -
-- * 'environment' - Sets the environment variables in the Docker container.
-- * 'processingJobName' - The name of the processing job.
-- * 'stoppingCondition' -
-- * 'experimentConfig' -
-- * 'lastModifiedTime' - The time the processing job was last modified.
-- * 'processingInputs' - For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
-- * 'networkConfig' -
-- * 'autoMLJobARN' - The Amazon Resource Name (ARN) of the AutoML job associated with this processing job.
-- * 'trainingJobARN' - The ARN of the training job associated with this processing job.
-- * 'processingJobStatus' - The status of the processing job.
-- * 'exitMessage' - A string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
-- * 'processingOutputConfig' -
-- * 'processingStartTime' - The time that the processing job started.
-- * 'processingEndTime' - The time that the processing job ended.
-- * 'tags' - An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
-- * 'processingJobARN' - The ARN of the processing job.
-- * 'roleARN' - The ARN of the role used to create the processing job.
mkProcessingJob ::
  ProcessingJob
mkProcessingJob =
  ProcessingJob'
    { creationTime = Lude.Nothing,
      failureReason = Lude.Nothing,
      monitoringScheduleARN = Lude.Nothing,
      appSpecification = Lude.Nothing,
      processingResources = Lude.Nothing,
      environment = Lude.Nothing,
      processingJobName = Lude.Nothing,
      stoppingCondition = Lude.Nothing,
      experimentConfig = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      processingInputs = Lude.Nothing,
      networkConfig = Lude.Nothing,
      autoMLJobARN = Lude.Nothing,
      trainingJobARN = Lude.Nothing,
      processingJobStatus = Lude.Nothing,
      exitMessage = Lude.Nothing,
      processingOutputConfig = Lude.Nothing,
      processingStartTime = Lude.Nothing,
      processingEndTime = Lude.Nothing,
      tags = Lude.Nothing,
      processingJobARN = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The time the processing job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjCreationTime :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Timestamp)
pjCreationTime = Lens.lens (creationTime :: ProcessingJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: ProcessingJob)
{-# DEPRECATED pjCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjFailureReason :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Text)
pjFailureReason = Lens.lens (failureReason :: ProcessingJob -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: ProcessingJob)
{-# DEPRECATED pjFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The ARN of a monitoring schedule for an endpoint associated with this processing job.
--
-- /Note:/ Consider using 'monitoringScheduleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjMonitoringScheduleARN :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Text)
pjMonitoringScheduleARN = Lens.lens (monitoringScheduleARN :: ProcessingJob -> Lude.Maybe Lude.Text) (\s a -> s {monitoringScheduleARN = a} :: ProcessingJob)
{-# DEPRECATED pjMonitoringScheduleARN "Use generic-lens or generic-optics with 'monitoringScheduleARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'appSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjAppSpecification :: Lens.Lens' ProcessingJob (Lude.Maybe AppSpecification)
pjAppSpecification = Lens.lens (appSpecification :: ProcessingJob -> Lude.Maybe AppSpecification) (\s a -> s {appSpecification = a} :: ProcessingJob)
{-# DEPRECATED pjAppSpecification "Use generic-lens or generic-optics with 'appSpecification' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'processingResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingResources :: Lens.Lens' ProcessingJob (Lude.Maybe ProcessingResources)
pjProcessingResources = Lens.lens (processingResources :: ProcessingJob -> Lude.Maybe ProcessingResources) (\s a -> s {processingResources = a} :: ProcessingJob)
{-# DEPRECATED pjProcessingResources "Use generic-lens or generic-optics with 'processingResources' instead." #-}

-- | Sets the environment variables in the Docker container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjEnvironment :: Lens.Lens' ProcessingJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pjEnvironment = Lens.lens (environment :: ProcessingJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {environment = a} :: ProcessingJob)
{-# DEPRECATED pjEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The name of the processing job.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingJobName :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Text)
pjProcessingJobName = Lens.lens (processingJobName :: ProcessingJob -> Lude.Maybe Lude.Text) (\s a -> s {processingJobName = a} :: ProcessingJob)
{-# DEPRECATED pjProcessingJobName "Use generic-lens or generic-optics with 'processingJobName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjStoppingCondition :: Lens.Lens' ProcessingJob (Lude.Maybe ProcessingStoppingCondition)
pjStoppingCondition = Lens.lens (stoppingCondition :: ProcessingJob -> Lude.Maybe ProcessingStoppingCondition) (\s a -> s {stoppingCondition = a} :: ProcessingJob)
{-# DEPRECATED pjStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjExperimentConfig :: Lens.Lens' ProcessingJob (Lude.Maybe ExperimentConfig)
pjExperimentConfig = Lens.lens (experimentConfig :: ProcessingJob -> Lude.Maybe ExperimentConfig) (\s a -> s {experimentConfig = a} :: ProcessingJob)
{-# DEPRECATED pjExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | The time the processing job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjLastModifiedTime :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Timestamp)
pjLastModifiedTime = Lens.lens (lastModifiedTime :: ProcessingJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: ProcessingJob)
{-# DEPRECATED pjLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
--
-- /Note:/ Consider using 'processingInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingInputs :: Lens.Lens' ProcessingJob (Lude.Maybe [ProcessingInput])
pjProcessingInputs = Lens.lens (processingInputs :: ProcessingJob -> Lude.Maybe [ProcessingInput]) (\s a -> s {processingInputs = a} :: ProcessingJob)
{-# DEPRECATED pjProcessingInputs "Use generic-lens or generic-optics with 'processingInputs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'networkConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjNetworkConfig :: Lens.Lens' ProcessingJob (Lude.Maybe NetworkConfig)
pjNetworkConfig = Lens.lens (networkConfig :: ProcessingJob -> Lude.Maybe NetworkConfig) (\s a -> s {networkConfig = a} :: ProcessingJob)
{-# DEPRECATED pjNetworkConfig "Use generic-lens or generic-optics with 'networkConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AutoML job associated with this processing job.
--
-- /Note:/ Consider using 'autoMLJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjAutoMLJobARN :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Text)
pjAutoMLJobARN = Lens.lens (autoMLJobARN :: ProcessingJob -> Lude.Maybe Lude.Text) (\s a -> s {autoMLJobARN = a} :: ProcessingJob)
{-# DEPRECATED pjAutoMLJobARN "Use generic-lens or generic-optics with 'autoMLJobARN' instead." #-}

-- | The ARN of the training job associated with this processing job.
--
-- /Note:/ Consider using 'trainingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjTrainingJobARN :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Text)
pjTrainingJobARN = Lens.lens (trainingJobARN :: ProcessingJob -> Lude.Maybe Lude.Text) (\s a -> s {trainingJobARN = a} :: ProcessingJob)
{-# DEPRECATED pjTrainingJobARN "Use generic-lens or generic-optics with 'trainingJobARN' instead." #-}

-- | The status of the processing job.
--
-- /Note:/ Consider using 'processingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingJobStatus :: Lens.Lens' ProcessingJob (Lude.Maybe ProcessingJobStatus)
pjProcessingJobStatus = Lens.lens (processingJobStatus :: ProcessingJob -> Lude.Maybe ProcessingJobStatus) (\s a -> s {processingJobStatus = a} :: ProcessingJob)
{-# DEPRECATED pjProcessingJobStatus "Use generic-lens or generic-optics with 'processingJobStatus' instead." #-}

-- | A string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
--
-- /Note:/ Consider using 'exitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjExitMessage :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Text)
pjExitMessage = Lens.lens (exitMessage :: ProcessingJob -> Lude.Maybe Lude.Text) (\s a -> s {exitMessage = a} :: ProcessingJob)
{-# DEPRECATED pjExitMessage "Use generic-lens or generic-optics with 'exitMessage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'processingOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingOutputConfig :: Lens.Lens' ProcessingJob (Lude.Maybe ProcessingOutputConfig)
pjProcessingOutputConfig = Lens.lens (processingOutputConfig :: ProcessingJob -> Lude.Maybe ProcessingOutputConfig) (\s a -> s {processingOutputConfig = a} :: ProcessingJob)
{-# DEPRECATED pjProcessingOutputConfig "Use generic-lens or generic-optics with 'processingOutputConfig' instead." #-}

-- | The time that the processing job started.
--
-- /Note:/ Consider using 'processingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingStartTime :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Timestamp)
pjProcessingStartTime = Lens.lens (processingStartTime :: ProcessingJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {processingStartTime = a} :: ProcessingJob)
{-# DEPRECATED pjProcessingStartTime "Use generic-lens or generic-optics with 'processingStartTime' instead." #-}

-- | The time that the processing job ended.
--
-- /Note:/ Consider using 'processingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingEndTime :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Timestamp)
pjProcessingEndTime = Lens.lens (processingEndTime :: ProcessingJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {processingEndTime = a} :: ProcessingJob)
{-# DEPRECATED pjProcessingEndTime "Use generic-lens or generic-optics with 'processingEndTime' instead." #-}

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjTags :: Lens.Lens' ProcessingJob (Lude.Maybe [Tag])
pjTags = Lens.lens (tags :: ProcessingJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ProcessingJob)
{-# DEPRECATED pjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ARN of the processing job.
--
-- /Note:/ Consider using 'processingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingJobARN :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Text)
pjProcessingJobARN = Lens.lens (processingJobARN :: ProcessingJob -> Lude.Maybe Lude.Text) (\s a -> s {processingJobARN = a} :: ProcessingJob)
{-# DEPRECATED pjProcessingJobARN "Use generic-lens or generic-optics with 'processingJobARN' instead." #-}

-- | The ARN of the role used to create the processing job.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjRoleARN :: Lens.Lens' ProcessingJob (Lude.Maybe Lude.Text)
pjRoleARN = Lens.lens (roleARN :: ProcessingJob -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ProcessingJob)
{-# DEPRECATED pjRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON ProcessingJob where
  parseJSON =
    Lude.withObject
      "ProcessingJob"
      ( \x ->
          ProcessingJob'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "MonitoringScheduleArn")
            Lude.<*> (x Lude..:? "AppSpecification")
            Lude.<*> (x Lude..:? "ProcessingResources")
            Lude.<*> (x Lude..:? "Environment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ProcessingJobName")
            Lude.<*> (x Lude..:? "StoppingCondition")
            Lude.<*> (x Lude..:? "ExperimentConfig")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "ProcessingInputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NetworkConfig")
            Lude.<*> (x Lude..:? "AutoMLJobArn")
            Lude.<*> (x Lude..:? "TrainingJobArn")
            Lude.<*> (x Lude..:? "ProcessingJobStatus")
            Lude.<*> (x Lude..:? "ExitMessage")
            Lude.<*> (x Lude..:? "ProcessingOutputConfig")
            Lude.<*> (x Lude..:? "ProcessingStartTime")
            Lude.<*> (x Lude..:? "ProcessingEndTime")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ProcessingJobArn")
            Lude.<*> (x Lude..:? "RoleArn")
      )
