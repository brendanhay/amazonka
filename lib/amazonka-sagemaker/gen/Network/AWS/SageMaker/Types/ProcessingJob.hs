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
    pjAppSpecification,
    pjAutoMLJobArn,
    pjCreationTime,
    pjEnvironment,
    pjExitMessage,
    pjExperimentConfig,
    pjFailureReason,
    pjLastModifiedTime,
    pjMonitoringScheduleArn,
    pjNetworkConfig,
    pjProcessingEndTime,
    pjProcessingInputs,
    pjProcessingJobArn,
    pjProcessingJobName,
    pjProcessingJobStatus,
    pjProcessingOutputConfig,
    pjProcessingResources,
    pjProcessingStartTime,
    pjRoleArn,
    pjStoppingCondition,
    pjTags,
    pjTrainingJobArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AppSpecification as Types
import qualified Network.AWS.SageMaker.Types.AutoMLJobArn as Types
import qualified Network.AWS.SageMaker.Types.ExitMessage as Types
import qualified Network.AWS.SageMaker.Types.ExperimentConfig as Types
import qualified Network.AWS.SageMaker.Types.FailureReason as Types
import qualified Network.AWS.SageMaker.Types.MonitoringScheduleArn as Types
import qualified Network.AWS.SageMaker.Types.NetworkConfig as Types
import qualified Network.AWS.SageMaker.Types.ProcessingEnvironmentKey as Types
import qualified Network.AWS.SageMaker.Types.ProcessingEnvironmentValue as Types
import qualified Network.AWS.SageMaker.Types.ProcessingInput as Types
import qualified Network.AWS.SageMaker.Types.ProcessingJobArn as Types
import qualified Network.AWS.SageMaker.Types.ProcessingJobName as Types
import qualified Network.AWS.SageMaker.Types.ProcessingJobStatus as Types
import qualified Network.AWS.SageMaker.Types.ProcessingOutputConfig as Types
import qualified Network.AWS.SageMaker.Types.ProcessingResources as Types
import qualified Network.AWS.SageMaker.Types.ProcessingStoppingCondition as Types
import qualified Network.AWS.SageMaker.Types.RoleArn as Types
import qualified Network.AWS.SageMaker.Types.Tag as Types
import qualified Network.AWS.SageMaker.Types.TrainingJobArn as Types

-- | An Amazon SageMaker processing job that is used to analyze data and evaluate models. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/processing-job.html Process Data and Evaluate Models> .
--
-- /See:/ 'mkProcessingJob' smart constructor.
data ProcessingJob = ProcessingJob'
  { appSpecification :: Core.Maybe Types.AppSpecification,
    -- | The Amazon Resource Name (ARN) of the AutoML job associated with this processing job.
    autoMLJobArn :: Core.Maybe Types.AutoMLJobArn,
    -- | The time the processing job was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | Sets the environment variables in the Docker container.
    environment :: Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue),
    -- | A string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
    exitMessage :: Core.Maybe Types.ExitMessage,
    experimentConfig :: Core.Maybe Types.ExperimentConfig,
    -- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The time the processing job was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of a monitoring schedule for an endpoint associated with this processing job.
    monitoringScheduleArn :: Core.Maybe Types.MonitoringScheduleArn,
    networkConfig :: Core.Maybe Types.NetworkConfig,
    -- | The time that the processing job ended.
    processingEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
    processingInputs :: Core.Maybe [Types.ProcessingInput],
    -- | The ARN of the processing job.
    processingJobArn :: Core.Maybe Types.ProcessingJobArn,
    -- | The name of the processing job.
    processingJobName :: Core.Maybe Types.ProcessingJobName,
    -- | The status of the processing job.
    processingJobStatus :: Core.Maybe Types.ProcessingJobStatus,
    processingOutputConfig :: Core.Maybe Types.ProcessingOutputConfig,
    processingResources :: Core.Maybe Types.ProcessingResources,
    -- | The time that the processing job started.
    processingStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the role used to create the processing job.
    roleArn :: Core.Maybe Types.RoleArn,
    stoppingCondition :: Core.Maybe Types.ProcessingStoppingCondition,
    -- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Core.Maybe [Types.Tag],
    -- | The ARN of the training job associated with this processing job.
    trainingJobArn :: Core.Maybe Types.TrainingJobArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ProcessingJob' value with any optional fields omitted.
mkProcessingJob ::
  ProcessingJob
mkProcessingJob =
  ProcessingJob'
    { appSpecification = Core.Nothing,
      autoMLJobArn = Core.Nothing,
      creationTime = Core.Nothing,
      environment = Core.Nothing,
      exitMessage = Core.Nothing,
      experimentConfig = Core.Nothing,
      failureReason = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      monitoringScheduleArn = Core.Nothing,
      networkConfig = Core.Nothing,
      processingEndTime = Core.Nothing,
      processingInputs = Core.Nothing,
      processingJobArn = Core.Nothing,
      processingJobName = Core.Nothing,
      processingJobStatus = Core.Nothing,
      processingOutputConfig = Core.Nothing,
      processingResources = Core.Nothing,
      processingStartTime = Core.Nothing,
      roleArn = Core.Nothing,
      stoppingCondition = Core.Nothing,
      tags = Core.Nothing,
      trainingJobArn = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'appSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjAppSpecification :: Lens.Lens' ProcessingJob (Core.Maybe Types.AppSpecification)
pjAppSpecification = Lens.field @"appSpecification"
{-# DEPRECATED pjAppSpecification "Use generic-lens or generic-optics with 'appSpecification' instead." #-}

-- | The Amazon Resource Name (ARN) of the AutoML job associated with this processing job.
--
-- /Note:/ Consider using 'autoMLJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjAutoMLJobArn :: Lens.Lens' ProcessingJob (Core.Maybe Types.AutoMLJobArn)
pjAutoMLJobArn = Lens.field @"autoMLJobArn"
{-# DEPRECATED pjAutoMLJobArn "Use generic-lens or generic-optics with 'autoMLJobArn' instead." #-}

-- | The time the processing job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjCreationTime :: Lens.Lens' ProcessingJob (Core.Maybe Core.NominalDiffTime)
pjCreationTime = Lens.field @"creationTime"
{-# DEPRECATED pjCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Sets the environment variables in the Docker container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjEnvironment :: Lens.Lens' ProcessingJob (Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue))
pjEnvironment = Lens.field @"environment"
{-# DEPRECATED pjEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | A string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
--
-- /Note:/ Consider using 'exitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjExitMessage :: Lens.Lens' ProcessingJob (Core.Maybe Types.ExitMessage)
pjExitMessage = Lens.field @"exitMessage"
{-# DEPRECATED pjExitMessage "Use generic-lens or generic-optics with 'exitMessage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjExperimentConfig :: Lens.Lens' ProcessingJob (Core.Maybe Types.ExperimentConfig)
pjExperimentConfig = Lens.field @"experimentConfig"
{-# DEPRECATED pjExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjFailureReason :: Lens.Lens' ProcessingJob (Core.Maybe Types.FailureReason)
pjFailureReason = Lens.field @"failureReason"
{-# DEPRECATED pjFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The time the processing job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjLastModifiedTime :: Lens.Lens' ProcessingJob (Core.Maybe Core.NominalDiffTime)
pjLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED pjLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The ARN of a monitoring schedule for an endpoint associated with this processing job.
--
-- /Note:/ Consider using 'monitoringScheduleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjMonitoringScheduleArn :: Lens.Lens' ProcessingJob (Core.Maybe Types.MonitoringScheduleArn)
pjMonitoringScheduleArn = Lens.field @"monitoringScheduleArn"
{-# DEPRECATED pjMonitoringScheduleArn "Use generic-lens or generic-optics with 'monitoringScheduleArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'networkConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjNetworkConfig :: Lens.Lens' ProcessingJob (Core.Maybe Types.NetworkConfig)
pjNetworkConfig = Lens.field @"networkConfig"
{-# DEPRECATED pjNetworkConfig "Use generic-lens or generic-optics with 'networkConfig' instead." #-}

-- | The time that the processing job ended.
--
-- /Note:/ Consider using 'processingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingEndTime :: Lens.Lens' ProcessingJob (Core.Maybe Core.NominalDiffTime)
pjProcessingEndTime = Lens.field @"processingEndTime"
{-# DEPRECATED pjProcessingEndTime "Use generic-lens or generic-optics with 'processingEndTime' instead." #-}

-- | For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
--
-- /Note:/ Consider using 'processingInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingInputs :: Lens.Lens' ProcessingJob (Core.Maybe [Types.ProcessingInput])
pjProcessingInputs = Lens.field @"processingInputs"
{-# DEPRECATED pjProcessingInputs "Use generic-lens or generic-optics with 'processingInputs' instead." #-}

-- | The ARN of the processing job.
--
-- /Note:/ Consider using 'processingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingJobArn :: Lens.Lens' ProcessingJob (Core.Maybe Types.ProcessingJobArn)
pjProcessingJobArn = Lens.field @"processingJobArn"
{-# DEPRECATED pjProcessingJobArn "Use generic-lens or generic-optics with 'processingJobArn' instead." #-}

-- | The name of the processing job.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingJobName :: Lens.Lens' ProcessingJob (Core.Maybe Types.ProcessingJobName)
pjProcessingJobName = Lens.field @"processingJobName"
{-# DEPRECATED pjProcessingJobName "Use generic-lens or generic-optics with 'processingJobName' instead." #-}

-- | The status of the processing job.
--
-- /Note:/ Consider using 'processingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingJobStatus :: Lens.Lens' ProcessingJob (Core.Maybe Types.ProcessingJobStatus)
pjProcessingJobStatus = Lens.field @"processingJobStatus"
{-# DEPRECATED pjProcessingJobStatus "Use generic-lens or generic-optics with 'processingJobStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'processingOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingOutputConfig :: Lens.Lens' ProcessingJob (Core.Maybe Types.ProcessingOutputConfig)
pjProcessingOutputConfig = Lens.field @"processingOutputConfig"
{-# DEPRECATED pjProcessingOutputConfig "Use generic-lens or generic-optics with 'processingOutputConfig' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'processingResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingResources :: Lens.Lens' ProcessingJob (Core.Maybe Types.ProcessingResources)
pjProcessingResources = Lens.field @"processingResources"
{-# DEPRECATED pjProcessingResources "Use generic-lens or generic-optics with 'processingResources' instead." #-}

-- | The time that the processing job started.
--
-- /Note:/ Consider using 'processingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjProcessingStartTime :: Lens.Lens' ProcessingJob (Core.Maybe Core.NominalDiffTime)
pjProcessingStartTime = Lens.field @"processingStartTime"
{-# DEPRECATED pjProcessingStartTime "Use generic-lens or generic-optics with 'processingStartTime' instead." #-}

-- | The ARN of the role used to create the processing job.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjRoleArn :: Lens.Lens' ProcessingJob (Core.Maybe Types.RoleArn)
pjRoleArn = Lens.field @"roleArn"
{-# DEPRECATED pjRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjStoppingCondition :: Lens.Lens' ProcessingJob (Core.Maybe Types.ProcessingStoppingCondition)
pjStoppingCondition = Lens.field @"stoppingCondition"
{-# DEPRECATED pjStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjTags :: Lens.Lens' ProcessingJob (Core.Maybe [Types.Tag])
pjTags = Lens.field @"tags"
{-# DEPRECATED pjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ARN of the training job associated with this processing job.
--
-- /Note:/ Consider using 'trainingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjTrainingJobArn :: Lens.Lens' ProcessingJob (Core.Maybe Types.TrainingJobArn)
pjTrainingJobArn = Lens.field @"trainingJobArn"
{-# DEPRECATED pjTrainingJobArn "Use generic-lens or generic-optics with 'trainingJobArn' instead." #-}

instance Core.FromJSON ProcessingJob where
  parseJSON =
    Core.withObject "ProcessingJob" Core.$
      \x ->
        ProcessingJob'
          Core.<$> (x Core..:? "AppSpecification")
          Core.<*> (x Core..:? "AutoMLJobArn")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "Environment")
          Core.<*> (x Core..:? "ExitMessage")
          Core.<*> (x Core..:? "ExperimentConfig")
          Core.<*> (x Core..:? "FailureReason")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "MonitoringScheduleArn")
          Core.<*> (x Core..:? "NetworkConfig")
          Core.<*> (x Core..:? "ProcessingEndTime")
          Core.<*> (x Core..:? "ProcessingInputs")
          Core.<*> (x Core..:? "ProcessingJobArn")
          Core.<*> (x Core..:? "ProcessingJobName")
          Core.<*> (x Core..:? "ProcessingJobStatus")
          Core.<*> (x Core..:? "ProcessingOutputConfig")
          Core.<*> (x Core..:? "ProcessingResources")
          Core.<*> (x Core..:? "ProcessingStartTime")
          Core.<*> (x Core..:? "RoleArn")
          Core.<*> (x Core..:? "StoppingCondition")
          Core.<*> (x Core..:? "Tags")
          Core.<*> (x Core..:? "TrainingJobArn")
