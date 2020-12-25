{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeProcessingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a processing job.
module Network.AWS.SageMaker.DescribeProcessingJob
  ( -- * Creating a request
    DescribeProcessingJob (..),
    mkDescribeProcessingJob,

    -- ** Request lenses
    dpjProcessingJobName,

    -- * Destructuring the response
    DescribeProcessingJobResponse (..),
    mkDescribeProcessingJobResponse,

    -- ** Response lenses
    dpjrrsProcessingJobName,
    dpjrrsProcessingResources,
    dpjrrsAppSpecification,
    dpjrrsProcessingJobArn,
    dpjrrsProcessingJobStatus,
    dpjrrsCreationTime,
    dpjrrsAutoMLJobArn,
    dpjrrsEnvironment,
    dpjrrsExitMessage,
    dpjrrsExperimentConfig,
    dpjrrsFailureReason,
    dpjrrsLastModifiedTime,
    dpjrrsMonitoringScheduleArn,
    dpjrrsNetworkConfig,
    dpjrrsProcessingEndTime,
    dpjrrsProcessingInputs,
    dpjrrsProcessingOutputConfig,
    dpjrrsProcessingStartTime,
    dpjrrsRoleArn,
    dpjrrsStoppingCondition,
    dpjrrsTrainingJobArn,
    dpjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeProcessingJob' smart constructor.
newtype DescribeProcessingJob = DescribeProcessingJob'
  { -- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
    processingJobName :: Types.ProcessingJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProcessingJob' value with any optional fields omitted.
mkDescribeProcessingJob ::
  -- | 'processingJobName'
  Types.ProcessingJobName ->
  DescribeProcessingJob
mkDescribeProcessingJob processingJobName =
  DescribeProcessingJob' {processingJobName}

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjProcessingJobName :: Lens.Lens' DescribeProcessingJob Types.ProcessingJobName
dpjProcessingJobName = Lens.field @"processingJobName"
{-# DEPRECATED dpjProcessingJobName "Use generic-lens or generic-optics with 'processingJobName' instead." #-}

instance Core.FromJSON DescribeProcessingJob where
  toJSON DescribeProcessingJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ProcessingJobName" Core..= processingJobName)]
      )

instance Core.AWSRequest DescribeProcessingJob where
  type Rs DescribeProcessingJob = DescribeProcessingJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeProcessingJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProcessingJobResponse'
            Core.<$> (x Core..: "ProcessingJobName")
            Core.<*> (x Core..: "ProcessingResources")
            Core.<*> (x Core..: "AppSpecification")
            Core.<*> (x Core..: "ProcessingJobArn")
            Core.<*> (x Core..: "ProcessingJobStatus")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..:? "AutoMLJobArn")
            Core.<*> (x Core..:? "Environment")
            Core.<*> (x Core..:? "ExitMessage")
            Core.<*> (x Core..:? "ExperimentConfig")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "MonitoringScheduleArn")
            Core.<*> (x Core..:? "NetworkConfig")
            Core.<*> (x Core..:? "ProcessingEndTime")
            Core.<*> (x Core..:? "ProcessingInputs")
            Core.<*> (x Core..:? "ProcessingOutputConfig")
            Core.<*> (x Core..:? "ProcessingStartTime")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "StoppingCondition")
            Core.<*> (x Core..:? "TrainingJobArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeProcessingJobResponse' smart constructor.
data DescribeProcessingJobResponse = DescribeProcessingJobResponse'
  { -- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
    processingJobName :: Types.ProcessingJobName,
    -- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
    processingResources :: Types.ProcessingResources,
    -- | Configures the processing job to run a specified container image.
    appSpecification :: Types.AppSpecification,
    -- | The Amazon Resource Name (ARN) of the processing job.
    processingJobArn :: Types.ProcessingJobArn,
    -- | Provides the status of a processing job.
    processingJobStatus :: Types.ProcessingJobStatus,
    -- | The time at which the processing job was created.
    creationTime :: Core.NominalDiffTime,
    -- | The ARN of an AutoML job associated with this processing job.
    autoMLJobArn :: Core.Maybe Types.AutoMLJobArn,
    -- | The environment variables set in the Docker container.
    environment :: Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue),
    -- | An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
    exitMessage :: Core.Maybe Types.ExitMessage,
    -- | The configuration information used to create an experiment.
    experimentConfig :: Core.Maybe Types.ExperimentConfig,
    -- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The time at which the processing job was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of a monitoring schedule for an endpoint associated with this processing job.
    monitoringScheduleArn :: Core.Maybe Types.MonitoringScheduleArn,
    -- | Networking options for a processing job.
    networkConfig :: Core.Maybe Types.NetworkConfig,
    -- | The time at which the processing job completed.
    processingEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | The inputs for a processing job.
    processingInputs :: Core.Maybe [Types.ProcessingInput],
    -- | Output configuration for the processing job.
    processingOutputConfig :: Core.Maybe Types.ProcessingOutputConfig,
    -- | The time at which the processing job started.
    processingStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | The time limit for how long the processing job is allowed to run.
    stoppingCondition :: Core.Maybe Types.ProcessingStoppingCondition,
    -- | The ARN of a training job associated with this processing job.
    trainingJobArn :: Core.Maybe Types.TrainingJobArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeProcessingJobResponse' value with any optional fields omitted.
mkDescribeProcessingJobResponse ::
  -- | 'processingJobName'
  Types.ProcessingJobName ->
  -- | 'processingResources'
  Types.ProcessingResources ->
  -- | 'appSpecification'
  Types.AppSpecification ->
  -- | 'processingJobArn'
  Types.ProcessingJobArn ->
  -- | 'processingJobStatus'
  Types.ProcessingJobStatus ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeProcessingJobResponse
mkDescribeProcessingJobResponse
  processingJobName
  processingResources
  appSpecification
  processingJobArn
  processingJobStatus
  creationTime
  responseStatus =
    DescribeProcessingJobResponse'
      { processingJobName,
        processingResources,
        appSpecification,
        processingJobArn,
        processingJobStatus,
        creationTime,
        autoMLJobArn = Core.Nothing,
        environment = Core.Nothing,
        exitMessage = Core.Nothing,
        experimentConfig = Core.Nothing,
        failureReason = Core.Nothing,
        lastModifiedTime = Core.Nothing,
        monitoringScheduleArn = Core.Nothing,
        networkConfig = Core.Nothing,
        processingEndTime = Core.Nothing,
        processingInputs = Core.Nothing,
        processingOutputConfig = Core.Nothing,
        processingStartTime = Core.Nothing,
        roleArn = Core.Nothing,
        stoppingCondition = Core.Nothing,
        trainingJobArn = Core.Nothing,
        responseStatus
      }

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingJobName :: Lens.Lens' DescribeProcessingJobResponse Types.ProcessingJobName
dpjrrsProcessingJobName = Lens.field @"processingJobName"
{-# DEPRECATED dpjrrsProcessingJobName "Use generic-lens or generic-optics with 'processingJobName' instead." #-}

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
--
-- /Note:/ Consider using 'processingResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingResources :: Lens.Lens' DescribeProcessingJobResponse Types.ProcessingResources
dpjrrsProcessingResources = Lens.field @"processingResources"
{-# DEPRECATED dpjrrsProcessingResources "Use generic-lens or generic-optics with 'processingResources' instead." #-}

-- | Configures the processing job to run a specified container image.
--
-- /Note:/ Consider using 'appSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsAppSpecification :: Lens.Lens' DescribeProcessingJobResponse Types.AppSpecification
dpjrrsAppSpecification = Lens.field @"appSpecification"
{-# DEPRECATED dpjrrsAppSpecification "Use generic-lens or generic-optics with 'appSpecification' instead." #-}

-- | The Amazon Resource Name (ARN) of the processing job.
--
-- /Note:/ Consider using 'processingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingJobArn :: Lens.Lens' DescribeProcessingJobResponse Types.ProcessingJobArn
dpjrrsProcessingJobArn = Lens.field @"processingJobArn"
{-# DEPRECATED dpjrrsProcessingJobArn "Use generic-lens or generic-optics with 'processingJobArn' instead." #-}

-- | Provides the status of a processing job.
--
-- /Note:/ Consider using 'processingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingJobStatus :: Lens.Lens' DescribeProcessingJobResponse Types.ProcessingJobStatus
dpjrrsProcessingJobStatus = Lens.field @"processingJobStatus"
{-# DEPRECATED dpjrrsProcessingJobStatus "Use generic-lens or generic-optics with 'processingJobStatus' instead." #-}

-- | The time at which the processing job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsCreationTime :: Lens.Lens' DescribeProcessingJobResponse Core.NominalDiffTime
dpjrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dpjrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The ARN of an AutoML job associated with this processing job.
--
-- /Note:/ Consider using 'autoMLJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsAutoMLJobArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.AutoMLJobArn)
dpjrrsAutoMLJobArn = Lens.field @"autoMLJobArn"
{-# DEPRECATED dpjrrsAutoMLJobArn "Use generic-lens or generic-optics with 'autoMLJobArn' instead." #-}

-- | The environment variables set in the Docker container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsEnvironment :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue))
dpjrrsEnvironment = Lens.field @"environment"
{-# DEPRECATED dpjrrsEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
--
-- /Note:/ Consider using 'exitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsExitMessage :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.ExitMessage)
dpjrrsExitMessage = Lens.field @"exitMessage"
{-# DEPRECATED dpjrrsExitMessage "Use generic-lens or generic-optics with 'exitMessage' instead." #-}

-- | The configuration information used to create an experiment.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsExperimentConfig :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.ExperimentConfig)
dpjrrsExperimentConfig = Lens.field @"experimentConfig"
{-# DEPRECATED dpjrrsExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsFailureReason :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.FailureReason)
dpjrrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED dpjrrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The time at which the processing job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsLastModifiedTime :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.NominalDiffTime)
dpjrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED dpjrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The ARN of a monitoring schedule for an endpoint associated with this processing job.
--
-- /Note:/ Consider using 'monitoringScheduleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsMonitoringScheduleArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.MonitoringScheduleArn)
dpjrrsMonitoringScheduleArn = Lens.field @"monitoringScheduleArn"
{-# DEPRECATED dpjrrsMonitoringScheduleArn "Use generic-lens or generic-optics with 'monitoringScheduleArn' instead." #-}

-- | Networking options for a processing job.
--
-- /Note:/ Consider using 'networkConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsNetworkConfig :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.NetworkConfig)
dpjrrsNetworkConfig = Lens.field @"networkConfig"
{-# DEPRECATED dpjrrsNetworkConfig "Use generic-lens or generic-optics with 'networkConfig' instead." #-}

-- | The time at which the processing job completed.
--
-- /Note:/ Consider using 'processingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingEndTime :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.NominalDiffTime)
dpjrrsProcessingEndTime = Lens.field @"processingEndTime"
{-# DEPRECATED dpjrrsProcessingEndTime "Use generic-lens or generic-optics with 'processingEndTime' instead." #-}

-- | The inputs for a processing job.
--
-- /Note:/ Consider using 'processingInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingInputs :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe [Types.ProcessingInput])
dpjrrsProcessingInputs = Lens.field @"processingInputs"
{-# DEPRECATED dpjrrsProcessingInputs "Use generic-lens or generic-optics with 'processingInputs' instead." #-}

-- | Output configuration for the processing job.
--
-- /Note:/ Consider using 'processingOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingOutputConfig :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.ProcessingOutputConfig)
dpjrrsProcessingOutputConfig = Lens.field @"processingOutputConfig"
{-# DEPRECATED dpjrrsProcessingOutputConfig "Use generic-lens or generic-optics with 'processingOutputConfig' instead." #-}

-- | The time at which the processing job started.
--
-- /Note:/ Consider using 'processingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingStartTime :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.NominalDiffTime)
dpjrrsProcessingStartTime = Lens.field @"processingStartTime"
{-# DEPRECATED dpjrrsProcessingStartTime "Use generic-lens or generic-optics with 'processingStartTime' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsRoleArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.RoleArn)
dpjrrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dpjrrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The time limit for how long the processing job is allowed to run.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsStoppingCondition :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.ProcessingStoppingCondition)
dpjrrsStoppingCondition = Lens.field @"stoppingCondition"
{-# DEPRECATED dpjrrsStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | The ARN of a training job associated with this processing job.
--
-- /Note:/ Consider using 'trainingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsTrainingJobArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.TrainingJobArn)
dpjrrsTrainingJobArn = Lens.field @"trainingJobArn"
{-# DEPRECATED dpjrrsTrainingJobArn "Use generic-lens or generic-optics with 'trainingJobArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsResponseStatus :: Lens.Lens' DescribeProcessingJobResponse Core.Int
dpjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
