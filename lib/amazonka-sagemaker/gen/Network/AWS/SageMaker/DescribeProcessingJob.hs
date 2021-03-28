{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeProcessingJob (..)
    , mkDescribeProcessingJob
    -- ** Request lenses
    , dpjProcessingJobName

    -- * Destructuring the response
    , DescribeProcessingJobResponse (..)
    , mkDescribeProcessingJobResponse
    -- ** Response lenses
    , dpjrrsProcessingJobName
    , dpjrrsProcessingResources
    , dpjrrsAppSpecification
    , dpjrrsProcessingJobArn
    , dpjrrsProcessingJobStatus
    , dpjrrsCreationTime
    , dpjrrsAutoMLJobArn
    , dpjrrsEnvironment
    , dpjrrsExitMessage
    , dpjrrsExperimentConfig
    , dpjrrsFailureReason
    , dpjrrsLastModifiedTime
    , dpjrrsMonitoringScheduleArn
    , dpjrrsNetworkConfig
    , dpjrrsProcessingEndTime
    , dpjrrsProcessingInputs
    , dpjrrsProcessingOutputConfig
    , dpjrrsProcessingStartTime
    , dpjrrsRoleArn
    , dpjrrsStoppingCondition
    , dpjrrsTrainingJobArn
    , dpjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeProcessingJob' smart constructor.
newtype DescribeProcessingJob = DescribeProcessingJob'
  { processingJobName :: Types.ProcessingJobName
    -- ^ The name of the processing job. The name must be unique within an AWS Region in the AWS account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProcessingJob' value with any optional fields omitted.
mkDescribeProcessingJob
    :: Types.ProcessingJobName -- ^ 'processingJobName'
    -> DescribeProcessingJob
mkDescribeProcessingJob processingJobName
  = DescribeProcessingJob'{processingJobName}

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjProcessingJobName :: Lens.Lens' DescribeProcessingJob Types.ProcessingJobName
dpjProcessingJobName = Lens.field @"processingJobName"
{-# INLINEABLE dpjProcessingJobName #-}
{-# DEPRECATED processingJobName "Use generic-lens or generic-optics with 'processingJobName' instead"  #-}

instance Core.ToQuery DescribeProcessingJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeProcessingJob where
        toHeaders DescribeProcessingJob{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeProcessingJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeProcessingJob where
        toJSON DescribeProcessingJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProcessingJobName" Core..= processingJobName)])

instance Core.AWSRequest DescribeProcessingJob where
        type Rs DescribeProcessingJob = DescribeProcessingJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeProcessingJobResponse' Core.<$>
                   (x Core..: "ProcessingJobName") Core.<*>
                     x Core..: "ProcessingResources"
                     Core.<*> x Core..: "AppSpecification"
                     Core.<*> x Core..: "ProcessingJobArn"
                     Core.<*> x Core..: "ProcessingJobStatus"
                     Core.<*> x Core..: "CreationTime"
                     Core.<*> x Core..:? "AutoMLJobArn"
                     Core.<*> x Core..:? "Environment"
                     Core.<*> x Core..:? "ExitMessage"
                     Core.<*> x Core..:? "ExperimentConfig"
                     Core.<*> x Core..:? "FailureReason"
                     Core.<*> x Core..:? "LastModifiedTime"
                     Core.<*> x Core..:? "MonitoringScheduleArn"
                     Core.<*> x Core..:? "NetworkConfig"
                     Core.<*> x Core..:? "ProcessingEndTime"
                     Core.<*> x Core..:? "ProcessingInputs"
                     Core.<*> x Core..:? "ProcessingOutputConfig"
                     Core.<*> x Core..:? "ProcessingStartTime"
                     Core.<*> x Core..:? "RoleArn"
                     Core.<*> x Core..:? "StoppingCondition"
                     Core.<*> x Core..:? "TrainingJobArn"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeProcessingJobResponse' smart constructor.
data DescribeProcessingJobResponse = DescribeProcessingJobResponse'
  { processingJobName :: Types.ProcessingJobName
    -- ^ The name of the processing job. The name must be unique within an AWS Region in the AWS account.
  , processingResources :: Types.ProcessingResources
    -- ^ Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
  , appSpecification :: Types.AppSpecification
    -- ^ Configures the processing job to run a specified container image.
  , processingJobArn :: Types.ProcessingJobArn
    -- ^ The Amazon Resource Name (ARN) of the processing job.
  , processingJobStatus :: Types.ProcessingJobStatus
    -- ^ Provides the status of a processing job.
  , creationTime :: Core.NominalDiffTime
    -- ^ The time at which the processing job was created.
  , autoMLJobArn :: Core.Maybe Types.AutoMLJobArn
    -- ^ The ARN of an AutoML job associated with this processing job.
  , environment :: Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue)
    -- ^ The environment variables set in the Docker container.
  , exitMessage :: Core.Maybe Types.ExitMessage
    -- ^ An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
  , experimentConfig :: Core.Maybe Types.ExperimentConfig
    -- ^ The configuration information used to create an experiment.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the processing job was last modified.
  , monitoringScheduleArn :: Core.Maybe Types.MonitoringScheduleArn
    -- ^ The ARN of a monitoring schedule for an endpoint associated with this processing job.
  , networkConfig :: Core.Maybe Types.NetworkConfig
    -- ^ Networking options for a processing job.
  , processingEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the processing job completed.
  , processingInputs :: Core.Maybe [Types.ProcessingInput]
    -- ^ The inputs for a processing job.
  , processingOutputConfig :: Core.Maybe Types.ProcessingOutputConfig
    -- ^ Output configuration for the processing job.
  , processingStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the processing job started.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
  , stoppingCondition :: Core.Maybe Types.ProcessingStoppingCondition
    -- ^ The time limit for how long the processing job is allowed to run.
  , trainingJobArn :: Core.Maybe Types.TrainingJobArn
    -- ^ The ARN of a training job associated with this processing job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeProcessingJobResponse' value with any optional fields omitted.
mkDescribeProcessingJobResponse
    :: Types.ProcessingJobName -- ^ 'processingJobName'
    -> Types.ProcessingResources -- ^ 'processingResources'
    -> Types.AppSpecification -- ^ 'appSpecification'
    -> Types.ProcessingJobArn -- ^ 'processingJobArn'
    -> Types.ProcessingJobStatus -- ^ 'processingJobStatus'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeProcessingJobResponse
mkDescribeProcessingJobResponse processingJobName
  processingResources appSpecification processingJobArn
  processingJobStatus creationTime responseStatus
  = DescribeProcessingJobResponse'{processingJobName,
                                   processingResources, appSpecification, processingJobArn,
                                   processingJobStatus, creationTime, autoMLJobArn = Core.Nothing,
                                   environment = Core.Nothing, exitMessage = Core.Nothing,
                                   experimentConfig = Core.Nothing, failureReason = Core.Nothing,
                                   lastModifiedTime = Core.Nothing,
                                   monitoringScheduleArn = Core.Nothing,
                                   networkConfig = Core.Nothing, processingEndTime = Core.Nothing,
                                   processingInputs = Core.Nothing,
                                   processingOutputConfig = Core.Nothing,
                                   processingStartTime = Core.Nothing, roleArn = Core.Nothing,
                                   stoppingCondition = Core.Nothing, trainingJobArn = Core.Nothing,
                                   responseStatus}

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingJobName :: Lens.Lens' DescribeProcessingJobResponse Types.ProcessingJobName
dpjrrsProcessingJobName = Lens.field @"processingJobName"
{-# INLINEABLE dpjrrsProcessingJobName #-}
{-# DEPRECATED processingJobName "Use generic-lens or generic-optics with 'processingJobName' instead"  #-}

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
--
-- /Note:/ Consider using 'processingResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingResources :: Lens.Lens' DescribeProcessingJobResponse Types.ProcessingResources
dpjrrsProcessingResources = Lens.field @"processingResources"
{-# INLINEABLE dpjrrsProcessingResources #-}
{-# DEPRECATED processingResources "Use generic-lens or generic-optics with 'processingResources' instead"  #-}

-- | Configures the processing job to run a specified container image.
--
-- /Note:/ Consider using 'appSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsAppSpecification :: Lens.Lens' DescribeProcessingJobResponse Types.AppSpecification
dpjrrsAppSpecification = Lens.field @"appSpecification"
{-# INLINEABLE dpjrrsAppSpecification #-}
{-# DEPRECATED appSpecification "Use generic-lens or generic-optics with 'appSpecification' instead"  #-}

-- | The Amazon Resource Name (ARN) of the processing job.
--
-- /Note:/ Consider using 'processingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingJobArn :: Lens.Lens' DescribeProcessingJobResponse Types.ProcessingJobArn
dpjrrsProcessingJobArn = Lens.field @"processingJobArn"
{-# INLINEABLE dpjrrsProcessingJobArn #-}
{-# DEPRECATED processingJobArn "Use generic-lens or generic-optics with 'processingJobArn' instead"  #-}

-- | Provides the status of a processing job.
--
-- /Note:/ Consider using 'processingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingJobStatus :: Lens.Lens' DescribeProcessingJobResponse Types.ProcessingJobStatus
dpjrrsProcessingJobStatus = Lens.field @"processingJobStatus"
{-# INLINEABLE dpjrrsProcessingJobStatus #-}
{-# DEPRECATED processingJobStatus "Use generic-lens or generic-optics with 'processingJobStatus' instead"  #-}

-- | The time at which the processing job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsCreationTime :: Lens.Lens' DescribeProcessingJobResponse Core.NominalDiffTime
dpjrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dpjrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The ARN of an AutoML job associated with this processing job.
--
-- /Note:/ Consider using 'autoMLJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsAutoMLJobArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.AutoMLJobArn)
dpjrrsAutoMLJobArn = Lens.field @"autoMLJobArn"
{-# INLINEABLE dpjrrsAutoMLJobArn #-}
{-# DEPRECATED autoMLJobArn "Use generic-lens or generic-optics with 'autoMLJobArn' instead"  #-}

-- | The environment variables set in the Docker container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsEnvironment :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue))
dpjrrsEnvironment = Lens.field @"environment"
{-# INLINEABLE dpjrrsEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
--
-- /Note:/ Consider using 'exitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsExitMessage :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.ExitMessage)
dpjrrsExitMessage = Lens.field @"exitMessage"
{-# INLINEABLE dpjrrsExitMessage #-}
{-# DEPRECATED exitMessage "Use generic-lens or generic-optics with 'exitMessage' instead"  #-}

-- | The configuration information used to create an experiment.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsExperimentConfig :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.ExperimentConfig)
dpjrrsExperimentConfig = Lens.field @"experimentConfig"
{-# INLINEABLE dpjrrsExperimentConfig #-}
{-# DEPRECATED experimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead"  #-}

-- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsFailureReason :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.FailureReason)
dpjrrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE dpjrrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The time at which the processing job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsLastModifiedTime :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.NominalDiffTime)
dpjrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE dpjrrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The ARN of a monitoring schedule for an endpoint associated with this processing job.
--
-- /Note:/ Consider using 'monitoringScheduleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsMonitoringScheduleArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.MonitoringScheduleArn)
dpjrrsMonitoringScheduleArn = Lens.field @"monitoringScheduleArn"
{-# INLINEABLE dpjrrsMonitoringScheduleArn #-}
{-# DEPRECATED monitoringScheduleArn "Use generic-lens or generic-optics with 'monitoringScheduleArn' instead"  #-}

-- | Networking options for a processing job.
--
-- /Note:/ Consider using 'networkConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsNetworkConfig :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.NetworkConfig)
dpjrrsNetworkConfig = Lens.field @"networkConfig"
{-# INLINEABLE dpjrrsNetworkConfig #-}
{-# DEPRECATED networkConfig "Use generic-lens or generic-optics with 'networkConfig' instead"  #-}

-- | The time at which the processing job completed.
--
-- /Note:/ Consider using 'processingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingEndTime :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.NominalDiffTime)
dpjrrsProcessingEndTime = Lens.field @"processingEndTime"
{-# INLINEABLE dpjrrsProcessingEndTime #-}
{-# DEPRECATED processingEndTime "Use generic-lens or generic-optics with 'processingEndTime' instead"  #-}

-- | The inputs for a processing job.
--
-- /Note:/ Consider using 'processingInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingInputs :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe [Types.ProcessingInput])
dpjrrsProcessingInputs = Lens.field @"processingInputs"
{-# INLINEABLE dpjrrsProcessingInputs #-}
{-# DEPRECATED processingInputs "Use generic-lens or generic-optics with 'processingInputs' instead"  #-}

-- | Output configuration for the processing job.
--
-- /Note:/ Consider using 'processingOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingOutputConfig :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.ProcessingOutputConfig)
dpjrrsProcessingOutputConfig = Lens.field @"processingOutputConfig"
{-# INLINEABLE dpjrrsProcessingOutputConfig #-}
{-# DEPRECATED processingOutputConfig "Use generic-lens or generic-optics with 'processingOutputConfig' instead"  #-}

-- | The time at which the processing job started.
--
-- /Note:/ Consider using 'processingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsProcessingStartTime :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.NominalDiffTime)
dpjrrsProcessingStartTime = Lens.field @"processingStartTime"
{-# INLINEABLE dpjrrsProcessingStartTime #-}
{-# DEPRECATED processingStartTime "Use generic-lens or generic-optics with 'processingStartTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsRoleArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.RoleArn)
dpjrrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dpjrrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The time limit for how long the processing job is allowed to run.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsStoppingCondition :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.ProcessingStoppingCondition)
dpjrrsStoppingCondition = Lens.field @"stoppingCondition"
{-# INLINEABLE dpjrrsStoppingCondition #-}
{-# DEPRECATED stoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead"  #-}

-- | The ARN of a training job associated with this processing job.
--
-- /Note:/ Consider using 'trainingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsTrainingJobArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Types.TrainingJobArn)
dpjrrsTrainingJobArn = Lens.field @"trainingJobArn"
{-# INLINEABLE dpjrrsTrainingJobArn #-}
{-# DEPRECATED trainingJobArn "Use generic-lens or generic-optics with 'trainingJobArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrrsResponseStatus :: Lens.Lens' DescribeProcessingJobResponse Core.Int
dpjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
