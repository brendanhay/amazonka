{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.JobDetail
  ( JobDetail (..)
  -- * Smart constructor
  , mkJobDetail
  -- * Lenses
  , jdJobName
  , jdJobId
  , jdJobQueue
  , jdStatus
  , jdStartedAt
  , jdJobDefinition
  , jdArrayProperties
  , jdAttempts
  , jdContainer
  , jdCreatedAt
  , jdDependsOn
  , jdJobArn
  , jdNodeDetails
  , jdNodeProperties
  , jdParameters
  , jdRetryStrategy
  , jdStatusReason
  , jdStoppedAt
  , jdTags
  , jdTimeout
  ) where

import qualified Network.AWS.Batch.Types.ArrayPropertiesDetail as Types
import qualified Network.AWS.Batch.Types.AttemptDetail as Types
import qualified Network.AWS.Batch.Types.ContainerDetail as Types
import qualified Network.AWS.Batch.Types.JobDependency as Types
import qualified Network.AWS.Batch.Types.JobStatus as Types
import qualified Network.AWS.Batch.Types.JobTimeout as Types
import qualified Network.AWS.Batch.Types.NodeDetails as Types
import qualified Network.AWS.Batch.Types.NodeProperties as Types
import qualified Network.AWS.Batch.Types.RetryStrategy as Types
import qualified Network.AWS.Batch.Types.TagKey as Types
import qualified Network.AWS.Batch.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an AWS Batch job.
--
-- /See:/ 'mkJobDetail' smart constructor.
data JobDetail = JobDetail'
  { jobName :: Core.Text
    -- ^ The name of the job.
  , jobId :: Core.Text
    -- ^ The ID for the job.
  , jobQueue :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the job queue with which the job is associated.
  , status :: Types.JobStatus
    -- ^ The current status for the job.
  , startedAt :: Core.Maybe Core.Integer
    -- ^ The Unix timestamp (in milliseconds) for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state). This parameter is not provided for child jobs of array jobs or multi-node parallel jobs.
  , jobDefinition :: Core.Text
    -- ^ The job definition that is used by this job.
  , arrayProperties :: Core.Maybe Types.ArrayPropertiesDetail
    -- ^ The array properties of the job, if it is an array job.
  , attempts :: Core.Maybe [Types.AttemptDetail]
    -- ^ A list of job attempts associated with this job.
  , container :: Core.Maybe Types.ContainerDetail
    -- ^ An object representing the details of the container that is associated with the job.
  , createdAt :: Core.Maybe Core.Integer
    -- ^ The Unix timestamp (in milliseconds) for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
  , dependsOn :: Core.Maybe [Types.JobDependency]
    -- ^ A list of job IDs on which this job depends.
  , jobArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the job.
  , nodeDetails :: Core.Maybe Types.NodeDetails
    -- ^ An object representing the details of a node that is associated with a multi-node parallel job.
  , nodeProperties :: Core.Maybe Types.NodeProperties
    -- ^ An object representing the node properties of a multi-node parallel job.
  , parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
  , retryStrategy :: Core.Maybe Types.RetryStrategy
    -- ^ The retry strategy to use for this job if an attempt fails.
  , statusReason :: Core.Maybe Core.Text
    -- ^ A short, human-readable string to provide additional details about the current status of the job.
  , stoppedAt :: Core.Maybe Core.Integer
    -- ^ The Unix timestamp (in milliseconds) for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags applied to the job.
  , timeout :: Core.Maybe Types.JobTimeout
    -- ^ The timeout configuration for the job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobDetail' value with any optional fields omitted.
mkJobDetail
    :: Core.Text -- ^ 'jobName'
    -> Core.Text -- ^ 'jobId'
    -> Core.Text -- ^ 'jobQueue'
    -> Types.JobStatus -- ^ 'status'
    -> Core.Text -- ^ 'jobDefinition'
    -> JobDetail
mkJobDetail jobName jobId jobQueue status jobDefinition
  = JobDetail'{jobName, jobId, jobQueue, status,
               startedAt = Core.Nothing, jobDefinition,
               arrayProperties = Core.Nothing, attempts = Core.Nothing,
               container = Core.Nothing, createdAt = Core.Nothing,
               dependsOn = Core.Nothing, jobArn = Core.Nothing,
               nodeDetails = Core.Nothing, nodeProperties = Core.Nothing,
               parameters = Core.Nothing, retryStrategy = Core.Nothing,
               statusReason = Core.Nothing, stoppedAt = Core.Nothing,
               tags = Core.Nothing, timeout = Core.Nothing}

-- | The name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobName :: Lens.Lens' JobDetail Core.Text
jdJobName = Lens.field @"jobName"
{-# INLINEABLE jdJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The ID for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobId :: Lens.Lens' JobDetail Core.Text
jdJobId = Lens.field @"jobId"
{-# INLINEABLE jdJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the job queue with which the job is associated.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobQueue :: Lens.Lens' JobDetail Core.Text
jdJobQueue = Lens.field @"jobQueue"
{-# INLINEABLE jdJobQueue #-}
{-# DEPRECATED jobQueue "Use generic-lens or generic-optics with 'jobQueue' instead"  #-}

-- | The current status for the job.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStatus :: Lens.Lens' JobDetail Types.JobStatus
jdStatus = Lens.field @"status"
{-# INLINEABLE jdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The Unix timestamp (in milliseconds) for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state). This parameter is not provided for child jobs of array jobs or multi-node parallel jobs.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStartedAt :: Lens.Lens' JobDetail (Core.Maybe Core.Integer)
jdStartedAt = Lens.field @"startedAt"
{-# INLINEABLE jdStartedAt #-}
{-# DEPRECATED startedAt "Use generic-lens or generic-optics with 'startedAt' instead"  #-}

-- | The job definition that is used by this job.
--
-- /Note:/ Consider using 'jobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobDefinition :: Lens.Lens' JobDetail Core.Text
jdJobDefinition = Lens.field @"jobDefinition"
{-# INLINEABLE jdJobDefinition #-}
{-# DEPRECATED jobDefinition "Use generic-lens or generic-optics with 'jobDefinition' instead"  #-}

-- | The array properties of the job, if it is an array job.
--
-- /Note:/ Consider using 'arrayProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdArrayProperties :: Lens.Lens' JobDetail (Core.Maybe Types.ArrayPropertiesDetail)
jdArrayProperties = Lens.field @"arrayProperties"
{-# INLINEABLE jdArrayProperties #-}
{-# DEPRECATED arrayProperties "Use generic-lens or generic-optics with 'arrayProperties' instead"  #-}

-- | A list of job attempts associated with this job.
--
-- /Note:/ Consider using 'attempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdAttempts :: Lens.Lens' JobDetail (Core.Maybe [Types.AttemptDetail])
jdAttempts = Lens.field @"attempts"
{-# INLINEABLE jdAttempts #-}
{-# DEPRECATED attempts "Use generic-lens or generic-optics with 'attempts' instead"  #-}

-- | An object representing the details of the container that is associated with the job.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdContainer :: Lens.Lens' JobDetail (Core.Maybe Types.ContainerDetail)
jdContainer = Lens.field @"container"
{-# INLINEABLE jdContainer #-}
{-# DEPRECATED container "Use generic-lens or generic-optics with 'container' instead"  #-}

-- | The Unix timestamp (in milliseconds) for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdCreatedAt :: Lens.Lens' JobDetail (Core.Maybe Core.Integer)
jdCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE jdCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | A list of job IDs on which this job depends.
--
-- /Note:/ Consider using 'dependsOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdDependsOn :: Lens.Lens' JobDetail (Core.Maybe [Types.JobDependency])
jdDependsOn = Lens.field @"dependsOn"
{-# INLINEABLE jdDependsOn #-}
{-# DEPRECATED dependsOn "Use generic-lens or generic-optics with 'dependsOn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the job.
--
-- /Note:/ Consider using 'jobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobArn :: Lens.Lens' JobDetail (Core.Maybe Core.Text)
jdJobArn = Lens.field @"jobArn"
{-# INLINEABLE jdJobArn #-}
{-# DEPRECATED jobArn "Use generic-lens or generic-optics with 'jobArn' instead"  #-}

-- | An object representing the details of a node that is associated with a multi-node parallel job.
--
-- /Note:/ Consider using 'nodeDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdNodeDetails :: Lens.Lens' JobDetail (Core.Maybe Types.NodeDetails)
jdNodeDetails = Lens.field @"nodeDetails"
{-# INLINEABLE jdNodeDetails #-}
{-# DEPRECATED nodeDetails "Use generic-lens or generic-optics with 'nodeDetails' instead"  #-}

-- | An object representing the node properties of a multi-node parallel job.
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdNodeProperties :: Lens.Lens' JobDetail (Core.Maybe Types.NodeProperties)
jdNodeProperties = Lens.field @"nodeProperties"
{-# INLINEABLE jdNodeProperties #-}
{-# DEPRECATED nodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead"  #-}

-- | Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdParameters :: Lens.Lens' JobDetail (Core.Maybe (Core.HashMap Core.Text Core.Text))
jdParameters = Lens.field @"parameters"
{-# INLINEABLE jdParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The retry strategy to use for this job if an attempt fails.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdRetryStrategy :: Lens.Lens' JobDetail (Core.Maybe Types.RetryStrategy)
jdRetryStrategy = Lens.field @"retryStrategy"
{-# INLINEABLE jdRetryStrategy #-}
{-# DEPRECATED retryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead"  #-}

-- | A short, human-readable string to provide additional details about the current status of the job.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStatusReason :: Lens.Lens' JobDetail (Core.Maybe Core.Text)
jdStatusReason = Lens.field @"statusReason"
{-# INLINEABLE jdStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

-- | The Unix timestamp (in milliseconds) for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
--
-- /Note:/ Consider using 'stoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStoppedAt :: Lens.Lens' JobDetail (Core.Maybe Core.Integer)
jdStoppedAt = Lens.field @"stoppedAt"
{-# INLINEABLE jdStoppedAt #-}
{-# DEPRECATED stoppedAt "Use generic-lens or generic-optics with 'stoppedAt' instead"  #-}

-- | The tags applied to the job.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdTags :: Lens.Lens' JobDetail (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
jdTags = Lens.field @"tags"
{-# INLINEABLE jdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The timeout configuration for the job.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdTimeout :: Lens.Lens' JobDetail (Core.Maybe Types.JobTimeout)
jdTimeout = Lens.field @"timeout"
{-# INLINEABLE jdTimeout #-}
{-# DEPRECATED timeout "Use generic-lens or generic-optics with 'timeout' instead"  #-}

instance Core.FromJSON JobDetail where
        parseJSON
          = Core.withObject "JobDetail" Core.$
              \ x ->
                JobDetail' Core.<$>
                  (x Core..: "jobName") Core.<*> x Core..: "jobId" Core.<*>
                    x Core..: "jobQueue"
                    Core.<*> x Core..: "status"
                    Core.<*> x Core..:? "startedAt"
                    Core.<*> x Core..: "jobDefinition"
                    Core.<*> x Core..:? "arrayProperties"
                    Core.<*> x Core..:? "attempts"
                    Core.<*> x Core..:? "container"
                    Core.<*> x Core..:? "createdAt"
                    Core.<*> x Core..:? "dependsOn"
                    Core.<*> x Core..:? "jobArn"
                    Core.<*> x Core..:? "nodeDetails"
                    Core.<*> x Core..:? "nodeProperties"
                    Core.<*> x Core..:? "parameters"
                    Core.<*> x Core..:? "retryStrategy"
                    Core.<*> x Core..:? "statusReason"
                    Core.<*> x Core..:? "stoppedAt"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "timeout"
