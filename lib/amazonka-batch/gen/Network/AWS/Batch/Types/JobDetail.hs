{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDetail
  ( JobDetail (..),

    -- * Smart constructor
    mkJobDetail,

    -- * Lenses
    jdJobName,
    jdJobId,
    jdJobQueue,
    jdStatus,
    jdStartedAt,
    jdJobDefinition,
    jdArrayProperties,
    jdAttempts,
    jdContainer,
    jdCreatedAt,
    jdDependsOn,
    jdJobArn,
    jdNodeDetails,
    jdNodeProperties,
    jdParameters,
    jdRetryStrategy,
    jdStatusReason,
    jdStoppedAt,
    jdTags,
    jdTimeout,
  )
where

import qualified Network.AWS.Batch.Types.ArrayPropertiesDetail as Types
import qualified Network.AWS.Batch.Types.AttemptDetail as Types
import qualified Network.AWS.Batch.Types.ContainerDetail as Types
import qualified Network.AWS.Batch.Types.JobDependency as Types
import qualified Network.AWS.Batch.Types.JobStatus as Types
import qualified Network.AWS.Batch.Types.JobTimeout as Types
import qualified Network.AWS.Batch.Types.NodeDetails as Types
import qualified Network.AWS.Batch.Types.NodeProperties as Types
import qualified Network.AWS.Batch.Types.RetryStrategy as Types
import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Batch.Types.TagKey as Types
import qualified Network.AWS.Batch.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an AWS Batch job.
--
-- /See:/ 'mkJobDetail' smart constructor.
data JobDetail = JobDetail'
  { -- | The name of the job.
    jobName :: Types.String,
    -- | The ID for the job.
    jobId :: Types.String,
    -- | The Amazon Resource Name (ARN) of the job queue with which the job is associated.
    jobQueue :: Types.String,
    -- | The current status for the job.
    status :: Types.JobStatus,
    -- | The Unix timestamp (in milliseconds) for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state). This parameter is not provided for child jobs of array jobs or multi-node parallel jobs.
    startedAt :: Core.Maybe Core.Integer,
    -- | The job definition that is used by this job.
    jobDefinition :: Types.String,
    -- | The array properties of the job, if it is an array job.
    arrayProperties :: Core.Maybe Types.ArrayPropertiesDetail,
    -- | A list of job attempts associated with this job.
    attempts :: Core.Maybe [Types.AttemptDetail],
    -- | An object representing the details of the container that is associated with the job.
    container :: Core.Maybe Types.ContainerDetail,
    -- | The Unix timestamp (in milliseconds) for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
    createdAt :: Core.Maybe Core.Integer,
    -- | A list of job IDs on which this job depends.
    dependsOn :: Core.Maybe [Types.JobDependency],
    -- | The Amazon Resource Name (ARN) of the job.
    jobArn :: Core.Maybe Types.String,
    -- | An object representing the details of a node that is associated with a multi-node parallel job.
    nodeDetails :: Core.Maybe Types.NodeDetails,
    -- | An object representing the node properties of a multi-node parallel job.
    nodeProperties :: Core.Maybe Types.NodeProperties,
    -- | Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
    parameters :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The retry strategy to use for this job if an attempt fails.
    retryStrategy :: Core.Maybe Types.RetryStrategy,
    -- | A short, human-readable string to provide additional details about the current status of the job.
    statusReason :: Core.Maybe Types.String,
    -- | The Unix timestamp (in milliseconds) for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
    stoppedAt :: Core.Maybe Core.Integer,
    -- | The tags applied to the job.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The timeout configuration for the job.
    timeout :: Core.Maybe Types.JobTimeout
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobDetail' value with any optional fields omitted.
mkJobDetail ::
  -- | 'jobName'
  Types.String ->
  -- | 'jobId'
  Types.String ->
  -- | 'jobQueue'
  Types.String ->
  -- | 'status'
  Types.JobStatus ->
  -- | 'jobDefinition'
  Types.String ->
  JobDetail
mkJobDetail jobName jobId jobQueue status jobDefinition =
  JobDetail'
    { jobName,
      jobId,
      jobQueue,
      status,
      startedAt = Core.Nothing,
      jobDefinition,
      arrayProperties = Core.Nothing,
      attempts = Core.Nothing,
      container = Core.Nothing,
      createdAt = Core.Nothing,
      dependsOn = Core.Nothing,
      jobArn = Core.Nothing,
      nodeDetails = Core.Nothing,
      nodeProperties = Core.Nothing,
      parameters = Core.Nothing,
      retryStrategy = Core.Nothing,
      statusReason = Core.Nothing,
      stoppedAt = Core.Nothing,
      tags = Core.Nothing,
      timeout = Core.Nothing
    }

-- | The name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobName :: Lens.Lens' JobDetail Types.String
jdJobName = Lens.field @"jobName"
{-# DEPRECATED jdJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The ID for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobId :: Lens.Lens' JobDetail Types.String
jdJobId = Lens.field @"jobId"
{-# DEPRECATED jdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The Amazon Resource Name (ARN) of the job queue with which the job is associated.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobQueue :: Lens.Lens' JobDetail Types.String
jdJobQueue = Lens.field @"jobQueue"
{-# DEPRECATED jdJobQueue "Use generic-lens or generic-optics with 'jobQueue' instead." #-}

-- | The current status for the job.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStatus :: Lens.Lens' JobDetail Types.JobStatus
jdStatus = Lens.field @"status"
{-# DEPRECATED jdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Unix timestamp (in milliseconds) for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state). This parameter is not provided for child jobs of array jobs or multi-node parallel jobs.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStartedAt :: Lens.Lens' JobDetail (Core.Maybe Core.Integer)
jdStartedAt = Lens.field @"startedAt"
{-# DEPRECATED jdStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The job definition that is used by this job.
--
-- /Note:/ Consider using 'jobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobDefinition :: Lens.Lens' JobDetail Types.String
jdJobDefinition = Lens.field @"jobDefinition"
{-# DEPRECATED jdJobDefinition "Use generic-lens or generic-optics with 'jobDefinition' instead." #-}

-- | The array properties of the job, if it is an array job.
--
-- /Note:/ Consider using 'arrayProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdArrayProperties :: Lens.Lens' JobDetail (Core.Maybe Types.ArrayPropertiesDetail)
jdArrayProperties = Lens.field @"arrayProperties"
{-# DEPRECATED jdArrayProperties "Use generic-lens or generic-optics with 'arrayProperties' instead." #-}

-- | A list of job attempts associated with this job.
--
-- /Note:/ Consider using 'attempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdAttempts :: Lens.Lens' JobDetail (Core.Maybe [Types.AttemptDetail])
jdAttempts = Lens.field @"attempts"
{-# DEPRECATED jdAttempts "Use generic-lens or generic-optics with 'attempts' instead." #-}

-- | An object representing the details of the container that is associated with the job.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdContainer :: Lens.Lens' JobDetail (Core.Maybe Types.ContainerDetail)
jdContainer = Lens.field @"container"
{-# DEPRECATED jdContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | The Unix timestamp (in milliseconds) for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdCreatedAt :: Lens.Lens' JobDetail (Core.Maybe Core.Integer)
jdCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED jdCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | A list of job IDs on which this job depends.
--
-- /Note:/ Consider using 'dependsOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdDependsOn :: Lens.Lens' JobDetail (Core.Maybe [Types.JobDependency])
jdDependsOn = Lens.field @"dependsOn"
{-# DEPRECATED jdDependsOn "Use generic-lens or generic-optics with 'dependsOn' instead." #-}

-- | The Amazon Resource Name (ARN) of the job.
--
-- /Note:/ Consider using 'jobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobArn :: Lens.Lens' JobDetail (Core.Maybe Types.String)
jdJobArn = Lens.field @"jobArn"
{-# DEPRECATED jdJobArn "Use generic-lens or generic-optics with 'jobArn' instead." #-}

-- | An object representing the details of a node that is associated with a multi-node parallel job.
--
-- /Note:/ Consider using 'nodeDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdNodeDetails :: Lens.Lens' JobDetail (Core.Maybe Types.NodeDetails)
jdNodeDetails = Lens.field @"nodeDetails"
{-# DEPRECATED jdNodeDetails "Use generic-lens or generic-optics with 'nodeDetails' instead." #-}

-- | An object representing the node properties of a multi-node parallel job.
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdNodeProperties :: Lens.Lens' JobDetail (Core.Maybe Types.NodeProperties)
jdNodeProperties = Lens.field @"nodeProperties"
{-# DEPRECATED jdNodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead." #-}

-- | Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdParameters :: Lens.Lens' JobDetail (Core.Maybe (Core.HashMap Types.String Types.String))
jdParameters = Lens.field @"parameters"
{-# DEPRECATED jdParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The retry strategy to use for this job if an attempt fails.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdRetryStrategy :: Lens.Lens' JobDetail (Core.Maybe Types.RetryStrategy)
jdRetryStrategy = Lens.field @"retryStrategy"
{-# DEPRECATED jdRetryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead." #-}

-- | A short, human-readable string to provide additional details about the current status of the job.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStatusReason :: Lens.Lens' JobDetail (Core.Maybe Types.String)
jdStatusReason = Lens.field @"statusReason"
{-# DEPRECATED jdStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The Unix timestamp (in milliseconds) for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
--
-- /Note:/ Consider using 'stoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStoppedAt :: Lens.Lens' JobDetail (Core.Maybe Core.Integer)
jdStoppedAt = Lens.field @"stoppedAt"
{-# DEPRECATED jdStoppedAt "Use generic-lens or generic-optics with 'stoppedAt' instead." #-}

-- | The tags applied to the job.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdTags :: Lens.Lens' JobDetail (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
jdTags = Lens.field @"tags"
{-# DEPRECATED jdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The timeout configuration for the job.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdTimeout :: Lens.Lens' JobDetail (Core.Maybe Types.JobTimeout)
jdTimeout = Lens.field @"timeout"
{-# DEPRECATED jdTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

instance Core.FromJSON JobDetail where
  parseJSON =
    Core.withObject "JobDetail" Core.$
      \x ->
        JobDetail'
          Core.<$> (x Core..: "jobName")
          Core.<*> (x Core..: "jobId")
          Core.<*> (x Core..: "jobQueue")
          Core.<*> (x Core..: "status")
          Core.<*> (x Core..:? "startedAt")
          Core.<*> (x Core..: "jobDefinition")
          Core.<*> (x Core..:? "arrayProperties")
          Core.<*> (x Core..:? "attempts")
          Core.<*> (x Core..:? "container")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "dependsOn")
          Core.<*> (x Core..:? "jobArn")
          Core.<*> (x Core..:? "nodeDetails")
          Core.<*> (x Core..:? "nodeProperties")
          Core.<*> (x Core..:? "parameters")
          Core.<*> (x Core..:? "retryStrategy")
          Core.<*> (x Core..:? "statusReason")
          Core.<*> (x Core..:? "stoppedAt")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "timeout")
