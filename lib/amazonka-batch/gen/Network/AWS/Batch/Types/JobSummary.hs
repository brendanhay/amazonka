{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobSummary
  ( JobSummary (..),

    -- * Smart constructor
    mkJobSummary,

    -- * Lenses
    jsJobId,
    jsJobName,
    jsArrayProperties,
    jsContainer,
    jsCreatedAt,
    jsJobArn,
    jsNodeProperties,
    jsStartedAt,
    jsStatus,
    jsStatusReason,
    jsStoppedAt,
  )
where

import qualified Network.AWS.Batch.Types.ArrayPropertiesSummary as Types
import qualified Network.AWS.Batch.Types.ContainerSummary as Types
import qualified Network.AWS.Batch.Types.JobStatus as Types
import qualified Network.AWS.Batch.Types.NodePropertiesSummary as Types
import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing summary details of a job.
--
-- /See:/ 'mkJobSummary' smart constructor.
data JobSummary = JobSummary'
  { -- | The ID of the job.
    jobId :: Types.String,
    -- | The name of the job.
    jobName :: Types.String,
    -- | The array properties of the job, if it is an array job.
    arrayProperties :: Core.Maybe Types.ArrayPropertiesSummary,
    -- | An object representing the details of the container that is associated with the job.
    container :: Core.Maybe Types.ContainerSummary,
    -- | The Unix timestamp for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
    createdAt :: Core.Maybe Core.Integer,
    -- | The Amazon Resource Name (ARN) of the job.
    jobArn :: Core.Maybe Types.String,
    -- | The node properties for a single node in a job summary list.
    nodeProperties :: Core.Maybe Types.NodePropertiesSummary,
    -- | The Unix timestamp for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state).
    startedAt :: Core.Maybe Core.Integer,
    -- | The current status for the job.
    status :: Core.Maybe Types.JobStatus,
    -- | A short, human-readable string to provide additional details about the current status of the job.
    statusReason :: Core.Maybe Types.String,
    -- | The Unix timestamp for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
    stoppedAt :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobSummary' value with any optional fields omitted.
mkJobSummary ::
  -- | 'jobId'
  Types.String ->
  -- | 'jobName'
  Types.String ->
  JobSummary
mkJobSummary jobId jobName =
  JobSummary'
    { jobId,
      jobName,
      arrayProperties = Core.Nothing,
      container = Core.Nothing,
      createdAt = Core.Nothing,
      jobArn = Core.Nothing,
      nodeProperties = Core.Nothing,
      startedAt = Core.Nothing,
      status = Core.Nothing,
      statusReason = Core.Nothing,
      stoppedAt = Core.Nothing
    }

-- | The ID of the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsJobId :: Lens.Lens' JobSummary Types.String
jsJobId = Lens.field @"jobId"
{-# DEPRECATED jsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsJobName :: Lens.Lens' JobSummary Types.String
jsJobName = Lens.field @"jobName"
{-# DEPRECATED jsJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The array properties of the job, if it is an array job.
--
-- /Note:/ Consider using 'arrayProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsArrayProperties :: Lens.Lens' JobSummary (Core.Maybe Types.ArrayPropertiesSummary)
jsArrayProperties = Lens.field @"arrayProperties"
{-# DEPRECATED jsArrayProperties "Use generic-lens or generic-optics with 'arrayProperties' instead." #-}

-- | An object representing the details of the container that is associated with the job.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsContainer :: Lens.Lens' JobSummary (Core.Maybe Types.ContainerSummary)
jsContainer = Lens.field @"container"
{-# DEPRECATED jsContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | The Unix timestamp for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsCreatedAt :: Lens.Lens' JobSummary (Core.Maybe Core.Integer)
jsCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED jsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The Amazon Resource Name (ARN) of the job.
--
-- /Note:/ Consider using 'jobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsJobArn :: Lens.Lens' JobSummary (Core.Maybe Types.String)
jsJobArn = Lens.field @"jobArn"
{-# DEPRECATED jsJobArn "Use generic-lens or generic-optics with 'jobArn' instead." #-}

-- | The node properties for a single node in a job summary list.
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsNodeProperties :: Lens.Lens' JobSummary (Core.Maybe Types.NodePropertiesSummary)
jsNodeProperties = Lens.field @"nodeProperties"
{-# DEPRECATED jsNodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead." #-}

-- | The Unix timestamp for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state).
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStartedAt :: Lens.Lens' JobSummary (Core.Maybe Core.Integer)
jsStartedAt = Lens.field @"startedAt"
{-# DEPRECATED jsStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The current status for the job.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStatus :: Lens.Lens' JobSummary (Core.Maybe Types.JobStatus)
jsStatus = Lens.field @"status"
{-# DEPRECATED jsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A short, human-readable string to provide additional details about the current status of the job.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStatusReason :: Lens.Lens' JobSummary (Core.Maybe Types.String)
jsStatusReason = Lens.field @"statusReason"
{-# DEPRECATED jsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The Unix timestamp for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
--
-- /Note:/ Consider using 'stoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStoppedAt :: Lens.Lens' JobSummary (Core.Maybe Core.Integer)
jsStoppedAt = Lens.field @"stoppedAt"
{-# DEPRECATED jsStoppedAt "Use generic-lens or generic-optics with 'stoppedAt' instead." #-}

instance Core.FromJSON JobSummary where
  parseJSON =
    Core.withObject "JobSummary" Core.$
      \x ->
        JobSummary'
          Core.<$> (x Core..: "jobId")
          Core.<*> (x Core..: "jobName")
          Core.<*> (x Core..:? "arrayProperties")
          Core.<*> (x Core..:? "container")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "jobArn")
          Core.<*> (x Core..:? "nodeProperties")
          Core.<*> (x Core..:? "startedAt")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "statusReason")
          Core.<*> (x Core..:? "stoppedAt")
