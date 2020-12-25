{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobQueueDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobQueueDetail
  ( JobQueueDetail (..),

    -- * Smart constructor
    mkJobQueueDetail,

    -- * Lenses
    jqdJobQueueName,
    jqdJobQueueArn,
    jqdState,
    jqdPriority,
    jqdComputeEnvironmentOrder,
    jqdStatus,
    jqdStatusReason,
    jqdTags,
  )
where

import qualified Network.AWS.Batch.Types.ComputeEnvironmentOrder as Types
import qualified Network.AWS.Batch.Types.JQState as Types
import qualified Network.AWS.Batch.Types.JQStatus as Types
import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Batch.Types.TagKey as Types
import qualified Network.AWS.Batch.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the details of an AWS Batch job queue.
--
-- /See:/ 'mkJobQueueDetail' smart constructor.
data JobQueueDetail = JobQueueDetail'
  { -- | The name of the job queue.
    jobQueueName :: Types.String,
    -- | The Amazon Resource Name (ARN) of the job queue.
    jobQueueArn :: Types.String,
    -- | Describes the ability of the queue to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
    state :: Types.JQState,
    -- | The priority of the job queue.
    priority :: Core.Int,
    -- | The compute environments that are attached to the job queue and the order in which job placement is preferred. Compute environments are selected for job placement in ascending order.
    computeEnvironmentOrder :: [Types.ComputeEnvironmentOrder],
    -- | The status of the job queue (for example, @CREATING@ or @VALID@ ).
    status :: Core.Maybe Types.JQStatus,
    -- | A short, human-readable string to provide additional details about the current status of the job queue.
    statusReason :: Core.Maybe Types.String,
    -- | The tags applied to the job queue.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobQueueDetail' value with any optional fields omitted.
mkJobQueueDetail ::
  -- | 'jobQueueName'
  Types.String ->
  -- | 'jobQueueArn'
  Types.String ->
  -- | 'state'
  Types.JQState ->
  -- | 'priority'
  Core.Int ->
  JobQueueDetail
mkJobQueueDetail jobQueueName jobQueueArn state priority =
  JobQueueDetail'
    { jobQueueName,
      jobQueueArn,
      state,
      priority,
      computeEnvironmentOrder = Core.mempty,
      status = Core.Nothing,
      statusReason = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the job queue.
--
-- /Note:/ Consider using 'jobQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdJobQueueName :: Lens.Lens' JobQueueDetail Types.String
jqdJobQueueName = Lens.field @"jobQueueName"
{-# DEPRECATED jqdJobQueueName "Use generic-lens or generic-optics with 'jobQueueName' instead." #-}

-- | The Amazon Resource Name (ARN) of the job queue.
--
-- /Note:/ Consider using 'jobQueueArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdJobQueueArn :: Lens.Lens' JobQueueDetail Types.String
jqdJobQueueArn = Lens.field @"jobQueueArn"
{-# DEPRECATED jqdJobQueueArn "Use generic-lens or generic-optics with 'jobQueueArn' instead." #-}

-- | Describes the ability of the queue to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdState :: Lens.Lens' JobQueueDetail Types.JQState
jqdState = Lens.field @"state"
{-# DEPRECATED jqdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The priority of the job queue.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdPriority :: Lens.Lens' JobQueueDetail Core.Int
jqdPriority = Lens.field @"priority"
{-# DEPRECATED jqdPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The compute environments that are attached to the job queue and the order in which job placement is preferred. Compute environments are selected for job placement in ascending order.
--
-- /Note:/ Consider using 'computeEnvironmentOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdComputeEnvironmentOrder :: Lens.Lens' JobQueueDetail [Types.ComputeEnvironmentOrder]
jqdComputeEnvironmentOrder = Lens.field @"computeEnvironmentOrder"
{-# DEPRECATED jqdComputeEnvironmentOrder "Use generic-lens or generic-optics with 'computeEnvironmentOrder' instead." #-}

-- | The status of the job queue (for example, @CREATING@ or @VALID@ ).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdStatus :: Lens.Lens' JobQueueDetail (Core.Maybe Types.JQStatus)
jqdStatus = Lens.field @"status"
{-# DEPRECATED jqdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A short, human-readable string to provide additional details about the current status of the job queue.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdStatusReason :: Lens.Lens' JobQueueDetail (Core.Maybe Types.String)
jqdStatusReason = Lens.field @"statusReason"
{-# DEPRECATED jqdStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The tags applied to the job queue.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdTags :: Lens.Lens' JobQueueDetail (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
jqdTags = Lens.field @"tags"
{-# DEPRECATED jqdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON JobQueueDetail where
  parseJSON =
    Core.withObject "JobQueueDetail" Core.$
      \x ->
        JobQueueDetail'
          Core.<$> (x Core..: "jobQueueName")
          Core.<*> (x Core..: "jobQueueArn")
          Core.<*> (x Core..: "state")
          Core.<*> (x Core..: "priority")
          Core.<*> (x Core..:? "computeEnvironmentOrder" Core..!= Core.mempty)
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "statusReason")
          Core.<*> (x Core..:? "tags")
