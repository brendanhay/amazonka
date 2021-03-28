{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobQueueDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.JobQueueDetail
  ( JobQueueDetail (..)
  -- * Smart constructor
  , mkJobQueueDetail
  -- * Lenses
  , jqdJobQueueName
  , jqdJobQueueArn
  , jqdState
  , jqdPriority
  , jqdComputeEnvironmentOrder
  , jqdStatus
  , jqdStatusReason
  , jqdTags
  ) where

import qualified Network.AWS.Batch.Types.ComputeEnvironmentOrder as Types
import qualified Network.AWS.Batch.Types.JQState as Types
import qualified Network.AWS.Batch.Types.JQStatus as Types
import qualified Network.AWS.Batch.Types.TagKey as Types
import qualified Network.AWS.Batch.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the details of an AWS Batch job queue.
--
-- /See:/ 'mkJobQueueDetail' smart constructor.
data JobQueueDetail = JobQueueDetail'
  { jobQueueName :: Core.Text
    -- ^ The name of the job queue.
  , jobQueueArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the job queue.
  , state :: Types.JQState
    -- ^ Describes the ability of the queue to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
  , priority :: Core.Int
    -- ^ The priority of the job queue.
  , computeEnvironmentOrder :: [Types.ComputeEnvironmentOrder]
    -- ^ The compute environments that are attached to the job queue and the order in which job placement is preferred. Compute environments are selected for job placement in ascending order.
  , status :: Core.Maybe Types.JQStatus
    -- ^ The status of the job queue (for example, @CREATING@ or @VALID@ ).
  , statusReason :: Core.Maybe Core.Text
    -- ^ A short, human-readable string to provide additional details about the current status of the job queue.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags applied to the job queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobQueueDetail' value with any optional fields omitted.
mkJobQueueDetail
    :: Core.Text -- ^ 'jobQueueName'
    -> Core.Text -- ^ 'jobQueueArn'
    -> Types.JQState -- ^ 'state'
    -> Core.Int -- ^ 'priority'
    -> JobQueueDetail
mkJobQueueDetail jobQueueName jobQueueArn state priority
  = JobQueueDetail'{jobQueueName, jobQueueArn, state, priority,
                    computeEnvironmentOrder = Core.mempty, status = Core.Nothing,
                    statusReason = Core.Nothing, tags = Core.Nothing}

-- | The name of the job queue.
--
-- /Note:/ Consider using 'jobQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdJobQueueName :: Lens.Lens' JobQueueDetail Core.Text
jqdJobQueueName = Lens.field @"jobQueueName"
{-# INLINEABLE jqdJobQueueName #-}
{-# DEPRECATED jobQueueName "Use generic-lens or generic-optics with 'jobQueueName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the job queue.
--
-- /Note:/ Consider using 'jobQueueArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdJobQueueArn :: Lens.Lens' JobQueueDetail Core.Text
jqdJobQueueArn = Lens.field @"jobQueueArn"
{-# INLINEABLE jqdJobQueueArn #-}
{-# DEPRECATED jobQueueArn "Use generic-lens or generic-optics with 'jobQueueArn' instead"  #-}

-- | Describes the ability of the queue to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdState :: Lens.Lens' JobQueueDetail Types.JQState
jqdState = Lens.field @"state"
{-# INLINEABLE jqdState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The priority of the job queue.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdPriority :: Lens.Lens' JobQueueDetail Core.Int
jqdPriority = Lens.field @"priority"
{-# INLINEABLE jqdPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | The compute environments that are attached to the job queue and the order in which job placement is preferred. Compute environments are selected for job placement in ascending order.
--
-- /Note:/ Consider using 'computeEnvironmentOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdComputeEnvironmentOrder :: Lens.Lens' JobQueueDetail [Types.ComputeEnvironmentOrder]
jqdComputeEnvironmentOrder = Lens.field @"computeEnvironmentOrder"
{-# INLINEABLE jqdComputeEnvironmentOrder #-}
{-# DEPRECATED computeEnvironmentOrder "Use generic-lens or generic-optics with 'computeEnvironmentOrder' instead"  #-}

-- | The status of the job queue (for example, @CREATING@ or @VALID@ ).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdStatus :: Lens.Lens' JobQueueDetail (Core.Maybe Types.JQStatus)
jqdStatus = Lens.field @"status"
{-# INLINEABLE jqdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A short, human-readable string to provide additional details about the current status of the job queue.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdStatusReason :: Lens.Lens' JobQueueDetail (Core.Maybe Core.Text)
jqdStatusReason = Lens.field @"statusReason"
{-# INLINEABLE jqdStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

-- | The tags applied to the job queue.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jqdTags :: Lens.Lens' JobQueueDetail (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
jqdTags = Lens.field @"tags"
{-# INLINEABLE jqdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON JobQueueDetail where
        parseJSON
          = Core.withObject "JobQueueDetail" Core.$
              \ x ->
                JobQueueDetail' Core.<$>
                  (x Core..: "jobQueueName") Core.<*> x Core..: "jobQueueArn"
                    Core.<*> x Core..: "state"
                    Core.<*> x Core..: "priority"
                    Core.<*> x Core..:? "computeEnvironmentOrder" Core..!= Core.mempty
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "statusReason"
                    Core.<*> x Core..:? "tags"
