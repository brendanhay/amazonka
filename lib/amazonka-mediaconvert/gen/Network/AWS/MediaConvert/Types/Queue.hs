{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Queue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Queue
  ( Queue (..)
  -- * Smart constructor
  , mkQueue
  -- * Lenses
  , qName
  , qArn
  , qCreatedAt
  , qDescription
  , qLastUpdated
  , qPricingPlan
  , qProgressingJobsCount
  , qReservationPlan
  , qStatus
  , qSubmittedJobsCount
  , qType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.PricingPlan as Types
import qualified Network.AWS.MediaConvert.Types.QueueStatus as Types
import qualified Network.AWS.MediaConvert.Types.ReservationPlan as Types
import qualified Network.AWS.MediaConvert.Types.Type as Types
import qualified Network.AWS.Prelude as Core

-- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
--
-- /See:/ 'mkQueue' smart constructor.
data Queue = Queue'
  { name :: Core.Text
    -- ^ A name that you create for each queue. Each name must be unique within your account.
  , arn :: Core.Maybe Core.Text
    -- ^ An identifier for this resource that is unique within all of AWS.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp in epoch seconds for when you created the queue.
  , description :: Core.Maybe Core.Text
    -- ^ An optional description that you create for each queue.
  , lastUpdated :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp in epoch seconds for when you most recently updated the queue.
  , pricingPlan :: Core.Maybe Types.PricingPlan
    -- ^ Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment.
  , progressingJobsCount :: Core.Maybe Core.Int
    -- ^ The estimated number of jobs with a PROGRESSING status.
  , reservationPlan :: Core.Maybe Types.ReservationPlan
    -- ^ Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
  , status :: Core.Maybe Types.QueueStatus
    -- ^ Queues can be ACTIVE or PAUSED. If you pause a queue, the service won't begin processing jobs in that queue. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
  , submittedJobsCount :: Core.Maybe Core.Int
    -- ^ The estimated number of jobs with a SUBMITTED status.
  , type' :: Core.Maybe Types.Type
    -- ^ Specifies whether this on-demand queue is system or custom. System queues are built in. You can't modify or delete system queues. You can create and modify custom queues.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Queue' value with any optional fields omitted.
mkQueue
    :: Core.Text -- ^ 'name'
    -> Queue
mkQueue name
  = Queue'{name, arn = Core.Nothing, createdAt = Core.Nothing,
           description = Core.Nothing, lastUpdated = Core.Nothing,
           pricingPlan = Core.Nothing, progressingJobsCount = Core.Nothing,
           reservationPlan = Core.Nothing, status = Core.Nothing,
           submittedJobsCount = Core.Nothing, type' = Core.Nothing}

-- | A name that you create for each queue. Each name must be unique within your account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qName :: Lens.Lens' Queue Core.Text
qName = Lens.field @"name"
{-# INLINEABLE qName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An identifier for this resource that is unique within all of AWS.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qArn :: Lens.Lens' Queue (Core.Maybe Core.Text)
qArn = Lens.field @"arn"
{-# INLINEABLE qArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The timestamp in epoch seconds for when you created the queue.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qCreatedAt :: Lens.Lens' Queue (Core.Maybe Core.NominalDiffTime)
qCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE qCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | An optional description that you create for each queue.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qDescription :: Lens.Lens' Queue (Core.Maybe Core.Text)
qDescription = Lens.field @"description"
{-# INLINEABLE qDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The timestamp in epoch seconds for when you most recently updated the queue.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qLastUpdated :: Lens.Lens' Queue (Core.Maybe Core.NominalDiffTime)
qLastUpdated = Lens.field @"lastUpdated"
{-# INLINEABLE qLastUpdated #-}
{-# DEPRECATED lastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead"  #-}

-- | Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment.
--
-- /Note:/ Consider using 'pricingPlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qPricingPlan :: Lens.Lens' Queue (Core.Maybe Types.PricingPlan)
qPricingPlan = Lens.field @"pricingPlan"
{-# INLINEABLE qPricingPlan #-}
{-# DEPRECATED pricingPlan "Use generic-lens or generic-optics with 'pricingPlan' instead"  #-}

-- | The estimated number of jobs with a PROGRESSING status.
--
-- /Note:/ Consider using 'progressingJobsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qProgressingJobsCount :: Lens.Lens' Queue (Core.Maybe Core.Int)
qProgressingJobsCount = Lens.field @"progressingJobsCount"
{-# INLINEABLE qProgressingJobsCount #-}
{-# DEPRECATED progressingJobsCount "Use generic-lens or generic-optics with 'progressingJobsCount' instead"  #-}

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /Note:/ Consider using 'reservationPlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qReservationPlan :: Lens.Lens' Queue (Core.Maybe Types.ReservationPlan)
qReservationPlan = Lens.field @"reservationPlan"
{-# INLINEABLE qReservationPlan #-}
{-# DEPRECATED reservationPlan "Use generic-lens or generic-optics with 'reservationPlan' instead"  #-}

-- | Queues can be ACTIVE or PAUSED. If you pause a queue, the service won't begin processing jobs in that queue. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qStatus :: Lens.Lens' Queue (Core.Maybe Types.QueueStatus)
qStatus = Lens.field @"status"
{-# INLINEABLE qStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The estimated number of jobs with a SUBMITTED status.
--
-- /Note:/ Consider using 'submittedJobsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qSubmittedJobsCount :: Lens.Lens' Queue (Core.Maybe Core.Int)
qSubmittedJobsCount = Lens.field @"submittedJobsCount"
{-# INLINEABLE qSubmittedJobsCount #-}
{-# DEPRECATED submittedJobsCount "Use generic-lens or generic-optics with 'submittedJobsCount' instead"  #-}

-- | Specifies whether this on-demand queue is system or custom. System queues are built in. You can't modify or delete system queues. You can create and modify custom queues.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qType :: Lens.Lens' Queue (Core.Maybe Types.Type)
qType = Lens.field @"type'"
{-# INLINEABLE qType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Queue where
        parseJSON
          = Core.withObject "Queue" Core.$
              \ x ->
                Queue' Core.<$>
                  (x Core..: "name") Core.<*> x Core..:? "arn" Core.<*>
                    x Core..:? "createdAt"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "lastUpdated"
                    Core.<*> x Core..:? "pricingPlan"
                    Core.<*> x Core..:? "progressingJobsCount"
                    Core.<*> x Core..:? "reservationPlan"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "submittedJobsCount"
                    Core.<*> x Core..:? "type"
