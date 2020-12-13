{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Queue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Queue
  ( Queue (..),

    -- * Smart constructor
    mkQueue,

    -- * Lenses
    qStatus,
    qLastUpdated,
    qARN,
    qCreatedAt,
    qReservationPlan,
    qPricingPlan,
    qSubmittedJobsCount,
    qName,
    qProgressingJobsCount,
    qType,
    qDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.PricingPlan
import Network.AWS.MediaConvert.Types.QueueStatus
import Network.AWS.MediaConvert.Types.ReservationPlan
import Network.AWS.MediaConvert.Types.Type
import qualified Network.AWS.Prelude as Lude

-- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
--
-- /See:/ 'mkQueue' smart constructor.
data Queue = Queue'
  { -- | Queues can be ACTIVE or PAUSED. If you pause a queue, the service won't begin processing jobs in that queue. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
    status :: Lude.Maybe QueueStatus,
    -- | The timestamp in epoch seconds for when you most recently updated the queue.
    lastUpdated :: Lude.Maybe Lude.Timestamp,
    -- | An identifier for this resource that is unique within all of AWS.
    arn :: Lude.Maybe Lude.Text,
    -- | The timestamp in epoch seconds for when you created the queue.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
    reservationPlan :: Lude.Maybe ReservationPlan,
    -- | Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment.
    pricingPlan :: Lude.Maybe PricingPlan,
    -- | The estimated number of jobs with a SUBMITTED status.
    submittedJobsCount :: Lude.Maybe Lude.Int,
    -- | A name that you create for each queue. Each name must be unique within your account.
    name :: Lude.Text,
    -- | The estimated number of jobs with a PROGRESSING status.
    progressingJobsCount :: Lude.Maybe Lude.Int,
    -- | Specifies whether this on-demand queue is system or custom. System queues are built in. You can't modify or delete system queues. You can create and modify custom queues.
    type' :: Lude.Maybe Type,
    -- | An optional description that you create for each queue.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Queue' with the minimum fields required to make a request.
--
-- * 'status' - Queues can be ACTIVE or PAUSED. If you pause a queue, the service won't begin processing jobs in that queue. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
-- * 'lastUpdated' - The timestamp in epoch seconds for when you most recently updated the queue.
-- * 'arn' - An identifier for this resource that is unique within all of AWS.
-- * 'createdAt' - The timestamp in epoch seconds for when you created the queue.
-- * 'reservationPlan' - Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
-- * 'pricingPlan' - Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment.
-- * 'submittedJobsCount' - The estimated number of jobs with a SUBMITTED status.
-- * 'name' - A name that you create for each queue. Each name must be unique within your account.
-- * 'progressingJobsCount' - The estimated number of jobs with a PROGRESSING status.
-- * 'type'' - Specifies whether this on-demand queue is system or custom. System queues are built in. You can't modify or delete system queues. You can create and modify custom queues.
-- * 'description' - An optional description that you create for each queue.
mkQueue ::
  -- | 'name'
  Lude.Text ->
  Queue
mkQueue pName_ =
  Queue'
    { status = Lude.Nothing,
      lastUpdated = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      reservationPlan = Lude.Nothing,
      pricingPlan = Lude.Nothing,
      submittedJobsCount = Lude.Nothing,
      name = pName_,
      progressingJobsCount = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Queues can be ACTIVE or PAUSED. If you pause a queue, the service won't begin processing jobs in that queue. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qStatus :: Lens.Lens' Queue (Lude.Maybe QueueStatus)
qStatus = Lens.lens (status :: Queue -> Lude.Maybe QueueStatus) (\s a -> s {status = a} :: Queue)
{-# DEPRECATED qStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The timestamp in epoch seconds for when you most recently updated the queue.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qLastUpdated :: Lens.Lens' Queue (Lude.Maybe Lude.Timestamp)
qLastUpdated = Lens.lens (lastUpdated :: Queue -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdated = a} :: Queue)
{-# DEPRECATED qLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | An identifier for this resource that is unique within all of AWS.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qARN :: Lens.Lens' Queue (Lude.Maybe Lude.Text)
qARN = Lens.lens (arn :: Queue -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Queue)
{-# DEPRECATED qARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp in epoch seconds for when you created the queue.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qCreatedAt :: Lens.Lens' Queue (Lude.Maybe Lude.Timestamp)
qCreatedAt = Lens.lens (createdAt :: Queue -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Queue)
{-# DEPRECATED qCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /Note:/ Consider using 'reservationPlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qReservationPlan :: Lens.Lens' Queue (Lude.Maybe ReservationPlan)
qReservationPlan = Lens.lens (reservationPlan :: Queue -> Lude.Maybe ReservationPlan) (\s a -> s {reservationPlan = a} :: Queue)
{-# DEPRECATED qReservationPlan "Use generic-lens or generic-optics with 'reservationPlan' instead." #-}

-- | Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment.
--
-- /Note:/ Consider using 'pricingPlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qPricingPlan :: Lens.Lens' Queue (Lude.Maybe PricingPlan)
qPricingPlan = Lens.lens (pricingPlan :: Queue -> Lude.Maybe PricingPlan) (\s a -> s {pricingPlan = a} :: Queue)
{-# DEPRECATED qPricingPlan "Use generic-lens or generic-optics with 'pricingPlan' instead." #-}

-- | The estimated number of jobs with a SUBMITTED status.
--
-- /Note:/ Consider using 'submittedJobsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qSubmittedJobsCount :: Lens.Lens' Queue (Lude.Maybe Lude.Int)
qSubmittedJobsCount = Lens.lens (submittedJobsCount :: Queue -> Lude.Maybe Lude.Int) (\s a -> s {submittedJobsCount = a} :: Queue)
{-# DEPRECATED qSubmittedJobsCount "Use generic-lens or generic-optics with 'submittedJobsCount' instead." #-}

-- | A name that you create for each queue. Each name must be unique within your account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qName :: Lens.Lens' Queue Lude.Text
qName = Lens.lens (name :: Queue -> Lude.Text) (\s a -> s {name = a} :: Queue)
{-# DEPRECATED qName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The estimated number of jobs with a PROGRESSING status.
--
-- /Note:/ Consider using 'progressingJobsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qProgressingJobsCount :: Lens.Lens' Queue (Lude.Maybe Lude.Int)
qProgressingJobsCount = Lens.lens (progressingJobsCount :: Queue -> Lude.Maybe Lude.Int) (\s a -> s {progressingJobsCount = a} :: Queue)
{-# DEPRECATED qProgressingJobsCount "Use generic-lens or generic-optics with 'progressingJobsCount' instead." #-}

-- | Specifies whether this on-demand queue is system or custom. System queues are built in. You can't modify or delete system queues. You can create and modify custom queues.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qType :: Lens.Lens' Queue (Lude.Maybe Type)
qType = Lens.lens (type' :: Queue -> Lude.Maybe Type) (\s a -> s {type' = a} :: Queue)
{-# DEPRECATED qType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | An optional description that you create for each queue.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qDescription :: Lens.Lens' Queue (Lude.Maybe Lude.Text)
qDescription = Lens.lens (description :: Queue -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Queue)
{-# DEPRECATED qDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Queue where
  parseJSON =
    Lude.withObject
      "Queue"
      ( \x ->
          Queue'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "lastUpdated")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "reservationPlan")
            Lude.<*> (x Lude..:? "pricingPlan")
            Lude.<*> (x Lude..:? "submittedJobsCount")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..:? "progressingJobsCount")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "description")
      )
