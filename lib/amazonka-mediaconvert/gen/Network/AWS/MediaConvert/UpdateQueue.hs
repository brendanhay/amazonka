{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.UpdateQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing queues.
module Network.AWS.MediaConvert.UpdateQueue
  ( -- * Creating a request
    UpdateQueue (..),
    mkUpdateQueue,

    -- ** Request lenses
    uqName,
    uqDescription,
    uqReservationPlanSettings,
    uqStatus,

    -- * Destructuring the response
    UpdateQueueResponse (..),
    mkUpdateQueueResponse,

    -- ** Response lenses
    uqrrsQueue,
    uqrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateQueue' smart constructor.
data UpdateQueue = UpdateQueue'
  { -- | The name of the queue that you are modifying.
    name :: Core.Text,
    -- | The new description for the queue, if you are changing it.
    description :: Core.Maybe Core.Text,
    -- | The new details of your pricing plan for your reserved queue. When you set up a new pricing plan to replace an expired one, you enter into another 12-month commitment. When you add capacity to your queue by increasing the number of RTS, you extend the term of your commitment to 12 months from when you add capacity. After you make these commitments, you can't cancel them.
    reservationPlanSettings :: Core.Maybe Types.ReservationPlanSettings,
    -- | Pause or activate a queue by changing its status between ACTIVE and PAUSED. If you pause a queue, jobs in that queue won't begin. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
    status :: Core.Maybe Types.QueueStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateQueue' value with any optional fields omitted.
mkUpdateQueue ::
  -- | 'name'
  Core.Text ->
  UpdateQueue
mkUpdateQueue name =
  UpdateQueue'
    { name,
      description = Core.Nothing,
      reservationPlanSettings = Core.Nothing,
      status = Core.Nothing
    }

-- | The name of the queue that you are modifying.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqName :: Lens.Lens' UpdateQueue Core.Text
uqName = Lens.field @"name"
{-# DEPRECATED uqName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The new description for the queue, if you are changing it.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqDescription :: Lens.Lens' UpdateQueue (Core.Maybe Core.Text)
uqDescription = Lens.field @"description"
{-# DEPRECATED uqDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new details of your pricing plan for your reserved queue. When you set up a new pricing plan to replace an expired one, you enter into another 12-month commitment. When you add capacity to your queue by increasing the number of RTS, you extend the term of your commitment to 12 months from when you add capacity. After you make these commitments, you can't cancel them.
--
-- /Note:/ Consider using 'reservationPlanSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqReservationPlanSettings :: Lens.Lens' UpdateQueue (Core.Maybe Types.ReservationPlanSettings)
uqReservationPlanSettings = Lens.field @"reservationPlanSettings"
{-# DEPRECATED uqReservationPlanSettings "Use generic-lens or generic-optics with 'reservationPlanSettings' instead." #-}

-- | Pause or activate a queue by changing its status between ACTIVE and PAUSED. If you pause a queue, jobs in that queue won't begin. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqStatus :: Lens.Lens' UpdateQueue (Core.Maybe Types.QueueStatus)
uqStatus = Lens.field @"status"
{-# DEPRECATED uqStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON UpdateQueue where
  toJSON UpdateQueue {..} =
    Core.object
      ( Core.catMaybes
          [ ("description" Core..=) Core.<$> description,
            ("reservationPlanSettings" Core..=)
              Core.<$> reservationPlanSettings,
            ("status" Core..=) Core.<$> status
          ]
      )

instance Core.AWSRequest UpdateQueue where
  type Rs UpdateQueue = UpdateQueueResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath ("/2017-08-29/queues/" Core.<> (Core.toText name)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateQueueResponse'
            Core.<$> (x Core..:? "queue") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateQueueResponse' smart constructor.
data UpdateQueueResponse = UpdateQueueResponse'
  { -- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
    queue :: Core.Maybe Types.Queue,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateQueueResponse' value with any optional fields omitted.
mkUpdateQueueResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateQueueResponse
mkUpdateQueueResponse responseStatus =
  UpdateQueueResponse' {queue = Core.Nothing, responseStatus}

-- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqrrsQueue :: Lens.Lens' UpdateQueueResponse (Core.Maybe Types.Queue)
uqrrsQueue = Lens.field @"queue"
{-# DEPRECATED uqrrsQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqrrsResponseStatus :: Lens.Lens' UpdateQueueResponse Core.Int
uqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
