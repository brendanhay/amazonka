{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.CreateQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new transcoding queue. For information about queues, see Working With Queues in the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html
module Network.AWS.MediaConvert.CreateQueue
  ( -- * Creating a request
    CreateQueue (..),
    mkCreateQueue,

    -- ** Request lenses
    cqName,
    cqDescription,
    cqPricingPlan,
    cqReservationPlanSettings,
    cqStatus,
    cqTags,

    -- * Destructuring the response
    CreateQueueResponse (..),
    mkCreateQueueResponse,

    -- ** Response lenses
    cqrrsQueue,
    cqrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateQueue' smart constructor.
data CreateQueue = CreateQueue'
  { -- | The name of the queue that you are creating.
    name :: Core.Text,
    -- | Optional. A description of the queue that you are creating.
    description :: Core.Maybe Core.Text,
    -- | Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment. When you use the API to create a queue, the default is on-demand.
    pricingPlan :: Core.Maybe Types.PricingPlan,
    -- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
    reservationPlanSettings :: Core.Maybe Types.ReservationPlanSettings,
    -- | Initial state of the queue. If you create a paused queue, then jobs in that queue won't begin.
    status :: Core.Maybe Types.QueueStatus,
    -- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateQueue' value with any optional fields omitted.
mkCreateQueue ::
  -- | 'name'
  Core.Text ->
  CreateQueue
mkCreateQueue name =
  CreateQueue'
    { name,
      description = Core.Nothing,
      pricingPlan = Core.Nothing,
      reservationPlanSettings = Core.Nothing,
      status = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the queue that you are creating.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqName :: Lens.Lens' CreateQueue Core.Text
cqName = Lens.field @"name"
{-# DEPRECATED cqName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Optional. A description of the queue that you are creating.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqDescription :: Lens.Lens' CreateQueue (Core.Maybe Core.Text)
cqDescription = Lens.field @"description"
{-# DEPRECATED cqDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment. When you use the API to create a queue, the default is on-demand.
--
-- /Note:/ Consider using 'pricingPlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqPricingPlan :: Lens.Lens' CreateQueue (Core.Maybe Types.PricingPlan)
cqPricingPlan = Lens.field @"pricingPlan"
{-# DEPRECATED cqPricingPlan "Use generic-lens or generic-optics with 'pricingPlan' instead." #-}

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /Note:/ Consider using 'reservationPlanSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqReservationPlanSettings :: Lens.Lens' CreateQueue (Core.Maybe Types.ReservationPlanSettings)
cqReservationPlanSettings = Lens.field @"reservationPlanSettings"
{-# DEPRECATED cqReservationPlanSettings "Use generic-lens or generic-optics with 'reservationPlanSettings' instead." #-}

-- | Initial state of the queue. If you create a paused queue, then jobs in that queue won't begin.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqStatus :: Lens.Lens' CreateQueue (Core.Maybe Types.QueueStatus)
cqStatus = Lens.field @"status"
{-# DEPRECATED cqStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqTags :: Lens.Lens' CreateQueue (Core.Maybe (Core.HashMap Core.Text Core.Text))
cqTags = Lens.field @"tags"
{-# DEPRECATED cqTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateQueue where
  toJSON CreateQueue {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            ("description" Core..=) Core.<$> description,
            ("pricingPlan" Core..=) Core.<$> pricingPlan,
            ("reservationPlanSettings" Core..=)
              Core.<$> reservationPlanSettings,
            ("status" Core..=) Core.<$> status,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateQueue where
  type Rs CreateQueue = CreateQueueResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2017-08-29/queues",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateQueueResponse'
            Core.<$> (x Core..:? "queue") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateQueueResponse' smart constructor.
data CreateQueueResponse = CreateQueueResponse'
  { -- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
    queue :: Core.Maybe Types.Queue,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateQueueResponse' value with any optional fields omitted.
mkCreateQueueResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateQueueResponse
mkCreateQueueResponse responseStatus =
  CreateQueueResponse' {queue = Core.Nothing, responseStatus}

-- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqrrsQueue :: Lens.Lens' CreateQueueResponse (Core.Maybe Types.Queue)
cqrrsQueue = Lens.field @"queue"
{-# DEPRECATED cqrrsQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqrrsResponseStatus :: Lens.Lens' CreateQueueResponse Core.Int
cqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
