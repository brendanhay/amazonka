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
    cqStatus,
    cqPricingPlan,
    cqName,
    cqDescription,
    cqReservationPlanSettings,
    cqTags,

    -- * Destructuring the response
    CreateQueueResponse (..),
    mkCreateQueueResponse,

    -- ** Response lenses
    cqrsQueue,
    cqrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateQueue' smart constructor.
data CreateQueue = CreateQueue'
  { -- | Initial state of the queue. If you create a paused queue, then jobs in that queue won't begin.
    status :: Lude.Maybe QueueStatus,
    -- | Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment. When you use the API to create a queue, the default is on-demand.
    pricingPlan :: Lude.Maybe PricingPlan,
    -- | The name of the queue that you are creating.
    name :: Lude.Text,
    -- | Optional. A description of the queue that you are creating.
    description :: Lude.Maybe Lude.Text,
    -- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
    reservationPlanSettings :: Lude.Maybe ReservationPlanSettings,
    -- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateQueue' with the minimum fields required to make a request.
--
-- * 'status' - Initial state of the queue. If you create a paused queue, then jobs in that queue won't begin.
-- * 'pricingPlan' - Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment. When you use the API to create a queue, the default is on-demand.
-- * 'name' - The name of the queue that you are creating.
-- * 'description' - Optional. A description of the queue that you are creating.
-- * 'reservationPlanSettings' - Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
-- * 'tags' - The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
mkCreateQueue ::
  -- | 'name'
  Lude.Text ->
  CreateQueue
mkCreateQueue pName_ =
  CreateQueue'
    { status = Lude.Nothing,
      pricingPlan = Lude.Nothing,
      name = pName_,
      description = Lude.Nothing,
      reservationPlanSettings = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Initial state of the queue. If you create a paused queue, then jobs in that queue won't begin.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqStatus :: Lens.Lens' CreateQueue (Lude.Maybe QueueStatus)
cqStatus = Lens.lens (status :: CreateQueue -> Lude.Maybe QueueStatus) (\s a -> s {status = a} :: CreateQueue)
{-# DEPRECATED cqStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment. When you use the API to create a queue, the default is on-demand.
--
-- /Note:/ Consider using 'pricingPlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqPricingPlan :: Lens.Lens' CreateQueue (Lude.Maybe PricingPlan)
cqPricingPlan = Lens.lens (pricingPlan :: CreateQueue -> Lude.Maybe PricingPlan) (\s a -> s {pricingPlan = a} :: CreateQueue)
{-# DEPRECATED cqPricingPlan "Use generic-lens or generic-optics with 'pricingPlan' instead." #-}

-- | The name of the queue that you are creating.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqName :: Lens.Lens' CreateQueue Lude.Text
cqName = Lens.lens (name :: CreateQueue -> Lude.Text) (\s a -> s {name = a} :: CreateQueue)
{-# DEPRECATED cqName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Optional. A description of the queue that you are creating.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqDescription :: Lens.Lens' CreateQueue (Lude.Maybe Lude.Text)
cqDescription = Lens.lens (description :: CreateQueue -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateQueue)
{-# DEPRECATED cqDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /Note:/ Consider using 'reservationPlanSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqReservationPlanSettings :: Lens.Lens' CreateQueue (Lude.Maybe ReservationPlanSettings)
cqReservationPlanSettings = Lens.lens (reservationPlanSettings :: CreateQueue -> Lude.Maybe ReservationPlanSettings) (\s a -> s {reservationPlanSettings = a} :: CreateQueue)
{-# DEPRECATED cqReservationPlanSettings "Use generic-lens or generic-optics with 'reservationPlanSettings' instead." #-}

-- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqTags :: Lens.Lens' CreateQueue (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cqTags = Lens.lens (tags :: CreateQueue -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateQueue)
{-# DEPRECATED cqTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateQueue where
  type Rs CreateQueue = CreateQueueResponse
  request = Req.postJSON mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateQueueResponse'
            Lude.<$> (x Lude..?> "queue") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateQueue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateQueue where
  toJSON CreateQueue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("pricingPlan" Lude..=) Lude.<$> pricingPlan,
            Lude.Just ("name" Lude..= name),
            ("description" Lude..=) Lude.<$> description,
            ("reservationPlanSettings" Lude..=)
              Lude.<$> reservationPlanSettings,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateQueue where
  toPath = Lude.const "/2017-08-29/queues"

instance Lude.ToQuery CreateQueue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateQueueResponse' smart constructor.
data CreateQueueResponse = CreateQueueResponse'
  { -- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
    queue :: Lude.Maybe Queue,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateQueueResponse' with the minimum fields required to make a request.
--
-- * 'queue' - You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
-- * 'responseStatus' - The response status code.
mkCreateQueueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateQueueResponse
mkCreateQueueResponse pResponseStatus_ =
  CreateQueueResponse'
    { queue = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqrsQueue :: Lens.Lens' CreateQueueResponse (Lude.Maybe Queue)
cqrsQueue = Lens.lens (queue :: CreateQueueResponse -> Lude.Maybe Queue) (\s a -> s {queue = a} :: CreateQueueResponse)
{-# DEPRECATED cqrsQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqrsResponseStatus :: Lens.Lens' CreateQueueResponse Lude.Int
cqrsResponseStatus = Lens.lens (responseStatus :: CreateQueueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateQueueResponse)
{-# DEPRECATED cqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
