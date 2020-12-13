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
    uqStatus,
    uqName,
    uqDescription,
    uqReservationPlanSettings,

    -- * Destructuring the response
    UpdateQueueResponse (..),
    mkUpdateQueueResponse,

    -- ** Response lenses
    uqrsQueue,
    uqrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateQueue' smart constructor.
data UpdateQueue = UpdateQueue'
  { -- | Pause or activate a queue by changing its status between ACTIVE and PAUSED. If you pause a queue, jobs in that queue won't begin. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
    status :: Lude.Maybe QueueStatus,
    -- | The name of the queue that you are modifying.
    name :: Lude.Text,
    -- | The new description for the queue, if you are changing it.
    description :: Lude.Maybe Lude.Text,
    -- | The new details of your pricing plan for your reserved queue. When you set up a new pricing plan to replace an expired one, you enter into another 12-month commitment. When you add capacity to your queue by increasing the number of RTS, you extend the term of your commitment to 12 months from when you add capacity. After you make these commitments, you can't cancel them.
    reservationPlanSettings :: Lude.Maybe ReservationPlanSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateQueue' with the minimum fields required to make a request.
--
-- * 'status' - Pause or activate a queue by changing its status between ACTIVE and PAUSED. If you pause a queue, jobs in that queue won't begin. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
-- * 'name' - The name of the queue that you are modifying.
-- * 'description' - The new description for the queue, if you are changing it.
-- * 'reservationPlanSettings' - The new details of your pricing plan for your reserved queue. When you set up a new pricing plan to replace an expired one, you enter into another 12-month commitment. When you add capacity to your queue by increasing the number of RTS, you extend the term of your commitment to 12 months from when you add capacity. After you make these commitments, you can't cancel them.
mkUpdateQueue ::
  -- | 'name'
  Lude.Text ->
  UpdateQueue
mkUpdateQueue pName_ =
  UpdateQueue'
    { status = Lude.Nothing,
      name = pName_,
      description = Lude.Nothing,
      reservationPlanSettings = Lude.Nothing
    }

-- | Pause or activate a queue by changing its status between ACTIVE and PAUSED. If you pause a queue, jobs in that queue won't begin. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqStatus :: Lens.Lens' UpdateQueue (Lude.Maybe QueueStatus)
uqStatus = Lens.lens (status :: UpdateQueue -> Lude.Maybe QueueStatus) (\s a -> s {status = a} :: UpdateQueue)
{-# DEPRECATED uqStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the queue that you are modifying.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqName :: Lens.Lens' UpdateQueue Lude.Text
uqName = Lens.lens (name :: UpdateQueue -> Lude.Text) (\s a -> s {name = a} :: UpdateQueue)
{-# DEPRECATED uqName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The new description for the queue, if you are changing it.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqDescription :: Lens.Lens' UpdateQueue (Lude.Maybe Lude.Text)
uqDescription = Lens.lens (description :: UpdateQueue -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateQueue)
{-# DEPRECATED uqDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new details of your pricing plan for your reserved queue. When you set up a new pricing plan to replace an expired one, you enter into another 12-month commitment. When you add capacity to your queue by increasing the number of RTS, you extend the term of your commitment to 12 months from when you add capacity. After you make these commitments, you can't cancel them.
--
-- /Note:/ Consider using 'reservationPlanSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqReservationPlanSettings :: Lens.Lens' UpdateQueue (Lude.Maybe ReservationPlanSettings)
uqReservationPlanSettings = Lens.lens (reservationPlanSettings :: UpdateQueue -> Lude.Maybe ReservationPlanSettings) (\s a -> s {reservationPlanSettings = a} :: UpdateQueue)
{-# DEPRECATED uqReservationPlanSettings "Use generic-lens or generic-optics with 'reservationPlanSettings' instead." #-}

instance Lude.AWSRequest UpdateQueue where
  type Rs UpdateQueue = UpdateQueueResponse
  request = Req.putJSON mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateQueueResponse'
            Lude.<$> (x Lude..?> "queue") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateQueue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateQueue where
  toJSON UpdateQueue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("description" Lude..=) Lude.<$> description,
            ("reservationPlanSettings" Lude..=)
              Lude.<$> reservationPlanSettings
          ]
      )

instance Lude.ToPath UpdateQueue where
  toPath UpdateQueue' {..} =
    Lude.mconcat ["/2017-08-29/queues/", Lude.toBS name]

instance Lude.ToQuery UpdateQueue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateQueueResponse' smart constructor.
data UpdateQueueResponse = UpdateQueueResponse'
  { -- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
    queue :: Lude.Maybe Queue,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateQueueResponse' with the minimum fields required to make a request.
--
-- * 'queue' - You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
-- * 'responseStatus' - The response status code.
mkUpdateQueueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateQueueResponse
mkUpdateQueueResponse pResponseStatus_ =
  UpdateQueueResponse'
    { queue = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqrsQueue :: Lens.Lens' UpdateQueueResponse (Lude.Maybe Queue)
uqrsQueue = Lens.lens (queue :: UpdateQueueResponse -> Lude.Maybe Queue) (\s a -> s {queue = a} :: UpdateQueueResponse)
{-# DEPRECATED uqrsQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqrsResponseStatus :: Lens.Lens' UpdateQueueResponse Lude.Int
uqrsResponseStatus = Lens.lens (responseStatus :: UpdateQueueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateQueueResponse)
{-# DEPRECATED uqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
