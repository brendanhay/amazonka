{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.GetQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific queue.
module Network.AWS.MediaConvert.GetQueue
  ( -- * Creating a request
    GetQueue (..),
    mkGetQueue,

    -- ** Request lenses
    gqName,

    -- * Destructuring the response
    GetQueueResponse (..),
    mkGetQueueResponse,

    -- ** Response lenses
    gqrsQueue,
    gqrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetQueue' smart constructor.
newtype GetQueue = GetQueue' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQueue' with the minimum fields required to make a request.
--
-- * 'name' - The name of the queue that you want information about.
mkGetQueue ::
  -- | 'name'
  Lude.Text ->
  GetQueue
mkGetQueue pName_ = GetQueue' {name = pName_}

-- | The name of the queue that you want information about.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqName :: Lens.Lens' GetQueue Lude.Text
gqName = Lens.lens (name :: GetQueue -> Lude.Text) (\s a -> s {name = a} :: GetQueue)
{-# DEPRECATED gqName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetQueue where
  type Rs GetQueue = GetQueueResponse
  request = Req.get mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetQueueResponse'
            Lude.<$> (x Lude..?> "queue") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetQueue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetQueue where
  toPath GetQueue' {..} =
    Lude.mconcat ["/2017-08-29/queues/", Lude.toBS name]

instance Lude.ToQuery GetQueue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetQueueResponse' smart constructor.
data GetQueueResponse = GetQueueResponse'
  { queue ::
      Lude.Maybe Queue,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQueueResponse' with the minimum fields required to make a request.
--
-- * 'queue' - You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
-- * 'responseStatus' - The response status code.
mkGetQueueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetQueueResponse
mkGetQueueResponse pResponseStatus_ =
  GetQueueResponse'
    { queue = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrsQueue :: Lens.Lens' GetQueueResponse (Lude.Maybe Queue)
gqrsQueue = Lens.lens (queue :: GetQueueResponse -> Lude.Maybe Queue) (\s a -> s {queue = a} :: GetQueueResponse)
{-# DEPRECATED gqrsQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrsResponseStatus :: Lens.Lens' GetQueueResponse Lude.Int
gqrsResponseStatus = Lens.lens (responseStatus :: GetQueueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetQueueResponse)
{-# DEPRECATED gqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
