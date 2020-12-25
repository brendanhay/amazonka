{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gqrrsQueue,
    gqrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetQueue' smart constructor.
newtype GetQueue = GetQueue'
  { -- | The name of the queue that you want information about.
    name :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetQueue' value with any optional fields omitted.
mkGetQueue ::
  -- | 'name'
  Core.Text ->
  GetQueue
mkGetQueue name = GetQueue' {name}

-- | The name of the queue that you want information about.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqName :: Lens.Lens' GetQueue Core.Text
gqName = Lens.field @"name"
{-# DEPRECATED gqName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.AWSRequest GetQueue where
  type Rs GetQueue = GetQueueResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/2017-08-29/queues/" Core.<> (Core.toText name)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQueueResponse'
            Core.<$> (x Core..:? "queue") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetQueueResponse' smart constructor.
data GetQueueResponse = GetQueueResponse'
  { -- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
    queue :: Core.Maybe Types.Queue,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetQueueResponse' value with any optional fields omitted.
mkGetQueueResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetQueueResponse
mkGetQueueResponse responseStatus =
  GetQueueResponse' {queue = Core.Nothing, responseStatus}

-- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrrsQueue :: Lens.Lens' GetQueueResponse (Core.Maybe Types.Queue)
gqrrsQueue = Lens.field @"queue"
{-# DEPRECATED gqrrsQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrrsResponseStatus :: Lens.Lens' GetQueueResponse Core.Int
gqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
