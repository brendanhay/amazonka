{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the event stream for an application.
module Network.AWS.Pinpoint.DeleteEventStream
  ( -- * Creating a request
    DeleteEventStream (..),
    mkDeleteEventStream,

    -- ** Request lenses
    desApplicationId,

    -- * Destructuring the response
    DeleteEventStreamResponse (..),
    mkDeleteEventStreamResponse,

    -- ** Response lenses
    desrrsEventStream,
    desrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEventStream' smart constructor.
newtype DeleteEventStream = DeleteEventStream'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEventStream' value with any optional fields omitted.
mkDeleteEventStream ::
  -- | 'applicationId'
  Core.Text ->
  DeleteEventStream
mkDeleteEventStream applicationId =
  DeleteEventStream' {applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desApplicationId :: Lens.Lens' DeleteEventStream Core.Text
desApplicationId = Lens.field @"applicationId"
{-# DEPRECATED desApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest DeleteEventStream where
  type Rs DeleteEventStream = DeleteEventStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/eventstream")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEventStreamResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteEventStreamResponse' smart constructor.
data DeleteEventStreamResponse = DeleteEventStreamResponse'
  { eventStream :: Types.EventStream,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEventStreamResponse' value with any optional fields omitted.
mkDeleteEventStreamResponse ::
  -- | 'eventStream'
  Types.EventStream ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteEventStreamResponse
mkDeleteEventStreamResponse eventStream responseStatus =
  DeleteEventStreamResponse' {eventStream, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsEventStream :: Lens.Lens' DeleteEventStreamResponse Types.EventStream
desrrsEventStream = Lens.field @"eventStream"
{-# DEPRECATED desrrsEventStream "Use generic-lens or generic-optics with 'eventStream' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsResponseStatus :: Lens.Lens' DeleteEventStreamResponse Core.Int
desrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED desrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
