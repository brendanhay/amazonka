{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.PutEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new event to record for endpoints, or creates or updates endpoint data that existing events are associated with.
module Network.AWS.Pinpoint.PutEvents
  ( -- * Creating a request
    PutEvents (..),
    mkPutEvents,

    -- ** Request lenses
    peApplicationId,
    peEventsRequest,

    -- * Destructuring the response
    PutEventsResponse (..),
    mkPutEventsResponse,

    -- ** Response lenses
    perrsEventsResponse,
    perrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutEvents' smart constructor.
data PutEvents = PutEvents'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    eventsRequest :: Types.EventsRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutEvents' value with any optional fields omitted.
mkPutEvents ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'eventsRequest'
  Types.EventsRequest ->
  PutEvents
mkPutEvents applicationId eventsRequest =
  PutEvents' {applicationId, eventsRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peApplicationId :: Lens.Lens' PutEvents Core.Text
peApplicationId = Lens.field @"applicationId"
{-# DEPRECATED peApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventsRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEventsRequest :: Lens.Lens' PutEvents Types.EventsRequest
peEventsRequest = Lens.field @"eventsRequest"
{-# DEPRECATED peEventsRequest "Use generic-lens or generic-optics with 'eventsRequest' instead." #-}

instance Core.FromJSON PutEvents where
  toJSON PutEvents {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EventsRequest" Core..= eventsRequest)]
      )

instance Core.AWSRequest PutEvents where
  type Rs PutEvents = PutEventsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/events")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEventsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutEventsResponse' smart constructor.
data PutEventsResponse = PutEventsResponse'
  { eventsResponse :: Types.EventsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutEventsResponse' value with any optional fields omitted.
mkPutEventsResponse ::
  -- | 'eventsResponse'
  Types.EventsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  PutEventsResponse
mkPutEventsResponse eventsResponse responseStatus =
  PutEventsResponse' {eventsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
perrsEventsResponse :: Lens.Lens' PutEventsResponse Types.EventsResponse
perrsEventsResponse = Lens.field @"eventsResponse"
{-# DEPRECATED perrsEventsResponse "Use generic-lens or generic-optics with 'eventsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
perrsResponseStatus :: Lens.Lens' PutEventsResponse Core.Int
perrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED perrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
