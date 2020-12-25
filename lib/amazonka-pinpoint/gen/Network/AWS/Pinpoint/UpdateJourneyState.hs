{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateJourneyState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels (stops) an active journey.
module Network.AWS.Pinpoint.UpdateJourneyState
  ( -- * Creating a request
    UpdateJourneyState (..),
    mkUpdateJourneyState,

    -- ** Request lenses
    ujsJourneyId,
    ujsApplicationId,
    ujsJourneyStateRequest,

    -- * Destructuring the response
    UpdateJourneyStateResponse (..),
    mkUpdateJourneyStateResponse,

    -- ** Response lenses
    ujsrrsJourneyResponse,
    ujsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateJourneyState' smart constructor.
data UpdateJourneyState = UpdateJourneyState'
  { -- | The unique identifier for the journey.
    journeyId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    journeyStateRequest :: Types.JourneyStateRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJourneyState' value with any optional fields omitted.
mkUpdateJourneyState ::
  -- | 'journeyId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  -- | 'journeyStateRequest'
  Types.JourneyStateRequest ->
  UpdateJourneyState
mkUpdateJourneyState journeyId applicationId journeyStateRequest =
  UpdateJourneyState'
    { journeyId,
      applicationId,
      journeyStateRequest
    }

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujsJourneyId :: Lens.Lens' UpdateJourneyState Core.Text
ujsJourneyId = Lens.field @"journeyId"
{-# DEPRECATED ujsJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujsApplicationId :: Lens.Lens' UpdateJourneyState Core.Text
ujsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED ujsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyStateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujsJourneyStateRequest :: Lens.Lens' UpdateJourneyState Types.JourneyStateRequest
ujsJourneyStateRequest = Lens.field @"journeyStateRequest"
{-# DEPRECATED ujsJourneyStateRequest "Use generic-lens or generic-optics with 'journeyStateRequest' instead." #-}

instance Core.FromJSON UpdateJourneyState where
  toJSON UpdateJourneyState {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("JourneyStateRequest" Core..= journeyStateRequest)]
      )

instance Core.AWSRequest UpdateJourneyState where
  type Rs UpdateJourneyState = UpdateJourneyStateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/journeys/")
                Core.<> (Core.toText journeyId)
                Core.<> ("/state")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJourneyStateResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateJourneyStateResponse' smart constructor.
data UpdateJourneyStateResponse = UpdateJourneyStateResponse'
  { journeyResponse :: Types.JourneyResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateJourneyStateResponse' value with any optional fields omitted.
mkUpdateJourneyStateResponse ::
  -- | 'journeyResponse'
  Types.JourneyResponse ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateJourneyStateResponse
mkUpdateJourneyStateResponse journeyResponse responseStatus =
  UpdateJourneyStateResponse' {journeyResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujsrrsJourneyResponse :: Lens.Lens' UpdateJourneyStateResponse Types.JourneyResponse
ujsrrsJourneyResponse = Lens.field @"journeyResponse"
{-# DEPRECATED ujsrrsJourneyResponse "Use generic-lens or generic-optics with 'journeyResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujsrrsResponseStatus :: Lens.Lens' UpdateJourneyStateResponse Core.Int
ujsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ujsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
