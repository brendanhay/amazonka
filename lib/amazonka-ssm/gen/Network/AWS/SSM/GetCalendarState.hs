{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetCalendarState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the state of the AWS Systems Manager Change Calendar at an optional, specified time. If you specify a time, @GetCalendarState@ returns the state of the calendar at a specific time, and returns the next time that the Change Calendar state will transition. If you do not specify a time, @GetCalendarState@ assumes the current time. Change Calendar entries have two possible states: @OPEN@ or @CLOSED@ .
--
-- If you specify more than one calendar in a request, the command returns the status of @OPEN@ only if all calendars in the request are open. If one or more calendars in the request are closed, the status returned is @CLOSED@ .
-- For more information about Systems Manager Change Calendar, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar.html AWS Systems Manager Change Calendar> in the /AWS Systems Manager User Guide/ .
module Network.AWS.SSM.GetCalendarState
    (
    -- * Creating a request
      GetCalendarState (..)
    , mkGetCalendarState
    -- ** Request lenses
    , gcsCalendarNames
    , gcsAtTime

    -- * Destructuring the response
    , GetCalendarStateResponse (..)
    , mkGetCalendarStateResponse
    -- ** Response lenses
    , grsAtTime
    , grsNextTransitionTime
    , grsState
    , grsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetCalendarState' smart constructor.
data GetCalendarState = GetCalendarState'
  { calendarNames :: [Types.CalendarNameOrARN]
    -- ^ The names or Amazon Resource Names (ARNs) of the Systems Manager documents that represent the calendar entries for which you want to get the state.
  , atTime :: Core.Maybe Types.AtTime
    -- ^ (Optional) The specific time for which you want to get calendar state information, in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> format. If you do not add @AtTime@ , the current time is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCalendarState' value with any optional fields omitted.
mkGetCalendarState
    :: GetCalendarState
mkGetCalendarState
  = GetCalendarState'{calendarNames = Core.mempty,
                      atTime = Core.Nothing}

-- | The names or Amazon Resource Names (ARNs) of the Systems Manager documents that represent the calendar entries for which you want to get the state.
--
-- /Note:/ Consider using 'calendarNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsCalendarNames :: Lens.Lens' GetCalendarState [Types.CalendarNameOrARN]
gcsCalendarNames = Lens.field @"calendarNames"
{-# INLINEABLE gcsCalendarNames #-}
{-# DEPRECATED calendarNames "Use generic-lens or generic-optics with 'calendarNames' instead"  #-}

-- | (Optional) The specific time for which you want to get calendar state information, in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> format. If you do not add @AtTime@ , the current time is assumed.
--
-- /Note:/ Consider using 'atTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsAtTime :: Lens.Lens' GetCalendarState (Core.Maybe Types.AtTime)
gcsAtTime = Lens.field @"atTime"
{-# INLINEABLE gcsAtTime #-}
{-# DEPRECATED atTime "Use generic-lens or generic-optics with 'atTime' instead"  #-}

instance Core.ToQuery GetCalendarState where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCalendarState where
        toHeaders GetCalendarState{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.GetCalendarState") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCalendarState where
        toJSON GetCalendarState{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CalendarNames" Core..= calendarNames),
                  ("AtTime" Core..=) Core.<$> atTime])

instance Core.AWSRequest GetCalendarState where
        type Rs GetCalendarState = GetCalendarStateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCalendarStateResponse' Core.<$>
                   (x Core..:? "AtTime") Core.<*> x Core..:? "NextTransitionTime"
                     Core.<*> x Core..:? "State"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCalendarStateResponse' smart constructor.
data GetCalendarStateResponse = GetCalendarStateResponse'
  { atTime :: Core.Maybe Types.AtTime
    -- ^ The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that you specified in your command. If you did not specify a time, @GetCalendarState@ uses the current time.
  , nextTransitionTime :: Core.Maybe Types.NextTransitionTime
    -- ^ The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that the calendar state will change. If the current calendar state is @OPEN@ , @NextTransitionTime@ indicates when the calendar state changes to @CLOSED@ , and vice-versa.
  , state :: Core.Maybe Types.CalendarState
    -- ^ The state of the calendar. An @OPEN@ calendar indicates that actions are allowed to proceed, and a @CLOSED@ calendar indicates that actions are not allowed to proceed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCalendarStateResponse' value with any optional fields omitted.
mkGetCalendarStateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCalendarStateResponse
mkGetCalendarStateResponse responseStatus
  = GetCalendarStateResponse'{atTime = Core.Nothing,
                              nextTransitionTime = Core.Nothing, state = Core.Nothing,
                              responseStatus}

-- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that you specified in your command. If you did not specify a time, @GetCalendarState@ uses the current time.
--
-- /Note:/ Consider using 'atTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsAtTime :: Lens.Lens' GetCalendarStateResponse (Core.Maybe Types.AtTime)
grsAtTime = Lens.field @"atTime"
{-# INLINEABLE grsAtTime #-}
{-# DEPRECATED atTime "Use generic-lens or generic-optics with 'atTime' instead"  #-}

-- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that the calendar state will change. If the current calendar state is @OPEN@ , @NextTransitionTime@ indicates when the calendar state changes to @CLOSED@ , and vice-versa.
--
-- /Note:/ Consider using 'nextTransitionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsNextTransitionTime :: Lens.Lens' GetCalendarStateResponse (Core.Maybe Types.NextTransitionTime)
grsNextTransitionTime = Lens.field @"nextTransitionTime"
{-# INLINEABLE grsNextTransitionTime #-}
{-# DEPRECATED nextTransitionTime "Use generic-lens or generic-optics with 'nextTransitionTime' instead"  #-}

-- | The state of the calendar. An @OPEN@ calendar indicates that actions are allowed to proceed, and a @CLOSED@ calendar indicates that actions are not allowed to proceed.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsState :: Lens.Lens' GetCalendarStateResponse (Core.Maybe Types.CalendarState)
grsState = Lens.field @"state"
{-# INLINEABLE grsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetCalendarStateResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
