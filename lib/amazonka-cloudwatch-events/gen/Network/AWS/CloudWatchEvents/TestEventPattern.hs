{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.TestEventPattern
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests whether the specified event pattern matches the provided event.
--
-- Most services in AWS treat : or / as the same character in Amazon Resource Names (ARNs). However, EventBridge uses an exact match in event patterns and rules. Be sure to use the correct ARN characters when creating event patterns so that they match the ARN syntax in the event you want to match.
module Network.AWS.CloudWatchEvents.TestEventPattern
  ( -- * Creating a request
    TestEventPattern (..),
    mkTestEventPattern,

    -- ** Request lenses
    tepEventPattern,
    tepEvent,

    -- * Destructuring the response
    TestEventPatternResponse (..),
    mkTestEventPatternResponse,

    -- ** Response lenses
    teprrsResult,
    teprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTestEventPattern' smart constructor.
data TestEventPattern = TestEventPattern'
  { -- | The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
    eventPattern :: Types.EventPattern,
    -- | The event, in JSON format, to test against the event pattern.
    event :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestEventPattern' value with any optional fields omitted.
mkTestEventPattern ::
  -- | 'eventPattern'
  Types.EventPattern ->
  -- | 'event'
  Types.String ->
  TestEventPattern
mkTestEventPattern eventPattern event =
  TestEventPattern' {eventPattern, event}

-- | The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tepEventPattern :: Lens.Lens' TestEventPattern Types.EventPattern
tepEventPattern = Lens.field @"eventPattern"
{-# DEPRECATED tepEventPattern "Use generic-lens or generic-optics with 'eventPattern' instead." #-}

-- | The event, in JSON format, to test against the event pattern.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tepEvent :: Lens.Lens' TestEventPattern Types.String
tepEvent = Lens.field @"event"
{-# DEPRECATED tepEvent "Use generic-lens or generic-optics with 'event' instead." #-}

instance Core.FromJSON TestEventPattern where
  toJSON TestEventPattern {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EventPattern" Core..= eventPattern),
            Core.Just ("Event" Core..= event)
          ]
      )

instance Core.AWSRequest TestEventPattern where
  type Rs TestEventPattern = TestEventPatternResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.TestEventPattern")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TestEventPatternResponse'
            Core.<$> (x Core..:? "Result") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTestEventPatternResponse' smart constructor.
data TestEventPatternResponse = TestEventPatternResponse'
  { -- | Indicates whether the event matches the event pattern.
    result :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestEventPatternResponse' value with any optional fields omitted.
mkTestEventPatternResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TestEventPatternResponse
mkTestEventPatternResponse responseStatus =
  TestEventPatternResponse' {result = Core.Nothing, responseStatus}

-- | Indicates whether the event matches the event pattern.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teprrsResult :: Lens.Lens' TestEventPatternResponse (Core.Maybe Core.Bool)
teprrsResult = Lens.field @"result"
{-# DEPRECATED teprrsResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teprrsResponseStatus :: Lens.Lens' TestEventPatternResponse Core.Int
teprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED teprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
