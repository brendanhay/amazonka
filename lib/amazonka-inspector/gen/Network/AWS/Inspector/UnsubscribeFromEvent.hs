{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.UnsubscribeFromEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the process of sending Amazon Simple Notification Service (SNS) notifications about a specified event to a specified SNS topic.
module Network.AWS.Inspector.UnsubscribeFromEvent
  ( -- * Creating a request
    UnsubscribeFromEvent (..),
    mkUnsubscribeFromEvent,

    -- ** Request lenses
    ufeResourceArn,
    ufeEvent,
    ufeTopicArn,

    -- * Destructuring the response
    UnsubscribeFromEventResponse (..),
    mkUnsubscribeFromEventResponse,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUnsubscribeFromEvent' smart constructor.
data UnsubscribeFromEvent = UnsubscribeFromEvent'
  { -- | The ARN of the assessment template that is used during the event for which you want to stop receiving SNS notifications.
    resourceArn :: Types.Arn,
    -- | The event for which you want to stop receiving SNS notifications.
    event :: Types.InspectorEvent,
    -- | The ARN of the SNS topic to which SNS notifications are sent.
    topicArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnsubscribeFromEvent' value with any optional fields omitted.
mkUnsubscribeFromEvent ::
  -- | 'resourceArn'
  Types.Arn ->
  -- | 'event'
  Types.InspectorEvent ->
  -- | 'topicArn'
  Types.Arn ->
  UnsubscribeFromEvent
mkUnsubscribeFromEvent resourceArn event topicArn =
  UnsubscribeFromEvent' {resourceArn, event, topicArn}

-- | The ARN of the assessment template that is used during the event for which you want to stop receiving SNS notifications.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeResourceArn :: Lens.Lens' UnsubscribeFromEvent Types.Arn
ufeResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED ufeResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | The event for which you want to stop receiving SNS notifications.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeEvent :: Lens.Lens' UnsubscribeFromEvent Types.InspectorEvent
ufeEvent = Lens.field @"event"
{-# DEPRECATED ufeEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | The ARN of the SNS topic to which SNS notifications are sent.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeTopicArn :: Lens.Lens' UnsubscribeFromEvent Types.Arn
ufeTopicArn = Lens.field @"topicArn"
{-# DEPRECATED ufeTopicArn "Use generic-lens or generic-optics with 'topicArn' instead." #-}

instance Core.FromJSON UnsubscribeFromEvent where
  toJSON UnsubscribeFromEvent {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceArn" Core..= resourceArn),
            Core.Just ("event" Core..= event),
            Core.Just ("topicArn" Core..= topicArn)
          ]
      )

instance Core.AWSRequest UnsubscribeFromEvent where
  type Rs UnsubscribeFromEvent = UnsubscribeFromEventResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "InspectorService.UnsubscribeFromEvent")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UnsubscribeFromEventResponse'

-- | /See:/ 'mkUnsubscribeFromEventResponse' smart constructor.
data UnsubscribeFromEventResponse = UnsubscribeFromEventResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnsubscribeFromEventResponse' value with any optional fields omitted.
mkUnsubscribeFromEventResponse ::
  UnsubscribeFromEventResponse
mkUnsubscribeFromEventResponse = UnsubscribeFromEventResponse'
