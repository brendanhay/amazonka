{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.SubscribeToEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the process of sending Amazon Simple Notification Service (SNS) notifications about a specified event to a specified SNS topic.
module Network.AWS.Inspector.SubscribeToEvent
  ( -- * Creating a request
    SubscribeToEvent (..),
    mkSubscribeToEvent,

    -- ** Request lenses
    steResourceArn,
    steEvent,
    steTopicArn,

    -- * Destructuring the response
    SubscribeToEventResponse (..),
    mkSubscribeToEventResponse,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSubscribeToEvent' smart constructor.
data SubscribeToEvent = SubscribeToEvent'
  { -- | The ARN of the assessment template that is used during the event for which you want to receive SNS notifications.
    resourceArn :: Types.Arn,
    -- | The event for which you want to receive SNS notifications.
    event :: Types.InspectorEvent,
    -- | The ARN of the SNS topic to which the SNS notifications are sent.
    topicArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubscribeToEvent' value with any optional fields omitted.
mkSubscribeToEvent ::
  -- | 'resourceArn'
  Types.Arn ->
  -- | 'event'
  Types.InspectorEvent ->
  -- | 'topicArn'
  Types.Arn ->
  SubscribeToEvent
mkSubscribeToEvent resourceArn event topicArn =
  SubscribeToEvent' {resourceArn, event, topicArn}

-- | The ARN of the assessment template that is used during the event for which you want to receive SNS notifications.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steResourceArn :: Lens.Lens' SubscribeToEvent Types.Arn
steResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED steResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | The event for which you want to receive SNS notifications.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steEvent :: Lens.Lens' SubscribeToEvent Types.InspectorEvent
steEvent = Lens.field @"event"
{-# DEPRECATED steEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | The ARN of the SNS topic to which the SNS notifications are sent.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steTopicArn :: Lens.Lens' SubscribeToEvent Types.Arn
steTopicArn = Lens.field @"topicArn"
{-# DEPRECATED steTopicArn "Use generic-lens or generic-optics with 'topicArn' instead." #-}

instance Core.FromJSON SubscribeToEvent where
  toJSON SubscribeToEvent {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceArn" Core..= resourceArn),
            Core.Just ("event" Core..= event),
            Core.Just ("topicArn" Core..= topicArn)
          ]
      )

instance Core.AWSRequest SubscribeToEvent where
  type Rs SubscribeToEvent = SubscribeToEventResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "InspectorService.SubscribeToEvent")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull SubscribeToEventResponse'

-- | /See:/ 'mkSubscribeToEventResponse' smart constructor.
data SubscribeToEventResponse = SubscribeToEventResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubscribeToEventResponse' value with any optional fields omitted.
mkSubscribeToEventResponse ::
  SubscribeToEventResponse
mkSubscribeToEventResponse = SubscribeToEventResponse'
