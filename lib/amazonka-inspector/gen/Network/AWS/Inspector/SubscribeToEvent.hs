{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SubscribeToEvent (..)
    , mkSubscribeToEvent
    -- ** Request lenses
    , steResourceArn
    , steEvent
    , steTopicArn

    -- * Destructuring the response
    , SubscribeToEventResponse (..)
    , mkSubscribeToEventResponse
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSubscribeToEvent' smart constructor.
data SubscribeToEvent = SubscribeToEvent'
  { resourceArn :: Types.Arn
    -- ^ The ARN of the assessment template that is used during the event for which you want to receive SNS notifications.
  , event :: Types.InspectorEvent
    -- ^ The event for which you want to receive SNS notifications.
  , topicArn :: Types.Arn
    -- ^ The ARN of the SNS topic to which the SNS notifications are sent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubscribeToEvent' value with any optional fields omitted.
mkSubscribeToEvent
    :: Types.Arn -- ^ 'resourceArn'
    -> Types.InspectorEvent -- ^ 'event'
    -> Types.Arn -- ^ 'topicArn'
    -> SubscribeToEvent
mkSubscribeToEvent resourceArn event topicArn
  = SubscribeToEvent'{resourceArn, event, topicArn}

-- | The ARN of the assessment template that is used during the event for which you want to receive SNS notifications.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steResourceArn :: Lens.Lens' SubscribeToEvent Types.Arn
steResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE steResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | The event for which you want to receive SNS notifications.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steEvent :: Lens.Lens' SubscribeToEvent Types.InspectorEvent
steEvent = Lens.field @"event"
{-# INLINEABLE steEvent #-}
{-# DEPRECATED event "Use generic-lens or generic-optics with 'event' instead"  #-}

-- | The ARN of the SNS topic to which the SNS notifications are sent.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steTopicArn :: Lens.Lens' SubscribeToEvent Types.Arn
steTopicArn = Lens.field @"topicArn"
{-# INLINEABLE steTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

instance Core.ToQuery SubscribeToEvent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SubscribeToEvent where
        toHeaders SubscribeToEvent{..}
          = Core.pure ("X-Amz-Target", "InspectorService.SubscribeToEvent")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SubscribeToEvent where
        toJSON SubscribeToEvent{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("resourceArn" Core..= resourceArn),
                  Core.Just ("event" Core..= event),
                  Core.Just ("topicArn" Core..= topicArn)])

instance Core.AWSRequest SubscribeToEvent where
        type Rs SubscribeToEvent = SubscribeToEventResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull SubscribeToEventResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSubscribeToEventResponse' smart constructor.
data SubscribeToEventResponse = SubscribeToEventResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubscribeToEventResponse' value with any optional fields omitted.
mkSubscribeToEventResponse
    :: SubscribeToEventResponse
mkSubscribeToEventResponse = SubscribeToEventResponse'
