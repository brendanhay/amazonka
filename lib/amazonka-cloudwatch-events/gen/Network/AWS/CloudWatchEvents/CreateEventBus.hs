{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.CreateEventBus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new event bus within your account. This can be a custom event bus which you can use to receive events from your custom applications and services, or it can be a partner event bus which can be matched to a partner event source.
module Network.AWS.CloudWatchEvents.CreateEventBus
  ( -- * Creating a request
    CreateEventBus (..),
    mkCreateEventBus,

    -- ** Request lenses
    cebName,
    cebEventSourceName,
    cebTags,

    -- * Destructuring the response
    CreateEventBusResponse (..),
    mkCreateEventBusResponse,

    -- ** Response lenses
    cebrrsEventBusArn,
    cebrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEventBus' smart constructor.
data CreateEventBus = CreateEventBus'
  { -- | The name of the new event bus.
    --
    -- Event bus names cannot contain the / character. You can't use the name @default@ for a custom event bus, as this name is already used for your account's default event bus.
    -- If this is a partner event bus, the name must exactly match the name of the partner event source that this event bus is matched to.
    name :: Types.Name,
    -- | If you are creating a partner event bus, this specifies the partner event source that the new event bus will be matched with.
    eventSourceName :: Core.Maybe Types.EventSourceName,
    -- | Tags to associate with the event bus.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEventBus' value with any optional fields omitted.
mkCreateEventBus ::
  -- | 'name'
  Types.Name ->
  CreateEventBus
mkCreateEventBus name =
  CreateEventBus'
    { name,
      eventSourceName = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the new event bus.
--
-- Event bus names cannot contain the / character. You can't use the name @default@ for a custom event bus, as this name is already used for your account's default event bus.
-- If this is a partner event bus, the name must exactly match the name of the partner event source that this event bus is matched to.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cebName :: Lens.Lens' CreateEventBus Types.Name
cebName = Lens.field @"name"
{-# DEPRECATED cebName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If you are creating a partner event bus, this specifies the partner event source that the new event bus will be matched with.
--
-- /Note:/ Consider using 'eventSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cebEventSourceName :: Lens.Lens' CreateEventBus (Core.Maybe Types.EventSourceName)
cebEventSourceName = Lens.field @"eventSourceName"
{-# DEPRECATED cebEventSourceName "Use generic-lens or generic-optics with 'eventSourceName' instead." #-}

-- | Tags to associate with the event bus.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cebTags :: Lens.Lens' CreateEventBus (Core.Maybe [Types.Tag])
cebTags = Lens.field @"tags"
{-# DEPRECATED cebTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateEventBus where
  toJSON CreateEventBus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("EventSourceName" Core..=) Core.<$> eventSourceName,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateEventBus where
  type Rs CreateEventBus = CreateEventBusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.CreateEventBus")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEventBusResponse'
            Core.<$> (x Core..:? "EventBusArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateEventBusResponse' smart constructor.
data CreateEventBusResponse = CreateEventBusResponse'
  { -- | The ARN of the new event bus.
    eventBusArn :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEventBusResponse' value with any optional fields omitted.
mkCreateEventBusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateEventBusResponse
mkCreateEventBusResponse responseStatus =
  CreateEventBusResponse'
    { eventBusArn = Core.Nothing,
      responseStatus
    }

-- | The ARN of the new event bus.
--
-- /Note:/ Consider using 'eventBusArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cebrrsEventBusArn :: Lens.Lens' CreateEventBusResponse (Core.Maybe Types.String)
cebrrsEventBusArn = Lens.field @"eventBusArn"
{-# DEPRECATED cebrrsEventBusArn "Use generic-lens or generic-optics with 'eventBusArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cebrrsResponseStatus :: Lens.Lens' CreateEventBusResponse Core.Int
cebrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cebrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
