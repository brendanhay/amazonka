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
    cebrsEventBusARN,
    cebrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateEventBus' smart constructor.
data CreateEventBus = CreateEventBus'
  { -- | The name of the new event bus.
    --
    -- Event bus names cannot contain the / character. You can't use the name @default@ for a custom event bus, as this name is already used for your account's default event bus.
    -- If this is a partner event bus, the name must exactly match the name of the partner event source that this event bus is matched to.
    name :: Lude.Text,
    -- | If you are creating a partner event bus, this specifies the partner event source that the new event bus will be matched with.
    eventSourceName :: Lude.Maybe Lude.Text,
    -- | Tags to associate with the event bus.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEventBus' with the minimum fields required to make a request.
--
-- * 'name' - The name of the new event bus.
--
-- Event bus names cannot contain the / character. You can't use the name @default@ for a custom event bus, as this name is already used for your account's default event bus.
-- If this is a partner event bus, the name must exactly match the name of the partner event source that this event bus is matched to.
-- * 'eventSourceName' - If you are creating a partner event bus, this specifies the partner event source that the new event bus will be matched with.
-- * 'tags' - Tags to associate with the event bus.
mkCreateEventBus ::
  -- | 'name'
  Lude.Text ->
  CreateEventBus
mkCreateEventBus pName_ =
  CreateEventBus'
    { name = pName_,
      eventSourceName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the new event bus.
--
-- Event bus names cannot contain the / character. You can't use the name @default@ for a custom event bus, as this name is already used for your account's default event bus.
-- If this is a partner event bus, the name must exactly match the name of the partner event source that this event bus is matched to.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cebName :: Lens.Lens' CreateEventBus Lude.Text
cebName = Lens.lens (name :: CreateEventBus -> Lude.Text) (\s a -> s {name = a} :: CreateEventBus)
{-# DEPRECATED cebName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If you are creating a partner event bus, this specifies the partner event source that the new event bus will be matched with.
--
-- /Note:/ Consider using 'eventSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cebEventSourceName :: Lens.Lens' CreateEventBus (Lude.Maybe Lude.Text)
cebEventSourceName = Lens.lens (eventSourceName :: CreateEventBus -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceName = a} :: CreateEventBus)
{-# DEPRECATED cebEventSourceName "Use generic-lens or generic-optics with 'eventSourceName' instead." #-}

-- | Tags to associate with the event bus.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cebTags :: Lens.Lens' CreateEventBus (Lude.Maybe [Tag])
cebTags = Lens.lens (tags :: CreateEventBus -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateEventBus)
{-# DEPRECATED cebTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateEventBus where
  type Rs CreateEventBus = CreateEventBusResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateEventBusResponse'
            Lude.<$> (x Lude..?> "EventBusArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateEventBus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.CreateEventBus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateEventBus where
  toJSON CreateEventBus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            ("EventSourceName" Lude..=) Lude.<$> eventSourceName,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateEventBus where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEventBus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateEventBusResponse' smart constructor.
data CreateEventBusResponse = CreateEventBusResponse'
  { -- | The ARN of the new event bus.
    eventBusARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEventBusResponse' with the minimum fields required to make a request.
--
-- * 'eventBusARN' - The ARN of the new event bus.
-- * 'responseStatus' - The response status code.
mkCreateEventBusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateEventBusResponse
mkCreateEventBusResponse pResponseStatus_ =
  CreateEventBusResponse'
    { eventBusARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the new event bus.
--
-- /Note:/ Consider using 'eventBusARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cebrsEventBusARN :: Lens.Lens' CreateEventBusResponse (Lude.Maybe Lude.Text)
cebrsEventBusARN = Lens.lens (eventBusARN :: CreateEventBusResponse -> Lude.Maybe Lude.Text) (\s a -> s {eventBusARN = a} :: CreateEventBusResponse)
{-# DEPRECATED cebrsEventBusARN "Use generic-lens or generic-optics with 'eventBusARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cebrsResponseStatus :: Lens.Lens' CreateEventBusResponse Lude.Int
cebrsResponseStatus = Lens.lens (responseStatus :: CreateEventBusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEventBusResponse)
{-# DEPRECATED cebrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
