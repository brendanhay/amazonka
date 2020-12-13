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
    steEvent,
    steTopicARN,
    steResourceARN,

    -- * Destructuring the response
    SubscribeToEventResponse (..),
    mkSubscribeToEventResponse,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSubscribeToEvent' smart constructor.
data SubscribeToEvent = SubscribeToEvent'
  { -- | The event for which you want to receive SNS notifications.
    event :: InspectorEvent,
    -- | The ARN of the SNS topic to which the SNS notifications are sent.
    topicARN :: Lude.Text,
    -- | The ARN of the assessment template that is used during the event for which you want to receive SNS notifications.
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscribeToEvent' with the minimum fields required to make a request.
--
-- * 'event' - The event for which you want to receive SNS notifications.
-- * 'topicARN' - The ARN of the SNS topic to which the SNS notifications are sent.
-- * 'resourceARN' - The ARN of the assessment template that is used during the event for which you want to receive SNS notifications.
mkSubscribeToEvent ::
  -- | 'event'
  InspectorEvent ->
  -- | 'topicARN'
  Lude.Text ->
  -- | 'resourceARN'
  Lude.Text ->
  SubscribeToEvent
mkSubscribeToEvent pEvent_ pTopicARN_ pResourceARN_ =
  SubscribeToEvent'
    { event = pEvent_,
      topicARN = pTopicARN_,
      resourceARN = pResourceARN_
    }

-- | The event for which you want to receive SNS notifications.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steEvent :: Lens.Lens' SubscribeToEvent InspectorEvent
steEvent = Lens.lens (event :: SubscribeToEvent -> InspectorEvent) (\s a -> s {event = a} :: SubscribeToEvent)
{-# DEPRECATED steEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | The ARN of the SNS topic to which the SNS notifications are sent.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steTopicARN :: Lens.Lens' SubscribeToEvent Lude.Text
steTopicARN = Lens.lens (topicARN :: SubscribeToEvent -> Lude.Text) (\s a -> s {topicARN = a} :: SubscribeToEvent)
{-# DEPRECATED steTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The ARN of the assessment template that is used during the event for which you want to receive SNS notifications.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
steResourceARN :: Lens.Lens' SubscribeToEvent Lude.Text
steResourceARN = Lens.lens (resourceARN :: SubscribeToEvent -> Lude.Text) (\s a -> s {resourceARN = a} :: SubscribeToEvent)
{-# DEPRECATED steResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest SubscribeToEvent where
  type Rs SubscribeToEvent = SubscribeToEventResponse
  request = Req.postJSON inspectorService
  response = Res.receiveNull SubscribeToEventResponse'

instance Lude.ToHeaders SubscribeToEvent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.SubscribeToEvent" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SubscribeToEvent where
  toJSON SubscribeToEvent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("event" Lude..= event),
            Lude.Just ("topicArn" Lude..= topicARN),
            Lude.Just ("resourceArn" Lude..= resourceARN)
          ]
      )

instance Lude.ToPath SubscribeToEvent where
  toPath = Lude.const "/"

instance Lude.ToQuery SubscribeToEvent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSubscribeToEventResponse' smart constructor.
data SubscribeToEventResponse = SubscribeToEventResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscribeToEventResponse' with the minimum fields required to make a request.
mkSubscribeToEventResponse ::
  SubscribeToEventResponse
mkSubscribeToEventResponse = SubscribeToEventResponse'
