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
    ufeEvent,
    ufeTopicARN,
    ufeResourceARN,

    -- * Destructuring the response
    UnsubscribeFromEventResponse (..),
    mkUnsubscribeFromEventResponse,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUnsubscribeFromEvent' smart constructor.
data UnsubscribeFromEvent = UnsubscribeFromEvent'
  { -- | The event for which you want to stop receiving SNS notifications.
    event :: InspectorEvent,
    -- | The ARN of the SNS topic to which SNS notifications are sent.
    topicARN :: Lude.Text,
    -- | The ARN of the assessment template that is used during the event for which you want to stop receiving SNS notifications.
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnsubscribeFromEvent' with the minimum fields required to make a request.
--
-- * 'event' - The event for which you want to stop receiving SNS notifications.
-- * 'topicARN' - The ARN of the SNS topic to which SNS notifications are sent.
-- * 'resourceARN' - The ARN of the assessment template that is used during the event for which you want to stop receiving SNS notifications.
mkUnsubscribeFromEvent ::
  -- | 'event'
  InspectorEvent ->
  -- | 'topicARN'
  Lude.Text ->
  -- | 'resourceARN'
  Lude.Text ->
  UnsubscribeFromEvent
mkUnsubscribeFromEvent pEvent_ pTopicARN_ pResourceARN_ =
  UnsubscribeFromEvent'
    { event = pEvent_,
      topicARN = pTopicARN_,
      resourceARN = pResourceARN_
    }

-- | The event for which you want to stop receiving SNS notifications.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeEvent :: Lens.Lens' UnsubscribeFromEvent InspectorEvent
ufeEvent = Lens.lens (event :: UnsubscribeFromEvent -> InspectorEvent) (\s a -> s {event = a} :: UnsubscribeFromEvent)
{-# DEPRECATED ufeEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | The ARN of the SNS topic to which SNS notifications are sent.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeTopicARN :: Lens.Lens' UnsubscribeFromEvent Lude.Text
ufeTopicARN = Lens.lens (topicARN :: UnsubscribeFromEvent -> Lude.Text) (\s a -> s {topicARN = a} :: UnsubscribeFromEvent)
{-# DEPRECATED ufeTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The ARN of the assessment template that is used during the event for which you want to stop receiving SNS notifications.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeResourceARN :: Lens.Lens' UnsubscribeFromEvent Lude.Text
ufeResourceARN = Lens.lens (resourceARN :: UnsubscribeFromEvent -> Lude.Text) (\s a -> s {resourceARN = a} :: UnsubscribeFromEvent)
{-# DEPRECATED ufeResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest UnsubscribeFromEvent where
  type Rs UnsubscribeFromEvent = UnsubscribeFromEventResponse
  request = Req.postJSON inspectorService
  response = Res.receiveNull UnsubscribeFromEventResponse'

instance Lude.ToHeaders UnsubscribeFromEvent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.UnsubscribeFromEvent" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UnsubscribeFromEvent where
  toJSON UnsubscribeFromEvent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("event" Lude..= event),
            Lude.Just ("topicArn" Lude..= topicARN),
            Lude.Just ("resourceArn" Lude..= resourceARN)
          ]
      )

instance Lude.ToPath UnsubscribeFromEvent where
  toPath = Lude.const "/"

instance Lude.ToQuery UnsubscribeFromEvent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUnsubscribeFromEventResponse' smart constructor.
data UnsubscribeFromEventResponse = UnsubscribeFromEventResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnsubscribeFromEventResponse' with the minimum fields required to make a request.
mkUnsubscribeFromEventResponse ::
  UnsubscribeFromEventResponse
mkUnsubscribeFromEventResponse = UnsubscribeFromEventResponse'
