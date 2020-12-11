{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    teprsResult,
    teprsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTestEventPattern' smart constructor.
data TestEventPattern = TestEventPattern'
  { eventPattern ::
      Lude.Text,
    event :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestEventPattern' with the minimum fields required to make a request.
--
-- * 'event' - The event, in JSON format, to test against the event pattern.
-- * 'eventPattern' - The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
mkTestEventPattern ::
  -- | 'eventPattern'
  Lude.Text ->
  -- | 'event'
  Lude.Text ->
  TestEventPattern
mkTestEventPattern pEventPattern_ pEvent_ =
  TestEventPattern' {eventPattern = pEventPattern_, event = pEvent_}

-- | The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tepEventPattern :: Lens.Lens' TestEventPattern Lude.Text
tepEventPattern = Lens.lens (eventPattern :: TestEventPattern -> Lude.Text) (\s a -> s {eventPattern = a} :: TestEventPattern)
{-# DEPRECATED tepEventPattern "Use generic-lens or generic-optics with 'eventPattern' instead." #-}

-- | The event, in JSON format, to test against the event pattern.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tepEvent :: Lens.Lens' TestEventPattern Lude.Text
tepEvent = Lens.lens (event :: TestEventPattern -> Lude.Text) (\s a -> s {event = a} :: TestEventPattern)
{-# DEPRECATED tepEvent "Use generic-lens or generic-optics with 'event' instead." #-}

instance Lude.AWSRequest TestEventPattern where
  type Rs TestEventPattern = TestEventPatternResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          TestEventPatternResponse'
            Lude.<$> (x Lude..?> "Result") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestEventPattern where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.TestEventPattern" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TestEventPattern where
  toJSON TestEventPattern' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EventPattern" Lude..= eventPattern),
            Lude.Just ("Event" Lude..= event)
          ]
      )

instance Lude.ToPath TestEventPattern where
  toPath = Lude.const "/"

instance Lude.ToQuery TestEventPattern where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTestEventPatternResponse' smart constructor.
data TestEventPatternResponse = TestEventPatternResponse'
  { result ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestEventPatternResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'result' - Indicates whether the event matches the event pattern.
mkTestEventPatternResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TestEventPatternResponse
mkTestEventPatternResponse pResponseStatus_ =
  TestEventPatternResponse'
    { result = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether the event matches the event pattern.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teprsResult :: Lens.Lens' TestEventPatternResponse (Lude.Maybe Lude.Bool)
teprsResult = Lens.lens (result :: TestEventPatternResponse -> Lude.Maybe Lude.Bool) (\s a -> s {result = a} :: TestEventPatternResponse)
{-# DEPRECATED teprsResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teprsResponseStatus :: Lens.Lens' TestEventPatternResponse Lude.Int
teprsResponseStatus = Lens.lens (responseStatus :: TestEventPatternResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestEventPatternResponse)
{-# DEPRECATED teprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
