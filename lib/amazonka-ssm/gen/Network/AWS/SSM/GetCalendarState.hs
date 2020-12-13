{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetCalendarState (..),
    mkGetCalendarState,

    -- ** Request lenses
    gcsAtTime,
    gcsCalendarNames,

    -- * Destructuring the response
    GetCalendarStateResponse (..),
    mkGetCalendarStateResponse,

    -- ** Response lenses
    grsState,
    grsNextTransitionTime,
    grsAtTime,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetCalendarState' smart constructor.
data GetCalendarState = GetCalendarState'
  { -- | (Optional) The specific time for which you want to get calendar state information, in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> format. If you do not add @AtTime@ , the current time is assumed.
    atTime :: Lude.Maybe Lude.Text,
    -- | The names or Amazon Resource Names (ARNs) of the Systems Manager documents that represent the calendar entries for which you want to get the state.
    calendarNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCalendarState' with the minimum fields required to make a request.
--
-- * 'atTime' - (Optional) The specific time for which you want to get calendar state information, in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> format. If you do not add @AtTime@ , the current time is assumed.
-- * 'calendarNames' - The names or Amazon Resource Names (ARNs) of the Systems Manager documents that represent the calendar entries for which you want to get the state.
mkGetCalendarState ::
  GetCalendarState
mkGetCalendarState =
  GetCalendarState'
    { atTime = Lude.Nothing,
      calendarNames = Lude.mempty
    }

-- | (Optional) The specific time for which you want to get calendar state information, in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> format. If you do not add @AtTime@ , the current time is assumed.
--
-- /Note:/ Consider using 'atTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsAtTime :: Lens.Lens' GetCalendarState (Lude.Maybe Lude.Text)
gcsAtTime = Lens.lens (atTime :: GetCalendarState -> Lude.Maybe Lude.Text) (\s a -> s {atTime = a} :: GetCalendarState)
{-# DEPRECATED gcsAtTime "Use generic-lens or generic-optics with 'atTime' instead." #-}

-- | The names or Amazon Resource Names (ARNs) of the Systems Manager documents that represent the calendar entries for which you want to get the state.
--
-- /Note:/ Consider using 'calendarNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsCalendarNames :: Lens.Lens' GetCalendarState [Lude.Text]
gcsCalendarNames = Lens.lens (calendarNames :: GetCalendarState -> [Lude.Text]) (\s a -> s {calendarNames = a} :: GetCalendarState)
{-# DEPRECATED gcsCalendarNames "Use generic-lens or generic-optics with 'calendarNames' instead." #-}

instance Lude.AWSRequest GetCalendarState where
  type Rs GetCalendarState = GetCalendarStateResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCalendarStateResponse'
            Lude.<$> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "NextTransitionTime")
            Lude.<*> (x Lude..?> "AtTime")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCalendarState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetCalendarState" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCalendarState where
  toJSON GetCalendarState' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AtTime" Lude..=) Lude.<$> atTime,
            Lude.Just ("CalendarNames" Lude..= calendarNames)
          ]
      )

instance Lude.ToPath GetCalendarState where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCalendarState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCalendarStateResponse' smart constructor.
data GetCalendarStateResponse = GetCalendarStateResponse'
  { -- | The state of the calendar. An @OPEN@ calendar indicates that actions are allowed to proceed, and a @CLOSED@ calendar indicates that actions are not allowed to proceed.
    state :: Lude.Maybe CalendarState,
    -- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that the calendar state will change. If the current calendar state is @OPEN@ , @NextTransitionTime@ indicates when the calendar state changes to @CLOSED@ , and vice-versa.
    nextTransitionTime :: Lude.Maybe Lude.Text,
    -- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that you specified in your command. If you did not specify a time, @GetCalendarState@ uses the current time.
    atTime :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCalendarStateResponse' with the minimum fields required to make a request.
--
-- * 'state' - The state of the calendar. An @OPEN@ calendar indicates that actions are allowed to proceed, and a @CLOSED@ calendar indicates that actions are not allowed to proceed.
-- * 'nextTransitionTime' - The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that the calendar state will change. If the current calendar state is @OPEN@ , @NextTransitionTime@ indicates when the calendar state changes to @CLOSED@ , and vice-versa.
-- * 'atTime' - The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that you specified in your command. If you did not specify a time, @GetCalendarState@ uses the current time.
-- * 'responseStatus' - The response status code.
mkGetCalendarStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCalendarStateResponse
mkGetCalendarStateResponse pResponseStatus_ =
  GetCalendarStateResponse'
    { state = Lude.Nothing,
      nextTransitionTime = Lude.Nothing,
      atTime = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state of the calendar. An @OPEN@ calendar indicates that actions are allowed to proceed, and a @CLOSED@ calendar indicates that actions are not allowed to proceed.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsState :: Lens.Lens' GetCalendarStateResponse (Lude.Maybe CalendarState)
grsState = Lens.lens (state :: GetCalendarStateResponse -> Lude.Maybe CalendarState) (\s a -> s {state = a} :: GetCalendarStateResponse)
{-# DEPRECATED grsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that the calendar state will change. If the current calendar state is @OPEN@ , @NextTransitionTime@ indicates when the calendar state changes to @CLOSED@ , and vice-versa.
--
-- /Note:/ Consider using 'nextTransitionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsNextTransitionTime :: Lens.Lens' GetCalendarStateResponse (Lude.Maybe Lude.Text)
grsNextTransitionTime = Lens.lens (nextTransitionTime :: GetCalendarStateResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextTransitionTime = a} :: GetCalendarStateResponse)
{-# DEPRECATED grsNextTransitionTime "Use generic-lens or generic-optics with 'nextTransitionTime' instead." #-}

-- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601> string, that you specified in your command. If you did not specify a time, @GetCalendarState@ uses the current time.
--
-- /Note:/ Consider using 'atTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsAtTime :: Lens.Lens' GetCalendarStateResponse (Lude.Maybe Lude.Text)
grsAtTime = Lens.lens (atTime :: GetCalendarStateResponse -> Lude.Maybe Lude.Text) (\s a -> s {atTime = a} :: GetCalendarStateResponse)
{-# DEPRECATED grsAtTime "Use generic-lens or generic-optics with 'atTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetCalendarStateResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetCalendarStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCalendarStateResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
