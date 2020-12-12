{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventItemResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventItemResponse
  ( EventItemResponse (..),

    -- * Smart constructor
    mkEventItemResponse,

    -- * Lenses
    eMessage,
    eStatusCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the status code and message that result from processing an event.
--
-- /See:/ 'mkEventItemResponse' smart constructor.
data EventItemResponse = EventItemResponse'
  { message ::
      Lude.Maybe Lude.Text,
    statusCode :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventItemResponse' with the minimum fields required to make a request.
--
-- * 'message' - A custom message that's returned in the response as a result of processing the event.
-- * 'statusCode' - The status code that's returned in the response as a result of processing the event. Possible values are: 202, for events that were accepted; and, 400, for events that weren't valid.
mkEventItemResponse ::
  EventItemResponse
mkEventItemResponse =
  EventItemResponse'
    { message = Lude.Nothing,
      statusCode = Lude.Nothing
    }

-- | A custom message that's returned in the response as a result of processing the event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' EventItemResponse (Lude.Maybe Lude.Text)
eMessage = Lens.lens (message :: EventItemResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: EventItemResponse)
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The status code that's returned in the response as a result of processing the event. Possible values are: 202, for events that were accepted; and, 400, for events that weren't valid.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStatusCode :: Lens.Lens' EventItemResponse (Lude.Maybe Lude.Int)
eStatusCode = Lens.lens (statusCode :: EventItemResponse -> Lude.Maybe Lude.Int) (\s a -> s {statusCode = a} :: EventItemResponse)
{-# DEPRECATED eStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON EventItemResponse where
  parseJSON =
    Lude.withObject
      "EventItemResponse"
      ( \x ->
          EventItemResponse'
            Lude.<$> (x Lude..:? "Message") Lude.<*> (x Lude..:? "StatusCode")
      )
