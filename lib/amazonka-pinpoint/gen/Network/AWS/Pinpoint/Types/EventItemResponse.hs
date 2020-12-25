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
import qualified Network.AWS.Prelude as Core

-- | Provides the status code and message that result from processing an event.
--
-- /See:/ 'mkEventItemResponse' smart constructor.
data EventItemResponse = EventItemResponse'
  { -- | A custom message that's returned in the response as a result of processing the event.
    message :: Core.Maybe Core.Text,
    -- | The status code that's returned in the response as a result of processing the event. Possible values are: 202, for events that were accepted; and, 400, for events that weren't valid.
    statusCode :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventItemResponse' value with any optional fields omitted.
mkEventItemResponse ::
  EventItemResponse
mkEventItemResponse =
  EventItemResponse'
    { message = Core.Nothing,
      statusCode = Core.Nothing
    }

-- | A custom message that's returned in the response as a result of processing the event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' EventItemResponse (Core.Maybe Core.Text)
eMessage = Lens.field @"message"
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The status code that's returned in the response as a result of processing the event. Possible values are: 202, for events that were accepted; and, 400, for events that weren't valid.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStatusCode :: Lens.Lens' EventItemResponse (Core.Maybe Core.Int)
eStatusCode = Lens.field @"statusCode"
{-# DEPRECATED eStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Core.FromJSON EventItemResponse where
  parseJSON =
    Core.withObject "EventItemResponse" Core.$
      \x ->
        EventItemResponse'
          Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "StatusCode")
