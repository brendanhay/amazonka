{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LogEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LogEvent
  ( LogEvent (..),

    -- * Smart constructor
    mkLogEvent,

    -- * Lenses
    leCreatedAt,
    leMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a database log event.
--
-- /See:/ 'mkLogEvent' smart constructor.
data LogEvent = LogEvent'
  { createdAt :: Lude.Maybe Lude.Timestamp,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogEvent' with the minimum fields required to make a request.
--
-- * 'createdAt' - The timestamp when the database log event was created.
-- * 'message' - The message of the database log event.
mkLogEvent ::
  LogEvent
mkLogEvent =
  LogEvent' {createdAt = Lude.Nothing, message = Lude.Nothing}

-- | The timestamp when the database log event was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCreatedAt :: Lens.Lens' LogEvent (Lude.Maybe Lude.Timestamp)
leCreatedAt = Lens.lens (createdAt :: LogEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: LogEvent)
{-# DEPRECATED leCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The message of the database log event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMessage :: Lens.Lens' LogEvent (Lude.Maybe Lude.Text)
leMessage = Lens.lens (message :: LogEvent -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: LogEvent)
{-# DEPRECATED leMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON LogEvent where
  parseJSON =
    Lude.withObject
      "LogEvent"
      ( \x ->
          LogEvent'
            Lude.<$> (x Lude..:? "createdAt") Lude.<*> (x Lude..:? "message")
      )
