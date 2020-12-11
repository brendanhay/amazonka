-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceLogEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceLogEvent
  ( ContainerServiceLogEvent (..),

    -- * Smart constructor
    mkContainerServiceLogEvent,

    -- * Lenses
    csleCreatedAt,
    csleMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the log events of a container of an Amazon Lightsail container service.
--
-- /See:/ 'mkContainerServiceLogEvent' smart constructor.
data ContainerServiceLogEvent = ContainerServiceLogEvent'
  { createdAt ::
      Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'ContainerServiceLogEvent' with the minimum fields required to make a request.
--
-- * 'createdAt' - The timestamp when the container service log event was created.
-- * 'message' - The message of the container service log event.
mkContainerServiceLogEvent ::
  ContainerServiceLogEvent
mkContainerServiceLogEvent =
  ContainerServiceLogEvent'
    { createdAt = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The timestamp when the container service log event was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csleCreatedAt :: Lens.Lens' ContainerServiceLogEvent (Lude.Maybe Lude.Timestamp)
csleCreatedAt = Lens.lens (createdAt :: ContainerServiceLogEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: ContainerServiceLogEvent)
{-# DEPRECATED csleCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The message of the container service log event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csleMessage :: Lens.Lens' ContainerServiceLogEvent (Lude.Maybe Lude.Text)
csleMessage = Lens.lens (message :: ContainerServiceLogEvent -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ContainerServiceLogEvent)
{-# DEPRECATED csleMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ContainerServiceLogEvent where
  parseJSON =
    Lude.withObject
      "ContainerServiceLogEvent"
      ( \x ->
          ContainerServiceLogEvent'
            Lude.<$> (x Lude..:? "createdAt") Lude.<*> (x Lude..:? "message")
      )
