-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ServiceEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ServiceEvent
  ( ServiceEvent (..),

    -- * Smart constructor
    mkServiceEvent,

    -- * Lenses
    seCreatedAt,
    seId,
    seMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on an event associated with a service.
--
-- /See:/ 'mkServiceEvent' smart constructor.
data ServiceEvent = ServiceEvent'
  { createdAt ::
      Lude.Maybe Lude.Timestamp,
    id :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ServiceEvent' with the minimum fields required to make a request.
--
-- * 'createdAt' - The Unix timestamp for when the event was triggered.
-- * 'id' - The ID string of the event.
-- * 'message' - The event message.
mkServiceEvent ::
  ServiceEvent
mkServiceEvent =
  ServiceEvent'
    { createdAt = Lude.Nothing,
      id = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The Unix timestamp for when the event was triggered.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seCreatedAt :: Lens.Lens' ServiceEvent (Lude.Maybe Lude.Timestamp)
seCreatedAt = Lens.lens (createdAt :: ServiceEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: ServiceEvent)
{-# DEPRECATED seCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ID string of the event.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seId :: Lens.Lens' ServiceEvent (Lude.Maybe Lude.Text)
seId = Lens.lens (id :: ServiceEvent -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ServiceEvent)
{-# DEPRECATED seId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The event message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMessage :: Lens.Lens' ServiceEvent (Lude.Maybe Lude.Text)
seMessage = Lens.lens (message :: ServiceEvent -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ServiceEvent)
{-# DEPRECATED seMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ServiceEvent where
  parseJSON =
    Lude.withObject
      "ServiceEvent"
      ( \x ->
          ServiceEvent'
            Lude.<$> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "message")
      )
