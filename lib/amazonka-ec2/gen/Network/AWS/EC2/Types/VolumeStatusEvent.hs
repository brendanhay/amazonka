-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusEvent
  ( VolumeStatusEvent (..),

    -- * Smart constructor
    mkVolumeStatusEvent,

    -- * Lenses
    vseInstanceId,
    vseNotBefore,
    vseEventType,
    vseDescription,
    vseNotAfter,
    vseEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a volume status event.
--
-- /See:/ 'mkVolumeStatusEvent' smart constructor.
data VolumeStatusEvent = VolumeStatusEvent'
  { instanceId ::
      Lude.Maybe Lude.Text,
    notBefore :: Lude.Maybe Lude.ISO8601,
    eventType :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    notAfter :: Lude.Maybe Lude.ISO8601,
    eventId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeStatusEvent' with the minimum fields required to make a request.
--
-- * 'description' - A description of the event.
-- * 'eventId' - The ID of this event.
-- * 'eventType' - The type of this event.
-- * 'instanceId' - The ID of the instance associated with the event.
-- * 'notAfter' - The latest end time of the event.
-- * 'notBefore' - The earliest start time of the event.
mkVolumeStatusEvent ::
  VolumeStatusEvent
mkVolumeStatusEvent =
  VolumeStatusEvent'
    { instanceId = Lude.Nothing,
      notBefore = Lude.Nothing,
      eventType = Lude.Nothing,
      description = Lude.Nothing,
      notAfter = Lude.Nothing,
      eventId = Lude.Nothing
    }

-- | The ID of the instance associated with the event.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseInstanceId :: Lens.Lens' VolumeStatusEvent (Lude.Maybe Lude.Text)
vseInstanceId = Lens.lens (instanceId :: VolumeStatusEvent -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: VolumeStatusEvent)
{-# DEPRECATED vseInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The earliest start time of the event.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseNotBefore :: Lens.Lens' VolumeStatusEvent (Lude.Maybe Lude.ISO8601)
vseNotBefore = Lens.lens (notBefore :: VolumeStatusEvent -> Lude.Maybe Lude.ISO8601) (\s a -> s {notBefore = a} :: VolumeStatusEvent)
{-# DEPRECATED vseNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

-- | The type of this event.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseEventType :: Lens.Lens' VolumeStatusEvent (Lude.Maybe Lude.Text)
vseEventType = Lens.lens (eventType :: VolumeStatusEvent -> Lude.Maybe Lude.Text) (\s a -> s {eventType = a} :: VolumeStatusEvent)
{-# DEPRECATED vseEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | A description of the event.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseDescription :: Lens.Lens' VolumeStatusEvent (Lude.Maybe Lude.Text)
vseDescription = Lens.lens (description :: VolumeStatusEvent -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: VolumeStatusEvent)
{-# DEPRECATED vseDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The latest end time of the event.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseNotAfter :: Lens.Lens' VolumeStatusEvent (Lude.Maybe Lude.ISO8601)
vseNotAfter = Lens.lens (notAfter :: VolumeStatusEvent -> Lude.Maybe Lude.ISO8601) (\s a -> s {notAfter = a} :: VolumeStatusEvent)
{-# DEPRECATED vseNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

-- | The ID of this event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vseEventId :: Lens.Lens' VolumeStatusEvent (Lude.Maybe Lude.Text)
vseEventId = Lens.lens (eventId :: VolumeStatusEvent -> Lude.Maybe Lude.Text) (\s a -> s {eventId = a} :: VolumeStatusEvent)
{-# DEPRECATED vseEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.FromXML VolumeStatusEvent where
  parseXML x =
    VolumeStatusEvent'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "notBefore")
      Lude.<*> (x Lude..@? "eventType")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "notAfter")
      Lude.<*> (x Lude..@? "eventId")
