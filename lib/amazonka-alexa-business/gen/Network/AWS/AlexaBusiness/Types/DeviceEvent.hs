-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceEvent
  ( DeviceEvent (..),

    -- * Smart constructor
    mkDeviceEvent,

    -- * Lenses
    deValue,
    deType,
    deTimestamp,
  )
where

import Network.AWS.AlexaBusiness.Types.DeviceEventType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The list of device events.
--
-- /See:/ 'mkDeviceEvent' smart constructor.
data DeviceEvent = DeviceEvent'
  { value :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe DeviceEventType,
    timestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceEvent' with the minimum fields required to make a request.
--
-- * 'timestamp' - The time (in epoch) when the event occurred.
-- * 'type'' - The type of device event.
-- * 'value' - The value of the event.
mkDeviceEvent ::
  DeviceEvent
mkDeviceEvent =
  DeviceEvent'
    { value = Lude.Nothing,
      type' = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | The value of the event.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deValue :: Lens.Lens' DeviceEvent (Lude.Maybe Lude.Text)
deValue = Lens.lens (value :: DeviceEvent -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: DeviceEvent)
{-# DEPRECATED deValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of device event.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deType :: Lens.Lens' DeviceEvent (Lude.Maybe DeviceEventType)
deType = Lens.lens (type' :: DeviceEvent -> Lude.Maybe DeviceEventType) (\s a -> s {type' = a} :: DeviceEvent)
{-# DEPRECATED deType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The time (in epoch) when the event occurred.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deTimestamp :: Lens.Lens' DeviceEvent (Lude.Maybe Lude.Timestamp)
deTimestamp = Lens.lens (timestamp :: DeviceEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: DeviceEvent)
{-# DEPRECATED deTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON DeviceEvent where
  parseJSON =
    Lude.withObject
      "DeviceEvent"
      ( \x ->
          DeviceEvent'
            Lude.<$> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Timestamp")
      )
