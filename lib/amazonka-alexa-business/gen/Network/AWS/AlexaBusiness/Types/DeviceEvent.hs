{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.DeviceEvent
  ( DeviceEvent (..)
  -- * Smart constructor
  , mkDeviceEvent
  -- * Lenses
  , deTimestamp
  , deType
  , deValue
  ) where

import qualified Network.AWS.AlexaBusiness.Types.DeviceEventType as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceEventValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The list of device events.
--
-- /See:/ 'mkDeviceEvent' smart constructor.
data DeviceEvent = DeviceEvent'
  { timestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The time (in epoch) when the event occurred. 
  , type' :: Core.Maybe Types.DeviceEventType
    -- ^ The type of device event.
  , value :: Core.Maybe Types.DeviceEventValue
    -- ^ The value of the event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeviceEvent' value with any optional fields omitted.
mkDeviceEvent
    :: DeviceEvent
mkDeviceEvent
  = DeviceEvent'{timestamp = Core.Nothing, type' = Core.Nothing,
                 value = Core.Nothing}

-- | The time (in epoch) when the event occurred. 
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deTimestamp :: Lens.Lens' DeviceEvent (Core.Maybe Core.NominalDiffTime)
deTimestamp = Lens.field @"timestamp"
{-# INLINEABLE deTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The type of device event.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deType :: Lens.Lens' DeviceEvent (Core.Maybe Types.DeviceEventType)
deType = Lens.field @"type'"
{-# INLINEABLE deType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The value of the event.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deValue :: Lens.Lens' DeviceEvent (Core.Maybe Types.DeviceEventValue)
deValue = Lens.field @"value"
{-# INLINEABLE deValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON DeviceEvent where
        parseJSON
          = Core.withObject "DeviceEvent" Core.$
              \ x ->
                DeviceEvent' Core.<$>
                  (x Core..:? "Timestamp") Core.<*> x Core..:? "Type" Core.<*>
                    x Core..:? "Value"
