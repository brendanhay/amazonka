{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceHdSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputDeviceHdSettings
  ( InputDeviceHdSettings (..)
  -- * Smart constructor
  , mkInputDeviceHdSettings
  -- * Lenses
  , idhsActiveInput
  , idhsConfiguredInput
  , idhsDeviceState
  , idhsFramerate
  , idhsHeight
  , idhsMaxBitrate
  , idhsScanType
  , idhsWidth
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputDeviceActiveInput as Types
import qualified Network.AWS.MediaLive.Types.InputDeviceConfiguredInput as Types
import qualified Network.AWS.MediaLive.Types.InputDeviceScanType as Types
import qualified Network.AWS.MediaLive.Types.InputDeviceState as Types
import qualified Network.AWS.Prelude as Core

-- | Settings that describe the active source from the input device, and the video characteristics of that source.
--
-- /See:/ 'mkInputDeviceHdSettings' smart constructor.
data InputDeviceHdSettings = InputDeviceHdSettings'
  { activeInput :: Core.Maybe Types.InputDeviceActiveInput
    -- ^ If you specified Auto as the configured input, specifies which of the sources is currently active (SDI or HDMI).
  , configuredInput :: Core.Maybe Types.InputDeviceConfiguredInput
    -- ^ The source at the input device that is currently active. You can specify this source.
  , deviceState :: Core.Maybe Types.InputDeviceState
    -- ^ The state of the input device.
  , framerate :: Core.Maybe Core.Double
    -- ^ The frame rate of the video source.
  , height :: Core.Maybe Core.Int
    -- ^ The height of the video source, in pixels.
  , maxBitrate :: Core.Maybe Core.Int
    -- ^ The current maximum bitrate for ingesting this source, in bits per second. You can specify this maximum.
  , scanType :: Core.Maybe Types.InputDeviceScanType
    -- ^ The scan type of the video source.
  , width :: Core.Maybe Core.Int
    -- ^ The width of the video source, in pixels.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputDeviceHdSettings' value with any optional fields omitted.
mkInputDeviceHdSettings
    :: InputDeviceHdSettings
mkInputDeviceHdSettings
  = InputDeviceHdSettings'{activeInput = Core.Nothing,
                           configuredInput = Core.Nothing, deviceState = Core.Nothing,
                           framerate = Core.Nothing, height = Core.Nothing,
                           maxBitrate = Core.Nothing, scanType = Core.Nothing,
                           width = Core.Nothing}

-- | If you specified Auto as the configured input, specifies which of the sources is currently active (SDI or HDMI).
--
-- /Note:/ Consider using 'activeInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsActiveInput :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Types.InputDeviceActiveInput)
idhsActiveInput = Lens.field @"activeInput"
{-# INLINEABLE idhsActiveInput #-}
{-# DEPRECATED activeInput "Use generic-lens or generic-optics with 'activeInput' instead"  #-}

-- | The source at the input device that is currently active. You can specify this source.
--
-- /Note:/ Consider using 'configuredInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsConfiguredInput :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Types.InputDeviceConfiguredInput)
idhsConfiguredInput = Lens.field @"configuredInput"
{-# INLINEABLE idhsConfiguredInput #-}
{-# DEPRECATED configuredInput "Use generic-lens or generic-optics with 'configuredInput' instead"  #-}

-- | The state of the input device.
--
-- /Note:/ Consider using 'deviceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsDeviceState :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Types.InputDeviceState)
idhsDeviceState = Lens.field @"deviceState"
{-# INLINEABLE idhsDeviceState #-}
{-# DEPRECATED deviceState "Use generic-lens or generic-optics with 'deviceState' instead"  #-}

-- | The frame rate of the video source.
--
-- /Note:/ Consider using 'framerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsFramerate :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Core.Double)
idhsFramerate = Lens.field @"framerate"
{-# INLINEABLE idhsFramerate #-}
{-# DEPRECATED framerate "Use generic-lens or generic-optics with 'framerate' instead"  #-}

-- | The height of the video source, in pixels.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsHeight :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Core.Int)
idhsHeight = Lens.field @"height"
{-# INLINEABLE idhsHeight #-}
{-# DEPRECATED height "Use generic-lens or generic-optics with 'height' instead"  #-}

-- | The current maximum bitrate for ingesting this source, in bits per second. You can specify this maximum.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsMaxBitrate :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Core.Int)
idhsMaxBitrate = Lens.field @"maxBitrate"
{-# INLINEABLE idhsMaxBitrate #-}
{-# DEPRECATED maxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead"  #-}

-- | The scan type of the video source.
--
-- /Note:/ Consider using 'scanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsScanType :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Types.InputDeviceScanType)
idhsScanType = Lens.field @"scanType"
{-# INLINEABLE idhsScanType #-}
{-# DEPRECATED scanType "Use generic-lens or generic-optics with 'scanType' instead"  #-}

-- | The width of the video source, in pixels.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsWidth :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Core.Int)
idhsWidth = Lens.field @"width"
{-# INLINEABLE idhsWidth #-}
{-# DEPRECATED width "Use generic-lens or generic-optics with 'width' instead"  #-}

instance Core.FromJSON InputDeviceHdSettings where
        parseJSON
          = Core.withObject "InputDeviceHdSettings" Core.$
              \ x ->
                InputDeviceHdSettings' Core.<$>
                  (x Core..:? "activeInput") Core.<*> x Core..:? "configuredInput"
                    Core.<*> x Core..:? "deviceState"
                    Core.<*> x Core..:? "framerate"
                    Core.<*> x Core..:? "height"
                    Core.<*> x Core..:? "maxBitrate"
                    Core.<*> x Core..:? "scanType"
                    Core.<*> x Core..:? "width"
