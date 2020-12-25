{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceConfigurableSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceConfigurableSettings
  ( InputDeviceConfigurableSettings (..),

    -- * Smart constructor
    mkInputDeviceConfigurableSettings,

    -- * Lenses
    idcsConfiguredInput,
    idcsMaxBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputDeviceConfiguredInput as Types
import qualified Network.AWS.Prelude as Core

-- | Configurable settings for the input device.
--
-- /See:/ 'mkInputDeviceConfigurableSettings' smart constructor.
data InputDeviceConfigurableSettings = InputDeviceConfigurableSettings'
  { -- | The input source that you want to use. If the device has a source connected to only one of its input ports, or if you don't care which source the device sends, specify Auto. If the device has sources connected to both its input ports, and you want to use a specific source, specify the source.
    configuredInput :: Core.Maybe Types.InputDeviceConfiguredInput,
    -- | The maximum bitrate in bits per second. Set a value here to throttle the bitrate of the source video.
    maxBitrate :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputDeviceConfigurableSettings' value with any optional fields omitted.
mkInputDeviceConfigurableSettings ::
  InputDeviceConfigurableSettings
mkInputDeviceConfigurableSettings =
  InputDeviceConfigurableSettings'
    { configuredInput = Core.Nothing,
      maxBitrate = Core.Nothing
    }

-- | The input source that you want to use. If the device has a source connected to only one of its input ports, or if you don't care which source the device sends, specify Auto. If the device has sources connected to both its input ports, and you want to use a specific source, specify the source.
--
-- /Note:/ Consider using 'configuredInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcsConfiguredInput :: Lens.Lens' InputDeviceConfigurableSettings (Core.Maybe Types.InputDeviceConfiguredInput)
idcsConfiguredInput = Lens.field @"configuredInput"
{-# DEPRECATED idcsConfiguredInput "Use generic-lens or generic-optics with 'configuredInput' instead." #-}

-- | The maximum bitrate in bits per second. Set a value here to throttle the bitrate of the source video.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcsMaxBitrate :: Lens.Lens' InputDeviceConfigurableSettings (Core.Maybe Core.Int)
idcsMaxBitrate = Lens.field @"maxBitrate"
{-# DEPRECATED idcsMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

instance Core.FromJSON InputDeviceConfigurableSettings where
  toJSON InputDeviceConfigurableSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("configuredInput" Core..=) Core.<$> configuredInput,
            ("maxBitrate" Core..=) Core.<$> maxBitrate
          ]
      )
