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
import Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
import qualified Network.AWS.Prelude as Lude

-- | Configurable settings for the input device.
--
-- /See:/ 'mkInputDeviceConfigurableSettings' smart constructor.
data InputDeviceConfigurableSettings = InputDeviceConfigurableSettings'
  { configuredInput ::
      Lude.Maybe
        InputDeviceConfiguredInput,
    maxBitrate ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputDeviceConfigurableSettings' with the minimum fields required to make a request.
--
-- * 'configuredInput' - The input source that you want to use. If the device has a source connected to only one of its input ports, or if you don't care which source the device sends, specify Auto. If the device has sources connected to both its input ports, and you want to use a specific source, specify the source.
-- * 'maxBitrate' - The maximum bitrate in bits per second. Set a value here to throttle the bitrate of the source video.
mkInputDeviceConfigurableSettings ::
  InputDeviceConfigurableSettings
mkInputDeviceConfigurableSettings =
  InputDeviceConfigurableSettings'
    { configuredInput = Lude.Nothing,
      maxBitrate = Lude.Nothing
    }

-- | The input source that you want to use. If the device has a source connected to only one of its input ports, or if you don't care which source the device sends, specify Auto. If the device has sources connected to both its input ports, and you want to use a specific source, specify the source.
--
-- /Note:/ Consider using 'configuredInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcsConfiguredInput :: Lens.Lens' InputDeviceConfigurableSettings (Lude.Maybe InputDeviceConfiguredInput)
idcsConfiguredInput = Lens.lens (configuredInput :: InputDeviceConfigurableSettings -> Lude.Maybe InputDeviceConfiguredInput) (\s a -> s {configuredInput = a} :: InputDeviceConfigurableSettings)
{-# DEPRECATED idcsConfiguredInput "Use generic-lens or generic-optics with 'configuredInput' instead." #-}

-- | The maximum bitrate in bits per second. Set a value here to throttle the bitrate of the source video.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcsMaxBitrate :: Lens.Lens' InputDeviceConfigurableSettings (Lude.Maybe Lude.Int)
idcsMaxBitrate = Lens.lens (maxBitrate :: InputDeviceConfigurableSettings -> Lude.Maybe Lude.Int) (\s a -> s {maxBitrate = a} :: InputDeviceConfigurableSettings)
{-# DEPRECATED idcsMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

instance Lude.ToJSON InputDeviceConfigurableSettings where
  toJSON InputDeviceConfigurableSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("configuredInput" Lude..=) Lude.<$> configuredInput,
            ("maxBitrate" Lude..=) Lude.<$> maxBitrate
          ]
      )
