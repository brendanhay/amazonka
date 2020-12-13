{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceHdSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceHdSettings
  ( InputDeviceHdSettings (..),

    -- * Smart constructor
    mkInputDeviceHdSettings,

    -- * Lenses
    idhsFramerate,
    idhsScanType,
    idhsDeviceState,
    idhsHeight,
    idhsActiveInput,
    idhsWidth,
    idhsConfiguredInput,
    idhsMaxBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDeviceActiveInput
import Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
import Network.AWS.MediaLive.Types.InputDeviceScanType
import Network.AWS.MediaLive.Types.InputDeviceState
import qualified Network.AWS.Prelude as Lude

-- | Settings that describe the active source from the input device, and the video characteristics of that source.
--
-- /See:/ 'mkInputDeviceHdSettings' smart constructor.
data InputDeviceHdSettings = InputDeviceHdSettings'
  { -- | The frame rate of the video source.
    framerate :: Lude.Maybe Lude.Double,
    -- | The scan type of the video source.
    scanType :: Lude.Maybe InputDeviceScanType,
    -- | The state of the input device.
    deviceState :: Lude.Maybe InputDeviceState,
    -- | The height of the video source, in pixels.
    height :: Lude.Maybe Lude.Int,
    -- | If you specified Auto as the configured input, specifies which of the sources is currently active (SDI or HDMI).
    activeInput :: Lude.Maybe InputDeviceActiveInput,
    -- | The width of the video source, in pixels.
    width :: Lude.Maybe Lude.Int,
    -- | The source at the input device that is currently active. You can specify this source.
    configuredInput :: Lude.Maybe InputDeviceConfiguredInput,
    -- | The current maximum bitrate for ingesting this source, in bits per second. You can specify this maximum.
    maxBitrate :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputDeviceHdSettings' with the minimum fields required to make a request.
--
-- * 'framerate' - The frame rate of the video source.
-- * 'scanType' - The scan type of the video source.
-- * 'deviceState' - The state of the input device.
-- * 'height' - The height of the video source, in pixels.
-- * 'activeInput' - If you specified Auto as the configured input, specifies which of the sources is currently active (SDI or HDMI).
-- * 'width' - The width of the video source, in pixels.
-- * 'configuredInput' - The source at the input device that is currently active. You can specify this source.
-- * 'maxBitrate' - The current maximum bitrate for ingesting this source, in bits per second. You can specify this maximum.
mkInputDeviceHdSettings ::
  InputDeviceHdSettings
mkInputDeviceHdSettings =
  InputDeviceHdSettings'
    { framerate = Lude.Nothing,
      scanType = Lude.Nothing,
      deviceState = Lude.Nothing,
      height = Lude.Nothing,
      activeInput = Lude.Nothing,
      width = Lude.Nothing,
      configuredInput = Lude.Nothing,
      maxBitrate = Lude.Nothing
    }

-- | The frame rate of the video source.
--
-- /Note:/ Consider using 'framerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsFramerate :: Lens.Lens' InputDeviceHdSettings (Lude.Maybe Lude.Double)
idhsFramerate = Lens.lens (framerate :: InputDeviceHdSettings -> Lude.Maybe Lude.Double) (\s a -> s {framerate = a} :: InputDeviceHdSettings)
{-# DEPRECATED idhsFramerate "Use generic-lens or generic-optics with 'framerate' instead." #-}

-- | The scan type of the video source.
--
-- /Note:/ Consider using 'scanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsScanType :: Lens.Lens' InputDeviceHdSettings (Lude.Maybe InputDeviceScanType)
idhsScanType = Lens.lens (scanType :: InputDeviceHdSettings -> Lude.Maybe InputDeviceScanType) (\s a -> s {scanType = a} :: InputDeviceHdSettings)
{-# DEPRECATED idhsScanType "Use generic-lens or generic-optics with 'scanType' instead." #-}

-- | The state of the input device.
--
-- /Note:/ Consider using 'deviceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsDeviceState :: Lens.Lens' InputDeviceHdSettings (Lude.Maybe InputDeviceState)
idhsDeviceState = Lens.lens (deviceState :: InputDeviceHdSettings -> Lude.Maybe InputDeviceState) (\s a -> s {deviceState = a} :: InputDeviceHdSettings)
{-# DEPRECATED idhsDeviceState "Use generic-lens or generic-optics with 'deviceState' instead." #-}

-- | The height of the video source, in pixels.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsHeight :: Lens.Lens' InputDeviceHdSettings (Lude.Maybe Lude.Int)
idhsHeight = Lens.lens (height :: InputDeviceHdSettings -> Lude.Maybe Lude.Int) (\s a -> s {height = a} :: InputDeviceHdSettings)
{-# DEPRECATED idhsHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | If you specified Auto as the configured input, specifies which of the sources is currently active (SDI or HDMI).
--
-- /Note:/ Consider using 'activeInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsActiveInput :: Lens.Lens' InputDeviceHdSettings (Lude.Maybe InputDeviceActiveInput)
idhsActiveInput = Lens.lens (activeInput :: InputDeviceHdSettings -> Lude.Maybe InputDeviceActiveInput) (\s a -> s {activeInput = a} :: InputDeviceHdSettings)
{-# DEPRECATED idhsActiveInput "Use generic-lens or generic-optics with 'activeInput' instead." #-}

-- | The width of the video source, in pixels.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsWidth :: Lens.Lens' InputDeviceHdSettings (Lude.Maybe Lude.Int)
idhsWidth = Lens.lens (width :: InputDeviceHdSettings -> Lude.Maybe Lude.Int) (\s a -> s {width = a} :: InputDeviceHdSettings)
{-# DEPRECATED idhsWidth "Use generic-lens or generic-optics with 'width' instead." #-}

-- | The source at the input device that is currently active. You can specify this source.
--
-- /Note:/ Consider using 'configuredInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsConfiguredInput :: Lens.Lens' InputDeviceHdSettings (Lude.Maybe InputDeviceConfiguredInput)
idhsConfiguredInput = Lens.lens (configuredInput :: InputDeviceHdSettings -> Lude.Maybe InputDeviceConfiguredInput) (\s a -> s {configuredInput = a} :: InputDeviceHdSettings)
{-# DEPRECATED idhsConfiguredInput "Use generic-lens or generic-optics with 'configuredInput' instead." #-}

-- | The current maximum bitrate for ingesting this source, in bits per second. You can specify this maximum.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idhsMaxBitrate :: Lens.Lens' InputDeviceHdSettings (Lude.Maybe Lude.Int)
idhsMaxBitrate = Lens.lens (maxBitrate :: InputDeviceHdSettings -> Lude.Maybe Lude.Int) (\s a -> s {maxBitrate = a} :: InputDeviceHdSettings)
{-# DEPRECATED idhsMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

instance Lude.FromJSON InputDeviceHdSettings where
  parseJSON =
    Lude.withObject
      "InputDeviceHdSettings"
      ( \x ->
          InputDeviceHdSettings'
            Lude.<$> (x Lude..:? "framerate")
            Lude.<*> (x Lude..:? "scanType")
            Lude.<*> (x Lude..:? "deviceState")
            Lude.<*> (x Lude..:? "height")
            Lude.<*> (x Lude..:? "activeInput")
            Lude.<*> (x Lude..:? "width")
            Lude.<*> (x Lude..:? "configuredInput")
            Lude.<*> (x Lude..:? "maxBitrate")
      )
