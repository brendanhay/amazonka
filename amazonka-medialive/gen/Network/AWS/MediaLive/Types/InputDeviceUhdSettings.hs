{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceUhdSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceUhdSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDeviceActiveInput
import Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
import Network.AWS.MediaLive.Types.InputDeviceScanType
import Network.AWS.MediaLive.Types.InputDeviceState

-- | Settings that describe the active source from the input device, and the
-- video characteristics of that source.
--
-- /See:/ 'newInputDeviceUhdSettings' smart constructor.
data InputDeviceUhdSettings = InputDeviceUhdSettings'
  { -- | The height of the video source, in pixels.
    height :: Core.Maybe Core.Int,
    -- | The scan type of the video source.
    scanType :: Core.Maybe InputDeviceScanType,
    -- | The width of the video source, in pixels.
    width :: Core.Maybe Core.Int,
    -- | The source at the input device that is currently active. You can specify
    -- this source.
    configuredInput :: Core.Maybe InputDeviceConfiguredInput,
    -- | The frame rate of the video source.
    framerate :: Core.Maybe Core.Double,
    -- | The state of the input device.
    deviceState :: Core.Maybe InputDeviceState,
    -- | The current maximum bitrate for ingesting this source, in bits per
    -- second. You can specify this maximum.
    maxBitrate :: Core.Maybe Core.Int,
    -- | If you specified Auto as the configured input, specifies which of the
    -- sources is currently active (SDI or HDMI).
    activeInput :: Core.Maybe InputDeviceActiveInput
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputDeviceUhdSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'inputDeviceUhdSettings_height' - The height of the video source, in pixels.
--
-- 'scanType', 'inputDeviceUhdSettings_scanType' - The scan type of the video source.
--
-- 'width', 'inputDeviceUhdSettings_width' - The width of the video source, in pixels.
--
-- 'configuredInput', 'inputDeviceUhdSettings_configuredInput' - The source at the input device that is currently active. You can specify
-- this source.
--
-- 'framerate', 'inputDeviceUhdSettings_framerate' - The frame rate of the video source.
--
-- 'deviceState', 'inputDeviceUhdSettings_deviceState' - The state of the input device.
--
-- 'maxBitrate', 'inputDeviceUhdSettings_maxBitrate' - The current maximum bitrate for ingesting this source, in bits per
-- second. You can specify this maximum.
--
-- 'activeInput', 'inputDeviceUhdSettings_activeInput' - If you specified Auto as the configured input, specifies which of the
-- sources is currently active (SDI or HDMI).
newInputDeviceUhdSettings ::
  InputDeviceUhdSettings
newInputDeviceUhdSettings =
  InputDeviceUhdSettings'
    { height = Core.Nothing,
      scanType = Core.Nothing,
      width = Core.Nothing,
      configuredInput = Core.Nothing,
      framerate = Core.Nothing,
      deviceState = Core.Nothing,
      maxBitrate = Core.Nothing,
      activeInput = Core.Nothing
    }

-- | The height of the video source, in pixels.
inputDeviceUhdSettings_height :: Lens.Lens' InputDeviceUhdSettings (Core.Maybe Core.Int)
inputDeviceUhdSettings_height = Lens.lens (\InputDeviceUhdSettings' {height} -> height) (\s@InputDeviceUhdSettings' {} a -> s {height = a} :: InputDeviceUhdSettings)

-- | The scan type of the video source.
inputDeviceUhdSettings_scanType :: Lens.Lens' InputDeviceUhdSettings (Core.Maybe InputDeviceScanType)
inputDeviceUhdSettings_scanType = Lens.lens (\InputDeviceUhdSettings' {scanType} -> scanType) (\s@InputDeviceUhdSettings' {} a -> s {scanType = a} :: InputDeviceUhdSettings)

-- | The width of the video source, in pixels.
inputDeviceUhdSettings_width :: Lens.Lens' InputDeviceUhdSettings (Core.Maybe Core.Int)
inputDeviceUhdSettings_width = Lens.lens (\InputDeviceUhdSettings' {width} -> width) (\s@InputDeviceUhdSettings' {} a -> s {width = a} :: InputDeviceUhdSettings)

-- | The source at the input device that is currently active. You can specify
-- this source.
inputDeviceUhdSettings_configuredInput :: Lens.Lens' InputDeviceUhdSettings (Core.Maybe InputDeviceConfiguredInput)
inputDeviceUhdSettings_configuredInput = Lens.lens (\InputDeviceUhdSettings' {configuredInput} -> configuredInput) (\s@InputDeviceUhdSettings' {} a -> s {configuredInput = a} :: InputDeviceUhdSettings)

-- | The frame rate of the video source.
inputDeviceUhdSettings_framerate :: Lens.Lens' InputDeviceUhdSettings (Core.Maybe Core.Double)
inputDeviceUhdSettings_framerate = Lens.lens (\InputDeviceUhdSettings' {framerate} -> framerate) (\s@InputDeviceUhdSettings' {} a -> s {framerate = a} :: InputDeviceUhdSettings)

-- | The state of the input device.
inputDeviceUhdSettings_deviceState :: Lens.Lens' InputDeviceUhdSettings (Core.Maybe InputDeviceState)
inputDeviceUhdSettings_deviceState = Lens.lens (\InputDeviceUhdSettings' {deviceState} -> deviceState) (\s@InputDeviceUhdSettings' {} a -> s {deviceState = a} :: InputDeviceUhdSettings)

-- | The current maximum bitrate for ingesting this source, in bits per
-- second. You can specify this maximum.
inputDeviceUhdSettings_maxBitrate :: Lens.Lens' InputDeviceUhdSettings (Core.Maybe Core.Int)
inputDeviceUhdSettings_maxBitrate = Lens.lens (\InputDeviceUhdSettings' {maxBitrate} -> maxBitrate) (\s@InputDeviceUhdSettings' {} a -> s {maxBitrate = a} :: InputDeviceUhdSettings)

-- | If you specified Auto as the configured input, specifies which of the
-- sources is currently active (SDI or HDMI).
inputDeviceUhdSettings_activeInput :: Lens.Lens' InputDeviceUhdSettings (Core.Maybe InputDeviceActiveInput)
inputDeviceUhdSettings_activeInput = Lens.lens (\InputDeviceUhdSettings' {activeInput} -> activeInput) (\s@InputDeviceUhdSettings' {} a -> s {activeInput = a} :: InputDeviceUhdSettings)

instance Core.FromJSON InputDeviceUhdSettings where
  parseJSON =
    Core.withObject
      "InputDeviceUhdSettings"
      ( \x ->
          InputDeviceUhdSettings'
            Core.<$> (x Core..:? "height")
            Core.<*> (x Core..:? "scanType")
            Core.<*> (x Core..:? "width")
            Core.<*> (x Core..:? "configuredInput")
            Core.<*> (x Core..:? "framerate")
            Core.<*> (x Core..:? "deviceState")
            Core.<*> (x Core..:? "maxBitrate")
            Core.<*> (x Core..:? "activeInput")
      )

instance Core.Hashable InputDeviceUhdSettings

instance Core.NFData InputDeviceUhdSettings
