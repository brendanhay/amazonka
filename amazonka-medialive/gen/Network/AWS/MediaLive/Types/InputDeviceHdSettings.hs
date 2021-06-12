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
-- Module      : Network.AWS.MediaLive.Types.InputDeviceHdSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceHdSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDeviceActiveInput
import Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
import Network.AWS.MediaLive.Types.InputDeviceScanType
import Network.AWS.MediaLive.Types.InputDeviceState

-- | Settings that describe the active source from the input device, and the
-- video characteristics of that source.
--
-- /See:/ 'newInputDeviceHdSettings' smart constructor.
data InputDeviceHdSettings = InputDeviceHdSettings'
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
-- Create a value of 'InputDeviceHdSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'inputDeviceHdSettings_height' - The height of the video source, in pixels.
--
-- 'scanType', 'inputDeviceHdSettings_scanType' - The scan type of the video source.
--
-- 'width', 'inputDeviceHdSettings_width' - The width of the video source, in pixels.
--
-- 'configuredInput', 'inputDeviceHdSettings_configuredInput' - The source at the input device that is currently active. You can specify
-- this source.
--
-- 'framerate', 'inputDeviceHdSettings_framerate' - The frame rate of the video source.
--
-- 'deviceState', 'inputDeviceHdSettings_deviceState' - The state of the input device.
--
-- 'maxBitrate', 'inputDeviceHdSettings_maxBitrate' - The current maximum bitrate for ingesting this source, in bits per
-- second. You can specify this maximum.
--
-- 'activeInput', 'inputDeviceHdSettings_activeInput' - If you specified Auto as the configured input, specifies which of the
-- sources is currently active (SDI or HDMI).
newInputDeviceHdSettings ::
  InputDeviceHdSettings
newInputDeviceHdSettings =
  InputDeviceHdSettings'
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
inputDeviceHdSettings_height :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Core.Int)
inputDeviceHdSettings_height = Lens.lens (\InputDeviceHdSettings' {height} -> height) (\s@InputDeviceHdSettings' {} a -> s {height = a} :: InputDeviceHdSettings)

-- | The scan type of the video source.
inputDeviceHdSettings_scanType :: Lens.Lens' InputDeviceHdSettings (Core.Maybe InputDeviceScanType)
inputDeviceHdSettings_scanType = Lens.lens (\InputDeviceHdSettings' {scanType} -> scanType) (\s@InputDeviceHdSettings' {} a -> s {scanType = a} :: InputDeviceHdSettings)

-- | The width of the video source, in pixels.
inputDeviceHdSettings_width :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Core.Int)
inputDeviceHdSettings_width = Lens.lens (\InputDeviceHdSettings' {width} -> width) (\s@InputDeviceHdSettings' {} a -> s {width = a} :: InputDeviceHdSettings)

-- | The source at the input device that is currently active. You can specify
-- this source.
inputDeviceHdSettings_configuredInput :: Lens.Lens' InputDeviceHdSettings (Core.Maybe InputDeviceConfiguredInput)
inputDeviceHdSettings_configuredInput = Lens.lens (\InputDeviceHdSettings' {configuredInput} -> configuredInput) (\s@InputDeviceHdSettings' {} a -> s {configuredInput = a} :: InputDeviceHdSettings)

-- | The frame rate of the video source.
inputDeviceHdSettings_framerate :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Core.Double)
inputDeviceHdSettings_framerate = Lens.lens (\InputDeviceHdSettings' {framerate} -> framerate) (\s@InputDeviceHdSettings' {} a -> s {framerate = a} :: InputDeviceHdSettings)

-- | The state of the input device.
inputDeviceHdSettings_deviceState :: Lens.Lens' InputDeviceHdSettings (Core.Maybe InputDeviceState)
inputDeviceHdSettings_deviceState = Lens.lens (\InputDeviceHdSettings' {deviceState} -> deviceState) (\s@InputDeviceHdSettings' {} a -> s {deviceState = a} :: InputDeviceHdSettings)

-- | The current maximum bitrate for ingesting this source, in bits per
-- second. You can specify this maximum.
inputDeviceHdSettings_maxBitrate :: Lens.Lens' InputDeviceHdSettings (Core.Maybe Core.Int)
inputDeviceHdSettings_maxBitrate = Lens.lens (\InputDeviceHdSettings' {maxBitrate} -> maxBitrate) (\s@InputDeviceHdSettings' {} a -> s {maxBitrate = a} :: InputDeviceHdSettings)

-- | If you specified Auto as the configured input, specifies which of the
-- sources is currently active (SDI or HDMI).
inputDeviceHdSettings_activeInput :: Lens.Lens' InputDeviceHdSettings (Core.Maybe InputDeviceActiveInput)
inputDeviceHdSettings_activeInput = Lens.lens (\InputDeviceHdSettings' {activeInput} -> activeInput) (\s@InputDeviceHdSettings' {} a -> s {activeInput = a} :: InputDeviceHdSettings)

instance Core.FromJSON InputDeviceHdSettings where
  parseJSON =
    Core.withObject
      "InputDeviceHdSettings"
      ( \x ->
          InputDeviceHdSettings'
            Core.<$> (x Core..:? "height")
            Core.<*> (x Core..:? "scanType")
            Core.<*> (x Core..:? "width")
            Core.<*> (x Core..:? "configuredInput")
            Core.<*> (x Core..:? "framerate")
            Core.<*> (x Core..:? "deviceState")
            Core.<*> (x Core..:? "maxBitrate")
            Core.<*> (x Core..:? "activeInput")
      )

instance Core.Hashable InputDeviceHdSettings

instance Core.NFData InputDeviceHdSettings
