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
-- Module      : Amazonka.MediaLive.Types.InputDeviceUhdSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDeviceUhdSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputDeviceActiveInput
import Amazonka.MediaLive.Types.InputDeviceConfiguredInput
import Amazonka.MediaLive.Types.InputDeviceScanType
import Amazonka.MediaLive.Types.InputDeviceState
import qualified Amazonka.Prelude as Prelude

-- | Settings that describe the active source from the input device, and the
-- video characteristics of that source.
--
-- /See:/ 'newInputDeviceUhdSettings' smart constructor.
data InputDeviceUhdSettings = InputDeviceUhdSettings'
  { -- | If you specified Auto as the configured input, specifies which of the
    -- sources is currently active (SDI or HDMI).
    activeInput :: Prelude.Maybe InputDeviceActiveInput,
    -- | The source at the input device that is currently active. You can specify
    -- this source.
    configuredInput :: Prelude.Maybe InputDeviceConfiguredInput,
    -- | The state of the input device.
    deviceState :: Prelude.Maybe InputDeviceState,
    -- | The frame rate of the video source.
    framerate :: Prelude.Maybe Prelude.Double,
    -- | The height of the video source, in pixels.
    height :: Prelude.Maybe Prelude.Int,
    -- | The Link device\'s buffer size (latency) in milliseconds (ms). You can
    -- specify this value.
    latencyMs :: Prelude.Maybe Prelude.Int,
    -- | The current maximum bitrate for ingesting this source, in bits per
    -- second. You can specify this maximum.
    maxBitrate :: Prelude.Maybe Prelude.Int,
    -- | The scan type of the video source.
    scanType :: Prelude.Maybe InputDeviceScanType,
    -- | The width of the video source, in pixels.
    width :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDeviceUhdSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeInput', 'inputDeviceUhdSettings_activeInput' - If you specified Auto as the configured input, specifies which of the
-- sources is currently active (SDI or HDMI).
--
-- 'configuredInput', 'inputDeviceUhdSettings_configuredInput' - The source at the input device that is currently active. You can specify
-- this source.
--
-- 'deviceState', 'inputDeviceUhdSettings_deviceState' - The state of the input device.
--
-- 'framerate', 'inputDeviceUhdSettings_framerate' - The frame rate of the video source.
--
-- 'height', 'inputDeviceUhdSettings_height' - The height of the video source, in pixels.
--
-- 'latencyMs', 'inputDeviceUhdSettings_latencyMs' - The Link device\'s buffer size (latency) in milliseconds (ms). You can
-- specify this value.
--
-- 'maxBitrate', 'inputDeviceUhdSettings_maxBitrate' - The current maximum bitrate for ingesting this source, in bits per
-- second. You can specify this maximum.
--
-- 'scanType', 'inputDeviceUhdSettings_scanType' - The scan type of the video source.
--
-- 'width', 'inputDeviceUhdSettings_width' - The width of the video source, in pixels.
newInputDeviceUhdSettings ::
  InputDeviceUhdSettings
newInputDeviceUhdSettings =
  InputDeviceUhdSettings'
    { activeInput =
        Prelude.Nothing,
      configuredInput = Prelude.Nothing,
      deviceState = Prelude.Nothing,
      framerate = Prelude.Nothing,
      height = Prelude.Nothing,
      latencyMs = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      scanType = Prelude.Nothing,
      width = Prelude.Nothing
    }

-- | If you specified Auto as the configured input, specifies which of the
-- sources is currently active (SDI or HDMI).
inputDeviceUhdSettings_activeInput :: Lens.Lens' InputDeviceUhdSettings (Prelude.Maybe InputDeviceActiveInput)
inputDeviceUhdSettings_activeInput = Lens.lens (\InputDeviceUhdSettings' {activeInput} -> activeInput) (\s@InputDeviceUhdSettings' {} a -> s {activeInput = a} :: InputDeviceUhdSettings)

-- | The source at the input device that is currently active. You can specify
-- this source.
inputDeviceUhdSettings_configuredInput :: Lens.Lens' InputDeviceUhdSettings (Prelude.Maybe InputDeviceConfiguredInput)
inputDeviceUhdSettings_configuredInput = Lens.lens (\InputDeviceUhdSettings' {configuredInput} -> configuredInput) (\s@InputDeviceUhdSettings' {} a -> s {configuredInput = a} :: InputDeviceUhdSettings)

-- | The state of the input device.
inputDeviceUhdSettings_deviceState :: Lens.Lens' InputDeviceUhdSettings (Prelude.Maybe InputDeviceState)
inputDeviceUhdSettings_deviceState = Lens.lens (\InputDeviceUhdSettings' {deviceState} -> deviceState) (\s@InputDeviceUhdSettings' {} a -> s {deviceState = a} :: InputDeviceUhdSettings)

-- | The frame rate of the video source.
inputDeviceUhdSettings_framerate :: Lens.Lens' InputDeviceUhdSettings (Prelude.Maybe Prelude.Double)
inputDeviceUhdSettings_framerate = Lens.lens (\InputDeviceUhdSettings' {framerate} -> framerate) (\s@InputDeviceUhdSettings' {} a -> s {framerate = a} :: InputDeviceUhdSettings)

-- | The height of the video source, in pixels.
inputDeviceUhdSettings_height :: Lens.Lens' InputDeviceUhdSettings (Prelude.Maybe Prelude.Int)
inputDeviceUhdSettings_height = Lens.lens (\InputDeviceUhdSettings' {height} -> height) (\s@InputDeviceUhdSettings' {} a -> s {height = a} :: InputDeviceUhdSettings)

-- | The Link device\'s buffer size (latency) in milliseconds (ms). You can
-- specify this value.
inputDeviceUhdSettings_latencyMs :: Lens.Lens' InputDeviceUhdSettings (Prelude.Maybe Prelude.Int)
inputDeviceUhdSettings_latencyMs = Lens.lens (\InputDeviceUhdSettings' {latencyMs} -> latencyMs) (\s@InputDeviceUhdSettings' {} a -> s {latencyMs = a} :: InputDeviceUhdSettings)

-- | The current maximum bitrate for ingesting this source, in bits per
-- second. You can specify this maximum.
inputDeviceUhdSettings_maxBitrate :: Lens.Lens' InputDeviceUhdSettings (Prelude.Maybe Prelude.Int)
inputDeviceUhdSettings_maxBitrate = Lens.lens (\InputDeviceUhdSettings' {maxBitrate} -> maxBitrate) (\s@InputDeviceUhdSettings' {} a -> s {maxBitrate = a} :: InputDeviceUhdSettings)

-- | The scan type of the video source.
inputDeviceUhdSettings_scanType :: Lens.Lens' InputDeviceUhdSettings (Prelude.Maybe InputDeviceScanType)
inputDeviceUhdSettings_scanType = Lens.lens (\InputDeviceUhdSettings' {scanType} -> scanType) (\s@InputDeviceUhdSettings' {} a -> s {scanType = a} :: InputDeviceUhdSettings)

-- | The width of the video source, in pixels.
inputDeviceUhdSettings_width :: Lens.Lens' InputDeviceUhdSettings (Prelude.Maybe Prelude.Int)
inputDeviceUhdSettings_width = Lens.lens (\InputDeviceUhdSettings' {width} -> width) (\s@InputDeviceUhdSettings' {} a -> s {width = a} :: InputDeviceUhdSettings)

instance Data.FromJSON InputDeviceUhdSettings where
  parseJSON =
    Data.withObject
      "InputDeviceUhdSettings"
      ( \x ->
          InputDeviceUhdSettings'
            Prelude.<$> (x Data..:? "activeInput")
            Prelude.<*> (x Data..:? "configuredInput")
            Prelude.<*> (x Data..:? "deviceState")
            Prelude.<*> (x Data..:? "framerate")
            Prelude.<*> (x Data..:? "height")
            Prelude.<*> (x Data..:? "latencyMs")
            Prelude.<*> (x Data..:? "maxBitrate")
            Prelude.<*> (x Data..:? "scanType")
            Prelude.<*> (x Data..:? "width")
      )

instance Prelude.Hashable InputDeviceUhdSettings where
  hashWithSalt _salt InputDeviceUhdSettings' {..} =
    _salt
      `Prelude.hashWithSalt` activeInput
      `Prelude.hashWithSalt` configuredInput
      `Prelude.hashWithSalt` deviceState
      `Prelude.hashWithSalt` framerate
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` latencyMs
      `Prelude.hashWithSalt` maxBitrate
      `Prelude.hashWithSalt` scanType
      `Prelude.hashWithSalt` width

instance Prelude.NFData InputDeviceUhdSettings where
  rnf InputDeviceUhdSettings' {..} =
    Prelude.rnf activeInput
      `Prelude.seq` Prelude.rnf configuredInput
      `Prelude.seq` Prelude.rnf deviceState
      `Prelude.seq` Prelude.rnf framerate
      `Prelude.seq` Prelude.rnf height
      `Prelude.seq` Prelude.rnf latencyMs
      `Prelude.seq` Prelude.rnf maxBitrate
      `Prelude.seq` Prelude.rnf scanType
      `Prelude.seq` Prelude.rnf width
