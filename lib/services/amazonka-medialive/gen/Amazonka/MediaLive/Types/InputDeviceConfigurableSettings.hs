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
-- Module      : Amazonka.MediaLive.Types.InputDeviceConfigurableSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDeviceConfigurableSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputDeviceConfiguredInput
import qualified Amazonka.Prelude as Prelude

-- | Configurable settings for the input device.
--
-- /See:/ 'newInputDeviceConfigurableSettings' smart constructor.
data InputDeviceConfigurableSettings = InputDeviceConfigurableSettings'
  { -- | The input source that you want to use. If the device has a source
    -- connected to only one of its input ports, or if you don\'t care which
    -- source the device sends, specify Auto. If the device has sources
    -- connected to both its input ports, and you want to use a specific
    -- source, specify the source.
    configuredInput :: Prelude.Maybe InputDeviceConfiguredInput,
    -- | The Link device\'s buffer size (latency) in milliseconds (ms).
    latencyMs :: Prelude.Maybe Prelude.Int,
    -- | The maximum bitrate in bits per second. Set a value here to throttle the
    -- bitrate of the source video.
    maxBitrate :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDeviceConfigurableSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuredInput', 'inputDeviceConfigurableSettings_configuredInput' - The input source that you want to use. If the device has a source
-- connected to only one of its input ports, or if you don\'t care which
-- source the device sends, specify Auto. If the device has sources
-- connected to both its input ports, and you want to use a specific
-- source, specify the source.
--
-- 'latencyMs', 'inputDeviceConfigurableSettings_latencyMs' - The Link device\'s buffer size (latency) in milliseconds (ms).
--
-- 'maxBitrate', 'inputDeviceConfigurableSettings_maxBitrate' - The maximum bitrate in bits per second. Set a value here to throttle the
-- bitrate of the source video.
newInputDeviceConfigurableSettings ::
  InputDeviceConfigurableSettings
newInputDeviceConfigurableSettings =
  InputDeviceConfigurableSettings'
    { configuredInput =
        Prelude.Nothing,
      latencyMs = Prelude.Nothing,
      maxBitrate = Prelude.Nothing
    }

-- | The input source that you want to use. If the device has a source
-- connected to only one of its input ports, or if you don\'t care which
-- source the device sends, specify Auto. If the device has sources
-- connected to both its input ports, and you want to use a specific
-- source, specify the source.
inputDeviceConfigurableSettings_configuredInput :: Lens.Lens' InputDeviceConfigurableSettings (Prelude.Maybe InputDeviceConfiguredInput)
inputDeviceConfigurableSettings_configuredInput = Lens.lens (\InputDeviceConfigurableSettings' {configuredInput} -> configuredInput) (\s@InputDeviceConfigurableSettings' {} a -> s {configuredInput = a} :: InputDeviceConfigurableSettings)

-- | The Link device\'s buffer size (latency) in milliseconds (ms).
inputDeviceConfigurableSettings_latencyMs :: Lens.Lens' InputDeviceConfigurableSettings (Prelude.Maybe Prelude.Int)
inputDeviceConfigurableSettings_latencyMs = Lens.lens (\InputDeviceConfigurableSettings' {latencyMs} -> latencyMs) (\s@InputDeviceConfigurableSettings' {} a -> s {latencyMs = a} :: InputDeviceConfigurableSettings)

-- | The maximum bitrate in bits per second. Set a value here to throttle the
-- bitrate of the source video.
inputDeviceConfigurableSettings_maxBitrate :: Lens.Lens' InputDeviceConfigurableSettings (Prelude.Maybe Prelude.Int)
inputDeviceConfigurableSettings_maxBitrate = Lens.lens (\InputDeviceConfigurableSettings' {maxBitrate} -> maxBitrate) (\s@InputDeviceConfigurableSettings' {} a -> s {maxBitrate = a} :: InputDeviceConfigurableSettings)

instance
  Prelude.Hashable
    InputDeviceConfigurableSettings
  where
  hashWithSalt
    _salt
    InputDeviceConfigurableSettings' {..} =
      _salt
        `Prelude.hashWithSalt` configuredInput
        `Prelude.hashWithSalt` latencyMs
        `Prelude.hashWithSalt` maxBitrate

instance
  Prelude.NFData
    InputDeviceConfigurableSettings
  where
  rnf InputDeviceConfigurableSettings' {..} =
    Prelude.rnf configuredInput
      `Prelude.seq` Prelude.rnf latencyMs
      `Prelude.seq` Prelude.rnf maxBitrate

instance Data.ToJSON InputDeviceConfigurableSettings where
  toJSON InputDeviceConfigurableSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configuredInput" Data..=)
              Prelude.<$> configuredInput,
            ("latencyMs" Data..=) Prelude.<$> latencyMs,
            ("maxBitrate" Data..=) Prelude.<$> maxBitrate
          ]
      )
