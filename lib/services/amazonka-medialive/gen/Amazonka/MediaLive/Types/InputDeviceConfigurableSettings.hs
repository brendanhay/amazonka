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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The maximum bitrate in bits per second. Set a value here to throttle the
    -- bitrate of the source video.
    maxBitrate :: Prelude.Maybe Prelude.Int,
    -- | The input source that you want to use. If the device has a source
    -- connected to only one of its input ports, or if you don\'t care which
    -- source the device sends, specify Auto. If the device has sources
    -- connected to both its input ports, and you want to use a specific
    -- source, specify the source.
    configuredInput :: Prelude.Maybe InputDeviceConfiguredInput
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
-- 'maxBitrate', 'inputDeviceConfigurableSettings_maxBitrate' - The maximum bitrate in bits per second. Set a value here to throttle the
-- bitrate of the source video.
--
-- 'configuredInput', 'inputDeviceConfigurableSettings_configuredInput' - The input source that you want to use. If the device has a source
-- connected to only one of its input ports, or if you don\'t care which
-- source the device sends, specify Auto. If the device has sources
-- connected to both its input ports, and you want to use a specific
-- source, specify the source.
newInputDeviceConfigurableSettings ::
  InputDeviceConfigurableSettings
newInputDeviceConfigurableSettings =
  InputDeviceConfigurableSettings'
    { maxBitrate =
        Prelude.Nothing,
      configuredInput = Prelude.Nothing
    }

-- | The maximum bitrate in bits per second. Set a value here to throttle the
-- bitrate of the source video.
inputDeviceConfigurableSettings_maxBitrate :: Lens.Lens' InputDeviceConfigurableSettings (Prelude.Maybe Prelude.Int)
inputDeviceConfigurableSettings_maxBitrate = Lens.lens (\InputDeviceConfigurableSettings' {maxBitrate} -> maxBitrate) (\s@InputDeviceConfigurableSettings' {} a -> s {maxBitrate = a} :: InputDeviceConfigurableSettings)

-- | The input source that you want to use. If the device has a source
-- connected to only one of its input ports, or if you don\'t care which
-- source the device sends, specify Auto. If the device has sources
-- connected to both its input ports, and you want to use a specific
-- source, specify the source.
inputDeviceConfigurableSettings_configuredInput :: Lens.Lens' InputDeviceConfigurableSettings (Prelude.Maybe InputDeviceConfiguredInput)
inputDeviceConfigurableSettings_configuredInput = Lens.lens (\InputDeviceConfigurableSettings' {configuredInput} -> configuredInput) (\s@InputDeviceConfigurableSettings' {} a -> s {configuredInput = a} :: InputDeviceConfigurableSettings)

instance
  Prelude.Hashable
    InputDeviceConfigurableSettings
  where
  hashWithSalt
    _salt
    InputDeviceConfigurableSettings' {..} =
      _salt `Prelude.hashWithSalt` maxBitrate
        `Prelude.hashWithSalt` configuredInput

instance
  Prelude.NFData
    InputDeviceConfigurableSettings
  where
  rnf InputDeviceConfigurableSettings' {..} =
    Prelude.rnf maxBitrate
      `Prelude.seq` Prelude.rnf configuredInput

instance Data.ToJSON InputDeviceConfigurableSettings where
  toJSON InputDeviceConfigurableSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxBitrate" Data..=) Prelude.<$> maxBitrate,
            ("configuredInput" Data..=)
              Prelude.<$> configuredInput
          ]
      )
