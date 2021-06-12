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
-- Module      : Network.AWS.MediaLive.Types.InputDeviceConfigurableSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceConfigurableSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDeviceConfiguredInput

-- | Configurable settings for the input device.
--
-- /See:/ 'newInputDeviceConfigurableSettings' smart constructor.
data InputDeviceConfigurableSettings = InputDeviceConfigurableSettings'
  { -- | The input source that you want to use. If the device has a source
    -- connected to only one of its input ports, or if you don\'t care which
    -- source the device sends, specify Auto. If the device has sources
    -- connected to both its input ports, and you want to use a specific
    -- source, specify the source.
    configuredInput :: Core.Maybe InputDeviceConfiguredInput,
    -- | The maximum bitrate in bits per second. Set a value here to throttle the
    -- bitrate of the source video.
    maxBitrate :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxBitrate', 'inputDeviceConfigurableSettings_maxBitrate' - The maximum bitrate in bits per second. Set a value here to throttle the
-- bitrate of the source video.
newInputDeviceConfigurableSettings ::
  InputDeviceConfigurableSettings
newInputDeviceConfigurableSettings =
  InputDeviceConfigurableSettings'
    { configuredInput =
        Core.Nothing,
      maxBitrate = Core.Nothing
    }

-- | The input source that you want to use. If the device has a source
-- connected to only one of its input ports, or if you don\'t care which
-- source the device sends, specify Auto. If the device has sources
-- connected to both its input ports, and you want to use a specific
-- source, specify the source.
inputDeviceConfigurableSettings_configuredInput :: Lens.Lens' InputDeviceConfigurableSettings (Core.Maybe InputDeviceConfiguredInput)
inputDeviceConfigurableSettings_configuredInput = Lens.lens (\InputDeviceConfigurableSettings' {configuredInput} -> configuredInput) (\s@InputDeviceConfigurableSettings' {} a -> s {configuredInput = a} :: InputDeviceConfigurableSettings)

-- | The maximum bitrate in bits per second. Set a value here to throttle the
-- bitrate of the source video.
inputDeviceConfigurableSettings_maxBitrate :: Lens.Lens' InputDeviceConfigurableSettings (Core.Maybe Core.Int)
inputDeviceConfigurableSettings_maxBitrate = Lens.lens (\InputDeviceConfigurableSettings' {maxBitrate} -> maxBitrate) (\s@InputDeviceConfigurableSettings' {} a -> s {maxBitrate = a} :: InputDeviceConfigurableSettings)

instance
  Core.Hashable
    InputDeviceConfigurableSettings

instance Core.NFData InputDeviceConfigurableSettings

instance Core.ToJSON InputDeviceConfigurableSettings where
  toJSON InputDeviceConfigurableSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("configuredInput" Core..=)
              Core.<$> configuredInput,
            ("maxBitrate" Core..=) Core.<$> maxBitrate
          ]
      )
