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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The configuration for the user pool\'s device tracking.
--
-- /See:/ 'newDeviceConfigurationType' smart constructor.
data DeviceConfigurationType = DeviceConfigurationType'
  { -- | Indicates whether a challenge is required on a new device. Only
    -- applicable to a new device.
    challengeRequiredOnNewDevice :: Core.Maybe Core.Bool,
    -- | If true, a device is only remembered on user prompt.
    deviceOnlyRememberedOnUserPrompt :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'challengeRequiredOnNewDevice', 'deviceConfigurationType_challengeRequiredOnNewDevice' - Indicates whether a challenge is required on a new device. Only
-- applicable to a new device.
--
-- 'deviceOnlyRememberedOnUserPrompt', 'deviceConfigurationType_deviceOnlyRememberedOnUserPrompt' - If true, a device is only remembered on user prompt.
newDeviceConfigurationType ::
  DeviceConfigurationType
newDeviceConfigurationType =
  DeviceConfigurationType'
    { challengeRequiredOnNewDevice =
        Core.Nothing,
      deviceOnlyRememberedOnUserPrompt = Core.Nothing
    }

-- | Indicates whether a challenge is required on a new device. Only
-- applicable to a new device.
deviceConfigurationType_challengeRequiredOnNewDevice :: Lens.Lens' DeviceConfigurationType (Core.Maybe Core.Bool)
deviceConfigurationType_challengeRequiredOnNewDevice = Lens.lens (\DeviceConfigurationType' {challengeRequiredOnNewDevice} -> challengeRequiredOnNewDevice) (\s@DeviceConfigurationType' {} a -> s {challengeRequiredOnNewDevice = a} :: DeviceConfigurationType)

-- | If true, a device is only remembered on user prompt.
deviceConfigurationType_deviceOnlyRememberedOnUserPrompt :: Lens.Lens' DeviceConfigurationType (Core.Maybe Core.Bool)
deviceConfigurationType_deviceOnlyRememberedOnUserPrompt = Lens.lens (\DeviceConfigurationType' {deviceOnlyRememberedOnUserPrompt} -> deviceOnlyRememberedOnUserPrompt) (\s@DeviceConfigurationType' {} a -> s {deviceOnlyRememberedOnUserPrompt = a} :: DeviceConfigurationType)

instance Core.FromJSON DeviceConfigurationType where
  parseJSON =
    Core.withObject
      "DeviceConfigurationType"
      ( \x ->
          DeviceConfigurationType'
            Core.<$> (x Core..:? "ChallengeRequiredOnNewDevice")
            Core.<*> (x Core..:? "DeviceOnlyRememberedOnUserPrompt")
      )

instance Core.Hashable DeviceConfigurationType

instance Core.NFData DeviceConfigurationType

instance Core.ToJSON DeviceConfigurationType where
  toJSON DeviceConfigurationType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ChallengeRequiredOnNewDevice" Core..=)
              Core.<$> challengeRequiredOnNewDevice,
            ("DeviceOnlyRememberedOnUserPrompt" Core..=)
              Core.<$> deviceOnlyRememberedOnUserPrompt
          ]
      )
