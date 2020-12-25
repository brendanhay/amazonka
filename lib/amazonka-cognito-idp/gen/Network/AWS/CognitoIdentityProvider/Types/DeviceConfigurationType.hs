{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType
  ( DeviceConfigurationType (..),

    -- * Smart constructor
    mkDeviceConfigurationType,

    -- * Lenses
    dctChallengeRequiredOnNewDevice,
    dctDeviceOnlyRememberedOnUserPrompt,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration for the user pool's device tracking.
--
-- /See:/ 'mkDeviceConfigurationType' smart constructor.
data DeviceConfigurationType = DeviceConfigurationType'
  { -- | Indicates whether a challenge is required on a new device. Only applicable to a new device.
    challengeRequiredOnNewDevice :: Core.Maybe Core.Bool,
    -- | If true, a device is only remembered on user prompt.
    deviceOnlyRememberedOnUserPrompt :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeviceConfigurationType' value with any optional fields omitted.
mkDeviceConfigurationType ::
  DeviceConfigurationType
mkDeviceConfigurationType =
  DeviceConfigurationType'
    { challengeRequiredOnNewDevice =
        Core.Nothing,
      deviceOnlyRememberedOnUserPrompt = Core.Nothing
    }

-- | Indicates whether a challenge is required on a new device. Only applicable to a new device.
--
-- /Note:/ Consider using 'challengeRequiredOnNewDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctChallengeRequiredOnNewDevice :: Lens.Lens' DeviceConfigurationType (Core.Maybe Core.Bool)
dctChallengeRequiredOnNewDevice = Lens.field @"challengeRequiredOnNewDevice"
{-# DEPRECATED dctChallengeRequiredOnNewDevice "Use generic-lens or generic-optics with 'challengeRequiredOnNewDevice' instead." #-}

-- | If true, a device is only remembered on user prompt.
--
-- /Note:/ Consider using 'deviceOnlyRememberedOnUserPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctDeviceOnlyRememberedOnUserPrompt :: Lens.Lens' DeviceConfigurationType (Core.Maybe Core.Bool)
dctDeviceOnlyRememberedOnUserPrompt = Lens.field @"deviceOnlyRememberedOnUserPrompt"
{-# DEPRECATED dctDeviceOnlyRememberedOnUserPrompt "Use generic-lens or generic-optics with 'deviceOnlyRememberedOnUserPrompt' instead." #-}

instance Core.FromJSON DeviceConfigurationType where
  toJSON DeviceConfigurationType {..} =
    Core.object
      ( Core.catMaybes
          [ ("ChallengeRequiredOnNewDevice" Core..=)
              Core.<$> challengeRequiredOnNewDevice,
            ("DeviceOnlyRememberedOnUserPrompt" Core..=)
              Core.<$> deviceOnlyRememberedOnUserPrompt
          ]
      )

instance Core.FromJSON DeviceConfigurationType where
  parseJSON =
    Core.withObject "DeviceConfigurationType" Core.$
      \x ->
        DeviceConfigurationType'
          Core.<$> (x Core..:? "ChallengeRequiredOnNewDevice")
          Core.<*> (x Core..:? "DeviceOnlyRememberedOnUserPrompt")
