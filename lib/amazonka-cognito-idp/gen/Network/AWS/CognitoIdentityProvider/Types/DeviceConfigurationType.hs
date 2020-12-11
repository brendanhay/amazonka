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
import qualified Network.AWS.Prelude as Lude

-- | The configuration for the user pool's device tracking.
--
-- /See:/ 'mkDeviceConfigurationType' smart constructor.
data DeviceConfigurationType = DeviceConfigurationType'
  { challengeRequiredOnNewDevice ::
      Lude.Maybe Lude.Bool,
    deviceOnlyRememberedOnUserPrompt ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceConfigurationType' with the minimum fields required to make a request.
--
-- * 'challengeRequiredOnNewDevice' - Indicates whether a challenge is required on a new device. Only applicable to a new device.
-- * 'deviceOnlyRememberedOnUserPrompt' - If true, a device is only remembered on user prompt.
mkDeviceConfigurationType ::
  DeviceConfigurationType
mkDeviceConfigurationType =
  DeviceConfigurationType'
    { challengeRequiredOnNewDevice =
        Lude.Nothing,
      deviceOnlyRememberedOnUserPrompt = Lude.Nothing
    }

-- | Indicates whether a challenge is required on a new device. Only applicable to a new device.
--
-- /Note:/ Consider using 'challengeRequiredOnNewDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctChallengeRequiredOnNewDevice :: Lens.Lens' DeviceConfigurationType (Lude.Maybe Lude.Bool)
dctChallengeRequiredOnNewDevice = Lens.lens (challengeRequiredOnNewDevice :: DeviceConfigurationType -> Lude.Maybe Lude.Bool) (\s a -> s {challengeRequiredOnNewDevice = a} :: DeviceConfigurationType)
{-# DEPRECATED dctChallengeRequiredOnNewDevice "Use generic-lens or generic-optics with 'challengeRequiredOnNewDevice' instead." #-}

-- | If true, a device is only remembered on user prompt.
--
-- /Note:/ Consider using 'deviceOnlyRememberedOnUserPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctDeviceOnlyRememberedOnUserPrompt :: Lens.Lens' DeviceConfigurationType (Lude.Maybe Lude.Bool)
dctDeviceOnlyRememberedOnUserPrompt = Lens.lens (deviceOnlyRememberedOnUserPrompt :: DeviceConfigurationType -> Lude.Maybe Lude.Bool) (\s a -> s {deviceOnlyRememberedOnUserPrompt = a} :: DeviceConfigurationType)
{-# DEPRECATED dctDeviceOnlyRememberedOnUserPrompt "Use generic-lens or generic-optics with 'deviceOnlyRememberedOnUserPrompt' instead." #-}

instance Lude.FromJSON DeviceConfigurationType where
  parseJSON =
    Lude.withObject
      "DeviceConfigurationType"
      ( \x ->
          DeviceConfigurationType'
            Lude.<$> (x Lude..:? "ChallengeRequiredOnNewDevice")
            Lude.<*> (x Lude..:? "DeviceOnlyRememberedOnUserPrompt")
      )

instance Lude.ToJSON DeviceConfigurationType where
  toJSON DeviceConfigurationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ChallengeRequiredOnNewDevice" Lude..=)
              Lude.<$> challengeRequiredOnNewDevice,
            ("DeviceOnlyRememberedOnUserPrompt" Lude..=)
              Lude.<$> deviceOnlyRememberedOnUserPrompt
          ]
      )
