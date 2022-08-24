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
-- Module      : Amazonka.CognitoIdentityProvider.Types.DeviceConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.DeviceConfigurationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The device-remembering configuration for a user pool. A null value
-- indicates that you have deactivated device remembering in your user
-- pool.
--
-- When you provide a value for any @DeviceConfiguration@ field, you
-- activate the Amazon Cognito device-remembering feature.
--
-- /See:/ 'newDeviceConfigurationType' smart constructor.
data DeviceConfigurationType = DeviceConfigurationType'
  { -- | When true, Amazon Cognito doesn\'t remember newly-confirmed devices.
    -- Users who want to authenticate with their device can instead opt in to
    -- remembering their device. To collect a choice from your user, create an
    -- input prompt in your app and return the value that the user chooses in
    -- an
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UpdateDeviceStatus.html UpdateDeviceStatus>
    -- API request.
    deviceOnlyRememberedOnUserPrompt :: Prelude.Maybe Prelude.Bool,
    -- | When true, device authentication can replace SMS and time-based one-time
    -- password (TOTP) factors for multi-factor authentication (MFA).
    --
    -- Regardless of the value of this field, users that sign in with new
    -- devices that have not been confirmed or remembered must provide a second
    -- factor if your user pool requires MFA.
    challengeRequiredOnNewDevice :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceOnlyRememberedOnUserPrompt', 'deviceConfigurationType_deviceOnlyRememberedOnUserPrompt' - When true, Amazon Cognito doesn\'t remember newly-confirmed devices.
-- Users who want to authenticate with their device can instead opt in to
-- remembering their device. To collect a choice from your user, create an
-- input prompt in your app and return the value that the user chooses in
-- an
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UpdateDeviceStatus.html UpdateDeviceStatus>
-- API request.
--
-- 'challengeRequiredOnNewDevice', 'deviceConfigurationType_challengeRequiredOnNewDevice' - When true, device authentication can replace SMS and time-based one-time
-- password (TOTP) factors for multi-factor authentication (MFA).
--
-- Regardless of the value of this field, users that sign in with new
-- devices that have not been confirmed or remembered must provide a second
-- factor if your user pool requires MFA.
newDeviceConfigurationType ::
  DeviceConfigurationType
newDeviceConfigurationType =
  DeviceConfigurationType'
    { deviceOnlyRememberedOnUserPrompt =
        Prelude.Nothing,
      challengeRequiredOnNewDevice = Prelude.Nothing
    }

-- | When true, Amazon Cognito doesn\'t remember newly-confirmed devices.
-- Users who want to authenticate with their device can instead opt in to
-- remembering their device. To collect a choice from your user, create an
-- input prompt in your app and return the value that the user chooses in
-- an
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UpdateDeviceStatus.html UpdateDeviceStatus>
-- API request.
deviceConfigurationType_deviceOnlyRememberedOnUserPrompt :: Lens.Lens' DeviceConfigurationType (Prelude.Maybe Prelude.Bool)
deviceConfigurationType_deviceOnlyRememberedOnUserPrompt = Lens.lens (\DeviceConfigurationType' {deviceOnlyRememberedOnUserPrompt} -> deviceOnlyRememberedOnUserPrompt) (\s@DeviceConfigurationType' {} a -> s {deviceOnlyRememberedOnUserPrompt = a} :: DeviceConfigurationType)

-- | When true, device authentication can replace SMS and time-based one-time
-- password (TOTP) factors for multi-factor authentication (MFA).
--
-- Regardless of the value of this field, users that sign in with new
-- devices that have not been confirmed or remembered must provide a second
-- factor if your user pool requires MFA.
deviceConfigurationType_challengeRequiredOnNewDevice :: Lens.Lens' DeviceConfigurationType (Prelude.Maybe Prelude.Bool)
deviceConfigurationType_challengeRequiredOnNewDevice = Lens.lens (\DeviceConfigurationType' {challengeRequiredOnNewDevice} -> challengeRequiredOnNewDevice) (\s@DeviceConfigurationType' {} a -> s {challengeRequiredOnNewDevice = a} :: DeviceConfigurationType)

instance Core.FromJSON DeviceConfigurationType where
  parseJSON =
    Core.withObject
      "DeviceConfigurationType"
      ( \x ->
          DeviceConfigurationType'
            Prelude.<$> (x Core..:? "DeviceOnlyRememberedOnUserPrompt")
            Prelude.<*> (x Core..:? "ChallengeRequiredOnNewDevice")
      )

instance Prelude.Hashable DeviceConfigurationType where
  hashWithSalt _salt DeviceConfigurationType' {..} =
    _salt
      `Prelude.hashWithSalt` deviceOnlyRememberedOnUserPrompt
      `Prelude.hashWithSalt` challengeRequiredOnNewDevice

instance Prelude.NFData DeviceConfigurationType where
  rnf DeviceConfigurationType' {..} =
    Prelude.rnf deviceOnlyRememberedOnUserPrompt
      `Prelude.seq` Prelude.rnf challengeRequiredOnNewDevice

instance Core.ToJSON DeviceConfigurationType where
  toJSON DeviceConfigurationType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeviceOnlyRememberedOnUserPrompt" Core..=)
              Prelude.<$> deviceOnlyRememberedOnUserPrompt,
            ("ChallengeRequiredOnNewDevice" Core..=)
              Prelude.<$> challengeRequiredOnNewDevice
          ]
      )
