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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.DeviceConfigurationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The device-remembering configuration for a user pool. A
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_DescribeUserPool.html DescribeUserPool>
-- request returns a null value for this object when the user pool isn\'t
-- configured to remember devices. When device remembering is active, you
-- can remember a user\'s device with a
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ConfirmDevice.html ConfirmDevice>
-- API request. Additionally. when the property
-- @DeviceOnlyRememberedOnUserPrompt@ is @true@, you must follow
-- @ConfirmDevice@ with an
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UpdateDeviceStatus.html UpdateDeviceStatus>
-- API request that sets the user\'s device to @remembered@ or
-- @not_remembered@.
--
-- To sign in with a remembered device, include @DEVICE_KEY@ in the
-- authentication parameters in your user\'s
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>
-- request. If your app doesn\'t include a @DEVICE_KEY@ parameter, the
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html#API_InitiateAuth_ResponseSyntax response>
-- from Amazon Cognito includes newly-generated @DEVICE_KEY@ and
-- @DEVICE_GROUP_KEY@ values under @NewDeviceMetadata@. Store these values
-- to use in future device-authentication requests.
--
-- When you provide a value for any property of @DeviceConfiguration@, you
-- activate the device remembering for the user pool.
--
-- /See:/ 'newDeviceConfigurationType' smart constructor.
data DeviceConfigurationType = DeviceConfigurationType'
  { -- | When true, a remembered device can sign in with device authentication
    -- instead of SMS and time-based one-time password (TOTP) factors for
    -- multi-factor authentication (MFA).
    --
    -- Whether or not @ChallengeRequiredOnNewDevice@ is true, users who sign in
    -- with devices that have not been confirmed or remembered must still
    -- provide a second factor in a user pool that requires MFA.
    challengeRequiredOnNewDevice :: Prelude.Maybe Prelude.Bool,
    -- | When true, Amazon Cognito doesn\'t automatically remember a user\'s
    -- device when your app sends a
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ConfirmDevice.html ConfirmDevice>
    -- API request. In your app, create a prompt for your user to choose
    -- whether they want to remember their device. Return the user\'s choice in
    -- an
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UpdateDeviceStatus.html UpdateDeviceStatus>
    -- API request.
    --
    -- When @DeviceOnlyRememberedOnUserPrompt@ is @false@, Amazon Cognito
    -- immediately remembers devices that you register in a @ConfirmDevice@ API
    -- request.
    deviceOnlyRememberedOnUserPrompt :: Prelude.Maybe Prelude.Bool
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
-- 'challengeRequiredOnNewDevice', 'deviceConfigurationType_challengeRequiredOnNewDevice' - When true, a remembered device can sign in with device authentication
-- instead of SMS and time-based one-time password (TOTP) factors for
-- multi-factor authentication (MFA).
--
-- Whether or not @ChallengeRequiredOnNewDevice@ is true, users who sign in
-- with devices that have not been confirmed or remembered must still
-- provide a second factor in a user pool that requires MFA.
--
-- 'deviceOnlyRememberedOnUserPrompt', 'deviceConfigurationType_deviceOnlyRememberedOnUserPrompt' - When true, Amazon Cognito doesn\'t automatically remember a user\'s
-- device when your app sends a
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ConfirmDevice.html ConfirmDevice>
-- API request. In your app, create a prompt for your user to choose
-- whether they want to remember their device. Return the user\'s choice in
-- an
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UpdateDeviceStatus.html UpdateDeviceStatus>
-- API request.
--
-- When @DeviceOnlyRememberedOnUserPrompt@ is @false@, Amazon Cognito
-- immediately remembers devices that you register in a @ConfirmDevice@ API
-- request.
newDeviceConfigurationType ::
  DeviceConfigurationType
newDeviceConfigurationType =
  DeviceConfigurationType'
    { challengeRequiredOnNewDevice =
        Prelude.Nothing,
      deviceOnlyRememberedOnUserPrompt = Prelude.Nothing
    }

-- | When true, a remembered device can sign in with device authentication
-- instead of SMS and time-based one-time password (TOTP) factors for
-- multi-factor authentication (MFA).
--
-- Whether or not @ChallengeRequiredOnNewDevice@ is true, users who sign in
-- with devices that have not been confirmed or remembered must still
-- provide a second factor in a user pool that requires MFA.
deviceConfigurationType_challengeRequiredOnNewDevice :: Lens.Lens' DeviceConfigurationType (Prelude.Maybe Prelude.Bool)
deviceConfigurationType_challengeRequiredOnNewDevice = Lens.lens (\DeviceConfigurationType' {challengeRequiredOnNewDevice} -> challengeRequiredOnNewDevice) (\s@DeviceConfigurationType' {} a -> s {challengeRequiredOnNewDevice = a} :: DeviceConfigurationType)

-- | When true, Amazon Cognito doesn\'t automatically remember a user\'s
-- device when your app sends a
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ConfirmDevice.html ConfirmDevice>
-- API request. In your app, create a prompt for your user to choose
-- whether they want to remember their device. Return the user\'s choice in
-- an
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UpdateDeviceStatus.html UpdateDeviceStatus>
-- API request.
--
-- When @DeviceOnlyRememberedOnUserPrompt@ is @false@, Amazon Cognito
-- immediately remembers devices that you register in a @ConfirmDevice@ API
-- request.
deviceConfigurationType_deviceOnlyRememberedOnUserPrompt :: Lens.Lens' DeviceConfigurationType (Prelude.Maybe Prelude.Bool)
deviceConfigurationType_deviceOnlyRememberedOnUserPrompt = Lens.lens (\DeviceConfigurationType' {deviceOnlyRememberedOnUserPrompt} -> deviceOnlyRememberedOnUserPrompt) (\s@DeviceConfigurationType' {} a -> s {deviceOnlyRememberedOnUserPrompt = a} :: DeviceConfigurationType)

instance Data.FromJSON DeviceConfigurationType where
  parseJSON =
    Data.withObject
      "DeviceConfigurationType"
      ( \x ->
          DeviceConfigurationType'
            Prelude.<$> (x Data..:? "ChallengeRequiredOnNewDevice")
            Prelude.<*> (x Data..:? "DeviceOnlyRememberedOnUserPrompt")
      )

instance Prelude.Hashable DeviceConfigurationType where
  hashWithSalt _salt DeviceConfigurationType' {..} =
    _salt
      `Prelude.hashWithSalt` challengeRequiredOnNewDevice
      `Prelude.hashWithSalt` deviceOnlyRememberedOnUserPrompt

instance Prelude.NFData DeviceConfigurationType where
  rnf DeviceConfigurationType' {..} =
    Prelude.rnf challengeRequiredOnNewDevice
      `Prelude.seq` Prelude.rnf deviceOnlyRememberedOnUserPrompt

instance Data.ToJSON DeviceConfigurationType where
  toJSON DeviceConfigurationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChallengeRequiredOnNewDevice" Data..=)
              Prelude.<$> challengeRequiredOnNewDevice,
            ("DeviceOnlyRememberedOnUserPrompt" Data..=)
              Prelude.<$> deviceOnlyRememberedOnUserPrompt
          ]
      )
