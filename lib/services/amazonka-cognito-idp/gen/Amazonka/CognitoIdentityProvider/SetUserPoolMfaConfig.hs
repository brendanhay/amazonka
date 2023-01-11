{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoIdentityProvider.SetUserPoolMfaConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the user pool multi-factor authentication (MFA) configuration.
--
-- This action might generate an SMS text message. Starting June 1, 2021,
-- US telecom carriers require you to register an origination phone number
-- before you can send SMS messages to US phone numbers. If you use SMS
-- text messages in Amazon Cognito, you must register a phone number with
-- <https://console.aws.amazon.com/pinpoint/home/ Amazon Pinpoint>. Amazon
-- Cognito uses the registered number automatically. Otherwise, Amazon
-- Cognito users who must receive SMS messages might not be able to sign
-- up, activate their accounts, or sign in.
--
-- If you have never used SMS text messages with Amazon Cognito or any
-- other Amazon Web Service, Amazon Simple Notification Service might place
-- your account in the SMS sandbox. In
-- /<https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox.html sandbox mode>/
-- , you can send messages only to verified phone numbers. After you test
-- your app while in the sandbox environment, you can move out of the
-- sandbox and into production. For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-sms-userpool-settings.html SMS message settings for Amazon Cognito user pools>
-- in the /Amazon Cognito Developer Guide/.
module Amazonka.CognitoIdentityProvider.SetUserPoolMfaConfig
  ( -- * Creating a Request
    SetUserPoolMfaConfig (..),
    newSetUserPoolMfaConfig,

    -- * Request Lenses
    setUserPoolMfaConfig_mfaConfiguration,
    setUserPoolMfaConfig_smsMfaConfiguration,
    setUserPoolMfaConfig_softwareTokenMfaConfiguration,
    setUserPoolMfaConfig_userPoolId,

    -- * Destructuring the Response
    SetUserPoolMfaConfigResponse (..),
    newSetUserPoolMfaConfigResponse,

    -- * Response Lenses
    setUserPoolMfaConfigResponse_mfaConfiguration,
    setUserPoolMfaConfigResponse_smsMfaConfiguration,
    setUserPoolMfaConfigResponse_softwareTokenMfaConfiguration,
    setUserPoolMfaConfigResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetUserPoolMfaConfig' smart constructor.
data SetUserPoolMfaConfig = SetUserPoolMfaConfig'
  { -- | The MFA configuration. If you set the MfaConfiguration value to ‘ON’,
    -- only users who have set up an MFA factor can sign in. To learn more, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-mfa.html Adding Multi-Factor Authentication (MFA) to a user pool>.
    -- Valid values include:
    --
    -- -   @OFF@ MFA won\'t be used for any users.
    --
    -- -   @ON@ MFA is required for all users to sign in.
    --
    -- -   @OPTIONAL@ MFA will be required only for individual users who have
    --     an MFA factor activated.
    mfaConfiguration :: Prelude.Maybe UserPoolMfaType,
    -- | The SMS text message MFA configuration.
    smsMfaConfiguration :: Prelude.Maybe SmsMfaConfigType,
    -- | The software token MFA configuration.
    softwareTokenMfaConfiguration :: Prelude.Maybe SoftwareTokenMfaConfigType,
    -- | The user pool ID.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetUserPoolMfaConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mfaConfiguration', 'setUserPoolMfaConfig_mfaConfiguration' - The MFA configuration. If you set the MfaConfiguration value to ‘ON’,
-- only users who have set up an MFA factor can sign in. To learn more, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-mfa.html Adding Multi-Factor Authentication (MFA) to a user pool>.
-- Valid values include:
--
-- -   @OFF@ MFA won\'t be used for any users.
--
-- -   @ON@ MFA is required for all users to sign in.
--
-- -   @OPTIONAL@ MFA will be required only for individual users who have
--     an MFA factor activated.
--
-- 'smsMfaConfiguration', 'setUserPoolMfaConfig_smsMfaConfiguration' - The SMS text message MFA configuration.
--
-- 'softwareTokenMfaConfiguration', 'setUserPoolMfaConfig_softwareTokenMfaConfiguration' - The software token MFA configuration.
--
-- 'userPoolId', 'setUserPoolMfaConfig_userPoolId' - The user pool ID.
newSetUserPoolMfaConfig ::
  -- | 'userPoolId'
  Prelude.Text ->
  SetUserPoolMfaConfig
newSetUserPoolMfaConfig pUserPoolId_ =
  SetUserPoolMfaConfig'
    { mfaConfiguration =
        Prelude.Nothing,
      smsMfaConfiguration = Prelude.Nothing,
      softwareTokenMfaConfiguration = Prelude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The MFA configuration. If you set the MfaConfiguration value to ‘ON’,
-- only users who have set up an MFA factor can sign in. To learn more, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-mfa.html Adding Multi-Factor Authentication (MFA) to a user pool>.
-- Valid values include:
--
-- -   @OFF@ MFA won\'t be used for any users.
--
-- -   @ON@ MFA is required for all users to sign in.
--
-- -   @OPTIONAL@ MFA will be required only for individual users who have
--     an MFA factor activated.
setUserPoolMfaConfig_mfaConfiguration :: Lens.Lens' SetUserPoolMfaConfig (Prelude.Maybe UserPoolMfaType)
setUserPoolMfaConfig_mfaConfiguration = Lens.lens (\SetUserPoolMfaConfig' {mfaConfiguration} -> mfaConfiguration) (\s@SetUserPoolMfaConfig' {} a -> s {mfaConfiguration = a} :: SetUserPoolMfaConfig)

-- | The SMS text message MFA configuration.
setUserPoolMfaConfig_smsMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfig (Prelude.Maybe SmsMfaConfigType)
setUserPoolMfaConfig_smsMfaConfiguration = Lens.lens (\SetUserPoolMfaConfig' {smsMfaConfiguration} -> smsMfaConfiguration) (\s@SetUserPoolMfaConfig' {} a -> s {smsMfaConfiguration = a} :: SetUserPoolMfaConfig)

-- | The software token MFA configuration.
setUserPoolMfaConfig_softwareTokenMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfig (Prelude.Maybe SoftwareTokenMfaConfigType)
setUserPoolMfaConfig_softwareTokenMfaConfiguration = Lens.lens (\SetUserPoolMfaConfig' {softwareTokenMfaConfiguration} -> softwareTokenMfaConfiguration) (\s@SetUserPoolMfaConfig' {} a -> s {softwareTokenMfaConfiguration = a} :: SetUserPoolMfaConfig)

-- | The user pool ID.
setUserPoolMfaConfig_userPoolId :: Lens.Lens' SetUserPoolMfaConfig Prelude.Text
setUserPoolMfaConfig_userPoolId = Lens.lens (\SetUserPoolMfaConfig' {userPoolId} -> userPoolId) (\s@SetUserPoolMfaConfig' {} a -> s {userPoolId = a} :: SetUserPoolMfaConfig)

instance Core.AWSRequest SetUserPoolMfaConfig where
  type
    AWSResponse SetUserPoolMfaConfig =
      SetUserPoolMfaConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SetUserPoolMfaConfigResponse'
            Prelude.<$> (x Data..?> "MfaConfiguration")
            Prelude.<*> (x Data..?> "SmsMfaConfiguration")
            Prelude.<*> (x Data..?> "SoftwareTokenMfaConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetUserPoolMfaConfig where
  hashWithSalt _salt SetUserPoolMfaConfig' {..} =
    _salt `Prelude.hashWithSalt` mfaConfiguration
      `Prelude.hashWithSalt` smsMfaConfiguration
      `Prelude.hashWithSalt` softwareTokenMfaConfiguration
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData SetUserPoolMfaConfig where
  rnf SetUserPoolMfaConfig' {..} =
    Prelude.rnf mfaConfiguration
      `Prelude.seq` Prelude.rnf smsMfaConfiguration
      `Prelude.seq` Prelude.rnf softwareTokenMfaConfiguration
      `Prelude.seq` Prelude.rnf userPoolId

instance Data.ToHeaders SetUserPoolMfaConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.SetUserPoolMfaConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetUserPoolMfaConfig where
  toJSON SetUserPoolMfaConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MfaConfiguration" Data..=)
              Prelude.<$> mfaConfiguration,
            ("SmsMfaConfiguration" Data..=)
              Prelude.<$> smsMfaConfiguration,
            ("SoftwareTokenMfaConfiguration" Data..=)
              Prelude.<$> softwareTokenMfaConfiguration,
            Prelude.Just ("UserPoolId" Data..= userPoolId)
          ]
      )

instance Data.ToPath SetUserPoolMfaConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery SetUserPoolMfaConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetUserPoolMfaConfigResponse' smart constructor.
data SetUserPoolMfaConfigResponse = SetUserPoolMfaConfigResponse'
  { -- | The MFA configuration. Valid values include:
    --
    -- -   @OFF@ MFA won\'t be used for any users.
    --
    -- -   @ON@ MFA is required for all users to sign in.
    --
    -- -   @OPTIONAL@ MFA will be required only for individual users who have
    --     an MFA factor enabled.
    mfaConfiguration :: Prelude.Maybe UserPoolMfaType,
    -- | The SMS text message MFA configuration.
    smsMfaConfiguration :: Prelude.Maybe SmsMfaConfigType,
    -- | The software token MFA configuration.
    softwareTokenMfaConfiguration :: Prelude.Maybe SoftwareTokenMfaConfigType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetUserPoolMfaConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mfaConfiguration', 'setUserPoolMfaConfigResponse_mfaConfiguration' - The MFA configuration. Valid values include:
--
-- -   @OFF@ MFA won\'t be used for any users.
--
-- -   @ON@ MFA is required for all users to sign in.
--
-- -   @OPTIONAL@ MFA will be required only for individual users who have
--     an MFA factor enabled.
--
-- 'smsMfaConfiguration', 'setUserPoolMfaConfigResponse_smsMfaConfiguration' - The SMS text message MFA configuration.
--
-- 'softwareTokenMfaConfiguration', 'setUserPoolMfaConfigResponse_softwareTokenMfaConfiguration' - The software token MFA configuration.
--
-- 'httpStatus', 'setUserPoolMfaConfigResponse_httpStatus' - The response's http status code.
newSetUserPoolMfaConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetUserPoolMfaConfigResponse
newSetUserPoolMfaConfigResponse pHttpStatus_ =
  SetUserPoolMfaConfigResponse'
    { mfaConfiguration =
        Prelude.Nothing,
      smsMfaConfiguration = Prelude.Nothing,
      softwareTokenMfaConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The MFA configuration. Valid values include:
--
-- -   @OFF@ MFA won\'t be used for any users.
--
-- -   @ON@ MFA is required for all users to sign in.
--
-- -   @OPTIONAL@ MFA will be required only for individual users who have
--     an MFA factor enabled.
setUserPoolMfaConfigResponse_mfaConfiguration :: Lens.Lens' SetUserPoolMfaConfigResponse (Prelude.Maybe UserPoolMfaType)
setUserPoolMfaConfigResponse_mfaConfiguration = Lens.lens (\SetUserPoolMfaConfigResponse' {mfaConfiguration} -> mfaConfiguration) (\s@SetUserPoolMfaConfigResponse' {} a -> s {mfaConfiguration = a} :: SetUserPoolMfaConfigResponse)

-- | The SMS text message MFA configuration.
setUserPoolMfaConfigResponse_smsMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfigResponse (Prelude.Maybe SmsMfaConfigType)
setUserPoolMfaConfigResponse_smsMfaConfiguration = Lens.lens (\SetUserPoolMfaConfigResponse' {smsMfaConfiguration} -> smsMfaConfiguration) (\s@SetUserPoolMfaConfigResponse' {} a -> s {smsMfaConfiguration = a} :: SetUserPoolMfaConfigResponse)

-- | The software token MFA configuration.
setUserPoolMfaConfigResponse_softwareTokenMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfigResponse (Prelude.Maybe SoftwareTokenMfaConfigType)
setUserPoolMfaConfigResponse_softwareTokenMfaConfiguration = Lens.lens (\SetUserPoolMfaConfigResponse' {softwareTokenMfaConfiguration} -> softwareTokenMfaConfiguration) (\s@SetUserPoolMfaConfigResponse' {} a -> s {softwareTokenMfaConfiguration = a} :: SetUserPoolMfaConfigResponse)

-- | The response's http status code.
setUserPoolMfaConfigResponse_httpStatus :: Lens.Lens' SetUserPoolMfaConfigResponse Prelude.Int
setUserPoolMfaConfigResponse_httpStatus = Lens.lens (\SetUserPoolMfaConfigResponse' {httpStatus} -> httpStatus) (\s@SetUserPoolMfaConfigResponse' {} a -> s {httpStatus = a} :: SetUserPoolMfaConfigResponse)

instance Prelude.NFData SetUserPoolMfaConfigResponse where
  rnf SetUserPoolMfaConfigResponse' {..} =
    Prelude.rnf mfaConfiguration
      `Prelude.seq` Prelude.rnf smsMfaConfiguration
      `Prelude.seq` Prelude.rnf softwareTokenMfaConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
