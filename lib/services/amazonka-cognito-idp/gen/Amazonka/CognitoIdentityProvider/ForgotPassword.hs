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
-- Module      : Amazonka.CognitoIdentityProvider.ForgotPassword
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Calling this API causes a message to be sent to the end user with a
-- confirmation code that is required to change the user\'s password. For
-- the @Username@ parameter, you can use the username or user alias. The
-- method used to send the confirmation code is sent according to the
-- specified AccountRecoverySetting. For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-recover-a-user-account.html Recovering User Accounts>
-- in the /Amazon Cognito Developer Guide/. If neither a verified phone
-- number nor a verified email exists, an @InvalidParameterException@ is
-- thrown. To use the confirmation code for resetting the password, call
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ConfirmForgotPassword.html ConfirmForgotPassword>.
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
module Amazonka.CognitoIdentityProvider.ForgotPassword
  ( -- * Creating a Request
    ForgotPassword (..),
    newForgotPassword,

    -- * Request Lenses
    forgotPassword_analyticsMetadata,
    forgotPassword_clientMetadata,
    forgotPassword_secretHash,
    forgotPassword_userContextData,
    forgotPassword_clientId,
    forgotPassword_username,

    -- * Destructuring the Response
    ForgotPasswordResponse (..),
    newForgotPasswordResponse,

    -- * Response Lenses
    forgotPasswordResponse_codeDeliveryDetails,
    forgotPasswordResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to reset a user\'s password.
--
-- /See:/ 'newForgotPassword' smart constructor.
data ForgotPassword = ForgotPassword'
  { -- | The Amazon Pinpoint analytics metadata that contributes to your metrics
    -- for @ForgotPassword@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the ForgotPassword API action, Amazon Cognito
    -- invokes any functions that are assigned to the following triggers: /pre
    -- sign-up/, /custom message/, and /user migration/. When Amazon Cognito
    -- invokes any of these functions, it passes a JSON payload, which the
    -- function receives as input. This payload contains a @clientMetadata@
    -- attribute, which provides the data that you assigned to the
    -- ClientMetadata parameter in your ForgotPassword request. In your
    -- function code in Lambda, you can process the @clientMetadata@ value to
    -- enhance your workflow for your specific needs.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing user pool Workflows with Lambda Triggers>
    -- in the /Amazon Cognito Developer Guide/.
    --
    -- When you use the ClientMetadata parameter, remember that Amazon Cognito
    -- won\'t do the following:
    --
    -- -   Store the ClientMetadata value. This data is available only to
    --     Lambda triggers that are assigned to a user pool to support custom
    --     workflows. If your user pool configuration doesn\'t include
    --     triggers, the ClientMetadata parameter serves no purpose.
    --
    -- -   Validate the ClientMetadata value.
    --
    -- -   Encrypt the ClientMetadata value. Don\'t use Amazon Cognito to
    --     provide sensitive information.
    clientMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A keyed-hash message authentication code (HMAC) calculated using the
    -- secret key of a user pool client and username plus the client ID in the
    -- message.
    secretHash :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Contextual data about your user session, such as the device fingerprint,
    -- IP address, or location. Amazon Cognito advanced security evaluates the
    -- risk of an authentication event based on the context that your app
    -- generates and passes to Amazon Cognito when it makes API requests.
    userContextData :: Prelude.Maybe UserContextDataType,
    -- | The ID of the client associated with the user pool.
    clientId :: Data.Sensitive Prelude.Text,
    -- | The user name of the user for whom you want to enter a code to reset a
    -- forgotten password.
    username :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForgotPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyticsMetadata', 'forgotPassword_analyticsMetadata' - The Amazon Pinpoint analytics metadata that contributes to your metrics
-- for @ForgotPassword@ calls.
--
-- 'clientMetadata', 'forgotPassword_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the ForgotPassword API action, Amazon Cognito
-- invokes any functions that are assigned to the following triggers: /pre
-- sign-up/, /custom message/, and /user migration/. When Amazon Cognito
-- invokes any of these functions, it passes a JSON payload, which the
-- function receives as input. This payload contains a @clientMetadata@
-- attribute, which provides the data that you assigned to the
-- ClientMetadata parameter in your ForgotPassword request. In your
-- function code in Lambda, you can process the @clientMetadata@ value to
-- enhance your workflow for your specific needs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing user pool Workflows with Lambda Triggers>
-- in the /Amazon Cognito Developer Guide/.
--
-- When you use the ClientMetadata parameter, remember that Amazon Cognito
-- won\'t do the following:
--
-- -   Store the ClientMetadata value. This data is available only to
--     Lambda triggers that are assigned to a user pool to support custom
--     workflows. If your user pool configuration doesn\'t include
--     triggers, the ClientMetadata parameter serves no purpose.
--
-- -   Validate the ClientMetadata value.
--
-- -   Encrypt the ClientMetadata value. Don\'t use Amazon Cognito to
--     provide sensitive information.
--
-- 'secretHash', 'forgotPassword_secretHash' - A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
--
-- 'userContextData', 'forgotPassword_userContextData' - Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
--
-- 'clientId', 'forgotPassword_clientId' - The ID of the client associated with the user pool.
--
-- 'username', 'forgotPassword_username' - The user name of the user for whom you want to enter a code to reset a
-- forgotten password.
newForgotPassword ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  ForgotPassword
newForgotPassword pClientId_ pUsername_ =
  ForgotPassword'
    { analyticsMetadata =
        Prelude.Nothing,
      clientMetadata = Prelude.Nothing,
      secretHash = Prelude.Nothing,
      userContextData = Prelude.Nothing,
      clientId = Data._Sensitive Lens.# pClientId_,
      username = Data._Sensitive Lens.# pUsername_
    }

-- | The Amazon Pinpoint analytics metadata that contributes to your metrics
-- for @ForgotPassword@ calls.
forgotPassword_analyticsMetadata :: Lens.Lens' ForgotPassword (Prelude.Maybe AnalyticsMetadataType)
forgotPassword_analyticsMetadata = Lens.lens (\ForgotPassword' {analyticsMetadata} -> analyticsMetadata) (\s@ForgotPassword' {} a -> s {analyticsMetadata = a} :: ForgotPassword)

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the ForgotPassword API action, Amazon Cognito
-- invokes any functions that are assigned to the following triggers: /pre
-- sign-up/, /custom message/, and /user migration/. When Amazon Cognito
-- invokes any of these functions, it passes a JSON payload, which the
-- function receives as input. This payload contains a @clientMetadata@
-- attribute, which provides the data that you assigned to the
-- ClientMetadata parameter in your ForgotPassword request. In your
-- function code in Lambda, you can process the @clientMetadata@ value to
-- enhance your workflow for your specific needs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing user pool Workflows with Lambda Triggers>
-- in the /Amazon Cognito Developer Guide/.
--
-- When you use the ClientMetadata parameter, remember that Amazon Cognito
-- won\'t do the following:
--
-- -   Store the ClientMetadata value. This data is available only to
--     Lambda triggers that are assigned to a user pool to support custom
--     workflows. If your user pool configuration doesn\'t include
--     triggers, the ClientMetadata parameter serves no purpose.
--
-- -   Validate the ClientMetadata value.
--
-- -   Encrypt the ClientMetadata value. Don\'t use Amazon Cognito to
--     provide sensitive information.
forgotPassword_clientMetadata :: Lens.Lens' ForgotPassword (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
forgotPassword_clientMetadata = Lens.lens (\ForgotPassword' {clientMetadata} -> clientMetadata) (\s@ForgotPassword' {} a -> s {clientMetadata = a} :: ForgotPassword) Prelude.. Lens.mapping Lens.coerced

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
forgotPassword_secretHash :: Lens.Lens' ForgotPassword (Prelude.Maybe Prelude.Text)
forgotPassword_secretHash = Lens.lens (\ForgotPassword' {secretHash} -> secretHash) (\s@ForgotPassword' {} a -> s {secretHash = a} :: ForgotPassword) Prelude.. Lens.mapping Data._Sensitive

-- | Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
forgotPassword_userContextData :: Lens.Lens' ForgotPassword (Prelude.Maybe UserContextDataType)
forgotPassword_userContextData = Lens.lens (\ForgotPassword' {userContextData} -> userContextData) (\s@ForgotPassword' {} a -> s {userContextData = a} :: ForgotPassword)

-- | The ID of the client associated with the user pool.
forgotPassword_clientId :: Lens.Lens' ForgotPassword Prelude.Text
forgotPassword_clientId = Lens.lens (\ForgotPassword' {clientId} -> clientId) (\s@ForgotPassword' {} a -> s {clientId = a} :: ForgotPassword) Prelude.. Data._Sensitive

-- | The user name of the user for whom you want to enter a code to reset a
-- forgotten password.
forgotPassword_username :: Lens.Lens' ForgotPassword Prelude.Text
forgotPassword_username = Lens.lens (\ForgotPassword' {username} -> username) (\s@ForgotPassword' {} a -> s {username = a} :: ForgotPassword) Prelude.. Data._Sensitive

instance Core.AWSRequest ForgotPassword where
  type
    AWSResponse ForgotPassword =
      ForgotPasswordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ForgotPasswordResponse'
            Prelude.<$> (x Data..?> "CodeDeliveryDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ForgotPassword where
  hashWithSalt _salt ForgotPassword' {..} =
    _salt
      `Prelude.hashWithSalt` analyticsMetadata
      `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` secretHash
      `Prelude.hashWithSalt` userContextData
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` username

instance Prelude.NFData ForgotPassword where
  rnf ForgotPassword' {..} =
    Prelude.rnf analyticsMetadata `Prelude.seq`
      Prelude.rnf clientMetadata `Prelude.seq`
        Prelude.rnf secretHash `Prelude.seq`
          Prelude.rnf userContextData `Prelude.seq`
            Prelude.rnf clientId `Prelude.seq`
              Prelude.rnf username

instance Data.ToHeaders ForgotPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.ForgotPassword" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ForgotPassword where
  toJSON ForgotPassword' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnalyticsMetadata" Data..=)
              Prelude.<$> analyticsMetadata,
            ("ClientMetadata" Data..=)
              Prelude.<$> clientMetadata,
            ("SecretHash" Data..=) Prelude.<$> secretHash,
            ("UserContextData" Data..=)
              Prelude.<$> userContextData,
            Prelude.Just ("ClientId" Data..= clientId),
            Prelude.Just ("Username" Data..= username)
          ]
      )

instance Data.ToPath ForgotPassword where
  toPath = Prelude.const "/"

instance Data.ToQuery ForgotPassword where
  toQuery = Prelude.const Prelude.mempty

-- | The response from Amazon Cognito to a request to reset a password.
--
-- /See:/ 'newForgotPasswordResponse' smart constructor.
data ForgotPasswordResponse = ForgotPasswordResponse'
  { -- | The code delivery details returned by the server in response to the
    -- request to reset a password.
    codeDeliveryDetails :: Prelude.Maybe CodeDeliveryDetailsType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForgotPasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeDeliveryDetails', 'forgotPasswordResponse_codeDeliveryDetails' - The code delivery details returned by the server in response to the
-- request to reset a password.
--
-- 'httpStatus', 'forgotPasswordResponse_httpStatus' - The response's http status code.
newForgotPasswordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ForgotPasswordResponse
newForgotPasswordResponse pHttpStatus_ =
  ForgotPasswordResponse'
    { codeDeliveryDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The code delivery details returned by the server in response to the
-- request to reset a password.
forgotPasswordResponse_codeDeliveryDetails :: Lens.Lens' ForgotPasswordResponse (Prelude.Maybe CodeDeliveryDetailsType)
forgotPasswordResponse_codeDeliveryDetails = Lens.lens (\ForgotPasswordResponse' {codeDeliveryDetails} -> codeDeliveryDetails) (\s@ForgotPasswordResponse' {} a -> s {codeDeliveryDetails = a} :: ForgotPasswordResponse)

-- | The response's http status code.
forgotPasswordResponse_httpStatus :: Lens.Lens' ForgotPasswordResponse Prelude.Int
forgotPasswordResponse_httpStatus = Lens.lens (\ForgotPasswordResponse' {httpStatus} -> httpStatus) (\s@ForgotPasswordResponse' {} a -> s {httpStatus = a} :: ForgotPasswordResponse)

instance Prelude.NFData ForgotPasswordResponse where
  rnf ForgotPasswordResponse' {..} =
    Prelude.rnf codeDeliveryDetails `Prelude.seq`
      Prelude.rnf httpStatus
