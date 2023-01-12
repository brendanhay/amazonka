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
-- Module      : Amazonka.CognitoIdentityProvider.SignUp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the user in the specified user pool and creates a user name,
-- password, and user attributes.
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
module Amazonka.CognitoIdentityProvider.SignUp
  ( -- * Creating a Request
    SignUp (..),
    newSignUp,

    -- * Request Lenses
    signUp_analyticsMetadata,
    signUp_clientMetadata,
    signUp_secretHash,
    signUp_userAttributes,
    signUp_userContextData,
    signUp_validationData,
    signUp_clientId,
    signUp_username,
    signUp_password,

    -- * Destructuring the Response
    SignUpResponse (..),
    newSignUpResponse,

    -- * Response Lenses
    signUpResponse_codeDeliveryDetails,
    signUpResponse_httpStatus,
    signUpResponse_userConfirmed,
    signUpResponse_userSub,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to register a user.
--
-- /See:/ 'newSignUp' smart constructor.
data SignUp = SignUp'
  { -- | The Amazon Pinpoint analytics metadata that contributes to your metrics
    -- for @SignUp@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the SignUp API action, Amazon Cognito invokes any
    -- functions that are assigned to the following triggers: /pre sign-up/,
    -- /custom message/, and /post confirmation/. When Amazon Cognito invokes
    -- any of these functions, it passes a JSON payload, which the function
    -- receives as input. This payload contains a @clientMetadata@ attribute,
    -- which provides the data that you assigned to the ClientMetadata
    -- parameter in your SignUp request. In your function code in Lambda, you
    -- can process the @clientMetadata@ value to enhance your workflow for your
    -- specific needs.
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
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    userAttributes :: Prelude.Maybe [AttributeType],
    -- | Contextual data about your user session, such as the device fingerprint,
    -- IP address, or location. Amazon Cognito advanced security evaluates the
    -- risk of an authentication event based on the context that your app
    -- generates and passes to Amazon Cognito when it makes API requests.
    userContextData :: Prelude.Maybe UserContextDataType,
    -- | The validation data in the request to register a user.
    validationData :: Prelude.Maybe [AttributeType],
    -- | The ID of the client associated with the user pool.
    clientId :: Data.Sensitive Prelude.Text,
    -- | The user name of the user you want to register.
    username :: Data.Sensitive Prelude.Text,
    -- | The password of the user you want to register.
    password :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignUp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyticsMetadata', 'signUp_analyticsMetadata' - The Amazon Pinpoint analytics metadata that contributes to your metrics
-- for @SignUp@ calls.
--
-- 'clientMetadata', 'signUp_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the SignUp API action, Amazon Cognito invokes any
-- functions that are assigned to the following triggers: /pre sign-up/,
-- /custom message/, and /post confirmation/. When Amazon Cognito invokes
-- any of these functions, it passes a JSON payload, which the function
-- receives as input. This payload contains a @clientMetadata@ attribute,
-- which provides the data that you assigned to the ClientMetadata
-- parameter in your SignUp request. In your function code in Lambda, you
-- can process the @clientMetadata@ value to enhance your workflow for your
-- specific needs.
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
-- 'secretHash', 'signUp_secretHash' - A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
--
-- 'userAttributes', 'signUp_userAttributes' - An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
--
-- 'userContextData', 'signUp_userContextData' - Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
--
-- 'validationData', 'signUp_validationData' - The validation data in the request to register a user.
--
-- 'clientId', 'signUp_clientId' - The ID of the client associated with the user pool.
--
-- 'username', 'signUp_username' - The user name of the user you want to register.
--
-- 'password', 'signUp_password' - The password of the user you want to register.
newSignUp ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  SignUp
newSignUp pClientId_ pUsername_ pPassword_ =
  SignUp'
    { analyticsMetadata = Prelude.Nothing,
      clientMetadata = Prelude.Nothing,
      secretHash = Prelude.Nothing,
      userAttributes = Prelude.Nothing,
      userContextData = Prelude.Nothing,
      validationData = Prelude.Nothing,
      clientId = Data._Sensitive Lens.# pClientId_,
      username = Data._Sensitive Lens.# pUsername_,
      password = Data._Sensitive Lens.# pPassword_
    }

-- | The Amazon Pinpoint analytics metadata that contributes to your metrics
-- for @SignUp@ calls.
signUp_analyticsMetadata :: Lens.Lens' SignUp (Prelude.Maybe AnalyticsMetadataType)
signUp_analyticsMetadata = Lens.lens (\SignUp' {analyticsMetadata} -> analyticsMetadata) (\s@SignUp' {} a -> s {analyticsMetadata = a} :: SignUp)

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the SignUp API action, Amazon Cognito invokes any
-- functions that are assigned to the following triggers: /pre sign-up/,
-- /custom message/, and /post confirmation/. When Amazon Cognito invokes
-- any of these functions, it passes a JSON payload, which the function
-- receives as input. This payload contains a @clientMetadata@ attribute,
-- which provides the data that you assigned to the ClientMetadata
-- parameter in your SignUp request. In your function code in Lambda, you
-- can process the @clientMetadata@ value to enhance your workflow for your
-- specific needs.
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
signUp_clientMetadata :: Lens.Lens' SignUp (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
signUp_clientMetadata = Lens.lens (\SignUp' {clientMetadata} -> clientMetadata) (\s@SignUp' {} a -> s {clientMetadata = a} :: SignUp) Prelude.. Lens.mapping Lens.coerced

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
signUp_secretHash :: Lens.Lens' SignUp (Prelude.Maybe Prelude.Text)
signUp_secretHash = Lens.lens (\SignUp' {secretHash} -> secretHash) (\s@SignUp' {} a -> s {secretHash = a} :: SignUp) Prelude.. Lens.mapping Data._Sensitive

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
signUp_userAttributes :: Lens.Lens' SignUp (Prelude.Maybe [AttributeType])
signUp_userAttributes = Lens.lens (\SignUp' {userAttributes} -> userAttributes) (\s@SignUp' {} a -> s {userAttributes = a} :: SignUp) Prelude.. Lens.mapping Lens.coerced

-- | Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
signUp_userContextData :: Lens.Lens' SignUp (Prelude.Maybe UserContextDataType)
signUp_userContextData = Lens.lens (\SignUp' {userContextData} -> userContextData) (\s@SignUp' {} a -> s {userContextData = a} :: SignUp)

-- | The validation data in the request to register a user.
signUp_validationData :: Lens.Lens' SignUp (Prelude.Maybe [AttributeType])
signUp_validationData = Lens.lens (\SignUp' {validationData} -> validationData) (\s@SignUp' {} a -> s {validationData = a} :: SignUp) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the client associated with the user pool.
signUp_clientId :: Lens.Lens' SignUp Prelude.Text
signUp_clientId = Lens.lens (\SignUp' {clientId} -> clientId) (\s@SignUp' {} a -> s {clientId = a} :: SignUp) Prelude.. Data._Sensitive

-- | The user name of the user you want to register.
signUp_username :: Lens.Lens' SignUp Prelude.Text
signUp_username = Lens.lens (\SignUp' {username} -> username) (\s@SignUp' {} a -> s {username = a} :: SignUp) Prelude.. Data._Sensitive

-- | The password of the user you want to register.
signUp_password :: Lens.Lens' SignUp Prelude.Text
signUp_password = Lens.lens (\SignUp' {password} -> password) (\s@SignUp' {} a -> s {password = a} :: SignUp) Prelude.. Data._Sensitive

instance Core.AWSRequest SignUp where
  type AWSResponse SignUp = SignUpResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SignUpResponse'
            Prelude.<$> (x Data..?> "CodeDeliveryDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UserConfirmed")
            Prelude.<*> (x Data..:> "UserSub")
      )

instance Prelude.Hashable SignUp where
  hashWithSalt _salt SignUp' {..} =
    _salt `Prelude.hashWithSalt` analyticsMetadata
      `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` secretHash
      `Prelude.hashWithSalt` userAttributes
      `Prelude.hashWithSalt` userContextData
      `Prelude.hashWithSalt` validationData
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` password

instance Prelude.NFData SignUp where
  rnf SignUp' {..} =
    Prelude.rnf analyticsMetadata
      `Prelude.seq` Prelude.rnf clientMetadata
      `Prelude.seq` Prelude.rnf secretHash
      `Prelude.seq` Prelude.rnf userAttributes
      `Prelude.seq` Prelude.rnf userContextData
      `Prelude.seq` Prelude.rnf validationData
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance Data.ToHeaders SignUp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.SignUp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SignUp where
  toJSON SignUp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnalyticsMetadata" Data..=)
              Prelude.<$> analyticsMetadata,
            ("ClientMetadata" Data..=)
              Prelude.<$> clientMetadata,
            ("SecretHash" Data..=) Prelude.<$> secretHash,
            ("UserAttributes" Data..=)
              Prelude.<$> userAttributes,
            ("UserContextData" Data..=)
              Prelude.<$> userContextData,
            ("ValidationData" Data..=)
              Prelude.<$> validationData,
            Prelude.Just ("ClientId" Data..= clientId),
            Prelude.Just ("Username" Data..= username),
            Prelude.Just ("Password" Data..= password)
          ]
      )

instance Data.ToPath SignUp where
  toPath = Prelude.const "/"

instance Data.ToQuery SignUp where
  toQuery = Prelude.const Prelude.mempty

-- | The response from the server for a registration request.
--
-- /See:/ 'newSignUpResponse' smart constructor.
data SignUpResponse = SignUpResponse'
  { -- | The code delivery details returned by the server response to the user
    -- registration request.
    codeDeliveryDetails :: Prelude.Maybe CodeDeliveryDetailsType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A response from the server indicating that a user registration has been
    -- confirmed.
    userConfirmed :: Prelude.Bool,
    -- | The UUID of the authenticated user. This isn\'t the same as @username@.
    userSub :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignUpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeDeliveryDetails', 'signUpResponse_codeDeliveryDetails' - The code delivery details returned by the server response to the user
-- registration request.
--
-- 'httpStatus', 'signUpResponse_httpStatus' - The response's http status code.
--
-- 'userConfirmed', 'signUpResponse_userConfirmed' - A response from the server indicating that a user registration has been
-- confirmed.
--
-- 'userSub', 'signUpResponse_userSub' - The UUID of the authenticated user. This isn\'t the same as @username@.
newSignUpResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'userConfirmed'
  Prelude.Bool ->
  -- | 'userSub'
  Prelude.Text ->
  SignUpResponse
newSignUpResponse
  pHttpStatus_
  pUserConfirmed_
  pUserSub_ =
    SignUpResponse'
      { codeDeliveryDetails =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        userConfirmed = pUserConfirmed_,
        userSub = pUserSub_
      }

-- | The code delivery details returned by the server response to the user
-- registration request.
signUpResponse_codeDeliveryDetails :: Lens.Lens' SignUpResponse (Prelude.Maybe CodeDeliveryDetailsType)
signUpResponse_codeDeliveryDetails = Lens.lens (\SignUpResponse' {codeDeliveryDetails} -> codeDeliveryDetails) (\s@SignUpResponse' {} a -> s {codeDeliveryDetails = a} :: SignUpResponse)

-- | The response's http status code.
signUpResponse_httpStatus :: Lens.Lens' SignUpResponse Prelude.Int
signUpResponse_httpStatus = Lens.lens (\SignUpResponse' {httpStatus} -> httpStatus) (\s@SignUpResponse' {} a -> s {httpStatus = a} :: SignUpResponse)

-- | A response from the server indicating that a user registration has been
-- confirmed.
signUpResponse_userConfirmed :: Lens.Lens' SignUpResponse Prelude.Bool
signUpResponse_userConfirmed = Lens.lens (\SignUpResponse' {userConfirmed} -> userConfirmed) (\s@SignUpResponse' {} a -> s {userConfirmed = a} :: SignUpResponse)

-- | The UUID of the authenticated user. This isn\'t the same as @username@.
signUpResponse_userSub :: Lens.Lens' SignUpResponse Prelude.Text
signUpResponse_userSub = Lens.lens (\SignUpResponse' {userSub} -> userSub) (\s@SignUpResponse' {} a -> s {userSub = a} :: SignUpResponse)

instance Prelude.NFData SignUpResponse where
  rnf SignUpResponse' {..} =
    Prelude.rnf codeDeliveryDetails
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf userConfirmed
      `Prelude.seq` Prelude.rnf userSub
