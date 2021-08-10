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
-- Module      : Network.AWS.CognitoIdentityProvider.SignUp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the user in the specified user pool and creates a user name,
-- password, and user attributes.
module Network.AWS.CognitoIdentityProvider.SignUp
  ( -- * Creating a Request
    SignUp (..),
    newSignUp,

    -- * Request Lenses
    signUp_clientMetadata,
    signUp_userContextData,
    signUp_secretHash,
    signUp_userAttributes,
    signUp_validationData,
    signUp_analyticsMetadata,
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to register a user.
--
-- /See:/ 'newSignUp' smart constructor.
data SignUp = SignUp'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the SignUp API action, Amazon Cognito
    -- invokes any functions that are assigned to the following triggers: /pre
    -- sign-up/, /custom message/, and /post confirmation/. When Amazon Cognito
    -- invokes any of these functions, it passes a JSON payload, which the
    -- function receives as input. This payload contains a @clientMetadata@
    -- attribute, which provides the data that you assigned to the
    -- ClientMetadata parameter in your SignUp request. In your function code
    -- in AWS Lambda, you can process the @clientMetadata@ value to enhance
    -- your workflow for your specific needs.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers>
    -- in the /Amazon Cognito Developer Guide/.
    --
    -- Take the following limitations into consideration when you use the
    -- ClientMetadata parameter:
    --
    -- -   Amazon Cognito does not store the ClientMetadata value. This data is
    --     available only to AWS Lambda triggers that are assigned to a user
    --     pool to support custom workflows. If your user pool configuration
    --     does not include triggers, the ClientMetadata parameter serves no
    --     purpose.
    --
    -- -   Amazon Cognito does not validate the ClientMetadata value.
    --
    -- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
    --     don\'t use it to provide sensitive information.
    clientMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contextual data such as the user\'s device fingerprint, IP address, or
    -- location used for evaluating the risk of an unexpected event by Amazon
    -- Cognito advanced security.
    userContextData :: Prelude.Maybe UserContextDataType,
    -- | A keyed-hash message authentication code (HMAC) calculated using the
    -- secret key of a user pool client and username plus the client ID in the
    -- message.
    secretHash :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    userAttributes :: Prelude.Maybe [AttributeType],
    -- | The validation data in the request to register a user.
    validationData :: Prelude.Maybe [AttributeType],
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for
    -- @SignUp@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | The ID of the client associated with the user pool.
    clientId :: Core.Sensitive Prelude.Text,
    -- | The user name of the user you wish to register.
    username :: Core.Sensitive Prelude.Text,
    -- | The password of the user you wish to register.
    password :: Core.Sensitive Prelude.Text
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
-- 'clientMetadata', 'signUp_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the SignUp API action, Amazon Cognito
-- invokes any functions that are assigned to the following triggers: /pre
-- sign-up/, /custom message/, and /post confirmation/. When Amazon Cognito
-- invokes any of these functions, it passes a JSON payload, which the
-- function receives as input. This payload contains a @clientMetadata@
-- attribute, which provides the data that you assigned to the
-- ClientMetadata parameter in your SignUp request. In your function code
-- in AWS Lambda, you can process the @clientMetadata@ value to enhance
-- your workflow for your specific needs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers>
-- in the /Amazon Cognito Developer Guide/.
--
-- Take the following limitations into consideration when you use the
-- ClientMetadata parameter:
--
-- -   Amazon Cognito does not store the ClientMetadata value. This data is
--     available only to AWS Lambda triggers that are assigned to a user
--     pool to support custom workflows. If your user pool configuration
--     does not include triggers, the ClientMetadata parameter serves no
--     purpose.
--
-- -   Amazon Cognito does not validate the ClientMetadata value.
--
-- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
--     don\'t use it to provide sensitive information.
--
-- 'userContextData', 'signUp_userContextData' - Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
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
-- 'validationData', 'signUp_validationData' - The validation data in the request to register a user.
--
-- 'analyticsMetadata', 'signUp_analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for
-- @SignUp@ calls.
--
-- 'clientId', 'signUp_clientId' - The ID of the client associated with the user pool.
--
-- 'username', 'signUp_username' - The user name of the user you wish to register.
--
-- 'password', 'signUp_password' - The password of the user you wish to register.
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
    { clientMetadata = Prelude.Nothing,
      userContextData = Prelude.Nothing,
      secretHash = Prelude.Nothing,
      userAttributes = Prelude.Nothing,
      validationData = Prelude.Nothing,
      analyticsMetadata = Prelude.Nothing,
      clientId = Core._Sensitive Lens.# pClientId_,
      username = Core._Sensitive Lens.# pUsername_,
      password = Core._Sensitive Lens.# pPassword_
    }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the SignUp API action, Amazon Cognito
-- invokes any functions that are assigned to the following triggers: /pre
-- sign-up/, /custom message/, and /post confirmation/. When Amazon Cognito
-- invokes any of these functions, it passes a JSON payload, which the
-- function receives as input. This payload contains a @clientMetadata@
-- attribute, which provides the data that you assigned to the
-- ClientMetadata parameter in your SignUp request. In your function code
-- in AWS Lambda, you can process the @clientMetadata@ value to enhance
-- your workflow for your specific needs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers>
-- in the /Amazon Cognito Developer Guide/.
--
-- Take the following limitations into consideration when you use the
-- ClientMetadata parameter:
--
-- -   Amazon Cognito does not store the ClientMetadata value. This data is
--     available only to AWS Lambda triggers that are assigned to a user
--     pool to support custom workflows. If your user pool configuration
--     does not include triggers, the ClientMetadata parameter serves no
--     purpose.
--
-- -   Amazon Cognito does not validate the ClientMetadata value.
--
-- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
--     don\'t use it to provide sensitive information.
signUp_clientMetadata :: Lens.Lens' SignUp (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
signUp_clientMetadata = Lens.lens (\SignUp' {clientMetadata} -> clientMetadata) (\s@SignUp' {} a -> s {clientMetadata = a} :: SignUp) Prelude.. Lens.mapping Lens._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
signUp_userContextData :: Lens.Lens' SignUp (Prelude.Maybe UserContextDataType)
signUp_userContextData = Lens.lens (\SignUp' {userContextData} -> userContextData) (\s@SignUp' {} a -> s {userContextData = a} :: SignUp)

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
signUp_secretHash :: Lens.Lens' SignUp (Prelude.Maybe Prelude.Text)
signUp_secretHash = Lens.lens (\SignUp' {secretHash} -> secretHash) (\s@SignUp' {} a -> s {secretHash = a} :: SignUp) Prelude.. Lens.mapping Core._Sensitive

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
signUp_userAttributes :: Lens.Lens' SignUp (Prelude.Maybe [AttributeType])
signUp_userAttributes = Lens.lens (\SignUp' {userAttributes} -> userAttributes) (\s@SignUp' {} a -> s {userAttributes = a} :: SignUp) Prelude.. Lens.mapping Lens._Coerce

-- | The validation data in the request to register a user.
signUp_validationData :: Lens.Lens' SignUp (Prelude.Maybe [AttributeType])
signUp_validationData = Lens.lens (\SignUp' {validationData} -> validationData) (\s@SignUp' {} a -> s {validationData = a} :: SignUp) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @SignUp@ calls.
signUp_analyticsMetadata :: Lens.Lens' SignUp (Prelude.Maybe AnalyticsMetadataType)
signUp_analyticsMetadata = Lens.lens (\SignUp' {analyticsMetadata} -> analyticsMetadata) (\s@SignUp' {} a -> s {analyticsMetadata = a} :: SignUp)

-- | The ID of the client associated with the user pool.
signUp_clientId :: Lens.Lens' SignUp Prelude.Text
signUp_clientId = Lens.lens (\SignUp' {clientId} -> clientId) (\s@SignUp' {} a -> s {clientId = a} :: SignUp) Prelude.. Core._Sensitive

-- | The user name of the user you wish to register.
signUp_username :: Lens.Lens' SignUp Prelude.Text
signUp_username = Lens.lens (\SignUp' {username} -> username) (\s@SignUp' {} a -> s {username = a} :: SignUp) Prelude.. Core._Sensitive

-- | The password of the user you wish to register.
signUp_password :: Lens.Lens' SignUp Prelude.Text
signUp_password = Lens.lens (\SignUp' {password} -> password) (\s@SignUp' {} a -> s {password = a} :: SignUp) Prelude.. Core._Sensitive

instance Core.AWSRequest SignUp where
  type AWSResponse SignUp = SignUpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SignUpResponse'
            Prelude.<$> (x Core..?> "CodeDeliveryDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "UserConfirmed")
            Prelude.<*> (x Core..:> "UserSub")
      )

instance Prelude.Hashable SignUp

instance Prelude.NFData SignUp

instance Core.ToHeaders SignUp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.SignUp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SignUp where
  toJSON SignUp' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Core..=)
              Prelude.<$> clientMetadata,
            ("UserContextData" Core..=)
              Prelude.<$> userContextData,
            ("SecretHash" Core..=) Prelude.<$> secretHash,
            ("UserAttributes" Core..=)
              Prelude.<$> userAttributes,
            ("ValidationData" Core..=)
              Prelude.<$> validationData,
            ("AnalyticsMetadata" Core..=)
              Prelude.<$> analyticsMetadata,
            Prelude.Just ("ClientId" Core..= clientId),
            Prelude.Just ("Username" Core..= username),
            Prelude.Just ("Password" Core..= password)
          ]
      )

instance Core.ToPath SignUp where
  toPath = Prelude.const "/"

instance Core.ToQuery SignUp where
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
    -- | The UUID of the authenticated user. This is not the same as @username@.
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
-- 'userSub', 'signUpResponse_userSub' - The UUID of the authenticated user. This is not the same as @username@.
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

-- | The UUID of the authenticated user. This is not the same as @username@.
signUpResponse_userSub :: Lens.Lens' SignUpResponse Prelude.Text
signUpResponse_userSub = Lens.lens (\SignUpResponse' {userSub} -> userSub) (\s@SignUpResponse' {} a -> s {userSub = a} :: SignUpResponse)

instance Prelude.NFData SignUpResponse
