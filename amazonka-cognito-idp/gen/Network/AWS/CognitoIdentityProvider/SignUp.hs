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
    clientMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Contextual data such as the user\'s device fingerprint, IP address, or
    -- location used for evaluating the risk of an unexpected event by Amazon
    -- Cognito advanced security.
    userContextData :: Core.Maybe UserContextDataType,
    -- | A keyed-hash message authentication code (HMAC) calculated using the
    -- secret key of a user pool client and username plus the client ID in the
    -- message.
    secretHash :: Core.Maybe (Core.Sensitive Core.Text),
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    userAttributes :: Core.Maybe [AttributeType],
    -- | The validation data in the request to register a user.
    validationData :: Core.Maybe [AttributeType],
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for
    -- @SignUp@ calls.
    analyticsMetadata :: Core.Maybe AnalyticsMetadataType,
    -- | The ID of the client associated with the user pool.
    clientId :: Core.Sensitive Core.Text,
    -- | The user name of the user you wish to register.
    username :: Core.Sensitive Core.Text,
    -- | The password of the user you wish to register.
    password :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'username'
  Core.Text ->
  -- | 'password'
  Core.Text ->
  SignUp
newSignUp pClientId_ pUsername_ pPassword_ =
  SignUp'
    { clientMetadata = Core.Nothing,
      userContextData = Core.Nothing,
      secretHash = Core.Nothing,
      userAttributes = Core.Nothing,
      validationData = Core.Nothing,
      analyticsMetadata = Core.Nothing,
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
signUp_clientMetadata :: Lens.Lens' SignUp (Core.Maybe (Core.HashMap Core.Text Core.Text))
signUp_clientMetadata = Lens.lens (\SignUp' {clientMetadata} -> clientMetadata) (\s@SignUp' {} a -> s {clientMetadata = a} :: SignUp) Core.. Lens.mapping Lens._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
signUp_userContextData :: Lens.Lens' SignUp (Core.Maybe UserContextDataType)
signUp_userContextData = Lens.lens (\SignUp' {userContextData} -> userContextData) (\s@SignUp' {} a -> s {userContextData = a} :: SignUp)

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
signUp_secretHash :: Lens.Lens' SignUp (Core.Maybe Core.Text)
signUp_secretHash = Lens.lens (\SignUp' {secretHash} -> secretHash) (\s@SignUp' {} a -> s {secretHash = a} :: SignUp) Core.. Lens.mapping Core._Sensitive

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
signUp_userAttributes :: Lens.Lens' SignUp (Core.Maybe [AttributeType])
signUp_userAttributes = Lens.lens (\SignUp' {userAttributes} -> userAttributes) (\s@SignUp' {} a -> s {userAttributes = a} :: SignUp) Core.. Lens.mapping Lens._Coerce

-- | The validation data in the request to register a user.
signUp_validationData :: Lens.Lens' SignUp (Core.Maybe [AttributeType])
signUp_validationData = Lens.lens (\SignUp' {validationData} -> validationData) (\s@SignUp' {} a -> s {validationData = a} :: SignUp) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @SignUp@ calls.
signUp_analyticsMetadata :: Lens.Lens' SignUp (Core.Maybe AnalyticsMetadataType)
signUp_analyticsMetadata = Lens.lens (\SignUp' {analyticsMetadata} -> analyticsMetadata) (\s@SignUp' {} a -> s {analyticsMetadata = a} :: SignUp)

-- | The ID of the client associated with the user pool.
signUp_clientId :: Lens.Lens' SignUp Core.Text
signUp_clientId = Lens.lens (\SignUp' {clientId} -> clientId) (\s@SignUp' {} a -> s {clientId = a} :: SignUp) Core.. Core._Sensitive

-- | The user name of the user you wish to register.
signUp_username :: Lens.Lens' SignUp Core.Text
signUp_username = Lens.lens (\SignUp' {username} -> username) (\s@SignUp' {} a -> s {username = a} :: SignUp) Core.. Core._Sensitive

-- | The password of the user you wish to register.
signUp_password :: Lens.Lens' SignUp Core.Text
signUp_password = Lens.lens (\SignUp' {password} -> password) (\s@SignUp' {} a -> s {password = a} :: SignUp) Core.. Core._Sensitive

instance Core.AWSRequest SignUp where
  type AWSResponse SignUp = SignUpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SignUpResponse'
            Core.<$> (x Core..?> "CodeDeliveryDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "UserConfirmed")
            Core.<*> (x Core..:> "UserSub")
      )

instance Core.Hashable SignUp

instance Core.NFData SignUp

instance Core.ToHeaders SignUp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.SignUp" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SignUp where
  toJSON SignUp' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientMetadata" Core..=) Core.<$> clientMetadata,
            ("UserContextData" Core..=) Core.<$> userContextData,
            ("SecretHash" Core..=) Core.<$> secretHash,
            ("UserAttributes" Core..=) Core.<$> userAttributes,
            ("ValidationData" Core..=) Core.<$> validationData,
            ("AnalyticsMetadata" Core..=)
              Core.<$> analyticsMetadata,
            Core.Just ("ClientId" Core..= clientId),
            Core.Just ("Username" Core..= username),
            Core.Just ("Password" Core..= password)
          ]
      )

instance Core.ToPath SignUp where
  toPath = Core.const "/"

instance Core.ToQuery SignUp where
  toQuery = Core.const Core.mempty

-- | The response from the server for a registration request.
--
-- /See:/ 'newSignUpResponse' smart constructor.
data SignUpResponse = SignUpResponse'
  { -- | The code delivery details returned by the server response to the user
    -- registration request.
    codeDeliveryDetails :: Core.Maybe CodeDeliveryDetailsType,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A response from the server indicating that a user registration has been
    -- confirmed.
    userConfirmed :: Core.Bool,
    -- | The UUID of the authenticated user. This is not the same as @username@.
    userSub :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'userConfirmed'
  Core.Bool ->
  -- | 'userSub'
  Core.Text ->
  SignUpResponse
newSignUpResponse
  pHttpStatus_
  pUserConfirmed_
  pUserSub_ =
    SignUpResponse'
      { codeDeliveryDetails = Core.Nothing,
        httpStatus = pHttpStatus_,
        userConfirmed = pUserConfirmed_,
        userSub = pUserSub_
      }

-- | The code delivery details returned by the server response to the user
-- registration request.
signUpResponse_codeDeliveryDetails :: Lens.Lens' SignUpResponse (Core.Maybe CodeDeliveryDetailsType)
signUpResponse_codeDeliveryDetails = Lens.lens (\SignUpResponse' {codeDeliveryDetails} -> codeDeliveryDetails) (\s@SignUpResponse' {} a -> s {codeDeliveryDetails = a} :: SignUpResponse)

-- | The response's http status code.
signUpResponse_httpStatus :: Lens.Lens' SignUpResponse Core.Int
signUpResponse_httpStatus = Lens.lens (\SignUpResponse' {httpStatus} -> httpStatus) (\s@SignUpResponse' {} a -> s {httpStatus = a} :: SignUpResponse)

-- | A response from the server indicating that a user registration has been
-- confirmed.
signUpResponse_userConfirmed :: Lens.Lens' SignUpResponse Core.Bool
signUpResponse_userConfirmed = Lens.lens (\SignUpResponse' {userConfirmed} -> userConfirmed) (\s@SignUpResponse' {} a -> s {userConfirmed = a} :: SignUpResponse)

-- | The UUID of the authenticated user. This is not the same as @username@.
signUpResponse_userSub :: Lens.Lens' SignUpResponse Core.Text
signUpResponse_userSub = Lens.lens (\SignUpResponse' {userSub} -> userSub) (\s@SignUpResponse' {} a -> s {userSub = a} :: SignUpResponse)

instance Core.NFData SignUpResponse
