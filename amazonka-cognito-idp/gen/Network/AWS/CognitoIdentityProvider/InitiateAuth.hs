{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoIdentityProvider.InitiateAuth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the authentication flow.
module Network.AWS.CognitoIdentityProvider.InitiateAuth
  ( -- * Creating a Request
    InitiateAuth (..),
    newInitiateAuth,

    -- * Request Lenses
    initiateAuth_clientMetadata,
    initiateAuth_userContextData,
    initiateAuth_analyticsMetadata,
    initiateAuth_authParameters,
    initiateAuth_authFlow,
    initiateAuth_clientId,

    -- * Destructuring the Response
    InitiateAuthResponse (..),
    newInitiateAuthResponse,

    -- * Response Lenses
    initiateAuthResponse_authenticationResult,
    initiateAuthResponse_challengeName,
    initiateAuthResponse_challengeParameters,
    initiateAuthResponse_session,
    initiateAuthResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Initiates the authentication request.
--
-- /See:/ 'newInitiateAuth' smart constructor.
data InitiateAuth = InitiateAuth'
  { -- | A map of custom key-value pairs that you can provide as input for
    -- certain custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the InitiateAuth API action, Amazon Cognito
    -- invokes the AWS Lambda functions that are specified for various
    -- triggers. The ClientMetadata value is passed as input to the functions
    -- for only the following triggers:
    --
    -- -   Pre signup
    --
    -- -   Pre authentication
    --
    -- -   User migration
    --
    -- When Amazon Cognito invokes the functions for these triggers, it passes
    -- a JSON payload, which the function receives as input. This payload
    -- contains a @validationData@ attribute, which provides the data that you
    -- assigned to the ClientMetadata parameter in your InitiateAuth request.
    -- In your function code in AWS Lambda, you can process the
    -- @validationData@ value to enhance your workflow for your specific needs.
    --
    -- When you use the InitiateAuth API action, Amazon Cognito also invokes
    -- the functions for the following triggers, but it does not provide the
    -- ClientMetadata value as input:
    --
    -- -   Post authentication
    --
    -- -   Custom message
    --
    -- -   Pre token generation
    --
    -- -   Create auth challenge
    --
    -- -   Define auth challenge
    --
    -- -   Verify auth challenge
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
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for
    -- @InitiateAuth@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | The authentication parameters. These are inputs corresponding to the
    -- @AuthFlow@ that you are invoking. The required values depend on the
    -- value of @AuthFlow@:
    --
    -- -   For @USER_SRP_AUTH@: @USERNAME@ (required), @SRP_A@ (required),
    --     @SECRET_HASH@ (required if the app client is configured with a
    --     client secret), @DEVICE_KEY@.
    --
    -- -   For @REFRESH_TOKEN_AUTH\/REFRESH_TOKEN@: @REFRESH_TOKEN@ (required),
    --     @SECRET_HASH@ (required if the app client is configured with a
    --     client secret), @DEVICE_KEY@.
    --
    -- -   For @CUSTOM_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if app
    --     client is configured with client secret), @DEVICE_KEY@. To start the
    --     authentication flow with password verification, include
    --     @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@.
    authParameters :: Prelude.Maybe (Prelude.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The authentication flow for this call to execute. The API action will
    -- depend on this value. For example:
    --
    -- -   @REFRESH_TOKEN_AUTH@ will take in a valid refresh token and return
    --     new tokens.
    --
    -- -   @USER_SRP_AUTH@ will take in @USERNAME@ and @SRP_A@ and return the
    --     SRP variables to be used for next challenge execution.
    --
    -- -   @USER_PASSWORD_AUTH@ will take in @USERNAME@ and @PASSWORD@ and
    --     return the next challenge or tokens.
    --
    -- Valid values include:
    --
    -- -   @USER_SRP_AUTH@: Authentication flow for the Secure Remote Password
    --     (SRP) protocol.
    --
    -- -   @REFRESH_TOKEN_AUTH@\/@REFRESH_TOKEN@: Authentication flow for
    --     refreshing the access token and ID token by supplying a valid
    --     refresh token.
    --
    -- -   @CUSTOM_AUTH@: Custom authentication flow.
    --
    -- -   @USER_PASSWORD_AUTH@: Non-SRP authentication flow; USERNAME and
    --     PASSWORD are passed directly. If a user migration Lambda trigger is
    --     set, this flow will invoke the user migration Lambda if the USERNAME
    --     is not found in the user pool.
    --
    -- -   @ADMIN_USER_PASSWORD_AUTH@: Admin-based user password
    --     authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication
    --     flow. In this flow, Cognito receives the password in the request
    --     instead of using the SRP process to verify passwords.
    --
    -- @ADMIN_NO_SRP_AUTH@ is not a valid value.
    authFlow :: AuthFlowType,
    -- | The app client ID.
    clientId :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InitiateAuth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'initiateAuth_clientMetadata' - A map of custom key-value pairs that you can provide as input for
-- certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the InitiateAuth API action, Amazon Cognito
-- invokes the AWS Lambda functions that are specified for various
-- triggers. The ClientMetadata value is passed as input to the functions
-- for only the following triggers:
--
-- -   Pre signup
--
-- -   Pre authentication
--
-- -   User migration
--
-- When Amazon Cognito invokes the functions for these triggers, it passes
-- a JSON payload, which the function receives as input. This payload
-- contains a @validationData@ attribute, which provides the data that you
-- assigned to the ClientMetadata parameter in your InitiateAuth request.
-- In your function code in AWS Lambda, you can process the
-- @validationData@ value to enhance your workflow for your specific needs.
--
-- When you use the InitiateAuth API action, Amazon Cognito also invokes
-- the functions for the following triggers, but it does not provide the
-- ClientMetadata value as input:
--
-- -   Post authentication
--
-- -   Custom message
--
-- -   Pre token generation
--
-- -   Create auth challenge
--
-- -   Define auth challenge
--
-- -   Verify auth challenge
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
-- 'userContextData', 'initiateAuth_userContextData' - Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
--
-- 'analyticsMetadata', 'initiateAuth_analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for
-- @InitiateAuth@ calls.
--
-- 'authParameters', 'initiateAuth_authParameters' - The authentication parameters. These are inputs corresponding to the
-- @AuthFlow@ that you are invoking. The required values depend on the
-- value of @AuthFlow@:
--
-- -   For @USER_SRP_AUTH@: @USERNAME@ (required), @SRP_A@ (required),
--     @SECRET_HASH@ (required if the app client is configured with a
--     client secret), @DEVICE_KEY@.
--
-- -   For @REFRESH_TOKEN_AUTH\/REFRESH_TOKEN@: @REFRESH_TOKEN@ (required),
--     @SECRET_HASH@ (required if the app client is configured with a
--     client secret), @DEVICE_KEY@.
--
-- -   For @CUSTOM_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if app
--     client is configured with client secret), @DEVICE_KEY@. To start the
--     authentication flow with password verification, include
--     @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@.
--
-- 'authFlow', 'initiateAuth_authFlow' - The authentication flow for this call to execute. The API action will
-- depend on this value. For example:
--
-- -   @REFRESH_TOKEN_AUTH@ will take in a valid refresh token and return
--     new tokens.
--
-- -   @USER_SRP_AUTH@ will take in @USERNAME@ and @SRP_A@ and return the
--     SRP variables to be used for next challenge execution.
--
-- -   @USER_PASSWORD_AUTH@ will take in @USERNAME@ and @PASSWORD@ and
--     return the next challenge or tokens.
--
-- Valid values include:
--
-- -   @USER_SRP_AUTH@: Authentication flow for the Secure Remote Password
--     (SRP) protocol.
--
-- -   @REFRESH_TOKEN_AUTH@\/@REFRESH_TOKEN@: Authentication flow for
--     refreshing the access token and ID token by supplying a valid
--     refresh token.
--
-- -   @CUSTOM_AUTH@: Custom authentication flow.
--
-- -   @USER_PASSWORD_AUTH@: Non-SRP authentication flow; USERNAME and
--     PASSWORD are passed directly. If a user migration Lambda trigger is
--     set, this flow will invoke the user migration Lambda if the USERNAME
--     is not found in the user pool.
--
-- -   @ADMIN_USER_PASSWORD_AUTH@: Admin-based user password
--     authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication
--     flow. In this flow, Cognito receives the password in the request
--     instead of using the SRP process to verify passwords.
--
-- @ADMIN_NO_SRP_AUTH@ is not a valid value.
--
-- 'clientId', 'initiateAuth_clientId' - The app client ID.
newInitiateAuth ::
  -- | 'authFlow'
  AuthFlowType ->
  -- | 'clientId'
  Prelude.Text ->
  InitiateAuth
newInitiateAuth pAuthFlow_ pClientId_ =
  InitiateAuth'
    { clientMetadata = Prelude.Nothing,
      userContextData = Prelude.Nothing,
      analyticsMetadata = Prelude.Nothing,
      authParameters = Prelude.Nothing,
      authFlow = pAuthFlow_,
      clientId = Prelude._Sensitive Lens.# pClientId_
    }

-- | A map of custom key-value pairs that you can provide as input for
-- certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the InitiateAuth API action, Amazon Cognito
-- invokes the AWS Lambda functions that are specified for various
-- triggers. The ClientMetadata value is passed as input to the functions
-- for only the following triggers:
--
-- -   Pre signup
--
-- -   Pre authentication
--
-- -   User migration
--
-- When Amazon Cognito invokes the functions for these triggers, it passes
-- a JSON payload, which the function receives as input. This payload
-- contains a @validationData@ attribute, which provides the data that you
-- assigned to the ClientMetadata parameter in your InitiateAuth request.
-- In your function code in AWS Lambda, you can process the
-- @validationData@ value to enhance your workflow for your specific needs.
--
-- When you use the InitiateAuth API action, Amazon Cognito also invokes
-- the functions for the following triggers, but it does not provide the
-- ClientMetadata value as input:
--
-- -   Post authentication
--
-- -   Custom message
--
-- -   Pre token generation
--
-- -   Create auth challenge
--
-- -   Define auth challenge
--
-- -   Verify auth challenge
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
initiateAuth_clientMetadata :: Lens.Lens' InitiateAuth (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
initiateAuth_clientMetadata = Lens.lens (\InitiateAuth' {clientMetadata} -> clientMetadata) (\s@InitiateAuth' {} a -> s {clientMetadata = a} :: InitiateAuth) Prelude.. Lens.mapping Prelude._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
initiateAuth_userContextData :: Lens.Lens' InitiateAuth (Prelude.Maybe UserContextDataType)
initiateAuth_userContextData = Lens.lens (\InitiateAuth' {userContextData} -> userContextData) (\s@InitiateAuth' {} a -> s {userContextData = a} :: InitiateAuth)

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @InitiateAuth@ calls.
initiateAuth_analyticsMetadata :: Lens.Lens' InitiateAuth (Prelude.Maybe AnalyticsMetadataType)
initiateAuth_analyticsMetadata = Lens.lens (\InitiateAuth' {analyticsMetadata} -> analyticsMetadata) (\s@InitiateAuth' {} a -> s {analyticsMetadata = a} :: InitiateAuth)

-- | The authentication parameters. These are inputs corresponding to the
-- @AuthFlow@ that you are invoking. The required values depend on the
-- value of @AuthFlow@:
--
-- -   For @USER_SRP_AUTH@: @USERNAME@ (required), @SRP_A@ (required),
--     @SECRET_HASH@ (required if the app client is configured with a
--     client secret), @DEVICE_KEY@.
--
-- -   For @REFRESH_TOKEN_AUTH\/REFRESH_TOKEN@: @REFRESH_TOKEN@ (required),
--     @SECRET_HASH@ (required if the app client is configured with a
--     client secret), @DEVICE_KEY@.
--
-- -   For @CUSTOM_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if app
--     client is configured with client secret), @DEVICE_KEY@. To start the
--     authentication flow with password verification, include
--     @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@.
initiateAuth_authParameters :: Lens.Lens' InitiateAuth (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
initiateAuth_authParameters = Lens.lens (\InitiateAuth' {authParameters} -> authParameters) (\s@InitiateAuth' {} a -> s {authParameters = a} :: InitiateAuth) Prelude.. Lens.mapping (Prelude._Sensitive Prelude.. Prelude._Coerce)

-- | The authentication flow for this call to execute. The API action will
-- depend on this value. For example:
--
-- -   @REFRESH_TOKEN_AUTH@ will take in a valid refresh token and return
--     new tokens.
--
-- -   @USER_SRP_AUTH@ will take in @USERNAME@ and @SRP_A@ and return the
--     SRP variables to be used for next challenge execution.
--
-- -   @USER_PASSWORD_AUTH@ will take in @USERNAME@ and @PASSWORD@ and
--     return the next challenge or tokens.
--
-- Valid values include:
--
-- -   @USER_SRP_AUTH@: Authentication flow for the Secure Remote Password
--     (SRP) protocol.
--
-- -   @REFRESH_TOKEN_AUTH@\/@REFRESH_TOKEN@: Authentication flow for
--     refreshing the access token and ID token by supplying a valid
--     refresh token.
--
-- -   @CUSTOM_AUTH@: Custom authentication flow.
--
-- -   @USER_PASSWORD_AUTH@: Non-SRP authentication flow; USERNAME and
--     PASSWORD are passed directly. If a user migration Lambda trigger is
--     set, this flow will invoke the user migration Lambda if the USERNAME
--     is not found in the user pool.
--
-- -   @ADMIN_USER_PASSWORD_AUTH@: Admin-based user password
--     authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication
--     flow. In this flow, Cognito receives the password in the request
--     instead of using the SRP process to verify passwords.
--
-- @ADMIN_NO_SRP_AUTH@ is not a valid value.
initiateAuth_authFlow :: Lens.Lens' InitiateAuth AuthFlowType
initiateAuth_authFlow = Lens.lens (\InitiateAuth' {authFlow} -> authFlow) (\s@InitiateAuth' {} a -> s {authFlow = a} :: InitiateAuth)

-- | The app client ID.
initiateAuth_clientId :: Lens.Lens' InitiateAuth Prelude.Text
initiateAuth_clientId = Lens.lens (\InitiateAuth' {clientId} -> clientId) (\s@InitiateAuth' {} a -> s {clientId = a} :: InitiateAuth) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest InitiateAuth where
  type Rs InitiateAuth = InitiateAuthResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          InitiateAuthResponse'
            Prelude.<$> (x Prelude..?> "AuthenticationResult")
            Prelude.<*> (x Prelude..?> "ChallengeName")
            Prelude.<*> ( x Prelude..?> "ChallengeParameters"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InitiateAuth

instance Prelude.NFData InitiateAuth

instance Prelude.ToHeaders InitiateAuth where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.InitiateAuth" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON InitiateAuth where
  toJSON InitiateAuth' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Prelude..=)
              Prelude.<$> clientMetadata,
            ("UserContextData" Prelude..=)
              Prelude.<$> userContextData,
            ("AnalyticsMetadata" Prelude..=)
              Prelude.<$> analyticsMetadata,
            ("AuthParameters" Prelude..=)
              Prelude.<$> authParameters,
            Prelude.Just ("AuthFlow" Prelude..= authFlow),
            Prelude.Just ("ClientId" Prelude..= clientId)
          ]
      )

instance Prelude.ToPath InitiateAuth where
  toPath = Prelude.const "/"

instance Prelude.ToQuery InitiateAuth where
  toQuery = Prelude.const Prelude.mempty

-- | Initiates the authentication response.
--
-- /See:/ 'newInitiateAuthResponse' smart constructor.
data InitiateAuthResponse = InitiateAuthResponse'
  { -- | The result of the authentication response. This is only returned if the
    -- caller does not need to pass another challenge. If the caller does need
    -- to pass another challenge before it gets tokens, @ChallengeName@,
    -- @ChallengeParameters@, and @Session@ are returned.
    authenticationResult :: Prelude.Maybe AuthenticationResultType,
    -- | The name of the challenge which you are responding to with this call.
    -- This is returned to you in the @AdminInitiateAuth@ response if you need
    -- to pass another challenge.
    --
    -- Valid values include the following. Note that all of these challenges
    -- require @USERNAME@ and @SECRET_HASH@ (if applicable) in the parameters.
    --
    -- -   @SMS_MFA@: Next challenge is to supply an @SMS_MFA_CODE@, delivered
    --     via SMS.
    --
    -- -   @PASSWORD_VERIFIER@: Next challenge is to supply
    --     @PASSWORD_CLAIM_SIGNATURE@, @PASSWORD_CLAIM_SECRET_BLOCK@, and
    --     @TIMESTAMP@ after the client-side SRP calculations.
    --
    -- -   @CUSTOM_CHALLENGE@: This is returned if your custom authentication
    --     flow determines that the user should pass another challenge before
    --     tokens are issued.
    --
    -- -   @DEVICE_SRP_AUTH@: If device tracking was enabled on your user pool
    --     and the previous challenges were passed, this challenge is returned
    --     so that Amazon Cognito can start tracking this device.
    --
    -- -   @DEVICE_PASSWORD_VERIFIER@: Similar to @PASSWORD_VERIFIER@, but for
    --     devices only.
    --
    -- -   @NEW_PASSWORD_REQUIRED@: For users which are required to change
    --     their passwords after successful first login. This challenge should
    --     be passed with @NEW_PASSWORD@ and any other required attributes.
    challengeName :: Prelude.Maybe ChallengeNameType,
    -- | The challenge parameters. These are returned to you in the
    -- @InitiateAuth@ response if you need to pass another challenge. The
    -- responses in this parameter should be used to compute inputs to the next
    -- call (@RespondToAuthChallenge@).
    --
    -- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
    challengeParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service. If the caller needs to go through another challenge,
    -- they return a session with other challenge parameters. This session
    -- should be passed as it is to the next @RespondToAuthChallenge@ API call.
    session :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InitiateAuthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationResult', 'initiateAuthResponse_authenticationResult' - The result of the authentication response. This is only returned if the
-- caller does not need to pass another challenge. If the caller does need
-- to pass another challenge before it gets tokens, @ChallengeName@,
-- @ChallengeParameters@, and @Session@ are returned.
--
-- 'challengeName', 'initiateAuthResponse_challengeName' - The name of the challenge which you are responding to with this call.
-- This is returned to you in the @AdminInitiateAuth@ response if you need
-- to pass another challenge.
--
-- Valid values include the following. Note that all of these challenges
-- require @USERNAME@ and @SECRET_HASH@ (if applicable) in the parameters.
--
-- -   @SMS_MFA@: Next challenge is to supply an @SMS_MFA_CODE@, delivered
--     via SMS.
--
-- -   @PASSWORD_VERIFIER@: Next challenge is to supply
--     @PASSWORD_CLAIM_SIGNATURE@, @PASSWORD_CLAIM_SECRET_BLOCK@, and
--     @TIMESTAMP@ after the client-side SRP calculations.
--
-- -   @CUSTOM_CHALLENGE@: This is returned if your custom authentication
--     flow determines that the user should pass another challenge before
--     tokens are issued.
--
-- -   @DEVICE_SRP_AUTH@: If device tracking was enabled on your user pool
--     and the previous challenges were passed, this challenge is returned
--     so that Amazon Cognito can start tracking this device.
--
-- -   @DEVICE_PASSWORD_VERIFIER@: Similar to @PASSWORD_VERIFIER@, but for
--     devices only.
--
-- -   @NEW_PASSWORD_REQUIRED@: For users which are required to change
--     their passwords after successful first login. This challenge should
--     be passed with @NEW_PASSWORD@ and any other required attributes.
--
-- 'challengeParameters', 'initiateAuthResponse_challengeParameters' - The challenge parameters. These are returned to you in the
-- @InitiateAuth@ response if you need to pass another challenge. The
-- responses in this parameter should be used to compute inputs to the next
-- call (@RespondToAuthChallenge@).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
--
-- 'session', 'initiateAuthResponse_session' - The session which should be passed both ways in challenge-response calls
-- to the service. If the caller needs to go through another challenge,
-- they return a session with other challenge parameters. This session
-- should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- 'httpStatus', 'initiateAuthResponse_httpStatus' - The response's http status code.
newInitiateAuthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InitiateAuthResponse
newInitiateAuthResponse pHttpStatus_ =
  InitiateAuthResponse'
    { authenticationResult =
        Prelude.Nothing,
      challengeName = Prelude.Nothing,
      challengeParameters = Prelude.Nothing,
      session = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The result of the authentication response. This is only returned if the
-- caller does not need to pass another challenge. If the caller does need
-- to pass another challenge before it gets tokens, @ChallengeName@,
-- @ChallengeParameters@, and @Session@ are returned.
initiateAuthResponse_authenticationResult :: Lens.Lens' InitiateAuthResponse (Prelude.Maybe AuthenticationResultType)
initiateAuthResponse_authenticationResult = Lens.lens (\InitiateAuthResponse' {authenticationResult} -> authenticationResult) (\s@InitiateAuthResponse' {} a -> s {authenticationResult = a} :: InitiateAuthResponse)

-- | The name of the challenge which you are responding to with this call.
-- This is returned to you in the @AdminInitiateAuth@ response if you need
-- to pass another challenge.
--
-- Valid values include the following. Note that all of these challenges
-- require @USERNAME@ and @SECRET_HASH@ (if applicable) in the parameters.
--
-- -   @SMS_MFA@: Next challenge is to supply an @SMS_MFA_CODE@, delivered
--     via SMS.
--
-- -   @PASSWORD_VERIFIER@: Next challenge is to supply
--     @PASSWORD_CLAIM_SIGNATURE@, @PASSWORD_CLAIM_SECRET_BLOCK@, and
--     @TIMESTAMP@ after the client-side SRP calculations.
--
-- -   @CUSTOM_CHALLENGE@: This is returned if your custom authentication
--     flow determines that the user should pass another challenge before
--     tokens are issued.
--
-- -   @DEVICE_SRP_AUTH@: If device tracking was enabled on your user pool
--     and the previous challenges were passed, this challenge is returned
--     so that Amazon Cognito can start tracking this device.
--
-- -   @DEVICE_PASSWORD_VERIFIER@: Similar to @PASSWORD_VERIFIER@, but for
--     devices only.
--
-- -   @NEW_PASSWORD_REQUIRED@: For users which are required to change
--     their passwords after successful first login. This challenge should
--     be passed with @NEW_PASSWORD@ and any other required attributes.
initiateAuthResponse_challengeName :: Lens.Lens' InitiateAuthResponse (Prelude.Maybe ChallengeNameType)
initiateAuthResponse_challengeName = Lens.lens (\InitiateAuthResponse' {challengeName} -> challengeName) (\s@InitiateAuthResponse' {} a -> s {challengeName = a} :: InitiateAuthResponse)

-- | The challenge parameters. These are returned to you in the
-- @InitiateAuth@ response if you need to pass another challenge. The
-- responses in this parameter should be used to compute inputs to the next
-- call (@RespondToAuthChallenge@).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
initiateAuthResponse_challengeParameters :: Lens.Lens' InitiateAuthResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
initiateAuthResponse_challengeParameters = Lens.lens (\InitiateAuthResponse' {challengeParameters} -> challengeParameters) (\s@InitiateAuthResponse' {} a -> s {challengeParameters = a} :: InitiateAuthResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The session which should be passed both ways in challenge-response calls
-- to the service. If the caller needs to go through another challenge,
-- they return a session with other challenge parameters. This session
-- should be passed as it is to the next @RespondToAuthChallenge@ API call.
initiateAuthResponse_session :: Lens.Lens' InitiateAuthResponse (Prelude.Maybe Prelude.Text)
initiateAuthResponse_session = Lens.lens (\InitiateAuthResponse' {session} -> session) (\s@InitiateAuthResponse' {} a -> s {session = a} :: InitiateAuthResponse)

-- | The response's http status code.
initiateAuthResponse_httpStatus :: Lens.Lens' InitiateAuthResponse Prelude.Int
initiateAuthResponse_httpStatus = Lens.lens (\InitiateAuthResponse' {httpStatus} -> httpStatus) (\s@InitiateAuthResponse' {} a -> s {httpStatus = a} :: InitiateAuthResponse)

instance Prelude.NFData InitiateAuthResponse
