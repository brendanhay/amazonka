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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the authentication flow, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
  ( -- * Creating a Request
    AdminInitiateAuth (..),
    newAdminInitiateAuth,

    -- * Request Lenses
    adminInitiateAuth_clientMetadata,
    adminInitiateAuth_contextData,
    adminInitiateAuth_analyticsMetadata,
    adminInitiateAuth_authParameters,
    adminInitiateAuth_userPoolId,
    adminInitiateAuth_clientId,
    adminInitiateAuth_authFlow,

    -- * Destructuring the Response
    AdminInitiateAuthResponse (..),
    newAdminInitiateAuthResponse,

    -- * Response Lenses
    adminInitiateAuthResponse_authenticationResult,
    adminInitiateAuthResponse_challengeName,
    adminInitiateAuthResponse_challengeParameters,
    adminInitiateAuthResponse_session,
    adminInitiateAuthResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Initiates the authorization request, as an administrator.
--
-- /See:/ 'newAdminInitiateAuth' smart constructor.
data AdminInitiateAuth = AdminInitiateAuth'
  { -- | A map of custom key-value pairs that you can provide as input for
    -- certain custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the AdminInitiateAuth API action, Amazon
    -- Cognito invokes the AWS Lambda functions that are specified for various
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
    -- assigned to the ClientMetadata parameter in your AdminInitiateAuth
    -- request. In your function code in AWS Lambda, you can process the
    -- @validationData@ value to enhance your workflow for your specific needs.
    --
    -- When you use the AdminInitiateAuth API action, Amazon Cognito also
    -- invokes the functions for the following triggers, but it does not
    -- provide the ClientMetadata value as input:
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
    clientMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Contextual data such as the user\'s device fingerprint, IP address, or
    -- location used for evaluating the risk of an unexpected event by Amazon
    -- Cognito advanced security.
    contextData :: Core.Maybe ContextDataType,
    -- | The analytics metadata for collecting Amazon Pinpoint metrics for
    -- @AdminInitiateAuth@ calls.
    analyticsMetadata :: Core.Maybe AnalyticsMetadataType,
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
    -- -   For @ADMIN_NO_SRP_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if
    --     app client is configured with client secret), @PASSWORD@ (required),
    --     @DEVICE_KEY@.
    --
    -- -   For @CUSTOM_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if app
    --     client is configured with client secret), @DEVICE_KEY@. To start the
    --     authentication flow with password verification, include
    --     @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@.
    authParameters :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text Core.Text)),
    -- | The ID of the Amazon Cognito user pool.
    userPoolId :: Core.Text,
    -- | The app client ID.
    clientId :: Core.Sensitive Core.Text,
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
    -- -   @ADMIN_NO_SRP_AUTH@: Non-SRP authentication flow; you can pass in
    --     the USERNAME and PASSWORD directly if the flow is enabled for
    --     calling the app client.
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
    authFlow :: AuthFlowType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminInitiateAuth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'adminInitiateAuth_clientMetadata' - A map of custom key-value pairs that you can provide as input for
-- certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the AdminInitiateAuth API action, Amazon
-- Cognito invokes the AWS Lambda functions that are specified for various
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
-- assigned to the ClientMetadata parameter in your AdminInitiateAuth
-- request. In your function code in AWS Lambda, you can process the
-- @validationData@ value to enhance your workflow for your specific needs.
--
-- When you use the AdminInitiateAuth API action, Amazon Cognito also
-- invokes the functions for the following triggers, but it does not
-- provide the ClientMetadata value as input:
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
-- 'contextData', 'adminInitiateAuth_contextData' - Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
--
-- 'analyticsMetadata', 'adminInitiateAuth_analyticsMetadata' - The analytics metadata for collecting Amazon Pinpoint metrics for
-- @AdminInitiateAuth@ calls.
--
-- 'authParameters', 'adminInitiateAuth_authParameters' - The authentication parameters. These are inputs corresponding to the
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
-- -   For @ADMIN_NO_SRP_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if
--     app client is configured with client secret), @PASSWORD@ (required),
--     @DEVICE_KEY@.
--
-- -   For @CUSTOM_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if app
--     client is configured with client secret), @DEVICE_KEY@. To start the
--     authentication flow with password verification, include
--     @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@.
--
-- 'userPoolId', 'adminInitiateAuth_userPoolId' - The ID of the Amazon Cognito user pool.
--
-- 'clientId', 'adminInitiateAuth_clientId' - The app client ID.
--
-- 'authFlow', 'adminInitiateAuth_authFlow' - The authentication flow for this call to execute. The API action will
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
-- -   @ADMIN_NO_SRP_AUTH@: Non-SRP authentication flow; you can pass in
--     the USERNAME and PASSWORD directly if the flow is enabled for
--     calling the app client.
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
newAdminInitiateAuth ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'clientId'
  Core.Text ->
  -- | 'authFlow'
  AuthFlowType ->
  AdminInitiateAuth
newAdminInitiateAuth
  pUserPoolId_
  pClientId_
  pAuthFlow_ =
    AdminInitiateAuth'
      { clientMetadata = Core.Nothing,
        contextData = Core.Nothing,
        analyticsMetadata = Core.Nothing,
        authParameters = Core.Nothing,
        userPoolId = pUserPoolId_,
        clientId = Core._Sensitive Lens.# pClientId_,
        authFlow = pAuthFlow_
      }

-- | A map of custom key-value pairs that you can provide as input for
-- certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the AdminInitiateAuth API action, Amazon
-- Cognito invokes the AWS Lambda functions that are specified for various
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
-- assigned to the ClientMetadata parameter in your AdminInitiateAuth
-- request. In your function code in AWS Lambda, you can process the
-- @validationData@ value to enhance your workflow for your specific needs.
--
-- When you use the AdminInitiateAuth API action, Amazon Cognito also
-- invokes the functions for the following triggers, but it does not
-- provide the ClientMetadata value as input:
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
adminInitiateAuth_clientMetadata :: Lens.Lens' AdminInitiateAuth (Core.Maybe (Core.HashMap Core.Text Core.Text))
adminInitiateAuth_clientMetadata = Lens.lens (\AdminInitiateAuth' {clientMetadata} -> clientMetadata) (\s@AdminInitiateAuth' {} a -> s {clientMetadata = a} :: AdminInitiateAuth) Core.. Lens.mapping Lens._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
adminInitiateAuth_contextData :: Lens.Lens' AdminInitiateAuth (Core.Maybe ContextDataType)
adminInitiateAuth_contextData = Lens.lens (\AdminInitiateAuth' {contextData} -> contextData) (\s@AdminInitiateAuth' {} a -> s {contextData = a} :: AdminInitiateAuth)

-- | The analytics metadata for collecting Amazon Pinpoint metrics for
-- @AdminInitiateAuth@ calls.
adminInitiateAuth_analyticsMetadata :: Lens.Lens' AdminInitiateAuth (Core.Maybe AnalyticsMetadataType)
adminInitiateAuth_analyticsMetadata = Lens.lens (\AdminInitiateAuth' {analyticsMetadata} -> analyticsMetadata) (\s@AdminInitiateAuth' {} a -> s {analyticsMetadata = a} :: AdminInitiateAuth)

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
-- -   For @ADMIN_NO_SRP_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if
--     app client is configured with client secret), @PASSWORD@ (required),
--     @DEVICE_KEY@.
--
-- -   For @CUSTOM_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if app
--     client is configured with client secret), @DEVICE_KEY@. To start the
--     authentication flow with password verification, include
--     @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@.
adminInitiateAuth_authParameters :: Lens.Lens' AdminInitiateAuth (Core.Maybe (Core.HashMap Core.Text Core.Text))
adminInitiateAuth_authParameters = Lens.lens (\AdminInitiateAuth' {authParameters} -> authParameters) (\s@AdminInitiateAuth' {} a -> s {authParameters = a} :: AdminInitiateAuth) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | The ID of the Amazon Cognito user pool.
adminInitiateAuth_userPoolId :: Lens.Lens' AdminInitiateAuth Core.Text
adminInitiateAuth_userPoolId = Lens.lens (\AdminInitiateAuth' {userPoolId} -> userPoolId) (\s@AdminInitiateAuth' {} a -> s {userPoolId = a} :: AdminInitiateAuth)

-- | The app client ID.
adminInitiateAuth_clientId :: Lens.Lens' AdminInitiateAuth Core.Text
adminInitiateAuth_clientId = Lens.lens (\AdminInitiateAuth' {clientId} -> clientId) (\s@AdminInitiateAuth' {} a -> s {clientId = a} :: AdminInitiateAuth) Core.. Core._Sensitive

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
-- -   @ADMIN_NO_SRP_AUTH@: Non-SRP authentication flow; you can pass in
--     the USERNAME and PASSWORD directly if the flow is enabled for
--     calling the app client.
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
adminInitiateAuth_authFlow :: Lens.Lens' AdminInitiateAuth AuthFlowType
adminInitiateAuth_authFlow = Lens.lens (\AdminInitiateAuth' {authFlow} -> authFlow) (\s@AdminInitiateAuth' {} a -> s {authFlow = a} :: AdminInitiateAuth)

instance Core.AWSRequest AdminInitiateAuth where
  type
    AWSResponse AdminInitiateAuth =
      AdminInitiateAuthResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminInitiateAuthResponse'
            Core.<$> (x Core..?> "AuthenticationResult")
            Core.<*> (x Core..?> "ChallengeName")
            Core.<*> ( x Core..?> "ChallengeParameters"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Session")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AdminInitiateAuth

instance Core.NFData AdminInitiateAuth

instance Core.ToHeaders AdminInitiateAuth where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminInitiateAuth" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminInitiateAuth where
  toJSON AdminInitiateAuth' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientMetadata" Core..=) Core.<$> clientMetadata,
            ("ContextData" Core..=) Core.<$> contextData,
            ("AnalyticsMetadata" Core..=)
              Core.<$> analyticsMetadata,
            ("AuthParameters" Core..=) Core.<$> authParameters,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ClientId" Core..= clientId),
            Core.Just ("AuthFlow" Core..= authFlow)
          ]
      )

instance Core.ToPath AdminInitiateAuth where
  toPath = Core.const "/"

instance Core.ToQuery AdminInitiateAuth where
  toQuery = Core.const Core.mempty

-- | Initiates the authentication response, as an administrator.
--
-- /See:/ 'newAdminInitiateAuthResponse' smart constructor.
data AdminInitiateAuthResponse = AdminInitiateAuthResponse'
  { -- | The result of the authentication response. This is only returned if the
    -- caller does not need to pass another challenge. If the caller does need
    -- to pass another challenge before it gets tokens, @ChallengeName@,
    -- @ChallengeParameters@, and @Session@ are returned.
    authenticationResult :: Core.Maybe AuthenticationResultType,
    -- | The name of the challenge which you are responding to with this call.
    -- This is returned to you in the @AdminInitiateAuth@ response if you need
    -- to pass another challenge.
    --
    -- -   @MFA_SETUP@: If MFA is required, users who do not have at least one
    --     of the MFA methods set up are presented with an @MFA_SETUP@
    --     challenge. The user must set up at least one MFA type to continue to
    --     authenticate.
    --
    -- -   @SELECT_MFA_TYPE@: Selects the MFA type. Valid MFA options are
    --     @SMS_MFA@ for text SMS MFA, and @SOFTWARE_TOKEN_MFA@ for TOTP
    --     software token MFA.
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
    -- -   @ADMIN_NO_SRP_AUTH@: This is returned if you need to authenticate
    --     with @USERNAME@ and @PASSWORD@ directly. An app client must be
    --     enabled to use this flow.
    --
    -- -   @NEW_PASSWORD_REQUIRED@: For users which are required to change
    --     their passwords after successful first login. This challenge should
    --     be passed with @NEW_PASSWORD@ and any other required attributes.
    challengeName :: Core.Maybe ChallengeNameType,
    -- | The challenge parameters. These are returned to you in the
    -- @AdminInitiateAuth@ response if you need to pass another challenge. The
    -- responses in this parameter should be used to compute inputs to the next
    -- call (@AdminRespondToAuthChallenge@).
    --
    -- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
    --
    -- The value of the @USER_ID_FOR_SRP@ attribute will be the user\'s actual
    -- username, not an alias (such as email address or phone number), even if
    -- you specified an alias in your call to @AdminInitiateAuth@. This is
    -- because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@,
    -- the @USERNAME@ attribute cannot be an alias.
    challengeParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@
    -- API call determines that the caller needs to go through another
    -- challenge, they return a session with other challenge parameters. This
    -- session should be passed as it is to the next
    -- @AdminRespondToAuthChallenge@ API call.
    session :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminInitiateAuthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationResult', 'adminInitiateAuthResponse_authenticationResult' - The result of the authentication response. This is only returned if the
-- caller does not need to pass another challenge. If the caller does need
-- to pass another challenge before it gets tokens, @ChallengeName@,
-- @ChallengeParameters@, and @Session@ are returned.
--
-- 'challengeName', 'adminInitiateAuthResponse_challengeName' - The name of the challenge which you are responding to with this call.
-- This is returned to you in the @AdminInitiateAuth@ response if you need
-- to pass another challenge.
--
-- -   @MFA_SETUP@: If MFA is required, users who do not have at least one
--     of the MFA methods set up are presented with an @MFA_SETUP@
--     challenge. The user must set up at least one MFA type to continue to
--     authenticate.
--
-- -   @SELECT_MFA_TYPE@: Selects the MFA type. Valid MFA options are
--     @SMS_MFA@ for text SMS MFA, and @SOFTWARE_TOKEN_MFA@ for TOTP
--     software token MFA.
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
-- -   @ADMIN_NO_SRP_AUTH@: This is returned if you need to authenticate
--     with @USERNAME@ and @PASSWORD@ directly. An app client must be
--     enabled to use this flow.
--
-- -   @NEW_PASSWORD_REQUIRED@: For users which are required to change
--     their passwords after successful first login. This challenge should
--     be passed with @NEW_PASSWORD@ and any other required attributes.
--
-- 'challengeParameters', 'adminInitiateAuthResponse_challengeParameters' - The challenge parameters. These are returned to you in the
-- @AdminInitiateAuth@ response if you need to pass another challenge. The
-- responses in this parameter should be used to compute inputs to the next
-- call (@AdminRespondToAuthChallenge@).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
--
-- The value of the @USER_ID_FOR_SRP@ attribute will be the user\'s actual
-- username, not an alias (such as email address or phone number), even if
-- you specified an alias in your call to @AdminInitiateAuth@. This is
-- because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@,
-- the @USERNAME@ attribute cannot be an alias.
--
-- 'session', 'adminInitiateAuthResponse_session' - The session which should be passed both ways in challenge-response calls
-- to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@
-- API call determines that the caller needs to go through another
-- challenge, they return a session with other challenge parameters. This
-- session should be passed as it is to the next
-- @AdminRespondToAuthChallenge@ API call.
--
-- 'httpStatus', 'adminInitiateAuthResponse_httpStatus' - The response's http status code.
newAdminInitiateAuthResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AdminInitiateAuthResponse
newAdminInitiateAuthResponse pHttpStatus_ =
  AdminInitiateAuthResponse'
    { authenticationResult =
        Core.Nothing,
      challengeName = Core.Nothing,
      challengeParameters = Core.Nothing,
      session = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The result of the authentication response. This is only returned if the
-- caller does not need to pass another challenge. If the caller does need
-- to pass another challenge before it gets tokens, @ChallengeName@,
-- @ChallengeParameters@, and @Session@ are returned.
adminInitiateAuthResponse_authenticationResult :: Lens.Lens' AdminInitiateAuthResponse (Core.Maybe AuthenticationResultType)
adminInitiateAuthResponse_authenticationResult = Lens.lens (\AdminInitiateAuthResponse' {authenticationResult} -> authenticationResult) (\s@AdminInitiateAuthResponse' {} a -> s {authenticationResult = a} :: AdminInitiateAuthResponse)

-- | The name of the challenge which you are responding to with this call.
-- This is returned to you in the @AdminInitiateAuth@ response if you need
-- to pass another challenge.
--
-- -   @MFA_SETUP@: If MFA is required, users who do not have at least one
--     of the MFA methods set up are presented with an @MFA_SETUP@
--     challenge. The user must set up at least one MFA type to continue to
--     authenticate.
--
-- -   @SELECT_MFA_TYPE@: Selects the MFA type. Valid MFA options are
--     @SMS_MFA@ for text SMS MFA, and @SOFTWARE_TOKEN_MFA@ for TOTP
--     software token MFA.
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
-- -   @ADMIN_NO_SRP_AUTH@: This is returned if you need to authenticate
--     with @USERNAME@ and @PASSWORD@ directly. An app client must be
--     enabled to use this flow.
--
-- -   @NEW_PASSWORD_REQUIRED@: For users which are required to change
--     their passwords after successful first login. This challenge should
--     be passed with @NEW_PASSWORD@ and any other required attributes.
adminInitiateAuthResponse_challengeName :: Lens.Lens' AdminInitiateAuthResponse (Core.Maybe ChallengeNameType)
adminInitiateAuthResponse_challengeName = Lens.lens (\AdminInitiateAuthResponse' {challengeName} -> challengeName) (\s@AdminInitiateAuthResponse' {} a -> s {challengeName = a} :: AdminInitiateAuthResponse)

-- | The challenge parameters. These are returned to you in the
-- @AdminInitiateAuth@ response if you need to pass another challenge. The
-- responses in this parameter should be used to compute inputs to the next
-- call (@AdminRespondToAuthChallenge@).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
--
-- The value of the @USER_ID_FOR_SRP@ attribute will be the user\'s actual
-- username, not an alias (such as email address or phone number), even if
-- you specified an alias in your call to @AdminInitiateAuth@. This is
-- because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@,
-- the @USERNAME@ attribute cannot be an alias.
adminInitiateAuthResponse_challengeParameters :: Lens.Lens' AdminInitiateAuthResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
adminInitiateAuthResponse_challengeParameters = Lens.lens (\AdminInitiateAuthResponse' {challengeParameters} -> challengeParameters) (\s@AdminInitiateAuthResponse' {} a -> s {challengeParameters = a} :: AdminInitiateAuthResponse) Core.. Lens.mapping Lens._Coerce

-- | The session which should be passed both ways in challenge-response calls
-- to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@
-- API call determines that the caller needs to go through another
-- challenge, they return a session with other challenge parameters. This
-- session should be passed as it is to the next
-- @AdminRespondToAuthChallenge@ API call.
adminInitiateAuthResponse_session :: Lens.Lens' AdminInitiateAuthResponse (Core.Maybe Core.Text)
adminInitiateAuthResponse_session = Lens.lens (\AdminInitiateAuthResponse' {session} -> session) (\s@AdminInitiateAuthResponse' {} a -> s {session = a} :: AdminInitiateAuthResponse)

-- | The response's http status code.
adminInitiateAuthResponse_httpStatus :: Lens.Lens' AdminInitiateAuthResponse Core.Int
adminInitiateAuthResponse_httpStatus = Lens.lens (\AdminInitiateAuthResponse' {httpStatus} -> httpStatus) (\s@AdminInitiateAuthResponse' {} a -> s {httpStatus = a} :: AdminInitiateAuthResponse)

instance Core.NFData AdminInitiateAuthResponse
