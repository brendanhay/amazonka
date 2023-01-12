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
-- Module      : Amazonka.CognitoIdentityProvider.AdminInitiateAuth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the authentication flow, as an administrator.
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
--
-- Calling this action requires developer credentials.
module Amazonka.CognitoIdentityProvider.AdminInitiateAuth
  ( -- * Creating a Request
    AdminInitiateAuth (..),
    newAdminInitiateAuth,

    -- * Request Lenses
    adminInitiateAuth_analyticsMetadata,
    adminInitiateAuth_authParameters,
    adminInitiateAuth_clientMetadata,
    adminInitiateAuth_contextData,
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Initiates the authorization request, as an administrator.
--
-- /See:/ 'newAdminInitiateAuth' smart constructor.
data AdminInitiateAuth = AdminInitiateAuth'
  { -- | The analytics metadata for collecting Amazon Pinpoint metrics for
    -- @AdminInitiateAuth@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | The authentication parameters. These are inputs corresponding to the
    -- @AuthFlow@ that you\'re invoking. The required values depend on the
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
    authParameters :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A map of custom key-value pairs that you can provide as input for
    -- certain custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the AdminInitiateAuth API action, Amazon Cognito
    -- invokes the Lambda functions that are specified for various triggers.
    -- The ClientMetadata value is passed as input to the functions for only
    -- the following triggers:
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
    -- request. In your function code in Lambda, you can process the
    -- @validationData@ value to enhance your workflow for your specific needs.
    --
    -- When you use the AdminInitiateAuth API action, Amazon Cognito also
    -- invokes the functions for the following triggers, but it doesn\'t
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
    -- | Contextual data about your user session, such as the device fingerprint,
    -- IP address, or location. Amazon Cognito advanced security evaluates the
    -- risk of an authentication event based on the context that your app
    -- generates and passes to Amazon Cognito when it makes API requests.
    contextData :: Prelude.Maybe ContextDataType,
    -- | The ID of the Amazon Cognito user pool.
    userPoolId :: Prelude.Text,
    -- | The app client ID.
    clientId :: Data.Sensitive Prelude.Text,
    -- | The authentication flow for this call to run. The API action will depend
    -- on this value. For example:
    --
    -- -   @REFRESH_TOKEN_AUTH@ will take in a valid refresh token and return
    --     new tokens.
    --
    -- -   @USER_SRP_AUTH@ will take in @USERNAME@ and @SRP_A@ and return the
    --     Secure Remote Password (SRP) protocol variables to be used for next
    --     challenge execution.
    --
    -- -   @ADMIN_USER_PASSWORD_AUTH@ will take in @USERNAME@ and @PASSWORD@
    --     and return the next challenge or tokens.
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
    -- -   @ADMIN_USER_PASSWORD_AUTH@: Admin-based user password
    --     authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication
    --     flow. In this flow, Amazon Cognito receives the password in the
    --     request instead of using the SRP process to verify passwords.
    authFlow :: AuthFlowType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminInitiateAuth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyticsMetadata', 'adminInitiateAuth_analyticsMetadata' - The analytics metadata for collecting Amazon Pinpoint metrics for
-- @AdminInitiateAuth@ calls.
--
-- 'authParameters', 'adminInitiateAuth_authParameters' - The authentication parameters. These are inputs corresponding to the
-- @AuthFlow@ that you\'re invoking. The required values depend on the
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
-- 'clientMetadata', 'adminInitiateAuth_clientMetadata' - A map of custom key-value pairs that you can provide as input for
-- certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the AdminInitiateAuth API action, Amazon Cognito
-- invokes the Lambda functions that are specified for various triggers.
-- The ClientMetadata value is passed as input to the functions for only
-- the following triggers:
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
-- request. In your function code in Lambda, you can process the
-- @validationData@ value to enhance your workflow for your specific needs.
--
-- When you use the AdminInitiateAuth API action, Amazon Cognito also
-- invokes the functions for the following triggers, but it doesn\'t
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
-- 'contextData', 'adminInitiateAuth_contextData' - Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
--
-- 'userPoolId', 'adminInitiateAuth_userPoolId' - The ID of the Amazon Cognito user pool.
--
-- 'clientId', 'adminInitiateAuth_clientId' - The app client ID.
--
-- 'authFlow', 'adminInitiateAuth_authFlow' - The authentication flow for this call to run. The API action will depend
-- on this value. For example:
--
-- -   @REFRESH_TOKEN_AUTH@ will take in a valid refresh token and return
--     new tokens.
--
-- -   @USER_SRP_AUTH@ will take in @USERNAME@ and @SRP_A@ and return the
--     Secure Remote Password (SRP) protocol variables to be used for next
--     challenge execution.
--
-- -   @ADMIN_USER_PASSWORD_AUTH@ will take in @USERNAME@ and @PASSWORD@
--     and return the next challenge or tokens.
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
-- -   @ADMIN_USER_PASSWORD_AUTH@: Admin-based user password
--     authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication
--     flow. In this flow, Amazon Cognito receives the password in the
--     request instead of using the SRP process to verify passwords.
newAdminInitiateAuth ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  -- | 'authFlow'
  AuthFlowType ->
  AdminInitiateAuth
newAdminInitiateAuth
  pUserPoolId_
  pClientId_
  pAuthFlow_ =
    AdminInitiateAuth'
      { analyticsMetadata =
          Prelude.Nothing,
        authParameters = Prelude.Nothing,
        clientMetadata = Prelude.Nothing,
        contextData = Prelude.Nothing,
        userPoolId = pUserPoolId_,
        clientId = Data._Sensitive Lens.# pClientId_,
        authFlow = pAuthFlow_
      }

-- | The analytics metadata for collecting Amazon Pinpoint metrics for
-- @AdminInitiateAuth@ calls.
adminInitiateAuth_analyticsMetadata :: Lens.Lens' AdminInitiateAuth (Prelude.Maybe AnalyticsMetadataType)
adminInitiateAuth_analyticsMetadata = Lens.lens (\AdminInitiateAuth' {analyticsMetadata} -> analyticsMetadata) (\s@AdminInitiateAuth' {} a -> s {analyticsMetadata = a} :: AdminInitiateAuth)

-- | The authentication parameters. These are inputs corresponding to the
-- @AuthFlow@ that you\'re invoking. The required values depend on the
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
adminInitiateAuth_authParameters :: Lens.Lens' AdminInitiateAuth (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminInitiateAuth_authParameters = Lens.lens (\AdminInitiateAuth' {authParameters} -> authParameters) (\s@AdminInitiateAuth' {} a -> s {authParameters = a} :: AdminInitiateAuth) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | A map of custom key-value pairs that you can provide as input for
-- certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the AdminInitiateAuth API action, Amazon Cognito
-- invokes the Lambda functions that are specified for various triggers.
-- The ClientMetadata value is passed as input to the functions for only
-- the following triggers:
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
-- request. In your function code in Lambda, you can process the
-- @validationData@ value to enhance your workflow for your specific needs.
--
-- When you use the AdminInitiateAuth API action, Amazon Cognito also
-- invokes the functions for the following triggers, but it doesn\'t
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
adminInitiateAuth_clientMetadata :: Lens.Lens' AdminInitiateAuth (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminInitiateAuth_clientMetadata = Lens.lens (\AdminInitiateAuth' {clientMetadata} -> clientMetadata) (\s@AdminInitiateAuth' {} a -> s {clientMetadata = a} :: AdminInitiateAuth) Prelude.. Lens.mapping Lens.coerced

-- | Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
adminInitiateAuth_contextData :: Lens.Lens' AdminInitiateAuth (Prelude.Maybe ContextDataType)
adminInitiateAuth_contextData = Lens.lens (\AdminInitiateAuth' {contextData} -> contextData) (\s@AdminInitiateAuth' {} a -> s {contextData = a} :: AdminInitiateAuth)

-- | The ID of the Amazon Cognito user pool.
adminInitiateAuth_userPoolId :: Lens.Lens' AdminInitiateAuth Prelude.Text
adminInitiateAuth_userPoolId = Lens.lens (\AdminInitiateAuth' {userPoolId} -> userPoolId) (\s@AdminInitiateAuth' {} a -> s {userPoolId = a} :: AdminInitiateAuth)

-- | The app client ID.
adminInitiateAuth_clientId :: Lens.Lens' AdminInitiateAuth Prelude.Text
adminInitiateAuth_clientId = Lens.lens (\AdminInitiateAuth' {clientId} -> clientId) (\s@AdminInitiateAuth' {} a -> s {clientId = a} :: AdminInitiateAuth) Prelude.. Data._Sensitive

-- | The authentication flow for this call to run. The API action will depend
-- on this value. For example:
--
-- -   @REFRESH_TOKEN_AUTH@ will take in a valid refresh token and return
--     new tokens.
--
-- -   @USER_SRP_AUTH@ will take in @USERNAME@ and @SRP_A@ and return the
--     Secure Remote Password (SRP) protocol variables to be used for next
--     challenge execution.
--
-- -   @ADMIN_USER_PASSWORD_AUTH@ will take in @USERNAME@ and @PASSWORD@
--     and return the next challenge or tokens.
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
-- -   @ADMIN_USER_PASSWORD_AUTH@: Admin-based user password
--     authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication
--     flow. In this flow, Amazon Cognito receives the password in the
--     request instead of using the SRP process to verify passwords.
adminInitiateAuth_authFlow :: Lens.Lens' AdminInitiateAuth AuthFlowType
adminInitiateAuth_authFlow = Lens.lens (\AdminInitiateAuth' {authFlow} -> authFlow) (\s@AdminInitiateAuth' {} a -> s {authFlow = a} :: AdminInitiateAuth)

instance Core.AWSRequest AdminInitiateAuth where
  type
    AWSResponse AdminInitiateAuth =
      AdminInitiateAuthResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminInitiateAuthResponse'
            Prelude.<$> (x Data..?> "AuthenticationResult")
            Prelude.<*> (x Data..?> "ChallengeName")
            Prelude.<*> ( x Data..?> "ChallengeParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminInitiateAuth where
  hashWithSalt _salt AdminInitiateAuth' {..} =
    _salt `Prelude.hashWithSalt` analyticsMetadata
      `Prelude.hashWithSalt` authParameters
      `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` contextData
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` authFlow

instance Prelude.NFData AdminInitiateAuth where
  rnf AdminInitiateAuth' {..} =
    Prelude.rnf analyticsMetadata
      `Prelude.seq` Prelude.rnf authParameters
      `Prelude.seq` Prelude.rnf clientMetadata
      `Prelude.seq` Prelude.rnf contextData
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf authFlow

instance Data.ToHeaders AdminInitiateAuth where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminInitiateAuth" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminInitiateAuth where
  toJSON AdminInitiateAuth' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnalyticsMetadata" Data..=)
              Prelude.<$> analyticsMetadata,
            ("AuthParameters" Data..=)
              Prelude.<$> authParameters,
            ("ClientMetadata" Data..=)
              Prelude.<$> clientMetadata,
            ("ContextData" Data..=) Prelude.<$> contextData,
            Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("ClientId" Data..= clientId),
            Prelude.Just ("AuthFlow" Data..= authFlow)
          ]
      )

instance Data.ToPath AdminInitiateAuth where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminInitiateAuth where
  toQuery = Prelude.const Prelude.mempty

-- | Initiates the authentication response, as an administrator.
--
-- /See:/ 'newAdminInitiateAuthResponse' smart constructor.
data AdminInitiateAuthResponse = AdminInitiateAuthResponse'
  { -- | The result of the authentication response. This is only returned if the
    -- caller doesn\'t need to pass another challenge. If the caller does need
    -- to pass another challenge before it gets tokens, @ChallengeName@,
    -- @ChallengeParameters@, and @Session@ are returned.
    authenticationResult :: Prelude.Maybe AuthenticationResultType,
    -- | The name of the challenge that you\'re responding to with this call.
    -- This is returned in the @AdminInitiateAuth@ response if you must pass
    -- another challenge.
    --
    -- -   @MFA_SETUP@: If MFA is required, users who don\'t have at least one
    --     of the MFA methods set up are presented with an @MFA_SETUP@
    --     challenge. The user must set up at least one MFA type to continue to
    --     authenticate.
    --
    -- -   @SELECT_MFA_TYPE@: Selects the MFA type. Valid MFA options are
    --     @SMS_MFA@ for text SMS MFA, and @SOFTWARE_TOKEN_MFA@ for time-based
    --     one-time password (TOTP) software token MFA.
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
    -- -   @DEVICE_SRP_AUTH@: If device tracking was activated in your user
    --     pool and the previous challenges were passed, this challenge is
    --     returned so that Amazon Cognito can start tracking this device.
    --
    -- -   @DEVICE_PASSWORD_VERIFIER@: Similar to @PASSWORD_VERIFIER@, but for
    --     devices only.
    --
    -- -   @ADMIN_NO_SRP_AUTH@: This is returned if you must authenticate with
    --     @USERNAME@ and @PASSWORD@ directly. An app client must be enabled to
    --     use this flow.
    --
    -- -   @NEW_PASSWORD_REQUIRED@: For users who are required to change their
    --     passwords after successful first login. Respond to this challenge
    --     with @NEW_PASSWORD@ and any required attributes that Amazon Cognito
    --     returned in the @requiredAttributes@ parameter. You can also set
    --     values for attributes that aren\'t required by your user pool and
    --     that your app client can write. For more information, see
    --     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminRespondToAuthChallenge.html AdminRespondToAuthChallenge>.
    --
    --     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
    --     required attribute that already has a value. In
    --     @AdminRespondToAuthChallenge@, set a value for any keys that Amazon
    --     Cognito returned in the @requiredAttributes@ parameter, then use the
    --     @AdminUpdateUserAttributes@ API operation to modify the value of any
    --     additional attributes.
    --
    -- -   @MFA_SETUP@: For users who are required to set up an MFA factor
    --     before they can sign in. The MFA types activated for the user pool
    --     will be listed in the challenge parameters @MFA_CAN_SETUP@ value.
    --
    --     To set up software token MFA, use the session returned here from
    --     @InitiateAuth@ as an input to @AssociateSoftwareToken@, and use the
    --     session returned by @VerifySoftwareToken@ as an input to
    --     @RespondToAuthChallenge@ with challenge name @MFA_SETUP@ to complete
    --     sign-in. To set up SMS MFA, users will need help from an
    --     administrator to add a phone number to their account and then call
    --     @InitiateAuth@ again to restart sign-in.
    challengeName :: Prelude.Maybe ChallengeNameType,
    -- | The challenge parameters. These are returned to you in the
    -- @AdminInitiateAuth@ response if you must pass another challenge. The
    -- responses in this parameter should be used to compute inputs to the next
    -- call (@AdminRespondToAuthChallenge@).
    --
    -- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
    --
    -- The value of the @USER_ID_FOR_SRP@ attribute is the user\'s actual
    -- username, not an alias (such as email address or phone number), even if
    -- you specified an alias in your call to @AdminInitiateAuth@. This happens
    -- because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@,
    -- the @USERNAME@ attribute can\'t be an alias.
    challengeParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The session that should be passed both ways in challenge-response calls
    -- to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@
    -- API call determines that the caller must pass another challenge, they
    -- return a session with other challenge parameters. This session should be
    -- passed as it is to the next @AdminRespondToAuthChallenge@ API call.
    session :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminInitiateAuthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationResult', 'adminInitiateAuthResponse_authenticationResult' - The result of the authentication response. This is only returned if the
-- caller doesn\'t need to pass another challenge. If the caller does need
-- to pass another challenge before it gets tokens, @ChallengeName@,
-- @ChallengeParameters@, and @Session@ are returned.
--
-- 'challengeName', 'adminInitiateAuthResponse_challengeName' - The name of the challenge that you\'re responding to with this call.
-- This is returned in the @AdminInitiateAuth@ response if you must pass
-- another challenge.
--
-- -   @MFA_SETUP@: If MFA is required, users who don\'t have at least one
--     of the MFA methods set up are presented with an @MFA_SETUP@
--     challenge. The user must set up at least one MFA type to continue to
--     authenticate.
--
-- -   @SELECT_MFA_TYPE@: Selects the MFA type. Valid MFA options are
--     @SMS_MFA@ for text SMS MFA, and @SOFTWARE_TOKEN_MFA@ for time-based
--     one-time password (TOTP) software token MFA.
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
-- -   @DEVICE_SRP_AUTH@: If device tracking was activated in your user
--     pool and the previous challenges were passed, this challenge is
--     returned so that Amazon Cognito can start tracking this device.
--
-- -   @DEVICE_PASSWORD_VERIFIER@: Similar to @PASSWORD_VERIFIER@, but for
--     devices only.
--
-- -   @ADMIN_NO_SRP_AUTH@: This is returned if you must authenticate with
--     @USERNAME@ and @PASSWORD@ directly. An app client must be enabled to
--     use this flow.
--
-- -   @NEW_PASSWORD_REQUIRED@: For users who are required to change their
--     passwords after successful first login. Respond to this challenge
--     with @NEW_PASSWORD@ and any required attributes that Amazon Cognito
--     returned in the @requiredAttributes@ parameter. You can also set
--     values for attributes that aren\'t required by your user pool and
--     that your app client can write. For more information, see
--     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminRespondToAuthChallenge.html AdminRespondToAuthChallenge>.
--
--     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
--     required attribute that already has a value. In
--     @AdminRespondToAuthChallenge@, set a value for any keys that Amazon
--     Cognito returned in the @requiredAttributes@ parameter, then use the
--     @AdminUpdateUserAttributes@ API operation to modify the value of any
--     additional attributes.
--
-- -   @MFA_SETUP@: For users who are required to set up an MFA factor
--     before they can sign in. The MFA types activated for the user pool
--     will be listed in the challenge parameters @MFA_CAN_SETUP@ value.
--
--     To set up software token MFA, use the session returned here from
--     @InitiateAuth@ as an input to @AssociateSoftwareToken@, and use the
--     session returned by @VerifySoftwareToken@ as an input to
--     @RespondToAuthChallenge@ with challenge name @MFA_SETUP@ to complete
--     sign-in. To set up SMS MFA, users will need help from an
--     administrator to add a phone number to their account and then call
--     @InitiateAuth@ again to restart sign-in.
--
-- 'challengeParameters', 'adminInitiateAuthResponse_challengeParameters' - The challenge parameters. These are returned to you in the
-- @AdminInitiateAuth@ response if you must pass another challenge. The
-- responses in this parameter should be used to compute inputs to the next
-- call (@AdminRespondToAuthChallenge@).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
--
-- The value of the @USER_ID_FOR_SRP@ attribute is the user\'s actual
-- username, not an alias (such as email address or phone number), even if
-- you specified an alias in your call to @AdminInitiateAuth@. This happens
-- because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@,
-- the @USERNAME@ attribute can\'t be an alias.
--
-- 'session', 'adminInitiateAuthResponse_session' - The session that should be passed both ways in challenge-response calls
-- to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@
-- API call determines that the caller must pass another challenge, they
-- return a session with other challenge parameters. This session should be
-- passed as it is to the next @AdminRespondToAuthChallenge@ API call.
--
-- 'httpStatus', 'adminInitiateAuthResponse_httpStatus' - The response's http status code.
newAdminInitiateAuthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminInitiateAuthResponse
newAdminInitiateAuthResponse pHttpStatus_ =
  AdminInitiateAuthResponse'
    { authenticationResult =
        Prelude.Nothing,
      challengeName = Prelude.Nothing,
      challengeParameters = Prelude.Nothing,
      session = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The result of the authentication response. This is only returned if the
-- caller doesn\'t need to pass another challenge. If the caller does need
-- to pass another challenge before it gets tokens, @ChallengeName@,
-- @ChallengeParameters@, and @Session@ are returned.
adminInitiateAuthResponse_authenticationResult :: Lens.Lens' AdminInitiateAuthResponse (Prelude.Maybe AuthenticationResultType)
adminInitiateAuthResponse_authenticationResult = Lens.lens (\AdminInitiateAuthResponse' {authenticationResult} -> authenticationResult) (\s@AdminInitiateAuthResponse' {} a -> s {authenticationResult = a} :: AdminInitiateAuthResponse)

-- | The name of the challenge that you\'re responding to with this call.
-- This is returned in the @AdminInitiateAuth@ response if you must pass
-- another challenge.
--
-- -   @MFA_SETUP@: If MFA is required, users who don\'t have at least one
--     of the MFA methods set up are presented with an @MFA_SETUP@
--     challenge. The user must set up at least one MFA type to continue to
--     authenticate.
--
-- -   @SELECT_MFA_TYPE@: Selects the MFA type. Valid MFA options are
--     @SMS_MFA@ for text SMS MFA, and @SOFTWARE_TOKEN_MFA@ for time-based
--     one-time password (TOTP) software token MFA.
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
-- -   @DEVICE_SRP_AUTH@: If device tracking was activated in your user
--     pool and the previous challenges were passed, this challenge is
--     returned so that Amazon Cognito can start tracking this device.
--
-- -   @DEVICE_PASSWORD_VERIFIER@: Similar to @PASSWORD_VERIFIER@, but for
--     devices only.
--
-- -   @ADMIN_NO_SRP_AUTH@: This is returned if you must authenticate with
--     @USERNAME@ and @PASSWORD@ directly. An app client must be enabled to
--     use this flow.
--
-- -   @NEW_PASSWORD_REQUIRED@: For users who are required to change their
--     passwords after successful first login. Respond to this challenge
--     with @NEW_PASSWORD@ and any required attributes that Amazon Cognito
--     returned in the @requiredAttributes@ parameter. You can also set
--     values for attributes that aren\'t required by your user pool and
--     that your app client can write. For more information, see
--     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminRespondToAuthChallenge.html AdminRespondToAuthChallenge>.
--
--     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
--     required attribute that already has a value. In
--     @AdminRespondToAuthChallenge@, set a value for any keys that Amazon
--     Cognito returned in the @requiredAttributes@ parameter, then use the
--     @AdminUpdateUserAttributes@ API operation to modify the value of any
--     additional attributes.
--
-- -   @MFA_SETUP@: For users who are required to set up an MFA factor
--     before they can sign in. The MFA types activated for the user pool
--     will be listed in the challenge parameters @MFA_CAN_SETUP@ value.
--
--     To set up software token MFA, use the session returned here from
--     @InitiateAuth@ as an input to @AssociateSoftwareToken@, and use the
--     session returned by @VerifySoftwareToken@ as an input to
--     @RespondToAuthChallenge@ with challenge name @MFA_SETUP@ to complete
--     sign-in. To set up SMS MFA, users will need help from an
--     administrator to add a phone number to their account and then call
--     @InitiateAuth@ again to restart sign-in.
adminInitiateAuthResponse_challengeName :: Lens.Lens' AdminInitiateAuthResponse (Prelude.Maybe ChallengeNameType)
adminInitiateAuthResponse_challengeName = Lens.lens (\AdminInitiateAuthResponse' {challengeName} -> challengeName) (\s@AdminInitiateAuthResponse' {} a -> s {challengeName = a} :: AdminInitiateAuthResponse)

-- | The challenge parameters. These are returned to you in the
-- @AdminInitiateAuth@ response if you must pass another challenge. The
-- responses in this parameter should be used to compute inputs to the next
-- call (@AdminRespondToAuthChallenge@).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
--
-- The value of the @USER_ID_FOR_SRP@ attribute is the user\'s actual
-- username, not an alias (such as email address or phone number), even if
-- you specified an alias in your call to @AdminInitiateAuth@. This happens
-- because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@,
-- the @USERNAME@ attribute can\'t be an alias.
adminInitiateAuthResponse_challengeParameters :: Lens.Lens' AdminInitiateAuthResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminInitiateAuthResponse_challengeParameters = Lens.lens (\AdminInitiateAuthResponse' {challengeParameters} -> challengeParameters) (\s@AdminInitiateAuthResponse' {} a -> s {challengeParameters = a} :: AdminInitiateAuthResponse) Prelude.. Lens.mapping Lens.coerced

-- | The session that should be passed both ways in challenge-response calls
-- to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@
-- API call determines that the caller must pass another challenge, they
-- return a session with other challenge parameters. This session should be
-- passed as it is to the next @AdminRespondToAuthChallenge@ API call.
adminInitiateAuthResponse_session :: Lens.Lens' AdminInitiateAuthResponse (Prelude.Maybe Prelude.Text)
adminInitiateAuthResponse_session = Lens.lens (\AdminInitiateAuthResponse' {session} -> session) (\s@AdminInitiateAuthResponse' {} a -> s {session = a} :: AdminInitiateAuthResponse)

-- | The response's http status code.
adminInitiateAuthResponse_httpStatus :: Lens.Lens' AdminInitiateAuthResponse Prelude.Int
adminInitiateAuthResponse_httpStatus = Lens.lens (\AdminInitiateAuthResponse' {httpStatus} -> httpStatus) (\s@AdminInitiateAuthResponse' {} a -> s {httpStatus = a} :: AdminInitiateAuthResponse)

instance Prelude.NFData AdminInitiateAuthResponse where
  rnf AdminInitiateAuthResponse' {..} =
    Prelude.rnf authenticationResult
      `Prelude.seq` Prelude.rnf challengeName
      `Prelude.seq` Prelude.rnf challengeParameters
      `Prelude.seq` Prelude.rnf session
      `Prelude.seq` Prelude.rnf httpStatus
