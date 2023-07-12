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
-- Module      : Amazonka.CognitoIdentityProvider.InitiateAuth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates sign-in for a user in the Amazon Cognito user directory. You
-- can\'t sign in a user with a federated IdP with @InitiateAuth@. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-identity-federation.html Adding user pool sign-in through a third party>.
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
module Amazonka.CognitoIdentityProvider.InitiateAuth
  ( -- * Creating a Request
    InitiateAuth (..),
    newInitiateAuth,

    -- * Request Lenses
    initiateAuth_analyticsMetadata,
    initiateAuth_authParameters,
    initiateAuth_clientMetadata,
    initiateAuth_userContextData,
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Initiates the authentication request.
--
-- /See:/ 'newInitiateAuth' smart constructor.
data InitiateAuth = InitiateAuth'
  { -- | The Amazon Pinpoint analytics metadata that contributes to your metrics
    -- for @InitiateAuth@ calls.
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
    -- -   For @CUSTOM_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if app
    --     client is configured with client secret), @DEVICE_KEY@. To start the
    --     authentication flow with password verification, include
    --     @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@.
    authParameters :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A map of custom key-value pairs that you can provide as input for
    -- certain custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the InitiateAuth API action, Amazon Cognito
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
    -- assigned to the ClientMetadata parameter in your InitiateAuth request.
    -- In your function code in Lambda, you can process the @validationData@
    -- value to enhance your workflow for your specific needs.
    --
    -- When you use the InitiateAuth API action, Amazon Cognito also invokes
    -- the functions for the following triggers, but it doesn\'t provide the
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
    userContextData :: Prelude.Maybe UserContextDataType,
    -- | The authentication flow for this call to run. The API action will depend
    -- on this value. For example:
    --
    -- -   @REFRESH_TOKEN_AUTH@ takes in a valid refresh token and returns new
    --     tokens.
    --
    -- -   @USER_SRP_AUTH@ takes in @USERNAME@ and @SRP_A@ and returns the SRP
    --     variables to be used for next challenge execution.
    --
    -- -   @USER_PASSWORD_AUTH@ takes in @USERNAME@ and @PASSWORD@ and returns
    --     the next challenge or tokens.
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
    -- -   @USER_PASSWORD_AUTH@: Non-SRP authentication flow; user name and
    --     password are passed directly. If a user migration Lambda trigger is
    --     set, this flow will invoke the user migration Lambda if it doesn\'t
    --     find the user name in the user pool.
    --
    -- @ADMIN_NO_SRP_AUTH@ isn\'t a valid value.
    authFlow :: AuthFlowType,
    -- | The app client ID.
    clientId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitiateAuth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyticsMetadata', 'initiateAuth_analyticsMetadata' - The Amazon Pinpoint analytics metadata that contributes to your metrics
-- for @InitiateAuth@ calls.
--
-- 'authParameters', 'initiateAuth_authParameters' - The authentication parameters. These are inputs corresponding to the
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
-- -   For @CUSTOM_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if app
--     client is configured with client secret), @DEVICE_KEY@. To start the
--     authentication flow with password verification, include
--     @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@.
--
-- 'clientMetadata', 'initiateAuth_clientMetadata' - A map of custom key-value pairs that you can provide as input for
-- certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the InitiateAuth API action, Amazon Cognito
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
-- assigned to the ClientMetadata parameter in your InitiateAuth request.
-- In your function code in Lambda, you can process the @validationData@
-- value to enhance your workflow for your specific needs.
--
-- When you use the InitiateAuth API action, Amazon Cognito also invokes
-- the functions for the following triggers, but it doesn\'t provide the
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
-- 'userContextData', 'initiateAuth_userContextData' - Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
--
-- 'authFlow', 'initiateAuth_authFlow' - The authentication flow for this call to run. The API action will depend
-- on this value. For example:
--
-- -   @REFRESH_TOKEN_AUTH@ takes in a valid refresh token and returns new
--     tokens.
--
-- -   @USER_SRP_AUTH@ takes in @USERNAME@ and @SRP_A@ and returns the SRP
--     variables to be used for next challenge execution.
--
-- -   @USER_PASSWORD_AUTH@ takes in @USERNAME@ and @PASSWORD@ and returns
--     the next challenge or tokens.
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
-- -   @USER_PASSWORD_AUTH@: Non-SRP authentication flow; user name and
--     password are passed directly. If a user migration Lambda trigger is
--     set, this flow will invoke the user migration Lambda if it doesn\'t
--     find the user name in the user pool.
--
-- @ADMIN_NO_SRP_AUTH@ isn\'t a valid value.
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
    { analyticsMetadata = Prelude.Nothing,
      authParameters = Prelude.Nothing,
      clientMetadata = Prelude.Nothing,
      userContextData = Prelude.Nothing,
      authFlow = pAuthFlow_,
      clientId = Data._Sensitive Lens.# pClientId_
    }

-- | The Amazon Pinpoint analytics metadata that contributes to your metrics
-- for @InitiateAuth@ calls.
initiateAuth_analyticsMetadata :: Lens.Lens' InitiateAuth (Prelude.Maybe AnalyticsMetadataType)
initiateAuth_analyticsMetadata = Lens.lens (\InitiateAuth' {analyticsMetadata} -> analyticsMetadata) (\s@InitiateAuth' {} a -> s {analyticsMetadata = a} :: InitiateAuth)

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
-- -   For @CUSTOM_AUTH@: @USERNAME@ (required), @SECRET_HASH@ (if app
--     client is configured with client secret), @DEVICE_KEY@. To start the
--     authentication flow with password verification, include
--     @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@.
initiateAuth_authParameters :: Lens.Lens' InitiateAuth (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
initiateAuth_authParameters = Lens.lens (\InitiateAuth' {authParameters} -> authParameters) (\s@InitiateAuth' {} a -> s {authParameters = a} :: InitiateAuth) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | A map of custom key-value pairs that you can provide as input for
-- certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the InitiateAuth API action, Amazon Cognito
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
-- assigned to the ClientMetadata parameter in your InitiateAuth request.
-- In your function code in Lambda, you can process the @validationData@
-- value to enhance your workflow for your specific needs.
--
-- When you use the InitiateAuth API action, Amazon Cognito also invokes
-- the functions for the following triggers, but it doesn\'t provide the
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
initiateAuth_clientMetadata :: Lens.Lens' InitiateAuth (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
initiateAuth_clientMetadata = Lens.lens (\InitiateAuth' {clientMetadata} -> clientMetadata) (\s@InitiateAuth' {} a -> s {clientMetadata = a} :: InitiateAuth) Prelude.. Lens.mapping Lens.coerced

-- | Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
initiateAuth_userContextData :: Lens.Lens' InitiateAuth (Prelude.Maybe UserContextDataType)
initiateAuth_userContextData = Lens.lens (\InitiateAuth' {userContextData} -> userContextData) (\s@InitiateAuth' {} a -> s {userContextData = a} :: InitiateAuth)

-- | The authentication flow for this call to run. The API action will depend
-- on this value. For example:
--
-- -   @REFRESH_TOKEN_AUTH@ takes in a valid refresh token and returns new
--     tokens.
--
-- -   @USER_SRP_AUTH@ takes in @USERNAME@ and @SRP_A@ and returns the SRP
--     variables to be used for next challenge execution.
--
-- -   @USER_PASSWORD_AUTH@ takes in @USERNAME@ and @PASSWORD@ and returns
--     the next challenge or tokens.
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
-- -   @USER_PASSWORD_AUTH@: Non-SRP authentication flow; user name and
--     password are passed directly. If a user migration Lambda trigger is
--     set, this flow will invoke the user migration Lambda if it doesn\'t
--     find the user name in the user pool.
--
-- @ADMIN_NO_SRP_AUTH@ isn\'t a valid value.
initiateAuth_authFlow :: Lens.Lens' InitiateAuth AuthFlowType
initiateAuth_authFlow = Lens.lens (\InitiateAuth' {authFlow} -> authFlow) (\s@InitiateAuth' {} a -> s {authFlow = a} :: InitiateAuth)

-- | The app client ID.
initiateAuth_clientId :: Lens.Lens' InitiateAuth Prelude.Text
initiateAuth_clientId = Lens.lens (\InitiateAuth' {clientId} -> clientId) (\s@InitiateAuth' {} a -> s {clientId = a} :: InitiateAuth) Prelude.. Data._Sensitive

instance Core.AWSRequest InitiateAuth where
  type AWSResponse InitiateAuth = InitiateAuthResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InitiateAuthResponse'
            Prelude.<$> (x Data..?> "AuthenticationResult")
            Prelude.<*> (x Data..?> "ChallengeName")
            Prelude.<*> ( x
                            Data..?> "ChallengeParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InitiateAuth where
  hashWithSalt _salt InitiateAuth' {..} =
    _salt
      `Prelude.hashWithSalt` analyticsMetadata
      `Prelude.hashWithSalt` authParameters
      `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` userContextData
      `Prelude.hashWithSalt` authFlow
      `Prelude.hashWithSalt` clientId

instance Prelude.NFData InitiateAuth where
  rnf InitiateAuth' {..} =
    Prelude.rnf analyticsMetadata
      `Prelude.seq` Prelude.rnf authParameters
      `Prelude.seq` Prelude.rnf clientMetadata
      `Prelude.seq` Prelude.rnf userContextData
      `Prelude.seq` Prelude.rnf authFlow
      `Prelude.seq` Prelude.rnf clientId

instance Data.ToHeaders InitiateAuth where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.InitiateAuth" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON InitiateAuth where
  toJSON InitiateAuth' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnalyticsMetadata" Data..=)
              Prelude.<$> analyticsMetadata,
            ("AuthParameters" Data..=)
              Prelude.<$> authParameters,
            ("ClientMetadata" Data..=)
              Prelude.<$> clientMetadata,
            ("UserContextData" Data..=)
              Prelude.<$> userContextData,
            Prelude.Just ("AuthFlow" Data..= authFlow),
            Prelude.Just ("ClientId" Data..= clientId)
          ]
      )

instance Data.ToPath InitiateAuth where
  toPath = Prelude.const "/"

instance Data.ToQuery InitiateAuth where
  toQuery = Prelude.const Prelude.mempty

-- | Initiates the authentication response.
--
-- /See:/ 'newInitiateAuthResponse' smart constructor.
data InitiateAuthResponse = InitiateAuthResponse'
  { -- | The result of the authentication response. This result is only returned
    -- if the caller doesn\'t need to pass another challenge. If the caller
    -- does need to pass another challenge before it gets tokens,
    -- @ChallengeName@, @ChallengeParameters@, and @Session@ are returned.
    authenticationResult :: Prelude.Maybe AuthenticationResultType,
    -- | The name of the challenge that you\'re responding to with this call.
    -- This name is returned in the @AdminInitiateAuth@ response if you must
    -- pass another challenge.
    --
    -- Valid values include the following:
    --
    -- All of the following challenges require @USERNAME@ and @SECRET_HASH@ (if
    -- applicable) in the parameters.
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
    -- -   @DEVICE_SRP_AUTH@: If device tracking was activated on your user
    --     pool and the previous challenges were passed, this challenge is
    --     returned so that Amazon Cognito can start tracking this device.
    --
    -- -   @DEVICE_PASSWORD_VERIFIER@: Similar to @PASSWORD_VERIFIER@, but for
    --     devices only.
    --
    -- -   @NEW_PASSWORD_REQUIRED@: For users who are required to change their
    --     passwords after successful first login.
    --
    --     Respond to this challenge with @NEW_PASSWORD@ and any required
    --     attributes that Amazon Cognito returned in the @requiredAttributes@
    --     parameter. You can also set values for attributes that aren\'t
    --     required by your user pool and that your app client can write. For
    --     more information, see
    --     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RespondToAuthChallenge.html RespondToAuthChallenge>.
    --
    --     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
    --     required attribute that already has a value. In
    --     @RespondToAuthChallenge@, set a value for any keys that Amazon
    --     Cognito returned in the @requiredAttributes@ parameter, then use the
    --     @UpdateUserAttributes@ API operation to modify the value of any
    --     additional attributes.
    --
    -- -   @MFA_SETUP@: For users who are required to setup an MFA factor
    --     before they can sign in. The MFA types activated for the user pool
    --     will be listed in the challenge parameters @MFA_CAN_SETUP@ value.
    --
    --     To set up software token MFA, use the session returned here from
    --     @InitiateAuth@ as an input to @AssociateSoftwareToken@. Use the
    --     session returned by @VerifySoftwareToken@ as an input to
    --     @RespondToAuthChallenge@ with challenge name @MFA_SETUP@ to complete
    --     sign-in. To set up SMS MFA, an administrator should help the user to
    --     add a phone number to their account, and then the user should call
    --     @InitiateAuth@ again to restart sign-in.
    challengeName :: Prelude.Maybe ChallengeNameType,
    -- | The challenge parameters. These are returned in the @InitiateAuth@
    -- response if you must pass another challenge. The responses in this
    -- parameter should be used to compute inputs to the next call
    -- (@RespondToAuthChallenge@).
    --
    -- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
    challengeParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The session that should pass both ways in challenge-response calls to
    -- the service. If the caller must pass another challenge, they return a
    -- session with other challenge parameters. This session should be passed
    -- as it is to the next @RespondToAuthChallenge@ API call.
    session :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitiateAuthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationResult', 'initiateAuthResponse_authenticationResult' - The result of the authentication response. This result is only returned
-- if the caller doesn\'t need to pass another challenge. If the caller
-- does need to pass another challenge before it gets tokens,
-- @ChallengeName@, @ChallengeParameters@, and @Session@ are returned.
--
-- 'challengeName', 'initiateAuthResponse_challengeName' - The name of the challenge that you\'re responding to with this call.
-- This name is returned in the @AdminInitiateAuth@ response if you must
-- pass another challenge.
--
-- Valid values include the following:
--
-- All of the following challenges require @USERNAME@ and @SECRET_HASH@ (if
-- applicable) in the parameters.
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
-- -   @DEVICE_SRP_AUTH@: If device tracking was activated on your user
--     pool and the previous challenges were passed, this challenge is
--     returned so that Amazon Cognito can start tracking this device.
--
-- -   @DEVICE_PASSWORD_VERIFIER@: Similar to @PASSWORD_VERIFIER@, but for
--     devices only.
--
-- -   @NEW_PASSWORD_REQUIRED@: For users who are required to change their
--     passwords after successful first login.
--
--     Respond to this challenge with @NEW_PASSWORD@ and any required
--     attributes that Amazon Cognito returned in the @requiredAttributes@
--     parameter. You can also set values for attributes that aren\'t
--     required by your user pool and that your app client can write. For
--     more information, see
--     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RespondToAuthChallenge.html RespondToAuthChallenge>.
--
--     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
--     required attribute that already has a value. In
--     @RespondToAuthChallenge@, set a value for any keys that Amazon
--     Cognito returned in the @requiredAttributes@ parameter, then use the
--     @UpdateUserAttributes@ API operation to modify the value of any
--     additional attributes.
--
-- -   @MFA_SETUP@: For users who are required to setup an MFA factor
--     before they can sign in. The MFA types activated for the user pool
--     will be listed in the challenge parameters @MFA_CAN_SETUP@ value.
--
--     To set up software token MFA, use the session returned here from
--     @InitiateAuth@ as an input to @AssociateSoftwareToken@. Use the
--     session returned by @VerifySoftwareToken@ as an input to
--     @RespondToAuthChallenge@ with challenge name @MFA_SETUP@ to complete
--     sign-in. To set up SMS MFA, an administrator should help the user to
--     add a phone number to their account, and then the user should call
--     @InitiateAuth@ again to restart sign-in.
--
-- 'challengeParameters', 'initiateAuthResponse_challengeParameters' - The challenge parameters. These are returned in the @InitiateAuth@
-- response if you must pass another challenge. The responses in this
-- parameter should be used to compute inputs to the next call
-- (@RespondToAuthChallenge@).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
--
-- 'session', 'initiateAuthResponse_session' - The session that should pass both ways in challenge-response calls to
-- the service. If the caller must pass another challenge, they return a
-- session with other challenge parameters. This session should be passed
-- as it is to the next @RespondToAuthChallenge@ API call.
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

-- | The result of the authentication response. This result is only returned
-- if the caller doesn\'t need to pass another challenge. If the caller
-- does need to pass another challenge before it gets tokens,
-- @ChallengeName@, @ChallengeParameters@, and @Session@ are returned.
initiateAuthResponse_authenticationResult :: Lens.Lens' InitiateAuthResponse (Prelude.Maybe AuthenticationResultType)
initiateAuthResponse_authenticationResult = Lens.lens (\InitiateAuthResponse' {authenticationResult} -> authenticationResult) (\s@InitiateAuthResponse' {} a -> s {authenticationResult = a} :: InitiateAuthResponse)

-- | The name of the challenge that you\'re responding to with this call.
-- This name is returned in the @AdminInitiateAuth@ response if you must
-- pass another challenge.
--
-- Valid values include the following:
--
-- All of the following challenges require @USERNAME@ and @SECRET_HASH@ (if
-- applicable) in the parameters.
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
-- -   @DEVICE_SRP_AUTH@: If device tracking was activated on your user
--     pool and the previous challenges were passed, this challenge is
--     returned so that Amazon Cognito can start tracking this device.
--
-- -   @DEVICE_PASSWORD_VERIFIER@: Similar to @PASSWORD_VERIFIER@, but for
--     devices only.
--
-- -   @NEW_PASSWORD_REQUIRED@: For users who are required to change their
--     passwords after successful first login.
--
--     Respond to this challenge with @NEW_PASSWORD@ and any required
--     attributes that Amazon Cognito returned in the @requiredAttributes@
--     parameter. You can also set values for attributes that aren\'t
--     required by your user pool and that your app client can write. For
--     more information, see
--     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RespondToAuthChallenge.html RespondToAuthChallenge>.
--
--     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
--     required attribute that already has a value. In
--     @RespondToAuthChallenge@, set a value for any keys that Amazon
--     Cognito returned in the @requiredAttributes@ parameter, then use the
--     @UpdateUserAttributes@ API operation to modify the value of any
--     additional attributes.
--
-- -   @MFA_SETUP@: For users who are required to setup an MFA factor
--     before they can sign in. The MFA types activated for the user pool
--     will be listed in the challenge parameters @MFA_CAN_SETUP@ value.
--
--     To set up software token MFA, use the session returned here from
--     @InitiateAuth@ as an input to @AssociateSoftwareToken@. Use the
--     session returned by @VerifySoftwareToken@ as an input to
--     @RespondToAuthChallenge@ with challenge name @MFA_SETUP@ to complete
--     sign-in. To set up SMS MFA, an administrator should help the user to
--     add a phone number to their account, and then the user should call
--     @InitiateAuth@ again to restart sign-in.
initiateAuthResponse_challengeName :: Lens.Lens' InitiateAuthResponse (Prelude.Maybe ChallengeNameType)
initiateAuthResponse_challengeName = Lens.lens (\InitiateAuthResponse' {challengeName} -> challengeName) (\s@InitiateAuthResponse' {} a -> s {challengeName = a} :: InitiateAuthResponse)

-- | The challenge parameters. These are returned in the @InitiateAuth@
-- response if you must pass another challenge. The responses in this
-- parameter should be used to compute inputs to the next call
-- (@RespondToAuthChallenge@).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
initiateAuthResponse_challengeParameters :: Lens.Lens' InitiateAuthResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
initiateAuthResponse_challengeParameters = Lens.lens (\InitiateAuthResponse' {challengeParameters} -> challengeParameters) (\s@InitiateAuthResponse' {} a -> s {challengeParameters = a} :: InitiateAuthResponse) Prelude.. Lens.mapping Lens.coerced

-- | The session that should pass both ways in challenge-response calls to
-- the service. If the caller must pass another challenge, they return a
-- session with other challenge parameters. This session should be passed
-- as it is to the next @RespondToAuthChallenge@ API call.
initiateAuthResponse_session :: Lens.Lens' InitiateAuthResponse (Prelude.Maybe Prelude.Text)
initiateAuthResponse_session = Lens.lens (\InitiateAuthResponse' {session} -> session) (\s@InitiateAuthResponse' {} a -> s {session = a} :: InitiateAuthResponse)

-- | The response's http status code.
initiateAuthResponse_httpStatus :: Lens.Lens' InitiateAuthResponse Prelude.Int
initiateAuthResponse_httpStatus = Lens.lens (\InitiateAuthResponse' {httpStatus} -> httpStatus) (\s@InitiateAuthResponse' {} a -> s {httpStatus = a} :: InitiateAuthResponse)

instance Prelude.NFData InitiateAuthResponse where
  rnf InitiateAuthResponse' {..} =
    Prelude.rnf authenticationResult
      `Prelude.seq` Prelude.rnf challengeName
      `Prelude.seq` Prelude.rnf challengeParameters
      `Prelude.seq` Prelude.rnf session
      `Prelude.seq` Prelude.rnf httpStatus
