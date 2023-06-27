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
-- Module      : Amazonka.CognitoIdentityProvider.RespondToAuthChallenge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Responds to the authentication challenge.
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
module Amazonka.CognitoIdentityProvider.RespondToAuthChallenge
  ( -- * Creating a Request
    RespondToAuthChallenge (..),
    newRespondToAuthChallenge,

    -- * Request Lenses
    respondToAuthChallenge_analyticsMetadata,
    respondToAuthChallenge_challengeResponses,
    respondToAuthChallenge_clientMetadata,
    respondToAuthChallenge_session,
    respondToAuthChallenge_userContextData,
    respondToAuthChallenge_clientId,
    respondToAuthChallenge_challengeName,

    -- * Destructuring the Response
    RespondToAuthChallengeResponse (..),
    newRespondToAuthChallengeResponse,

    -- * Response Lenses
    respondToAuthChallengeResponse_authenticationResult,
    respondToAuthChallengeResponse_challengeName,
    respondToAuthChallengeResponse_challengeParameters,
    respondToAuthChallengeResponse_session,
    respondToAuthChallengeResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to respond to an authentication challenge.
--
-- /See:/ 'newRespondToAuthChallenge' smart constructor.
data RespondToAuthChallenge = RespondToAuthChallenge'
  { -- | The Amazon Pinpoint analytics metadata that contributes to your metrics
    -- for @RespondToAuthChallenge@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | The challenge responses. These are inputs corresponding to the value of
    -- @ChallengeName@, for example:
    --
    -- @SECRET_HASH@ (if app client is configured with client secret) applies
    -- to all of the inputs that follow (including @SOFTWARE_TOKEN_MFA@).
    --
    -- -   @SMS_MFA@: @SMS_MFA_CODE@, @USERNAME@.
    --
    -- -   @PASSWORD_VERIFIER@: @PASSWORD_CLAIM_SIGNATURE@,
    --     @PASSWORD_CLAIM_SECRET_BLOCK@, @TIMESTAMP@, @USERNAME@.
    --
    --     @PASSWORD_VERIFIER@ requires @DEVICE_KEY@ when you sign in with a
    --     remembered device.
    --
    -- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, @USERNAME@, @SECRET_HASH@
    --     (if app client is configured with client secret). To set any
    --     required attributes that Amazon Cognito returned as
    --     @requiredAttributes@ in the @InitiateAuth@ response, add a
    --     @userAttributes.@/@attributename@/@ @ parameter. This parameter can
    --     also set values for writable attributes that aren\'t required by
    --     your user pool.
    --
    --     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
    --     required attribute that already has a value. In
    --     @RespondToAuthChallenge@, set a value for any keys that Amazon
    --     Cognito returned in the @requiredAttributes@ parameter, then use the
    --     @UpdateUserAttributes@ API operation to modify the value of any
    --     additional attributes.
    --
    -- -   @SOFTWARE_TOKEN_MFA@: @USERNAME@ and @SOFTWARE_TOKEN_MFA_CODE@ are
    --     required attributes.
    --
    -- -   @DEVICE_SRP_AUTH@ requires @USERNAME@, @DEVICE_KEY@, @SRP_A@ (and
    --     @SECRET_HASH@).
    --
    -- -   @DEVICE_PASSWORD_VERIFIER@ requires everything that
    --     @PASSWORD_VERIFIER@ requires, plus @DEVICE_KEY@.
    --
    -- -   @MFA_SETUP@ requires @USERNAME@, plus you must use the session value
    --     returned by @VerifySoftwareToken@ in the @Session@ parameter.
    challengeResponses :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the RespondToAuthChallenge API action, Amazon
    -- Cognito invokes any functions that are assigned to the following
    -- triggers: /post authentication/, /pre token generation/, /define auth
    -- challenge/, /create auth challenge/, and /verify auth challenge/. When
    -- Amazon Cognito invokes any of these functions, it passes a JSON payload,
    -- which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your RespondToAuthChallenge request. In
    -- your function code in Lambda, you can process the @clientMetadata@ value
    -- to enhance your workflow for your specific needs.
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
    -- | The session that should be passed both ways in challenge-response calls
    -- to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call
    -- determines that the caller must pass another challenge, they return a
    -- session with other challenge parameters. This session should be passed
    -- as it is to the next @RespondToAuthChallenge@ API call.
    session :: Prelude.Maybe Prelude.Text,
    -- | Contextual data about your user session, such as the device fingerprint,
    -- IP address, or location. Amazon Cognito advanced security evaluates the
    -- risk of an authentication event based on the context that your app
    -- generates and passes to Amazon Cognito when it makes API requests.
    userContextData :: Prelude.Maybe UserContextDataType,
    -- | The app client ID.
    clientId :: Data.Sensitive Prelude.Text,
    -- | The challenge name. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
    --
    -- @ADMIN_NO_SRP_AUTH@ isn\'t a valid value.
    challengeName :: ChallengeNameType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RespondToAuthChallenge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyticsMetadata', 'respondToAuthChallenge_analyticsMetadata' - The Amazon Pinpoint analytics metadata that contributes to your metrics
-- for @RespondToAuthChallenge@ calls.
--
-- 'challengeResponses', 'respondToAuthChallenge_challengeResponses' - The challenge responses. These are inputs corresponding to the value of
-- @ChallengeName@, for example:
--
-- @SECRET_HASH@ (if app client is configured with client secret) applies
-- to all of the inputs that follow (including @SOFTWARE_TOKEN_MFA@).
--
-- -   @SMS_MFA@: @SMS_MFA_CODE@, @USERNAME@.
--
-- -   @PASSWORD_VERIFIER@: @PASSWORD_CLAIM_SIGNATURE@,
--     @PASSWORD_CLAIM_SECRET_BLOCK@, @TIMESTAMP@, @USERNAME@.
--
--     @PASSWORD_VERIFIER@ requires @DEVICE_KEY@ when you sign in with a
--     remembered device.
--
-- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, @USERNAME@, @SECRET_HASH@
--     (if app client is configured with client secret). To set any
--     required attributes that Amazon Cognito returned as
--     @requiredAttributes@ in the @InitiateAuth@ response, add a
--     @userAttributes.@/@attributename@/@ @ parameter. This parameter can
--     also set values for writable attributes that aren\'t required by
--     your user pool.
--
--     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
--     required attribute that already has a value. In
--     @RespondToAuthChallenge@, set a value for any keys that Amazon
--     Cognito returned in the @requiredAttributes@ parameter, then use the
--     @UpdateUserAttributes@ API operation to modify the value of any
--     additional attributes.
--
-- -   @SOFTWARE_TOKEN_MFA@: @USERNAME@ and @SOFTWARE_TOKEN_MFA_CODE@ are
--     required attributes.
--
-- -   @DEVICE_SRP_AUTH@ requires @USERNAME@, @DEVICE_KEY@, @SRP_A@ (and
--     @SECRET_HASH@).
--
-- -   @DEVICE_PASSWORD_VERIFIER@ requires everything that
--     @PASSWORD_VERIFIER@ requires, plus @DEVICE_KEY@.
--
-- -   @MFA_SETUP@ requires @USERNAME@, plus you must use the session value
--     returned by @VerifySoftwareToken@ in the @Session@ parameter.
--
-- 'clientMetadata', 'respondToAuthChallenge_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the RespondToAuthChallenge API action, Amazon
-- Cognito invokes any functions that are assigned to the following
-- triggers: /post authentication/, /pre token generation/, /define auth
-- challenge/, /create auth challenge/, and /verify auth challenge/. When
-- Amazon Cognito invokes any of these functions, it passes a JSON payload,
-- which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your RespondToAuthChallenge request. In
-- your function code in Lambda, you can process the @clientMetadata@ value
-- to enhance your workflow for your specific needs.
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
-- 'session', 'respondToAuthChallenge_session' - The session that should be passed both ways in challenge-response calls
-- to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call
-- determines that the caller must pass another challenge, they return a
-- session with other challenge parameters. This session should be passed
-- as it is to the next @RespondToAuthChallenge@ API call.
--
-- 'userContextData', 'respondToAuthChallenge_userContextData' - Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
--
-- 'clientId', 'respondToAuthChallenge_clientId' - The app client ID.
--
-- 'challengeName', 'respondToAuthChallenge_challengeName' - The challenge name. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
--
-- @ADMIN_NO_SRP_AUTH@ isn\'t a valid value.
newRespondToAuthChallenge ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'challengeName'
  ChallengeNameType ->
  RespondToAuthChallenge
newRespondToAuthChallenge pClientId_ pChallengeName_ =
  RespondToAuthChallenge'
    { analyticsMetadata =
        Prelude.Nothing,
      challengeResponses = Prelude.Nothing,
      clientMetadata = Prelude.Nothing,
      session = Prelude.Nothing,
      userContextData = Prelude.Nothing,
      clientId = Data._Sensitive Lens.# pClientId_,
      challengeName = pChallengeName_
    }

-- | The Amazon Pinpoint analytics metadata that contributes to your metrics
-- for @RespondToAuthChallenge@ calls.
respondToAuthChallenge_analyticsMetadata :: Lens.Lens' RespondToAuthChallenge (Prelude.Maybe AnalyticsMetadataType)
respondToAuthChallenge_analyticsMetadata = Lens.lens (\RespondToAuthChallenge' {analyticsMetadata} -> analyticsMetadata) (\s@RespondToAuthChallenge' {} a -> s {analyticsMetadata = a} :: RespondToAuthChallenge)

-- | The challenge responses. These are inputs corresponding to the value of
-- @ChallengeName@, for example:
--
-- @SECRET_HASH@ (if app client is configured with client secret) applies
-- to all of the inputs that follow (including @SOFTWARE_TOKEN_MFA@).
--
-- -   @SMS_MFA@: @SMS_MFA_CODE@, @USERNAME@.
--
-- -   @PASSWORD_VERIFIER@: @PASSWORD_CLAIM_SIGNATURE@,
--     @PASSWORD_CLAIM_SECRET_BLOCK@, @TIMESTAMP@, @USERNAME@.
--
--     @PASSWORD_VERIFIER@ requires @DEVICE_KEY@ when you sign in with a
--     remembered device.
--
-- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, @USERNAME@, @SECRET_HASH@
--     (if app client is configured with client secret). To set any
--     required attributes that Amazon Cognito returned as
--     @requiredAttributes@ in the @InitiateAuth@ response, add a
--     @userAttributes.@/@attributename@/@ @ parameter. This parameter can
--     also set values for writable attributes that aren\'t required by
--     your user pool.
--
--     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
--     required attribute that already has a value. In
--     @RespondToAuthChallenge@, set a value for any keys that Amazon
--     Cognito returned in the @requiredAttributes@ parameter, then use the
--     @UpdateUserAttributes@ API operation to modify the value of any
--     additional attributes.
--
-- -   @SOFTWARE_TOKEN_MFA@: @USERNAME@ and @SOFTWARE_TOKEN_MFA_CODE@ are
--     required attributes.
--
-- -   @DEVICE_SRP_AUTH@ requires @USERNAME@, @DEVICE_KEY@, @SRP_A@ (and
--     @SECRET_HASH@).
--
-- -   @DEVICE_PASSWORD_VERIFIER@ requires everything that
--     @PASSWORD_VERIFIER@ requires, plus @DEVICE_KEY@.
--
-- -   @MFA_SETUP@ requires @USERNAME@, plus you must use the session value
--     returned by @VerifySoftwareToken@ in the @Session@ parameter.
respondToAuthChallenge_challengeResponses :: Lens.Lens' RespondToAuthChallenge (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
respondToAuthChallenge_challengeResponses = Lens.lens (\RespondToAuthChallenge' {challengeResponses} -> challengeResponses) (\s@RespondToAuthChallenge' {} a -> s {challengeResponses = a} :: RespondToAuthChallenge) Prelude.. Lens.mapping Lens.coerced

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the RespondToAuthChallenge API action, Amazon
-- Cognito invokes any functions that are assigned to the following
-- triggers: /post authentication/, /pre token generation/, /define auth
-- challenge/, /create auth challenge/, and /verify auth challenge/. When
-- Amazon Cognito invokes any of these functions, it passes a JSON payload,
-- which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your RespondToAuthChallenge request. In
-- your function code in Lambda, you can process the @clientMetadata@ value
-- to enhance your workflow for your specific needs.
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
respondToAuthChallenge_clientMetadata :: Lens.Lens' RespondToAuthChallenge (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
respondToAuthChallenge_clientMetadata = Lens.lens (\RespondToAuthChallenge' {clientMetadata} -> clientMetadata) (\s@RespondToAuthChallenge' {} a -> s {clientMetadata = a} :: RespondToAuthChallenge) Prelude.. Lens.mapping Lens.coerced

-- | The session that should be passed both ways in challenge-response calls
-- to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call
-- determines that the caller must pass another challenge, they return a
-- session with other challenge parameters. This session should be passed
-- as it is to the next @RespondToAuthChallenge@ API call.
respondToAuthChallenge_session :: Lens.Lens' RespondToAuthChallenge (Prelude.Maybe Prelude.Text)
respondToAuthChallenge_session = Lens.lens (\RespondToAuthChallenge' {session} -> session) (\s@RespondToAuthChallenge' {} a -> s {session = a} :: RespondToAuthChallenge)

-- | Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
respondToAuthChallenge_userContextData :: Lens.Lens' RespondToAuthChallenge (Prelude.Maybe UserContextDataType)
respondToAuthChallenge_userContextData = Lens.lens (\RespondToAuthChallenge' {userContextData} -> userContextData) (\s@RespondToAuthChallenge' {} a -> s {userContextData = a} :: RespondToAuthChallenge)

-- | The app client ID.
respondToAuthChallenge_clientId :: Lens.Lens' RespondToAuthChallenge Prelude.Text
respondToAuthChallenge_clientId = Lens.lens (\RespondToAuthChallenge' {clientId} -> clientId) (\s@RespondToAuthChallenge' {} a -> s {clientId = a} :: RespondToAuthChallenge) Prelude.. Data._Sensitive

-- | The challenge name. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
--
-- @ADMIN_NO_SRP_AUTH@ isn\'t a valid value.
respondToAuthChallenge_challengeName :: Lens.Lens' RespondToAuthChallenge ChallengeNameType
respondToAuthChallenge_challengeName = Lens.lens (\RespondToAuthChallenge' {challengeName} -> challengeName) (\s@RespondToAuthChallenge' {} a -> s {challengeName = a} :: RespondToAuthChallenge)

instance Core.AWSRequest RespondToAuthChallenge where
  type
    AWSResponse RespondToAuthChallenge =
      RespondToAuthChallengeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RespondToAuthChallengeResponse'
            Prelude.<$> (x Data..?> "AuthenticationResult")
            Prelude.<*> (x Data..?> "ChallengeName")
            Prelude.<*> ( x
                            Data..?> "ChallengeParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RespondToAuthChallenge where
  hashWithSalt _salt RespondToAuthChallenge' {..} =
    _salt
      `Prelude.hashWithSalt` analyticsMetadata
      `Prelude.hashWithSalt` challengeResponses
      `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` session
      `Prelude.hashWithSalt` userContextData
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` challengeName

instance Prelude.NFData RespondToAuthChallenge where
  rnf RespondToAuthChallenge' {..} =
    Prelude.rnf analyticsMetadata
      `Prelude.seq` Prelude.rnf challengeResponses
      `Prelude.seq` Prelude.rnf clientMetadata
      `Prelude.seq` Prelude.rnf session
      `Prelude.seq` Prelude.rnf userContextData
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf challengeName

instance Data.ToHeaders RespondToAuthChallenge where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.RespondToAuthChallenge" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RespondToAuthChallenge where
  toJSON RespondToAuthChallenge' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnalyticsMetadata" Data..=)
              Prelude.<$> analyticsMetadata,
            ("ChallengeResponses" Data..=)
              Prelude.<$> challengeResponses,
            ("ClientMetadata" Data..=)
              Prelude.<$> clientMetadata,
            ("Session" Data..=) Prelude.<$> session,
            ("UserContextData" Data..=)
              Prelude.<$> userContextData,
            Prelude.Just ("ClientId" Data..= clientId),
            Prelude.Just
              ("ChallengeName" Data..= challengeName)
          ]
      )

instance Data.ToPath RespondToAuthChallenge where
  toPath = Prelude.const "/"

instance Data.ToQuery RespondToAuthChallenge where
  toQuery = Prelude.const Prelude.mempty

-- | The response to respond to the authentication challenge.
--
-- /See:/ 'newRespondToAuthChallengeResponse' smart constructor.
data RespondToAuthChallengeResponse = RespondToAuthChallengeResponse'
  { -- | The result returned by the server in response to the request to respond
    -- to the authentication challenge.
    authenticationResult :: Prelude.Maybe AuthenticationResultType,
    -- | The challenge name. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
    challengeName :: Prelude.Maybe ChallengeNameType,
    -- | The challenge parameters. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
    challengeParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The session that should be passed both ways in challenge-response calls
    -- to the service. If the caller must pass another challenge, they return a
    -- session with other challenge parameters. This session should be passed
    -- as it is to the next @RespondToAuthChallenge@ API call.
    session :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RespondToAuthChallengeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationResult', 'respondToAuthChallengeResponse_authenticationResult' - The result returned by the server in response to the request to respond
-- to the authentication challenge.
--
-- 'challengeName', 'respondToAuthChallengeResponse_challengeName' - The challenge name. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
--
-- 'challengeParameters', 'respondToAuthChallengeResponse_challengeParameters' - The challenge parameters. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
--
-- 'session', 'respondToAuthChallengeResponse_session' - The session that should be passed both ways in challenge-response calls
-- to the service. If the caller must pass another challenge, they return a
-- session with other challenge parameters. This session should be passed
-- as it is to the next @RespondToAuthChallenge@ API call.
--
-- 'httpStatus', 'respondToAuthChallengeResponse_httpStatus' - The response's http status code.
newRespondToAuthChallengeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RespondToAuthChallengeResponse
newRespondToAuthChallengeResponse pHttpStatus_ =
  RespondToAuthChallengeResponse'
    { authenticationResult =
        Prelude.Nothing,
      challengeName = Prelude.Nothing,
      challengeParameters = Prelude.Nothing,
      session = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The result returned by the server in response to the request to respond
-- to the authentication challenge.
respondToAuthChallengeResponse_authenticationResult :: Lens.Lens' RespondToAuthChallengeResponse (Prelude.Maybe AuthenticationResultType)
respondToAuthChallengeResponse_authenticationResult = Lens.lens (\RespondToAuthChallengeResponse' {authenticationResult} -> authenticationResult) (\s@RespondToAuthChallengeResponse' {} a -> s {authenticationResult = a} :: RespondToAuthChallengeResponse)

-- | The challenge name. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
respondToAuthChallengeResponse_challengeName :: Lens.Lens' RespondToAuthChallengeResponse (Prelude.Maybe ChallengeNameType)
respondToAuthChallengeResponse_challengeName = Lens.lens (\RespondToAuthChallengeResponse' {challengeName} -> challengeName) (\s@RespondToAuthChallengeResponse' {} a -> s {challengeName = a} :: RespondToAuthChallengeResponse)

-- | The challenge parameters. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
respondToAuthChallengeResponse_challengeParameters :: Lens.Lens' RespondToAuthChallengeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
respondToAuthChallengeResponse_challengeParameters = Lens.lens (\RespondToAuthChallengeResponse' {challengeParameters} -> challengeParameters) (\s@RespondToAuthChallengeResponse' {} a -> s {challengeParameters = a} :: RespondToAuthChallengeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The session that should be passed both ways in challenge-response calls
-- to the service. If the caller must pass another challenge, they return a
-- session with other challenge parameters. This session should be passed
-- as it is to the next @RespondToAuthChallenge@ API call.
respondToAuthChallengeResponse_session :: Lens.Lens' RespondToAuthChallengeResponse (Prelude.Maybe Prelude.Text)
respondToAuthChallengeResponse_session = Lens.lens (\RespondToAuthChallengeResponse' {session} -> session) (\s@RespondToAuthChallengeResponse' {} a -> s {session = a} :: RespondToAuthChallengeResponse)

-- | The response's http status code.
respondToAuthChallengeResponse_httpStatus :: Lens.Lens' RespondToAuthChallengeResponse Prelude.Int
respondToAuthChallengeResponse_httpStatus = Lens.lens (\RespondToAuthChallengeResponse' {httpStatus} -> httpStatus) (\s@RespondToAuthChallengeResponse' {} a -> s {httpStatus = a} :: RespondToAuthChallengeResponse)

instance
  Prelude.NFData
    RespondToAuthChallengeResponse
  where
  rnf RespondToAuthChallengeResponse' {..} =
    Prelude.rnf authenticationResult
      `Prelude.seq` Prelude.rnf challengeName
      `Prelude.seq` Prelude.rnf challengeParameters
      `Prelude.seq` Prelude.rnf session
      `Prelude.seq` Prelude.rnf httpStatus
