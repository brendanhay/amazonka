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
-- Module      : Amazonka.CognitoIdentityProvider.AdminRespondToAuthChallenge
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Responds to an authentication challenge, as an administrator.
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
module Amazonka.CognitoIdentityProvider.AdminRespondToAuthChallenge
  ( -- * Creating a Request
    AdminRespondToAuthChallenge (..),
    newAdminRespondToAuthChallenge,

    -- * Request Lenses
    adminRespondToAuthChallenge_analyticsMetadata,
    adminRespondToAuthChallenge_clientMetadata,
    adminRespondToAuthChallenge_session,
    adminRespondToAuthChallenge_contextData,
    adminRespondToAuthChallenge_challengeResponses,
    adminRespondToAuthChallenge_userPoolId,
    adminRespondToAuthChallenge_clientId,
    adminRespondToAuthChallenge_challengeName,

    -- * Destructuring the Response
    AdminRespondToAuthChallengeResponse (..),
    newAdminRespondToAuthChallengeResponse,

    -- * Response Lenses
    adminRespondToAuthChallengeResponse_authenticationResult,
    adminRespondToAuthChallengeResponse_session,
    adminRespondToAuthChallengeResponse_challengeName,
    adminRespondToAuthChallengeResponse_challengeParameters,
    adminRespondToAuthChallengeResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to respond to the authentication challenge, as an
-- administrator.
--
-- /See:/ 'newAdminRespondToAuthChallenge' smart constructor.
data AdminRespondToAuthChallenge = AdminRespondToAuthChallenge'
  { -- | The analytics metadata for collecting Amazon Pinpoint metrics for
    -- @AdminRespondToAuthChallenge@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the AdminRespondToAuthChallenge API action,
    -- Amazon Cognito invokes any functions that you have assigned to the
    -- following triggers:
    --
    -- -   pre sign-up
    --
    -- -   custom message
    --
    -- -   post authentication
    --
    -- -   user migration
    --
    -- -   pre token generation
    --
    -- -   define auth challenge
    --
    -- -   create auth challenge
    --
    -- -   verify auth challenge response
    --
    -- When Amazon Cognito invokes any of these functions, it passes a JSON
    -- payload, which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute that provides the data that you assigned to
    -- the ClientMetadata parameter in your AdminRespondToAuthChallenge
    -- request. In your function code in Lambda, you can process the
    -- @clientMetadata@ value to enhance your workflow for your specific needs.
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
    -- to the service. If an @InitiateAuth@ or @RespondToAuthChallenge@ API
    -- call determines that the caller must pass another challenge, it returns
    -- a session with other challenge parameters. This session should be passed
    -- as it is to the next @RespondToAuthChallenge@ API call.
    session :: Prelude.Maybe Prelude.Text,
    -- | Contextual data about your user session, such as the device fingerprint,
    -- IP address, or location. Amazon Cognito advanced security evaluates the
    -- risk of an authentication event based on the context that your app
    -- generates and passes to Amazon Cognito when it makes API requests.
    contextData :: Prelude.Maybe ContextDataType,
    -- | The challenge responses. These are inputs corresponding to the value of
    -- @ChallengeName@, for example:
    --
    -- -   @SMS_MFA@: @SMS_MFA_CODE@, @USERNAME@, @SECRET_HASH@ (if app client
    --     is configured with client secret).
    --
    -- -   @PASSWORD_VERIFIER@: @PASSWORD_CLAIM_SIGNATURE@,
    --     @PASSWORD_CLAIM_SECRET_BLOCK@, @TIMESTAMP@, @USERNAME@,
    --     @SECRET_HASH@ (if app client is configured with client secret).
    --
    --     @PASSWORD_VERIFIER@ requires @DEVICE_KEY@ when signing in with a
    --     remembered device.
    --
    -- -   @ADMIN_NO_SRP_AUTH@: @PASSWORD@, @USERNAME@, @SECRET_HASH@ (if app
    --     client is configured with client secret).
    --
    -- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, @USERNAME@, @SECRET_HASH@
    --     (if app client is configured with client secret). To set any
    --     required attributes that Amazon Cognito returned as
    --     @requiredAttributes@ in the @AdminInitiateAuth@ response, add a
    --     @userAttributes.attributename @ parameter. This parameter can also
    --     set values for writable attributes that aren\'t required by your
    --     user pool.
    --
    --     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
    --     required attribute that already has a value. In
    --     @AdminRespondToAuthChallenge@, set a value for any keys that Amazon
    --     Cognito returned in the @requiredAttributes@ parameter, then use the
    --     @AdminUpdateUserAttributes@ API operation to modify the value of any
    --     additional attributes.
    --
    -- -   @MFA_SETUP@ requires @USERNAME@, plus you must use the session value
    --     returned by @VerifySoftwareToken@ in the @Session@ parameter.
    --
    -- The value of the @USERNAME@ attribute must be the user\'s actual
    -- username, not an alias (such as an email address or phone number). To
    -- make this simpler, the @AdminInitiateAuth@ response includes the actual
    -- username value in the @USERNAMEUSER_ID_FOR_SRP@ attribute. This happens
    -- even if you specified an alias in your call to @AdminInitiateAuth@.
    challengeResponses :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the Amazon Cognito user pool.
    userPoolId :: Prelude.Text,
    -- | The app client ID.
    clientId :: Data.Sensitive Prelude.Text,
    -- | The challenge name. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
    challengeName :: ChallengeNameType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminRespondToAuthChallenge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyticsMetadata', 'adminRespondToAuthChallenge_analyticsMetadata' - The analytics metadata for collecting Amazon Pinpoint metrics for
-- @AdminRespondToAuthChallenge@ calls.
--
-- 'clientMetadata', 'adminRespondToAuthChallenge_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the AdminRespondToAuthChallenge API action,
-- Amazon Cognito invokes any functions that you have assigned to the
-- following triggers:
--
-- -   pre sign-up
--
-- -   custom message
--
-- -   post authentication
--
-- -   user migration
--
-- -   pre token generation
--
-- -   define auth challenge
--
-- -   create auth challenge
--
-- -   verify auth challenge response
--
-- When Amazon Cognito invokes any of these functions, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute that provides the data that you assigned to
-- the ClientMetadata parameter in your AdminRespondToAuthChallenge
-- request. In your function code in Lambda, you can process the
-- @clientMetadata@ value to enhance your workflow for your specific needs.
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
-- 'session', 'adminRespondToAuthChallenge_session' - The session that should be passed both ways in challenge-response calls
-- to the service. If an @InitiateAuth@ or @RespondToAuthChallenge@ API
-- call determines that the caller must pass another challenge, it returns
-- a session with other challenge parameters. This session should be passed
-- as it is to the next @RespondToAuthChallenge@ API call.
--
-- 'contextData', 'adminRespondToAuthChallenge_contextData' - Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
--
-- 'challengeResponses', 'adminRespondToAuthChallenge_challengeResponses' - The challenge responses. These are inputs corresponding to the value of
-- @ChallengeName@, for example:
--
-- -   @SMS_MFA@: @SMS_MFA_CODE@, @USERNAME@, @SECRET_HASH@ (if app client
--     is configured with client secret).
--
-- -   @PASSWORD_VERIFIER@: @PASSWORD_CLAIM_SIGNATURE@,
--     @PASSWORD_CLAIM_SECRET_BLOCK@, @TIMESTAMP@, @USERNAME@,
--     @SECRET_HASH@ (if app client is configured with client secret).
--
--     @PASSWORD_VERIFIER@ requires @DEVICE_KEY@ when signing in with a
--     remembered device.
--
-- -   @ADMIN_NO_SRP_AUTH@: @PASSWORD@, @USERNAME@, @SECRET_HASH@ (if app
--     client is configured with client secret).
--
-- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, @USERNAME@, @SECRET_HASH@
--     (if app client is configured with client secret). To set any
--     required attributes that Amazon Cognito returned as
--     @requiredAttributes@ in the @AdminInitiateAuth@ response, add a
--     @userAttributes.attributename @ parameter. This parameter can also
--     set values for writable attributes that aren\'t required by your
--     user pool.
--
--     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
--     required attribute that already has a value. In
--     @AdminRespondToAuthChallenge@, set a value for any keys that Amazon
--     Cognito returned in the @requiredAttributes@ parameter, then use the
--     @AdminUpdateUserAttributes@ API operation to modify the value of any
--     additional attributes.
--
-- -   @MFA_SETUP@ requires @USERNAME@, plus you must use the session value
--     returned by @VerifySoftwareToken@ in the @Session@ parameter.
--
-- The value of the @USERNAME@ attribute must be the user\'s actual
-- username, not an alias (such as an email address or phone number). To
-- make this simpler, the @AdminInitiateAuth@ response includes the actual
-- username value in the @USERNAMEUSER_ID_FOR_SRP@ attribute. This happens
-- even if you specified an alias in your call to @AdminInitiateAuth@.
--
-- 'userPoolId', 'adminRespondToAuthChallenge_userPoolId' - The ID of the Amazon Cognito user pool.
--
-- 'clientId', 'adminRespondToAuthChallenge_clientId' - The app client ID.
--
-- 'challengeName', 'adminRespondToAuthChallenge_challengeName' - The challenge name. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
newAdminRespondToAuthChallenge ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  -- | 'challengeName'
  ChallengeNameType ->
  AdminRespondToAuthChallenge
newAdminRespondToAuthChallenge
  pUserPoolId_
  pClientId_
  pChallengeName_ =
    AdminRespondToAuthChallenge'
      { analyticsMetadata =
          Prelude.Nothing,
        clientMetadata = Prelude.Nothing,
        session = Prelude.Nothing,
        contextData = Prelude.Nothing,
        challengeResponses = Prelude.Nothing,
        userPoolId = pUserPoolId_,
        clientId = Data._Sensitive Lens.# pClientId_,
        challengeName = pChallengeName_
      }

-- | The analytics metadata for collecting Amazon Pinpoint metrics for
-- @AdminRespondToAuthChallenge@ calls.
adminRespondToAuthChallenge_analyticsMetadata :: Lens.Lens' AdminRespondToAuthChallenge (Prelude.Maybe AnalyticsMetadataType)
adminRespondToAuthChallenge_analyticsMetadata = Lens.lens (\AdminRespondToAuthChallenge' {analyticsMetadata} -> analyticsMetadata) (\s@AdminRespondToAuthChallenge' {} a -> s {analyticsMetadata = a} :: AdminRespondToAuthChallenge)

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the AdminRespondToAuthChallenge API action,
-- Amazon Cognito invokes any functions that you have assigned to the
-- following triggers:
--
-- -   pre sign-up
--
-- -   custom message
--
-- -   post authentication
--
-- -   user migration
--
-- -   pre token generation
--
-- -   define auth challenge
--
-- -   create auth challenge
--
-- -   verify auth challenge response
--
-- When Amazon Cognito invokes any of these functions, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute that provides the data that you assigned to
-- the ClientMetadata parameter in your AdminRespondToAuthChallenge
-- request. In your function code in Lambda, you can process the
-- @clientMetadata@ value to enhance your workflow for your specific needs.
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
adminRespondToAuthChallenge_clientMetadata :: Lens.Lens' AdminRespondToAuthChallenge (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminRespondToAuthChallenge_clientMetadata = Lens.lens (\AdminRespondToAuthChallenge' {clientMetadata} -> clientMetadata) (\s@AdminRespondToAuthChallenge' {} a -> s {clientMetadata = a} :: AdminRespondToAuthChallenge) Prelude.. Lens.mapping Lens.coerced

-- | The session that should be passed both ways in challenge-response calls
-- to the service. If an @InitiateAuth@ or @RespondToAuthChallenge@ API
-- call determines that the caller must pass another challenge, it returns
-- a session with other challenge parameters. This session should be passed
-- as it is to the next @RespondToAuthChallenge@ API call.
adminRespondToAuthChallenge_session :: Lens.Lens' AdminRespondToAuthChallenge (Prelude.Maybe Prelude.Text)
adminRespondToAuthChallenge_session = Lens.lens (\AdminRespondToAuthChallenge' {session} -> session) (\s@AdminRespondToAuthChallenge' {} a -> s {session = a} :: AdminRespondToAuthChallenge)

-- | Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
adminRespondToAuthChallenge_contextData :: Lens.Lens' AdminRespondToAuthChallenge (Prelude.Maybe ContextDataType)
adminRespondToAuthChallenge_contextData = Lens.lens (\AdminRespondToAuthChallenge' {contextData} -> contextData) (\s@AdminRespondToAuthChallenge' {} a -> s {contextData = a} :: AdminRespondToAuthChallenge)

-- | The challenge responses. These are inputs corresponding to the value of
-- @ChallengeName@, for example:
--
-- -   @SMS_MFA@: @SMS_MFA_CODE@, @USERNAME@, @SECRET_HASH@ (if app client
--     is configured with client secret).
--
-- -   @PASSWORD_VERIFIER@: @PASSWORD_CLAIM_SIGNATURE@,
--     @PASSWORD_CLAIM_SECRET_BLOCK@, @TIMESTAMP@, @USERNAME@,
--     @SECRET_HASH@ (if app client is configured with client secret).
--
--     @PASSWORD_VERIFIER@ requires @DEVICE_KEY@ when signing in with a
--     remembered device.
--
-- -   @ADMIN_NO_SRP_AUTH@: @PASSWORD@, @USERNAME@, @SECRET_HASH@ (if app
--     client is configured with client secret).
--
-- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, @USERNAME@, @SECRET_HASH@
--     (if app client is configured with client secret). To set any
--     required attributes that Amazon Cognito returned as
--     @requiredAttributes@ in the @AdminInitiateAuth@ response, add a
--     @userAttributes.attributename @ parameter. This parameter can also
--     set values for writable attributes that aren\'t required by your
--     user pool.
--
--     In a @NEW_PASSWORD_REQUIRED@ challenge response, you can\'t modify a
--     required attribute that already has a value. In
--     @AdminRespondToAuthChallenge@, set a value for any keys that Amazon
--     Cognito returned in the @requiredAttributes@ parameter, then use the
--     @AdminUpdateUserAttributes@ API operation to modify the value of any
--     additional attributes.
--
-- -   @MFA_SETUP@ requires @USERNAME@, plus you must use the session value
--     returned by @VerifySoftwareToken@ in the @Session@ parameter.
--
-- The value of the @USERNAME@ attribute must be the user\'s actual
-- username, not an alias (such as an email address or phone number). To
-- make this simpler, the @AdminInitiateAuth@ response includes the actual
-- username value in the @USERNAMEUSER_ID_FOR_SRP@ attribute. This happens
-- even if you specified an alias in your call to @AdminInitiateAuth@.
adminRespondToAuthChallenge_challengeResponses :: Lens.Lens' AdminRespondToAuthChallenge (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminRespondToAuthChallenge_challengeResponses = Lens.lens (\AdminRespondToAuthChallenge' {challengeResponses} -> challengeResponses) (\s@AdminRespondToAuthChallenge' {} a -> s {challengeResponses = a} :: AdminRespondToAuthChallenge) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Cognito user pool.
adminRespondToAuthChallenge_userPoolId :: Lens.Lens' AdminRespondToAuthChallenge Prelude.Text
adminRespondToAuthChallenge_userPoolId = Lens.lens (\AdminRespondToAuthChallenge' {userPoolId} -> userPoolId) (\s@AdminRespondToAuthChallenge' {} a -> s {userPoolId = a} :: AdminRespondToAuthChallenge)

-- | The app client ID.
adminRespondToAuthChallenge_clientId :: Lens.Lens' AdminRespondToAuthChallenge Prelude.Text
adminRespondToAuthChallenge_clientId = Lens.lens (\AdminRespondToAuthChallenge' {clientId} -> clientId) (\s@AdminRespondToAuthChallenge' {} a -> s {clientId = a} :: AdminRespondToAuthChallenge) Prelude.. Data._Sensitive

-- | The challenge name. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
adminRespondToAuthChallenge_challengeName :: Lens.Lens' AdminRespondToAuthChallenge ChallengeNameType
adminRespondToAuthChallenge_challengeName = Lens.lens (\AdminRespondToAuthChallenge' {challengeName} -> challengeName) (\s@AdminRespondToAuthChallenge' {} a -> s {challengeName = a} :: AdminRespondToAuthChallenge)

instance Core.AWSRequest AdminRespondToAuthChallenge where
  type
    AWSResponse AdminRespondToAuthChallenge =
      AdminRespondToAuthChallengeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminRespondToAuthChallengeResponse'
            Prelude.<$> (x Data..?> "AuthenticationResult")
            Prelude.<*> (x Data..?> "Session")
            Prelude.<*> (x Data..?> "ChallengeName")
            Prelude.<*> ( x Data..?> "ChallengeParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminRespondToAuthChallenge where
  hashWithSalt _salt AdminRespondToAuthChallenge' {..} =
    _salt `Prelude.hashWithSalt` analyticsMetadata
      `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` session
      `Prelude.hashWithSalt` contextData
      `Prelude.hashWithSalt` challengeResponses
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` challengeName

instance Prelude.NFData AdminRespondToAuthChallenge where
  rnf AdminRespondToAuthChallenge' {..} =
    Prelude.rnf analyticsMetadata
      `Prelude.seq` Prelude.rnf clientMetadata
      `Prelude.seq` Prelude.rnf session
      `Prelude.seq` Prelude.rnf contextData
      `Prelude.seq` Prelude.rnf challengeResponses
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf challengeName

instance Data.ToHeaders AdminRespondToAuthChallenge where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminRespondToAuthChallenge" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminRespondToAuthChallenge where
  toJSON AdminRespondToAuthChallenge' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnalyticsMetadata" Data..=)
              Prelude.<$> analyticsMetadata,
            ("ClientMetadata" Data..=)
              Prelude.<$> clientMetadata,
            ("Session" Data..=) Prelude.<$> session,
            ("ContextData" Data..=) Prelude.<$> contextData,
            ("ChallengeResponses" Data..=)
              Prelude.<$> challengeResponses,
            Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("ClientId" Data..= clientId),
            Prelude.Just
              ("ChallengeName" Data..= challengeName)
          ]
      )

instance Data.ToPath AdminRespondToAuthChallenge where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminRespondToAuthChallenge where
  toQuery = Prelude.const Prelude.mempty

-- | Responds to the authentication challenge, as an administrator.
--
-- /See:/ 'newAdminRespondToAuthChallengeResponse' smart constructor.
data AdminRespondToAuthChallengeResponse = AdminRespondToAuthChallengeResponse'
  { -- | The result returned by the server in response to the authentication
    -- request.
    authenticationResult :: Prelude.Maybe AuthenticationResultType,
    -- | The session that should be passed both ways in challenge-response calls
    -- to the service. If the caller must pass another challenge, they return a
    -- session with other challenge parameters. This session should be passed
    -- as it is to the next @RespondToAuthChallenge@ API call.
    session :: Prelude.Maybe Prelude.Text,
    -- | The name of the challenge. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
    challengeName :: Prelude.Maybe ChallengeNameType,
    -- | The challenge parameters. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
    challengeParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminRespondToAuthChallengeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationResult', 'adminRespondToAuthChallengeResponse_authenticationResult' - The result returned by the server in response to the authentication
-- request.
--
-- 'session', 'adminRespondToAuthChallengeResponse_session' - The session that should be passed both ways in challenge-response calls
-- to the service. If the caller must pass another challenge, they return a
-- session with other challenge parameters. This session should be passed
-- as it is to the next @RespondToAuthChallenge@ API call.
--
-- 'challengeName', 'adminRespondToAuthChallengeResponse_challengeName' - The name of the challenge. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
--
-- 'challengeParameters', 'adminRespondToAuthChallengeResponse_challengeParameters' - The challenge parameters. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
--
-- 'httpStatus', 'adminRespondToAuthChallengeResponse_httpStatus' - The response's http status code.
newAdminRespondToAuthChallengeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminRespondToAuthChallengeResponse
newAdminRespondToAuthChallengeResponse pHttpStatus_ =
  AdminRespondToAuthChallengeResponse'
    { authenticationResult =
        Prelude.Nothing,
      session = Prelude.Nothing,
      challengeName = Prelude.Nothing,
      challengeParameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The result returned by the server in response to the authentication
-- request.
adminRespondToAuthChallengeResponse_authenticationResult :: Lens.Lens' AdminRespondToAuthChallengeResponse (Prelude.Maybe AuthenticationResultType)
adminRespondToAuthChallengeResponse_authenticationResult = Lens.lens (\AdminRespondToAuthChallengeResponse' {authenticationResult} -> authenticationResult) (\s@AdminRespondToAuthChallengeResponse' {} a -> s {authenticationResult = a} :: AdminRespondToAuthChallengeResponse)

-- | The session that should be passed both ways in challenge-response calls
-- to the service. If the caller must pass another challenge, they return a
-- session with other challenge parameters. This session should be passed
-- as it is to the next @RespondToAuthChallenge@ API call.
adminRespondToAuthChallengeResponse_session :: Lens.Lens' AdminRespondToAuthChallengeResponse (Prelude.Maybe Prelude.Text)
adminRespondToAuthChallengeResponse_session = Lens.lens (\AdminRespondToAuthChallengeResponse' {session} -> session) (\s@AdminRespondToAuthChallengeResponse' {} a -> s {session = a} :: AdminRespondToAuthChallengeResponse)

-- | The name of the challenge. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
adminRespondToAuthChallengeResponse_challengeName :: Lens.Lens' AdminRespondToAuthChallengeResponse (Prelude.Maybe ChallengeNameType)
adminRespondToAuthChallengeResponse_challengeName = Lens.lens (\AdminRespondToAuthChallengeResponse' {challengeName} -> challengeName) (\s@AdminRespondToAuthChallengeResponse' {} a -> s {challengeName = a} :: AdminRespondToAuthChallengeResponse)

-- | The challenge parameters. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
adminRespondToAuthChallengeResponse_challengeParameters :: Lens.Lens' AdminRespondToAuthChallengeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminRespondToAuthChallengeResponse_challengeParameters = Lens.lens (\AdminRespondToAuthChallengeResponse' {challengeParameters} -> challengeParameters) (\s@AdminRespondToAuthChallengeResponse' {} a -> s {challengeParameters = a} :: AdminRespondToAuthChallengeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
adminRespondToAuthChallengeResponse_httpStatus :: Lens.Lens' AdminRespondToAuthChallengeResponse Prelude.Int
adminRespondToAuthChallengeResponse_httpStatus = Lens.lens (\AdminRespondToAuthChallengeResponse' {httpStatus} -> httpStatus) (\s@AdminRespondToAuthChallengeResponse' {} a -> s {httpStatus = a} :: AdminRespondToAuthChallengeResponse)

instance
  Prelude.NFData
    AdminRespondToAuthChallengeResponse
  where
  rnf AdminRespondToAuthChallengeResponse' {..} =
    Prelude.rnf authenticationResult
      `Prelude.seq` Prelude.rnf session
      `Prelude.seq` Prelude.rnf challengeName
      `Prelude.seq` Prelude.rnf challengeParameters
      `Prelude.seq` Prelude.rnf httpStatus
