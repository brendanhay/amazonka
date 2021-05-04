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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminRespondToAuthChallenge
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Responds to an authentication challenge, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminRespondToAuthChallenge
  ( -- * Creating a Request
    AdminRespondToAuthChallenge (..),
    newAdminRespondToAuthChallenge,

    -- * Request Lenses
    adminRespondToAuthChallenge_clientMetadata,
    adminRespondToAuthChallenge_challengeResponses,
    adminRespondToAuthChallenge_contextData,
    adminRespondToAuthChallenge_session,
    adminRespondToAuthChallenge_analyticsMetadata,
    adminRespondToAuthChallenge_userPoolId,
    adminRespondToAuthChallenge_clientId,
    adminRespondToAuthChallenge_challengeName,

    -- * Destructuring the Response
    AdminRespondToAuthChallengeResponse (..),
    newAdminRespondToAuthChallengeResponse,

    -- * Response Lenses
    adminRespondToAuthChallengeResponse_authenticationResult,
    adminRespondToAuthChallengeResponse_challengeName,
    adminRespondToAuthChallengeResponse_challengeParameters,
    adminRespondToAuthChallengeResponse_session,
    adminRespondToAuthChallengeResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to respond to the authentication challenge, as an
-- administrator.
--
-- /See:/ 'newAdminRespondToAuthChallenge' smart constructor.
data AdminRespondToAuthChallenge = AdminRespondToAuthChallenge'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the AdminRespondToAuthChallenge API action,
    -- Amazon Cognito invokes any functions that are assigned to the following
    -- triggers: /pre sign-up/, /custom message/, /post authentication/, /user
    -- migration/, /pre token generation/, /define auth challenge/, /create
    -- auth challenge/, and /verify auth challenge response/. When Amazon
    -- Cognito invokes any of these functions, it passes a JSON payload, which
    -- the function receives as input. This payload contains a @clientMetadata@
    -- attribute, which provides the data that you assigned to the
    -- ClientMetadata parameter in your AdminRespondToAuthChallenge request. In
    -- your function code in AWS Lambda, you can process the @clientMetadata@
    -- value to enhance your workflow for your specific needs.
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
    -- -   @ADMIN_NO_SRP_AUTH@: @PASSWORD@, @USERNAME@, @SECRET_HASH@ (if app
    --     client is configured with client secret).
    --
    -- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, any other required
    --     attributes, @USERNAME@, @SECRET_HASH@ (if app client is configured
    --     with client secret).
    --
    -- The value of the @USERNAME@ attribute must be the user\'s actual
    -- username, not an alias (such as email address or phone number). To make
    -- this easier, the @AdminInitiateAuth@ response includes the actual
    -- username value in the @USERNAMEUSER_ID_FOR_SRP@ attribute, even if you
    -- specified an alias in your call to @AdminInitiateAuth@.
    challengeResponses :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contextual data such as the user\'s device fingerprint, IP address, or
    -- location used for evaluating the risk of an unexpected event by Amazon
    -- Cognito advanced security.
    contextData :: Prelude.Maybe ContextDataType,
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call
    -- determines that the caller needs to go through another challenge, they
    -- return a session with other challenge parameters. This session should be
    -- passed as it is to the next @RespondToAuthChallenge@ API call.
    session :: Prelude.Maybe Prelude.Text,
    -- | The analytics metadata for collecting Amazon Pinpoint metrics for
    -- @AdminRespondToAuthChallenge@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | The ID of the Amazon Cognito user pool.
    userPoolId :: Prelude.Text,
    -- | The app client ID.
    clientId :: Prelude.Sensitive Prelude.Text,
    -- | The challenge name. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
    challengeName :: ChallengeNameType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminRespondToAuthChallenge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'adminRespondToAuthChallenge_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the AdminRespondToAuthChallenge API action,
-- Amazon Cognito invokes any functions that are assigned to the following
-- triggers: /pre sign-up/, /custom message/, /post authentication/, /user
-- migration/, /pre token generation/, /define auth challenge/, /create
-- auth challenge/, and /verify auth challenge response/. When Amazon
-- Cognito invokes any of these functions, it passes a JSON payload, which
-- the function receives as input. This payload contains a @clientMetadata@
-- attribute, which provides the data that you assigned to the
-- ClientMetadata parameter in your AdminRespondToAuthChallenge request. In
-- your function code in AWS Lambda, you can process the @clientMetadata@
-- value to enhance your workflow for your specific needs.
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
-- -   @ADMIN_NO_SRP_AUTH@: @PASSWORD@, @USERNAME@, @SECRET_HASH@ (if app
--     client is configured with client secret).
--
-- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, any other required
--     attributes, @USERNAME@, @SECRET_HASH@ (if app client is configured
--     with client secret).
--
-- The value of the @USERNAME@ attribute must be the user\'s actual
-- username, not an alias (such as email address or phone number). To make
-- this easier, the @AdminInitiateAuth@ response includes the actual
-- username value in the @USERNAMEUSER_ID_FOR_SRP@ attribute, even if you
-- specified an alias in your call to @AdminInitiateAuth@.
--
-- 'contextData', 'adminRespondToAuthChallenge_contextData' - Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
--
-- 'session', 'adminRespondToAuthChallenge_session' - The session which should be passed both ways in challenge-response calls
-- to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call
-- determines that the caller needs to go through another challenge, they
-- return a session with other challenge parameters. This session should be
-- passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- 'analyticsMetadata', 'adminRespondToAuthChallenge_analyticsMetadata' - The analytics metadata for collecting Amazon Pinpoint metrics for
-- @AdminRespondToAuthChallenge@ calls.
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
      { clientMetadata =
          Prelude.Nothing,
        challengeResponses = Prelude.Nothing,
        contextData = Prelude.Nothing,
        session = Prelude.Nothing,
        analyticsMetadata = Prelude.Nothing,
        userPoolId = pUserPoolId_,
        clientId =
          Prelude._Sensitive Lens.# pClientId_,
        challengeName = pChallengeName_
      }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the AdminRespondToAuthChallenge API action,
-- Amazon Cognito invokes any functions that are assigned to the following
-- triggers: /pre sign-up/, /custom message/, /post authentication/, /user
-- migration/, /pre token generation/, /define auth challenge/, /create
-- auth challenge/, and /verify auth challenge response/. When Amazon
-- Cognito invokes any of these functions, it passes a JSON payload, which
-- the function receives as input. This payload contains a @clientMetadata@
-- attribute, which provides the data that you assigned to the
-- ClientMetadata parameter in your AdminRespondToAuthChallenge request. In
-- your function code in AWS Lambda, you can process the @clientMetadata@
-- value to enhance your workflow for your specific needs.
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
adminRespondToAuthChallenge_clientMetadata :: Lens.Lens' AdminRespondToAuthChallenge (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminRespondToAuthChallenge_clientMetadata = Lens.lens (\AdminRespondToAuthChallenge' {clientMetadata} -> clientMetadata) (\s@AdminRespondToAuthChallenge' {} a -> s {clientMetadata = a} :: AdminRespondToAuthChallenge) Prelude.. Lens.mapping Prelude._Coerce

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
-- -   @ADMIN_NO_SRP_AUTH@: @PASSWORD@, @USERNAME@, @SECRET_HASH@ (if app
--     client is configured with client secret).
--
-- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, any other required
--     attributes, @USERNAME@, @SECRET_HASH@ (if app client is configured
--     with client secret).
--
-- The value of the @USERNAME@ attribute must be the user\'s actual
-- username, not an alias (such as email address or phone number). To make
-- this easier, the @AdminInitiateAuth@ response includes the actual
-- username value in the @USERNAMEUSER_ID_FOR_SRP@ attribute, even if you
-- specified an alias in your call to @AdminInitiateAuth@.
adminRespondToAuthChallenge_challengeResponses :: Lens.Lens' AdminRespondToAuthChallenge (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminRespondToAuthChallenge_challengeResponses = Lens.lens (\AdminRespondToAuthChallenge' {challengeResponses} -> challengeResponses) (\s@AdminRespondToAuthChallenge' {} a -> s {challengeResponses = a} :: AdminRespondToAuthChallenge) Prelude.. Lens.mapping Prelude._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
adminRespondToAuthChallenge_contextData :: Lens.Lens' AdminRespondToAuthChallenge (Prelude.Maybe ContextDataType)
adminRespondToAuthChallenge_contextData = Lens.lens (\AdminRespondToAuthChallenge' {contextData} -> contextData) (\s@AdminRespondToAuthChallenge' {} a -> s {contextData = a} :: AdminRespondToAuthChallenge)

-- | The session which should be passed both ways in challenge-response calls
-- to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call
-- determines that the caller needs to go through another challenge, they
-- return a session with other challenge parameters. This session should be
-- passed as it is to the next @RespondToAuthChallenge@ API call.
adminRespondToAuthChallenge_session :: Lens.Lens' AdminRespondToAuthChallenge (Prelude.Maybe Prelude.Text)
adminRespondToAuthChallenge_session = Lens.lens (\AdminRespondToAuthChallenge' {session} -> session) (\s@AdminRespondToAuthChallenge' {} a -> s {session = a} :: AdminRespondToAuthChallenge)

-- | The analytics metadata for collecting Amazon Pinpoint metrics for
-- @AdminRespondToAuthChallenge@ calls.
adminRespondToAuthChallenge_analyticsMetadata :: Lens.Lens' AdminRespondToAuthChallenge (Prelude.Maybe AnalyticsMetadataType)
adminRespondToAuthChallenge_analyticsMetadata = Lens.lens (\AdminRespondToAuthChallenge' {analyticsMetadata} -> analyticsMetadata) (\s@AdminRespondToAuthChallenge' {} a -> s {analyticsMetadata = a} :: AdminRespondToAuthChallenge)

-- | The ID of the Amazon Cognito user pool.
adminRespondToAuthChallenge_userPoolId :: Lens.Lens' AdminRespondToAuthChallenge Prelude.Text
adminRespondToAuthChallenge_userPoolId = Lens.lens (\AdminRespondToAuthChallenge' {userPoolId} -> userPoolId) (\s@AdminRespondToAuthChallenge' {} a -> s {userPoolId = a} :: AdminRespondToAuthChallenge)

-- | The app client ID.
adminRespondToAuthChallenge_clientId :: Lens.Lens' AdminRespondToAuthChallenge Prelude.Text
adminRespondToAuthChallenge_clientId = Lens.lens (\AdminRespondToAuthChallenge' {clientId} -> clientId) (\s@AdminRespondToAuthChallenge' {} a -> s {clientId = a} :: AdminRespondToAuthChallenge) Prelude.. Prelude._Sensitive

-- | The challenge name. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
adminRespondToAuthChallenge_challengeName :: Lens.Lens' AdminRespondToAuthChallenge ChallengeNameType
adminRespondToAuthChallenge_challengeName = Lens.lens (\AdminRespondToAuthChallenge' {challengeName} -> challengeName) (\s@AdminRespondToAuthChallenge' {} a -> s {challengeName = a} :: AdminRespondToAuthChallenge)

instance
  Prelude.AWSRequest
    AdminRespondToAuthChallenge
  where
  type
    Rs AdminRespondToAuthChallenge =
      AdminRespondToAuthChallengeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminRespondToAuthChallengeResponse'
            Prelude.<$> (x Prelude..?> "AuthenticationResult")
            Prelude.<*> (x Prelude..?> "ChallengeName")
            Prelude.<*> ( x Prelude..?> "ChallengeParameters"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminRespondToAuthChallenge

instance Prelude.NFData AdminRespondToAuthChallenge

instance
  Prelude.ToHeaders
    AdminRespondToAuthChallenge
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminRespondToAuthChallenge" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminRespondToAuthChallenge where
  toJSON AdminRespondToAuthChallenge' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Prelude..=)
              Prelude.<$> clientMetadata,
            ("ChallengeResponses" Prelude..=)
              Prelude.<$> challengeResponses,
            ("ContextData" Prelude..=) Prelude.<$> contextData,
            ("Session" Prelude..=) Prelude.<$> session,
            ("AnalyticsMetadata" Prelude..=)
              Prelude.<$> analyticsMetadata,
            Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("ClientId" Prelude..= clientId),
            Prelude.Just
              ("ChallengeName" Prelude..= challengeName)
          ]
      )

instance Prelude.ToPath AdminRespondToAuthChallenge where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminRespondToAuthChallenge where
  toQuery = Prelude.const Prelude.mempty

-- | Responds to the authentication challenge, as an administrator.
--
-- /See:/ 'newAdminRespondToAuthChallengeResponse' smart constructor.
data AdminRespondToAuthChallengeResponse = AdminRespondToAuthChallengeResponse'
  { -- | The result returned by the server in response to the authentication
    -- request.
    authenticationResult :: Prelude.Maybe AuthenticationResultType,
    -- | The name of the challenge. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
    challengeName :: Prelude.Maybe ChallengeNameType,
    -- | The challenge parameters. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
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
-- 'challengeName', 'adminRespondToAuthChallengeResponse_challengeName' - The name of the challenge. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
--
-- 'challengeParameters', 'adminRespondToAuthChallengeResponse_challengeParameters' - The challenge parameters. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
--
-- 'session', 'adminRespondToAuthChallengeResponse_session' - The session which should be passed both ways in challenge-response calls
-- to the service. If the caller needs to go through another challenge,
-- they return a session with other challenge parameters. This session
-- should be passed as it is to the next @RespondToAuthChallenge@ API call.
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
      challengeName = Prelude.Nothing,
      challengeParameters = Prelude.Nothing,
      session = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The result returned by the server in response to the authentication
-- request.
adminRespondToAuthChallengeResponse_authenticationResult :: Lens.Lens' AdminRespondToAuthChallengeResponse (Prelude.Maybe AuthenticationResultType)
adminRespondToAuthChallengeResponse_authenticationResult = Lens.lens (\AdminRespondToAuthChallengeResponse' {authenticationResult} -> authenticationResult) (\s@AdminRespondToAuthChallengeResponse' {} a -> s {authenticationResult = a} :: AdminRespondToAuthChallengeResponse)

-- | The name of the challenge. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
adminRespondToAuthChallengeResponse_challengeName :: Lens.Lens' AdminRespondToAuthChallengeResponse (Prelude.Maybe ChallengeNameType)
adminRespondToAuthChallengeResponse_challengeName = Lens.lens (\AdminRespondToAuthChallengeResponse' {challengeName} -> challengeName) (\s@AdminRespondToAuthChallengeResponse' {} a -> s {challengeName = a} :: AdminRespondToAuthChallengeResponse)

-- | The challenge parameters. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth>.
adminRespondToAuthChallengeResponse_challengeParameters :: Lens.Lens' AdminRespondToAuthChallengeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminRespondToAuthChallengeResponse_challengeParameters = Lens.lens (\AdminRespondToAuthChallengeResponse' {challengeParameters} -> challengeParameters) (\s@AdminRespondToAuthChallengeResponse' {} a -> s {challengeParameters = a} :: AdminRespondToAuthChallengeResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The session which should be passed both ways in challenge-response calls
-- to the service. If the caller needs to go through another challenge,
-- they return a session with other challenge parameters. This session
-- should be passed as it is to the next @RespondToAuthChallenge@ API call.
adminRespondToAuthChallengeResponse_session :: Lens.Lens' AdminRespondToAuthChallengeResponse (Prelude.Maybe Prelude.Text)
adminRespondToAuthChallengeResponse_session = Lens.lens (\AdminRespondToAuthChallengeResponse' {session} -> session) (\s@AdminRespondToAuthChallengeResponse' {} a -> s {session = a} :: AdminRespondToAuthChallengeResponse)

-- | The response's http status code.
adminRespondToAuthChallengeResponse_httpStatus :: Lens.Lens' AdminRespondToAuthChallengeResponse Prelude.Int
adminRespondToAuthChallengeResponse_httpStatus = Lens.lens (\AdminRespondToAuthChallengeResponse' {httpStatus} -> httpStatus) (\s@AdminRespondToAuthChallengeResponse' {} a -> s {httpStatus = a} :: AdminRespondToAuthChallengeResponse)

instance
  Prelude.NFData
    AdminRespondToAuthChallengeResponse
