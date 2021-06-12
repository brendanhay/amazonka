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
-- Module      : Network.AWS.CognitoIdentityProvider.RespondToAuthChallenge
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Responds to the authentication challenge.
module Network.AWS.CognitoIdentityProvider.RespondToAuthChallenge
  ( -- * Creating a Request
    RespondToAuthChallenge (..),
    newRespondToAuthChallenge,

    -- * Request Lenses
    respondToAuthChallenge_clientMetadata,
    respondToAuthChallenge_userContextData,
    respondToAuthChallenge_challengeResponses,
    respondToAuthChallenge_session,
    respondToAuthChallenge_analyticsMetadata,
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to respond to an authentication challenge.
--
-- /See:/ 'newRespondToAuthChallenge' smart constructor.
data RespondToAuthChallenge = RespondToAuthChallenge'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the RespondToAuthChallenge API action,
    -- Amazon Cognito invokes any functions that are assigned to the following
    -- triggers: /post authentication/, /pre token generation/, /define auth
    -- challenge/, /create auth challenge/, and /verify auth challenge/. When
    -- Amazon Cognito invokes any of these functions, it passes a JSON payload,
    -- which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your RespondToAuthChallenge request. In
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
    clientMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Contextual data such as the user\'s device fingerprint, IP address, or
    -- location used for evaluating the risk of an unexpected event by Amazon
    -- Cognito advanced security.
    userContextData :: Core.Maybe UserContextDataType,
    -- | The challenge responses. These are inputs corresponding to the value of
    -- @ChallengeName@, for example:
    --
    -- @SECRET_HASH@ (if app client is configured with client secret) applies
    -- to all inputs below (including @SOFTWARE_TOKEN_MFA@).
    --
    -- -   @SMS_MFA@: @SMS_MFA_CODE@, @USERNAME@.
    --
    -- -   @PASSWORD_VERIFIER@: @PASSWORD_CLAIM_SIGNATURE@,
    --     @PASSWORD_CLAIM_SECRET_BLOCK@, @TIMESTAMP@, @USERNAME@.
    --
    -- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, any other required
    --     attributes, @USERNAME@.
    --
    -- -   @SOFTWARE_TOKEN_MFA@: @USERNAME@ and @SOFTWARE_TOKEN_MFA_CODE@ are
    --     required attributes.
    --
    -- -   @DEVICE_SRP_AUTH@ requires @USERNAME@, @DEVICE_KEY@, @SRP_A@ (and
    --     @SECRET_HASH@).
    --
    -- -   @DEVICE_PASSWORD_VERIFIER@ requires everything that
    --     @PASSWORD_VERIFIER@ requires plus @DEVICE_KEY@.
    challengeResponses :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call
    -- determines that the caller needs to go through another challenge, they
    -- return a session with other challenge parameters. This session should be
    -- passed as it is to the next @RespondToAuthChallenge@ API call.
    session :: Core.Maybe Core.Text,
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for
    -- @RespondToAuthChallenge@ calls.
    analyticsMetadata :: Core.Maybe AnalyticsMetadataType,
    -- | The app client ID.
    clientId :: Core.Sensitive Core.Text,
    -- | The challenge name. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
    --
    -- @ADMIN_NO_SRP_AUTH@ is not a valid value.
    challengeName :: ChallengeNameType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'RespondToAuthChallenge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'respondToAuthChallenge_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the RespondToAuthChallenge API action,
-- Amazon Cognito invokes any functions that are assigned to the following
-- triggers: /post authentication/, /pre token generation/, /define auth
-- challenge/, /create auth challenge/, and /verify auth challenge/. When
-- Amazon Cognito invokes any of these functions, it passes a JSON payload,
-- which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your RespondToAuthChallenge request. In
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
-- 'userContextData', 'respondToAuthChallenge_userContextData' - Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
--
-- 'challengeResponses', 'respondToAuthChallenge_challengeResponses' - The challenge responses. These are inputs corresponding to the value of
-- @ChallengeName@, for example:
--
-- @SECRET_HASH@ (if app client is configured with client secret) applies
-- to all inputs below (including @SOFTWARE_TOKEN_MFA@).
--
-- -   @SMS_MFA@: @SMS_MFA_CODE@, @USERNAME@.
--
-- -   @PASSWORD_VERIFIER@: @PASSWORD_CLAIM_SIGNATURE@,
--     @PASSWORD_CLAIM_SECRET_BLOCK@, @TIMESTAMP@, @USERNAME@.
--
-- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, any other required
--     attributes, @USERNAME@.
--
-- -   @SOFTWARE_TOKEN_MFA@: @USERNAME@ and @SOFTWARE_TOKEN_MFA_CODE@ are
--     required attributes.
--
-- -   @DEVICE_SRP_AUTH@ requires @USERNAME@, @DEVICE_KEY@, @SRP_A@ (and
--     @SECRET_HASH@).
--
-- -   @DEVICE_PASSWORD_VERIFIER@ requires everything that
--     @PASSWORD_VERIFIER@ requires plus @DEVICE_KEY@.
--
-- 'session', 'respondToAuthChallenge_session' - The session which should be passed both ways in challenge-response calls
-- to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call
-- determines that the caller needs to go through another challenge, they
-- return a session with other challenge parameters. This session should be
-- passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- 'analyticsMetadata', 'respondToAuthChallenge_analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for
-- @RespondToAuthChallenge@ calls.
--
-- 'clientId', 'respondToAuthChallenge_clientId' - The app client ID.
--
-- 'challengeName', 'respondToAuthChallenge_challengeName' - The challenge name. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
--
-- @ADMIN_NO_SRP_AUTH@ is not a valid value.
newRespondToAuthChallenge ::
  -- | 'clientId'
  Core.Text ->
  -- | 'challengeName'
  ChallengeNameType ->
  RespondToAuthChallenge
newRespondToAuthChallenge pClientId_ pChallengeName_ =
  RespondToAuthChallenge'
    { clientMetadata =
        Core.Nothing,
      userContextData = Core.Nothing,
      challengeResponses = Core.Nothing,
      session = Core.Nothing,
      analyticsMetadata = Core.Nothing,
      clientId = Core._Sensitive Lens.# pClientId_,
      challengeName = pChallengeName_
    }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the RespondToAuthChallenge API action,
-- Amazon Cognito invokes any functions that are assigned to the following
-- triggers: /post authentication/, /pre token generation/, /define auth
-- challenge/, /create auth challenge/, and /verify auth challenge/. When
-- Amazon Cognito invokes any of these functions, it passes a JSON payload,
-- which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your RespondToAuthChallenge request. In
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
respondToAuthChallenge_clientMetadata :: Lens.Lens' RespondToAuthChallenge (Core.Maybe (Core.HashMap Core.Text Core.Text))
respondToAuthChallenge_clientMetadata = Lens.lens (\RespondToAuthChallenge' {clientMetadata} -> clientMetadata) (\s@RespondToAuthChallenge' {} a -> s {clientMetadata = a} :: RespondToAuthChallenge) Core.. Lens.mapping Lens._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
respondToAuthChallenge_userContextData :: Lens.Lens' RespondToAuthChallenge (Core.Maybe UserContextDataType)
respondToAuthChallenge_userContextData = Lens.lens (\RespondToAuthChallenge' {userContextData} -> userContextData) (\s@RespondToAuthChallenge' {} a -> s {userContextData = a} :: RespondToAuthChallenge)

-- | The challenge responses. These are inputs corresponding to the value of
-- @ChallengeName@, for example:
--
-- @SECRET_HASH@ (if app client is configured with client secret) applies
-- to all inputs below (including @SOFTWARE_TOKEN_MFA@).
--
-- -   @SMS_MFA@: @SMS_MFA_CODE@, @USERNAME@.
--
-- -   @PASSWORD_VERIFIER@: @PASSWORD_CLAIM_SIGNATURE@,
--     @PASSWORD_CLAIM_SECRET_BLOCK@, @TIMESTAMP@, @USERNAME@.
--
-- -   @NEW_PASSWORD_REQUIRED@: @NEW_PASSWORD@, any other required
--     attributes, @USERNAME@.
--
-- -   @SOFTWARE_TOKEN_MFA@: @USERNAME@ and @SOFTWARE_TOKEN_MFA_CODE@ are
--     required attributes.
--
-- -   @DEVICE_SRP_AUTH@ requires @USERNAME@, @DEVICE_KEY@, @SRP_A@ (and
--     @SECRET_HASH@).
--
-- -   @DEVICE_PASSWORD_VERIFIER@ requires everything that
--     @PASSWORD_VERIFIER@ requires plus @DEVICE_KEY@.
respondToAuthChallenge_challengeResponses :: Lens.Lens' RespondToAuthChallenge (Core.Maybe (Core.HashMap Core.Text Core.Text))
respondToAuthChallenge_challengeResponses = Lens.lens (\RespondToAuthChallenge' {challengeResponses} -> challengeResponses) (\s@RespondToAuthChallenge' {} a -> s {challengeResponses = a} :: RespondToAuthChallenge) Core.. Lens.mapping Lens._Coerce

-- | The session which should be passed both ways in challenge-response calls
-- to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call
-- determines that the caller needs to go through another challenge, they
-- return a session with other challenge parameters. This session should be
-- passed as it is to the next @RespondToAuthChallenge@ API call.
respondToAuthChallenge_session :: Lens.Lens' RespondToAuthChallenge (Core.Maybe Core.Text)
respondToAuthChallenge_session = Lens.lens (\RespondToAuthChallenge' {session} -> session) (\s@RespondToAuthChallenge' {} a -> s {session = a} :: RespondToAuthChallenge)

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @RespondToAuthChallenge@ calls.
respondToAuthChallenge_analyticsMetadata :: Lens.Lens' RespondToAuthChallenge (Core.Maybe AnalyticsMetadataType)
respondToAuthChallenge_analyticsMetadata = Lens.lens (\RespondToAuthChallenge' {analyticsMetadata} -> analyticsMetadata) (\s@RespondToAuthChallenge' {} a -> s {analyticsMetadata = a} :: RespondToAuthChallenge)

-- | The app client ID.
respondToAuthChallenge_clientId :: Lens.Lens' RespondToAuthChallenge Core.Text
respondToAuthChallenge_clientId = Lens.lens (\RespondToAuthChallenge' {clientId} -> clientId) (\s@RespondToAuthChallenge' {} a -> s {clientId = a} :: RespondToAuthChallenge) Core.. Core._Sensitive

-- | The challenge name. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
--
-- @ADMIN_NO_SRP_AUTH@ is not a valid value.
respondToAuthChallenge_challengeName :: Lens.Lens' RespondToAuthChallenge ChallengeNameType
respondToAuthChallenge_challengeName = Lens.lens (\RespondToAuthChallenge' {challengeName} -> challengeName) (\s@RespondToAuthChallenge' {} a -> s {challengeName = a} :: RespondToAuthChallenge)

instance Core.AWSRequest RespondToAuthChallenge where
  type
    AWSResponse RespondToAuthChallenge =
      RespondToAuthChallengeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RespondToAuthChallengeResponse'
            Core.<$> (x Core..?> "AuthenticationResult")
            Core.<*> (x Core..?> "ChallengeName")
            Core.<*> ( x Core..?> "ChallengeParameters"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Session")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RespondToAuthChallenge

instance Core.NFData RespondToAuthChallenge

instance Core.ToHeaders RespondToAuthChallenge where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.RespondToAuthChallenge" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RespondToAuthChallenge where
  toJSON RespondToAuthChallenge' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientMetadata" Core..=) Core.<$> clientMetadata,
            ("UserContextData" Core..=) Core.<$> userContextData,
            ("ChallengeResponses" Core..=)
              Core.<$> challengeResponses,
            ("Session" Core..=) Core.<$> session,
            ("AnalyticsMetadata" Core..=)
              Core.<$> analyticsMetadata,
            Core.Just ("ClientId" Core..= clientId),
            Core.Just ("ChallengeName" Core..= challengeName)
          ]
      )

instance Core.ToPath RespondToAuthChallenge where
  toPath = Core.const "/"

instance Core.ToQuery RespondToAuthChallenge where
  toQuery = Core.const Core.mempty

-- | The response to respond to the authentication challenge.
--
-- /See:/ 'newRespondToAuthChallengeResponse' smart constructor.
data RespondToAuthChallengeResponse = RespondToAuthChallengeResponse'
  { -- | The result returned by the server in response to the request to respond
    -- to the authentication challenge.
    authenticationResult :: Core.Maybe AuthenticationResultType,
    -- | The challenge name. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
    challengeName :: Core.Maybe ChallengeNameType,
    -- | The challenge parameters. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
    challengeParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service. If the caller needs to go through another challenge,
    -- they return a session with other challenge parameters. This session
    -- should be passed as it is to the next @RespondToAuthChallenge@ API call.
    session :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- 'session', 'respondToAuthChallengeResponse_session' - The session which should be passed both ways in challenge-response calls
-- to the service. If the caller needs to go through another challenge,
-- they return a session with other challenge parameters. This session
-- should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- 'httpStatus', 'respondToAuthChallengeResponse_httpStatus' - The response's http status code.
newRespondToAuthChallengeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RespondToAuthChallengeResponse
newRespondToAuthChallengeResponse pHttpStatus_ =
  RespondToAuthChallengeResponse'
    { authenticationResult =
        Core.Nothing,
      challengeName = Core.Nothing,
      challengeParameters = Core.Nothing,
      session = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The result returned by the server in response to the request to respond
-- to the authentication challenge.
respondToAuthChallengeResponse_authenticationResult :: Lens.Lens' RespondToAuthChallengeResponse (Core.Maybe AuthenticationResultType)
respondToAuthChallengeResponse_authenticationResult = Lens.lens (\RespondToAuthChallengeResponse' {authenticationResult} -> authenticationResult) (\s@RespondToAuthChallengeResponse' {} a -> s {authenticationResult = a} :: RespondToAuthChallengeResponse)

-- | The challenge name. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
respondToAuthChallengeResponse_challengeName :: Lens.Lens' RespondToAuthChallengeResponse (Core.Maybe ChallengeNameType)
respondToAuthChallengeResponse_challengeName = Lens.lens (\RespondToAuthChallengeResponse' {challengeName} -> challengeName) (\s@RespondToAuthChallengeResponse' {} a -> s {challengeName = a} :: RespondToAuthChallengeResponse)

-- | The challenge parameters. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
respondToAuthChallengeResponse_challengeParameters :: Lens.Lens' RespondToAuthChallengeResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
respondToAuthChallengeResponse_challengeParameters = Lens.lens (\RespondToAuthChallengeResponse' {challengeParameters} -> challengeParameters) (\s@RespondToAuthChallengeResponse' {} a -> s {challengeParameters = a} :: RespondToAuthChallengeResponse) Core.. Lens.mapping Lens._Coerce

-- | The session which should be passed both ways in challenge-response calls
-- to the service. If the caller needs to go through another challenge,
-- they return a session with other challenge parameters. This session
-- should be passed as it is to the next @RespondToAuthChallenge@ API call.
respondToAuthChallengeResponse_session :: Lens.Lens' RespondToAuthChallengeResponse (Core.Maybe Core.Text)
respondToAuthChallengeResponse_session = Lens.lens (\RespondToAuthChallengeResponse' {session} -> session) (\s@RespondToAuthChallengeResponse' {} a -> s {session = a} :: RespondToAuthChallengeResponse)

-- | The response's http status code.
respondToAuthChallengeResponse_httpStatus :: Lens.Lens' RespondToAuthChallengeResponse Core.Int
respondToAuthChallengeResponse_httpStatus = Lens.lens (\RespondToAuthChallengeResponse' {httpStatus} -> httpStatus) (\s@RespondToAuthChallengeResponse' {} a -> s {httpStatus = a} :: RespondToAuthChallengeResponse)

instance Core.NFData RespondToAuthChallengeResponse
