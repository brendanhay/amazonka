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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    clientMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contextual data such as the user\'s device fingerprint, IP address, or
    -- location used for evaluating the risk of an unexpected event by Amazon
    -- Cognito advanced security.
    userContextData :: Prelude.Maybe UserContextDataType,
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
    challengeResponses :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The session which should be passed both ways in challenge-response calls
    -- to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call
    -- determines that the caller needs to go through another challenge, they
    -- return a session with other challenge parameters. This session should be
    -- passed as it is to the next @RespondToAuthChallenge@ API call.
    session :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for
    -- @RespondToAuthChallenge@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | The app client ID.
    clientId :: Prelude.Sensitive Prelude.Text,
    -- | The challenge name. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
    --
    -- @ADMIN_NO_SRP_AUTH@ is not a valid value.
    challengeName :: ChallengeNameType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'challengeName'
  ChallengeNameType ->
  RespondToAuthChallenge
newRespondToAuthChallenge pClientId_ pChallengeName_ =
  RespondToAuthChallenge'
    { clientMetadata =
        Prelude.Nothing,
      userContextData = Prelude.Nothing,
      challengeResponses = Prelude.Nothing,
      session = Prelude.Nothing,
      analyticsMetadata = Prelude.Nothing,
      clientId = Prelude._Sensitive Lens.# pClientId_,
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
respondToAuthChallenge_clientMetadata :: Lens.Lens' RespondToAuthChallenge (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
respondToAuthChallenge_clientMetadata = Lens.lens (\RespondToAuthChallenge' {clientMetadata} -> clientMetadata) (\s@RespondToAuthChallenge' {} a -> s {clientMetadata = a} :: RespondToAuthChallenge) Prelude.. Lens.mapping Prelude._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
respondToAuthChallenge_userContextData :: Lens.Lens' RespondToAuthChallenge (Prelude.Maybe UserContextDataType)
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
respondToAuthChallenge_challengeResponses :: Lens.Lens' RespondToAuthChallenge (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
respondToAuthChallenge_challengeResponses = Lens.lens (\RespondToAuthChallenge' {challengeResponses} -> challengeResponses) (\s@RespondToAuthChallenge' {} a -> s {challengeResponses = a} :: RespondToAuthChallenge) Prelude.. Lens.mapping Prelude._Coerce

-- | The session which should be passed both ways in challenge-response calls
-- to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call
-- determines that the caller needs to go through another challenge, they
-- return a session with other challenge parameters. This session should be
-- passed as it is to the next @RespondToAuthChallenge@ API call.
respondToAuthChallenge_session :: Lens.Lens' RespondToAuthChallenge (Prelude.Maybe Prelude.Text)
respondToAuthChallenge_session = Lens.lens (\RespondToAuthChallenge' {session} -> session) (\s@RespondToAuthChallenge' {} a -> s {session = a} :: RespondToAuthChallenge)

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @RespondToAuthChallenge@ calls.
respondToAuthChallenge_analyticsMetadata :: Lens.Lens' RespondToAuthChallenge (Prelude.Maybe AnalyticsMetadataType)
respondToAuthChallenge_analyticsMetadata = Lens.lens (\RespondToAuthChallenge' {analyticsMetadata} -> analyticsMetadata) (\s@RespondToAuthChallenge' {} a -> s {analyticsMetadata = a} :: RespondToAuthChallenge)

-- | The app client ID.
respondToAuthChallenge_clientId :: Lens.Lens' RespondToAuthChallenge Prelude.Text
respondToAuthChallenge_clientId = Lens.lens (\RespondToAuthChallenge' {clientId} -> clientId) (\s@RespondToAuthChallenge' {} a -> s {clientId = a} :: RespondToAuthChallenge) Prelude.. Prelude._Sensitive

-- | The challenge name. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth>.
--
-- @ADMIN_NO_SRP_AUTH@ is not a valid value.
respondToAuthChallenge_challengeName :: Lens.Lens' RespondToAuthChallenge ChallengeNameType
respondToAuthChallenge_challengeName = Lens.lens (\RespondToAuthChallenge' {challengeName} -> challengeName) (\s@RespondToAuthChallenge' {} a -> s {challengeName = a} :: RespondToAuthChallenge)

instance Prelude.AWSRequest RespondToAuthChallenge where
  type
    Rs RespondToAuthChallenge =
      RespondToAuthChallengeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RespondToAuthChallengeResponse'
            Prelude.<$> (x Prelude..?> "AuthenticationResult")
            Prelude.<*> (x Prelude..?> "ChallengeName")
            Prelude.<*> ( x Prelude..?> "ChallengeParameters"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RespondToAuthChallenge

instance Prelude.NFData RespondToAuthChallenge

instance Prelude.ToHeaders RespondToAuthChallenge where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.RespondToAuthChallenge" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RespondToAuthChallenge where
  toJSON RespondToAuthChallenge' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Prelude..=)
              Prelude.<$> clientMetadata,
            ("UserContextData" Prelude..=)
              Prelude.<$> userContextData,
            ("ChallengeResponses" Prelude..=)
              Prelude.<$> challengeResponses,
            ("Session" Prelude..=) Prelude.<$> session,
            ("AnalyticsMetadata" Prelude..=)
              Prelude.<$> analyticsMetadata,
            Prelude.Just ("ClientId" Prelude..= clientId),
            Prelude.Just
              ("ChallengeName" Prelude..= challengeName)
          ]
      )

instance Prelude.ToPath RespondToAuthChallenge where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RespondToAuthChallenge where
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
respondToAuthChallengeResponse_challengeParameters = Lens.lens (\RespondToAuthChallengeResponse' {challengeParameters} -> challengeParameters) (\s@RespondToAuthChallengeResponse' {} a -> s {challengeParameters = a} :: RespondToAuthChallengeResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The session which should be passed both ways in challenge-response calls
-- to the service. If the caller needs to go through another challenge,
-- they return a session with other challenge parameters. This session
-- should be passed as it is to the next @RespondToAuthChallenge@ API call.
respondToAuthChallengeResponse_session :: Lens.Lens' RespondToAuthChallengeResponse (Prelude.Maybe Prelude.Text)
respondToAuthChallengeResponse_session = Lens.lens (\RespondToAuthChallengeResponse' {session} -> session) (\s@RespondToAuthChallengeResponse' {} a -> s {session = a} :: RespondToAuthChallengeResponse)

-- | The response's http status code.
respondToAuthChallengeResponse_httpStatus :: Lens.Lens' RespondToAuthChallengeResponse Prelude.Int
respondToAuthChallengeResponse_httpStatus = Lens.lens (\RespondToAuthChallengeResponse' {httpStatus} -> httpStatus) (\s@RespondToAuthChallengeResponse' {} a -> s {httpStatus = a} :: RespondToAuthChallengeResponse)

instance
  Prelude.NFData
    RespondToAuthChallengeResponse
