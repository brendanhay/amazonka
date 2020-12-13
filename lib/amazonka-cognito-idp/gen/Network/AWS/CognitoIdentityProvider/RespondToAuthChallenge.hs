{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.RespondToAuthChallenge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Responds to the authentication challenge.
module Network.AWS.CognitoIdentityProvider.RespondToAuthChallenge
  ( -- * Creating a request
    RespondToAuthChallenge (..),
    mkRespondToAuthChallenge,

    -- ** Request lenses
    rtacClientMetadata,
    rtacChallengeName,
    rtacClientId,
    rtacAnalyticsMetadata,
    rtacChallengeResponses,
    rtacUserContextData,
    rtacSession,

    -- * Destructuring the response
    RespondToAuthChallengeResponse (..),
    mkRespondToAuthChallengeResponse,

    -- ** Response lenses
    rtacrsChallengeName,
    rtacrsChallengeParameters,
    rtacrsAuthenticationResult,
    rtacrsSession,
    rtacrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to respond to an authentication challenge.
--
-- /See:/ 'mkRespondToAuthChallenge' smart constructor.
data RespondToAuthChallenge = RespondToAuthChallenge'
  { -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the RespondToAuthChallenge API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /post authentication/ , /pre token generation/ , /define auth challenge/ , /create auth challenge/ , and /verify auth challenge/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your RespondToAuthChallenge request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
    --
    -- @ADMIN_NO_SRP_AUTH@ is not a valid value.
    challengeName :: ChallengeNameType,
    -- | The app client ID.
    clientId :: Lude.Sensitive Lude.Text,
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for @RespondToAuthChallenge@ calls.
    analyticsMetadata :: Lude.Maybe AnalyticsMetadataType,
    -- | The challenge responses. These are inputs corresponding to the value of @ChallengeName@ , for example:
    --
    --
    --     * @SMS_MFA@ : @SMS_MFA_CODE@ , @USERNAME@ .
    --
    --
    --     * @PASSWORD_VERIFIER@ : @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , @TIMESTAMP@ , @USERNAME@ .
    --
    --
    --     * @NEW_PASSWORD_REQUIRED@ : @NEW_PASSWORD@ , any other required attributes, @USERNAME@ .
    --
    --
    --     * @SOFTWARE_TOKEN_MFA@ : @USERNAME@ and @SOFTWARE_TOKEN_MFA_CODE@ are required attributes.
    --
    --
    --     * @DEVICE_SRP_AUTH@ requires @USERNAME@ , @DEVICE_KEY@ , @SRP_A@ (and @SECRET_HASH@ ).
    --
    --
    --     * @DEVICE_PASSWORD_VERIFIER@ requires everything that @PASSWORD_VERIFIER@ requires plus @DEVICE_KEY@ .
    challengeResponses :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
    userContextData :: Lude.Maybe UserContextDataType,
    -- | The session which should be passed both ways in challenge-response calls to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
    session :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RespondToAuthChallenge' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the RespondToAuthChallenge API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /post authentication/ , /pre token generation/ , /define auth challenge/ , /create auth challenge/ , and /verify auth challenge/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your RespondToAuthChallenge request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'challengeName' - The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
--
-- @ADMIN_NO_SRP_AUTH@ is not a valid value.
-- * 'clientId' - The app client ID.
-- * 'analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for @RespondToAuthChallenge@ calls.
-- * 'challengeResponses' - The challenge responses. These are inputs corresponding to the value of @ChallengeName@ , for example:
--
--
--     * @SMS_MFA@ : @SMS_MFA_CODE@ , @USERNAME@ .
--
--
--     * @PASSWORD_VERIFIER@ : @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , @TIMESTAMP@ , @USERNAME@ .
--
--
--     * @NEW_PASSWORD_REQUIRED@ : @NEW_PASSWORD@ , any other required attributes, @USERNAME@ .
--
--
--     * @SOFTWARE_TOKEN_MFA@ : @USERNAME@ and @SOFTWARE_TOKEN_MFA_CODE@ are required attributes.
--
--
--     * @DEVICE_SRP_AUTH@ requires @USERNAME@ , @DEVICE_KEY@ , @SRP_A@ (and @SECRET_HASH@ ).
--
--
--     * @DEVICE_PASSWORD_VERIFIER@ requires everything that @PASSWORD_VERIFIER@ requires plus @DEVICE_KEY@ .
--
--
-- * 'userContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
-- * 'session' - The session which should be passed both ways in challenge-response calls to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
mkRespondToAuthChallenge ::
  -- | 'challengeName'
  ChallengeNameType ->
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  RespondToAuthChallenge
mkRespondToAuthChallenge pChallengeName_ pClientId_ =
  RespondToAuthChallenge'
    { clientMetadata = Lude.Nothing,
      challengeName = pChallengeName_,
      clientId = pClientId_,
      analyticsMetadata = Lude.Nothing,
      challengeResponses = Lude.Nothing,
      userContextData = Lude.Nothing,
      session = Lude.Nothing
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the RespondToAuthChallenge API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /post authentication/ , /pre token generation/ , /define auth challenge/ , /create auth challenge/ , and /verify auth challenge/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your RespondToAuthChallenge request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacClientMetadata :: Lens.Lens' RespondToAuthChallenge (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rtacClientMetadata = Lens.lens (clientMetadata :: RespondToAuthChallenge -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: RespondToAuthChallenge)
{-# DEPRECATED rtacClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
--
-- @ADMIN_NO_SRP_AUTH@ is not a valid value.
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacChallengeName :: Lens.Lens' RespondToAuthChallenge ChallengeNameType
rtacChallengeName = Lens.lens (challengeName :: RespondToAuthChallenge -> ChallengeNameType) (\s a -> s {challengeName = a} :: RespondToAuthChallenge)
{-# DEPRECATED rtacChallengeName "Use generic-lens or generic-optics with 'challengeName' instead." #-}

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacClientId :: Lens.Lens' RespondToAuthChallenge (Lude.Sensitive Lude.Text)
rtacClientId = Lens.lens (clientId :: RespondToAuthChallenge -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: RespondToAuthChallenge)
{-# DEPRECATED rtacClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @RespondToAuthChallenge@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacAnalyticsMetadata :: Lens.Lens' RespondToAuthChallenge (Lude.Maybe AnalyticsMetadataType)
rtacAnalyticsMetadata = Lens.lens (analyticsMetadata :: RespondToAuthChallenge -> Lude.Maybe AnalyticsMetadataType) (\s a -> s {analyticsMetadata = a} :: RespondToAuthChallenge)
{-# DEPRECATED rtacAnalyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead." #-}

-- | The challenge responses. These are inputs corresponding to the value of @ChallengeName@ , for example:
--
--
--     * @SMS_MFA@ : @SMS_MFA_CODE@ , @USERNAME@ .
--
--
--     * @PASSWORD_VERIFIER@ : @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , @TIMESTAMP@ , @USERNAME@ .
--
--
--     * @NEW_PASSWORD_REQUIRED@ : @NEW_PASSWORD@ , any other required attributes, @USERNAME@ .
--
--
--     * @SOFTWARE_TOKEN_MFA@ : @USERNAME@ and @SOFTWARE_TOKEN_MFA_CODE@ are required attributes.
--
--
--     * @DEVICE_SRP_AUTH@ requires @USERNAME@ , @DEVICE_KEY@ , @SRP_A@ (and @SECRET_HASH@ ).
--
--
--     * @DEVICE_PASSWORD_VERIFIER@ requires everything that @PASSWORD_VERIFIER@ requires plus @DEVICE_KEY@ .
--
--
--
-- /Note:/ Consider using 'challengeResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacChallengeResponses :: Lens.Lens' RespondToAuthChallenge (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rtacChallengeResponses = Lens.lens (challengeResponses :: RespondToAuthChallenge -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {challengeResponses = a} :: RespondToAuthChallenge)
{-# DEPRECATED rtacChallengeResponses "Use generic-lens or generic-optics with 'challengeResponses' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacUserContextData :: Lens.Lens' RespondToAuthChallenge (Lude.Maybe UserContextDataType)
rtacUserContextData = Lens.lens (userContextData :: RespondToAuthChallenge -> Lude.Maybe UserContextDataType) (\s a -> s {userContextData = a} :: RespondToAuthChallenge)
{-# DEPRECATED rtacUserContextData "Use generic-lens or generic-optics with 'userContextData' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacSession :: Lens.Lens' RespondToAuthChallenge (Lude.Maybe Lude.Text)
rtacSession = Lens.lens (session :: RespondToAuthChallenge -> Lude.Maybe Lude.Text) (\s a -> s {session = a} :: RespondToAuthChallenge)
{-# DEPRECATED rtacSession "Use generic-lens or generic-optics with 'session' instead." #-}

instance Lude.AWSRequest RespondToAuthChallenge where
  type Rs RespondToAuthChallenge = RespondToAuthChallengeResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          RespondToAuthChallengeResponse'
            Lude.<$> (x Lude..?> "ChallengeName")
            Lude.<*> (x Lude..?> "ChallengeParameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "AuthenticationResult")
            Lude.<*> (x Lude..?> "Session")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RespondToAuthChallenge where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.RespondToAuthChallenge" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RespondToAuthChallenge where
  toJSON RespondToAuthChallenge' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("ChallengeName" Lude..= challengeName),
            Lude.Just ("ClientId" Lude..= clientId),
            ("AnalyticsMetadata" Lude..=) Lude.<$> analyticsMetadata,
            ("ChallengeResponses" Lude..=) Lude.<$> challengeResponses,
            ("UserContextData" Lude..=) Lude.<$> userContextData,
            ("Session" Lude..=) Lude.<$> session
          ]
      )

instance Lude.ToPath RespondToAuthChallenge where
  toPath = Lude.const "/"

instance Lude.ToQuery RespondToAuthChallenge where
  toQuery = Lude.const Lude.mempty

-- | The response to respond to the authentication challenge.
--
-- /See:/ 'mkRespondToAuthChallengeResponse' smart constructor.
data RespondToAuthChallengeResponse = RespondToAuthChallengeResponse'
  { -- | The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
    challengeName :: Lude.Maybe ChallengeNameType,
    -- | The challenge parameters. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
    challengeParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The result returned by the server in response to the request to respond to the authentication challenge.
    authenticationResult :: Lude.Maybe AuthenticationResultType,
    -- | The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
    session :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RespondToAuthChallengeResponse' with the minimum fields required to make a request.
--
-- * 'challengeName' - The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
-- * 'challengeParameters' - The challenge parameters. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
-- * 'authenticationResult' - The result returned by the server in response to the request to respond to the authentication challenge.
-- * 'session' - The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
-- * 'responseStatus' - The response status code.
mkRespondToAuthChallengeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RespondToAuthChallengeResponse
mkRespondToAuthChallengeResponse pResponseStatus_ =
  RespondToAuthChallengeResponse'
    { challengeName = Lude.Nothing,
      challengeParameters = Lude.Nothing,
      authenticationResult = Lude.Nothing,
      session = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrsChallengeName :: Lens.Lens' RespondToAuthChallengeResponse (Lude.Maybe ChallengeNameType)
rtacrsChallengeName = Lens.lens (challengeName :: RespondToAuthChallengeResponse -> Lude.Maybe ChallengeNameType) (\s a -> s {challengeName = a} :: RespondToAuthChallengeResponse)
{-# DEPRECATED rtacrsChallengeName "Use generic-lens or generic-optics with 'challengeName' instead." #-}

-- | The challenge parameters. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
--
-- /Note:/ Consider using 'challengeParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrsChallengeParameters :: Lens.Lens' RespondToAuthChallengeResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rtacrsChallengeParameters = Lens.lens (challengeParameters :: RespondToAuthChallengeResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {challengeParameters = a} :: RespondToAuthChallengeResponse)
{-# DEPRECATED rtacrsChallengeParameters "Use generic-lens or generic-optics with 'challengeParameters' instead." #-}

-- | The result returned by the server in response to the request to respond to the authentication challenge.
--
-- /Note:/ Consider using 'authenticationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrsAuthenticationResult :: Lens.Lens' RespondToAuthChallengeResponse (Lude.Maybe AuthenticationResultType)
rtacrsAuthenticationResult = Lens.lens (authenticationResult :: RespondToAuthChallengeResponse -> Lude.Maybe AuthenticationResultType) (\s a -> s {authenticationResult = a} :: RespondToAuthChallengeResponse)
{-# DEPRECATED rtacrsAuthenticationResult "Use generic-lens or generic-optics with 'authenticationResult' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrsSession :: Lens.Lens' RespondToAuthChallengeResponse (Lude.Maybe Lude.Text)
rtacrsSession = Lens.lens (session :: RespondToAuthChallengeResponse -> Lude.Maybe Lude.Text) (\s a -> s {session = a} :: RespondToAuthChallengeResponse)
{-# DEPRECATED rtacrsSession "Use generic-lens or generic-optics with 'session' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrsResponseStatus :: Lens.Lens' RespondToAuthChallengeResponse Lude.Int
rtacrsResponseStatus = Lens.lens (responseStatus :: RespondToAuthChallengeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RespondToAuthChallengeResponse)
{-# DEPRECATED rtacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
