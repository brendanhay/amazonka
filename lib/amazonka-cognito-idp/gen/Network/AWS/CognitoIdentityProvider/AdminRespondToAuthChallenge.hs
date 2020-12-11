{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminRespondToAuthChallenge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Responds to an authentication challenge, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminRespondToAuthChallenge
  ( -- * Creating a request
    AdminRespondToAuthChallenge (..),
    mkAdminRespondToAuthChallenge,

    -- ** Request lenses
    artacClientMetadata,
    artacContextData,
    artacAnalyticsMetadata,
    artacChallengeResponses,
    artacSession,
    artacUserPoolId,
    artacClientId,
    artacChallengeName,

    -- * Destructuring the response
    AdminRespondToAuthChallengeResponse (..),
    mkAdminRespondToAuthChallengeResponse,

    -- ** Response lenses
    artacrsChallengeName,
    artacrsChallengeParameters,
    artacrsAuthenticationResult,
    artacrsSession,
    artacrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to respond to the authentication challenge, as an administrator.
--
-- /See:/ 'mkAdminRespondToAuthChallenge' smart constructor.
data AdminRespondToAuthChallenge = AdminRespondToAuthChallenge'
  { clientMetadata ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    contextData ::
      Lude.Maybe ContextDataType,
    analyticsMetadata ::
      Lude.Maybe AnalyticsMetadataType,
    challengeResponses ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    session :: Lude.Maybe Lude.Text,
    userPoolId :: Lude.Text,
    clientId ::
      Lude.Sensitive Lude.Text,
    challengeName :: ChallengeNameType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminRespondToAuthChallenge' with the minimum fields required to make a request.
--
-- * 'analyticsMetadata' - The analytics metadata for collecting Amazon Pinpoint metrics for @AdminRespondToAuthChallenge@ calls.
-- * 'challengeName' - The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
-- * 'challengeResponses' - The challenge responses. These are inputs corresponding to the value of @ChallengeName@ , for example:
--
--
--     * @SMS_MFA@ : @SMS_MFA_CODE@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).
--
--
--     * @PASSWORD_VERIFIER@ : @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , @TIMESTAMP@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).
--
--
--     * @ADMIN_NO_SRP_AUTH@ : @PASSWORD@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).
--
--
--     * @NEW_PASSWORD_REQUIRED@ : @NEW_PASSWORD@ , any other required attributes, @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).
--
--
-- The value of the @USERNAME@ attribute must be the user's actual username, not an alias (such as email address or phone number). To make this easier, the @AdminInitiateAuth@ response includes the actual username value in the @USERNAMEUSER_ID_FOR_SRP@ attribute, even if you specified an alias in your call to @AdminInitiateAuth@ .
-- * 'clientId' - The app client ID.
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminRespondToAuthChallenge API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /pre sign-up/ , /custom message/ , /post authentication/ , /user migration/ , /pre token generation/ , /define auth challenge/ , /create auth challenge/ , and /verify auth challenge response/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminRespondToAuthChallenge request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'contextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
-- * 'session' - The session which should be passed both ways in challenge-response calls to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
-- * 'userPoolId' - The ID of the Amazon Cognito user pool.
mkAdminRespondToAuthChallenge ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  -- | 'challengeName'
  ChallengeNameType ->
  AdminRespondToAuthChallenge
mkAdminRespondToAuthChallenge
  pUserPoolId_
  pClientId_
  pChallengeName_ =
    AdminRespondToAuthChallenge'
      { clientMetadata = Lude.Nothing,
        contextData = Lude.Nothing,
        analyticsMetadata = Lude.Nothing,
        challengeResponses = Lude.Nothing,
        session = Lude.Nothing,
        userPoolId = pUserPoolId_,
        clientId = pClientId_,
        challengeName = pChallengeName_
      }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminRespondToAuthChallenge API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /pre sign-up/ , /custom message/ , /post authentication/ , /user migration/ , /pre token generation/ , /define auth challenge/ , /create auth challenge/ , and /verify auth challenge response/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminRespondToAuthChallenge request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacClientMetadata :: Lens.Lens' AdminRespondToAuthChallenge (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
artacClientMetadata = Lens.lens (clientMetadata :: AdminRespondToAuthChallenge -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: AdminRespondToAuthChallenge)
{-# DEPRECATED artacClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'contextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacContextData :: Lens.Lens' AdminRespondToAuthChallenge (Lude.Maybe ContextDataType)
artacContextData = Lens.lens (contextData :: AdminRespondToAuthChallenge -> Lude.Maybe ContextDataType) (\s a -> s {contextData = a} :: AdminRespondToAuthChallenge)
{-# DEPRECATED artacContextData "Use generic-lens or generic-optics with 'contextData' instead." #-}

-- | The analytics metadata for collecting Amazon Pinpoint metrics for @AdminRespondToAuthChallenge@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacAnalyticsMetadata :: Lens.Lens' AdminRespondToAuthChallenge (Lude.Maybe AnalyticsMetadataType)
artacAnalyticsMetadata = Lens.lens (analyticsMetadata :: AdminRespondToAuthChallenge -> Lude.Maybe AnalyticsMetadataType) (\s a -> s {analyticsMetadata = a} :: AdminRespondToAuthChallenge)
{-# DEPRECATED artacAnalyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead." #-}

-- | The challenge responses. These are inputs corresponding to the value of @ChallengeName@ , for example:
--
--
--     * @SMS_MFA@ : @SMS_MFA_CODE@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).
--
--
--     * @PASSWORD_VERIFIER@ : @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , @TIMESTAMP@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).
--
--
--     * @ADMIN_NO_SRP_AUTH@ : @PASSWORD@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).
--
--
--     * @NEW_PASSWORD_REQUIRED@ : @NEW_PASSWORD@ , any other required attributes, @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).
--
--
-- The value of the @USERNAME@ attribute must be the user's actual username, not an alias (such as email address or phone number). To make this easier, the @AdminInitiateAuth@ response includes the actual username value in the @USERNAMEUSER_ID_FOR_SRP@ attribute, even if you specified an alias in your call to @AdminInitiateAuth@ .
--
-- /Note:/ Consider using 'challengeResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacChallengeResponses :: Lens.Lens' AdminRespondToAuthChallenge (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
artacChallengeResponses = Lens.lens (challengeResponses :: AdminRespondToAuthChallenge -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {challengeResponses = a} :: AdminRespondToAuthChallenge)
{-# DEPRECATED artacChallengeResponses "Use generic-lens or generic-optics with 'challengeResponses' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacSession :: Lens.Lens' AdminRespondToAuthChallenge (Lude.Maybe Lude.Text)
artacSession = Lens.lens (session :: AdminRespondToAuthChallenge -> Lude.Maybe Lude.Text) (\s a -> s {session = a} :: AdminRespondToAuthChallenge)
{-# DEPRECATED artacSession "Use generic-lens or generic-optics with 'session' instead." #-}

-- | The ID of the Amazon Cognito user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacUserPoolId :: Lens.Lens' AdminRespondToAuthChallenge Lude.Text
artacUserPoolId = Lens.lens (userPoolId :: AdminRespondToAuthChallenge -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminRespondToAuthChallenge)
{-# DEPRECATED artacUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacClientId :: Lens.Lens' AdminRespondToAuthChallenge (Lude.Sensitive Lude.Text)
artacClientId = Lens.lens (clientId :: AdminRespondToAuthChallenge -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: AdminRespondToAuthChallenge)
{-# DEPRECATED artacClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacChallengeName :: Lens.Lens' AdminRespondToAuthChallenge ChallengeNameType
artacChallengeName = Lens.lens (challengeName :: AdminRespondToAuthChallenge -> ChallengeNameType) (\s a -> s {challengeName = a} :: AdminRespondToAuthChallenge)
{-# DEPRECATED artacChallengeName "Use generic-lens or generic-optics with 'challengeName' instead." #-}

instance Lude.AWSRequest AdminRespondToAuthChallenge where
  type
    Rs AdminRespondToAuthChallenge =
      AdminRespondToAuthChallengeResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          AdminRespondToAuthChallengeResponse'
            Lude.<$> (x Lude..?> "ChallengeName")
            Lude.<*> (x Lude..?> "ChallengeParameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "AuthenticationResult")
            Lude.<*> (x Lude..?> "Session")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminRespondToAuthChallenge where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminRespondToAuthChallenge" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminRespondToAuthChallenge where
  toJSON AdminRespondToAuthChallenge' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            ("ContextData" Lude..=) Lude.<$> contextData,
            ("AnalyticsMetadata" Lude..=) Lude.<$> analyticsMetadata,
            ("ChallengeResponses" Lude..=) Lude.<$> challengeResponses,
            ("Session" Lude..=) Lude.<$> session,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("ClientId" Lude..= clientId),
            Lude.Just ("ChallengeName" Lude..= challengeName)
          ]
      )

instance Lude.ToPath AdminRespondToAuthChallenge where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminRespondToAuthChallenge where
  toQuery = Lude.const Lude.mempty

-- | Responds to the authentication challenge, as an administrator.
--
-- /See:/ 'mkAdminRespondToAuthChallengeResponse' smart constructor.
data AdminRespondToAuthChallengeResponse = AdminRespondToAuthChallengeResponse'
  { challengeName ::
      Lude.Maybe
        ChallengeNameType,
    challengeParameters ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    authenticationResult ::
      Lude.Maybe
        AuthenticationResultType,
    session ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminRespondToAuthChallengeResponse' with the minimum fields required to make a request.
--
-- * 'authenticationResult' - The result returned by the server in response to the authentication request.
-- * 'challengeName' - The name of the challenge. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
-- * 'challengeParameters' - The challenge parameters. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
-- * 'responseStatus' - The response status code.
-- * 'session' - The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
mkAdminRespondToAuthChallengeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminRespondToAuthChallengeResponse
mkAdminRespondToAuthChallengeResponse pResponseStatus_ =
  AdminRespondToAuthChallengeResponse'
    { challengeName =
        Lude.Nothing,
      challengeParameters = Lude.Nothing,
      authenticationResult = Lude.Nothing,
      session = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the challenge. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacrsChallengeName :: Lens.Lens' AdminRespondToAuthChallengeResponse (Lude.Maybe ChallengeNameType)
artacrsChallengeName = Lens.lens (challengeName :: AdminRespondToAuthChallengeResponse -> Lude.Maybe ChallengeNameType) (\s a -> s {challengeName = a} :: AdminRespondToAuthChallengeResponse)
{-# DEPRECATED artacrsChallengeName "Use generic-lens or generic-optics with 'challengeName' instead." #-}

-- | The challenge parameters. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
--
-- /Note:/ Consider using 'challengeParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacrsChallengeParameters :: Lens.Lens' AdminRespondToAuthChallengeResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
artacrsChallengeParameters = Lens.lens (challengeParameters :: AdminRespondToAuthChallengeResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {challengeParameters = a} :: AdminRespondToAuthChallengeResponse)
{-# DEPRECATED artacrsChallengeParameters "Use generic-lens or generic-optics with 'challengeParameters' instead." #-}

-- | The result returned by the server in response to the authentication request.
--
-- /Note:/ Consider using 'authenticationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacrsAuthenticationResult :: Lens.Lens' AdminRespondToAuthChallengeResponse (Lude.Maybe AuthenticationResultType)
artacrsAuthenticationResult = Lens.lens (authenticationResult :: AdminRespondToAuthChallengeResponse -> Lude.Maybe AuthenticationResultType) (\s a -> s {authenticationResult = a} :: AdminRespondToAuthChallengeResponse)
{-# DEPRECATED artacrsAuthenticationResult "Use generic-lens or generic-optics with 'authenticationResult' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacrsSession :: Lens.Lens' AdminRespondToAuthChallengeResponse (Lude.Maybe Lude.Text)
artacrsSession = Lens.lens (session :: AdminRespondToAuthChallengeResponse -> Lude.Maybe Lude.Text) (\s a -> s {session = a} :: AdminRespondToAuthChallengeResponse)
{-# DEPRECATED artacrsSession "Use generic-lens or generic-optics with 'session' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacrsResponseStatus :: Lens.Lens' AdminRespondToAuthChallengeResponse Lude.Int
artacrsResponseStatus = Lens.lens (responseStatus :: AdminRespondToAuthChallengeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminRespondToAuthChallengeResponse)
{-# DEPRECATED artacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
