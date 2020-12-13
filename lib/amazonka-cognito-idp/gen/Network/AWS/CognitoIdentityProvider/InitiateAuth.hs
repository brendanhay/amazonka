{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.InitiateAuth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the authentication flow.
module Network.AWS.CognitoIdentityProvider.InitiateAuth
  ( -- * Creating a request
    InitiateAuth (..),
    mkInitiateAuth,

    -- ** Request lenses
    iaClientMetadata,
    iaClientId,
    iaAuthFlow,
    iaAnalyticsMetadata,
    iaUserContextData,
    iaAuthParameters,

    -- * Destructuring the response
    InitiateAuthResponse (..),
    mkInitiateAuthResponse,

    -- ** Response lenses
    iarsChallengeName,
    iarsChallengeParameters,
    iarsAuthenticationResult,
    iarsSession,
    iarsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Initiates the authentication request.
--
-- /See:/ 'mkInitiateAuth' smart constructor.
data InitiateAuth = InitiateAuth'
  { -- | A map of custom key-value pairs that you can provide as input for certain custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the InitiateAuth API action, Amazon Cognito invokes the AWS Lambda functions that are specified for various triggers. The ClientMetadata value is passed as input to the functions for only the following triggers:
    --
    --     * Pre signup
    --
    --
    --     * Pre authentication
    --
    --
    --     * User migration
    --
    --
    -- When Amazon Cognito invokes the functions for these triggers, it passes a JSON payload, which the function receives as input. This payload contains a @validationData@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your InitiateAuth request. In your function code in AWS Lambda, you can process the @validationData@ value to enhance your workflow for your specific needs.
    -- When you use the InitiateAuth API action, Amazon Cognito also invokes the functions for the following triggers, but it does not provide the ClientMetadata value as input:
    --
    --     * Post authentication
    --
    --
    --     * Custom message
    --
    --
    --     * Pre token generation
    --
    --
    --     * Create auth challenge
    --
    --
    --     * Define auth challenge
    --
    --
    --     * Verify auth challenge
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The app client ID.
    clientId :: Lude.Sensitive Lude.Text,
    -- | The authentication flow for this call to execute. The API action will depend on this value. For example:
    --
    --
    --     * @REFRESH_TOKEN_AUTH@ will take in a valid refresh token and return new tokens.
    --
    --
    --     * @USER_SRP_AUTH@ will take in @USERNAME@ and @SRP_A@ and return the SRP variables to be used for next challenge execution.
    --
    --
    --     * @USER_PASSWORD_AUTH@ will take in @USERNAME@ and @PASSWORD@ and return the next challenge or tokens.
    --
    --
    -- Valid values include:
    --
    --     * @USER_SRP_AUTH@ : Authentication flow for the Secure Remote Password (SRP) protocol.
    --
    --
    --     * @REFRESH_TOKEN_AUTH@ /@REFRESH_TOKEN@ : Authentication flow for refreshing the access token and ID token by supplying a valid refresh token.
    --
    --
    --     * @CUSTOM_AUTH@ : Custom authentication flow.
    --
    --
    --     * @USER_PASSWORD_AUTH@ : Non-SRP authentication flow; USERNAME and PASSWORD are passed directly. If a user migration Lambda trigger is set, this flow will invoke the user migration Lambda if the USERNAME is not found in the user pool.
    --
    --
    --     * @ADMIN_USER_PASSWORD_AUTH@ : Admin-based user password authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication flow. In this flow, Cognito receives the password in the request instead of using the SRP process to verify passwords.
    --
    --
    -- @ADMIN_NO_SRP_AUTH@ is not a valid value.
    authFlow :: AuthFlowType,
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for @InitiateAuth@ calls.
    analyticsMetadata :: Lude.Maybe AnalyticsMetadataType,
    -- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
    userContextData :: Lude.Maybe UserContextDataType,
    -- | The authentication parameters. These are inputs corresponding to the @AuthFlow@ that you are invoking. The required values depend on the value of @AuthFlow@ :
    --
    --
    --     * For @USER_SRP_AUTH@ : @USERNAME@ (required), @SRP_A@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
    --
    --
    --     * For @REFRESH_TOKEN_AUTH/REFRESH_TOKEN@ : @REFRESH_TOKEN@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
    --
    --
    --     * For @CUSTOM_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @DEVICE_KEY@ . To start the authentication flow with password verification, include @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@ .
    authParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateAuth' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the InitiateAuth API action, Amazon Cognito invokes the AWS Lambda functions that are specified for various triggers. The ClientMetadata value is passed as input to the functions for only the following triggers:
--
--     * Pre signup
--
--
--     * Pre authentication
--
--
--     * User migration
--
--
-- When Amazon Cognito invokes the functions for these triggers, it passes a JSON payload, which the function receives as input. This payload contains a @validationData@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your InitiateAuth request. In your function code in AWS Lambda, you can process the @validationData@ value to enhance your workflow for your specific needs.
-- When you use the InitiateAuth API action, Amazon Cognito also invokes the functions for the following triggers, but it does not provide the ClientMetadata value as input:
--
--     * Post authentication
--
--
--     * Custom message
--
--
--     * Pre token generation
--
--
--     * Create auth challenge
--
--
--     * Define auth challenge
--
--
--     * Verify auth challenge
--
--
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'clientId' - The app client ID.
-- * 'authFlow' - The authentication flow for this call to execute. The API action will depend on this value. For example:
--
--
--     * @REFRESH_TOKEN_AUTH@ will take in a valid refresh token and return new tokens.
--
--
--     * @USER_SRP_AUTH@ will take in @USERNAME@ and @SRP_A@ and return the SRP variables to be used for next challenge execution.
--
--
--     * @USER_PASSWORD_AUTH@ will take in @USERNAME@ and @PASSWORD@ and return the next challenge or tokens.
--
--
-- Valid values include:
--
--     * @USER_SRP_AUTH@ : Authentication flow for the Secure Remote Password (SRP) protocol.
--
--
--     * @REFRESH_TOKEN_AUTH@ /@REFRESH_TOKEN@ : Authentication flow for refreshing the access token and ID token by supplying a valid refresh token.
--
--
--     * @CUSTOM_AUTH@ : Custom authentication flow.
--
--
--     * @USER_PASSWORD_AUTH@ : Non-SRP authentication flow; USERNAME and PASSWORD are passed directly. If a user migration Lambda trigger is set, this flow will invoke the user migration Lambda if the USERNAME is not found in the user pool.
--
--
--     * @ADMIN_USER_PASSWORD_AUTH@ : Admin-based user password authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication flow. In this flow, Cognito receives the password in the request instead of using the SRP process to verify passwords.
--
--
-- @ADMIN_NO_SRP_AUTH@ is not a valid value.
-- * 'analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for @InitiateAuth@ calls.
-- * 'userContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
-- * 'authParameters' - The authentication parameters. These are inputs corresponding to the @AuthFlow@ that you are invoking. The required values depend on the value of @AuthFlow@ :
--
--
--     * For @USER_SRP_AUTH@ : @USERNAME@ (required), @SRP_A@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
--
--
--     * For @REFRESH_TOKEN_AUTH/REFRESH_TOKEN@ : @REFRESH_TOKEN@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
--
--
--     * For @CUSTOM_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @DEVICE_KEY@ . To start the authentication flow with password verification, include @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@ .
mkInitiateAuth ::
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  -- | 'authFlow'
  AuthFlowType ->
  InitiateAuth
mkInitiateAuth pClientId_ pAuthFlow_ =
  InitiateAuth'
    { clientMetadata = Lude.Nothing,
      clientId = pClientId_,
      authFlow = pAuthFlow_,
      analyticsMetadata = Lude.Nothing,
      userContextData = Lude.Nothing,
      authParameters = Lude.Nothing
    }

-- | A map of custom key-value pairs that you can provide as input for certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the InitiateAuth API action, Amazon Cognito invokes the AWS Lambda functions that are specified for various triggers. The ClientMetadata value is passed as input to the functions for only the following triggers:
--
--     * Pre signup
--
--
--     * Pre authentication
--
--
--     * User migration
--
--
-- When Amazon Cognito invokes the functions for these triggers, it passes a JSON payload, which the function receives as input. This payload contains a @validationData@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your InitiateAuth request. In your function code in AWS Lambda, you can process the @validationData@ value to enhance your workflow for your specific needs.
-- When you use the InitiateAuth API action, Amazon Cognito also invokes the functions for the following triggers, but it does not provide the ClientMetadata value as input:
--
--     * Post authentication
--
--
--     * Custom message
--
--
--     * Pre token generation
--
--
--     * Create auth challenge
--
--
--     * Define auth challenge
--
--
--     * Verify auth challenge
--
--
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaClientMetadata :: Lens.Lens' InitiateAuth (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
iaClientMetadata = Lens.lens (clientMetadata :: InitiateAuth -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: InitiateAuth)
{-# DEPRECATED iaClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaClientId :: Lens.Lens' InitiateAuth (Lude.Sensitive Lude.Text)
iaClientId = Lens.lens (clientId :: InitiateAuth -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: InitiateAuth)
{-# DEPRECATED iaClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The authentication flow for this call to execute. The API action will depend on this value. For example:
--
--
--     * @REFRESH_TOKEN_AUTH@ will take in a valid refresh token and return new tokens.
--
--
--     * @USER_SRP_AUTH@ will take in @USERNAME@ and @SRP_A@ and return the SRP variables to be used for next challenge execution.
--
--
--     * @USER_PASSWORD_AUTH@ will take in @USERNAME@ and @PASSWORD@ and return the next challenge or tokens.
--
--
-- Valid values include:
--
--     * @USER_SRP_AUTH@ : Authentication flow for the Secure Remote Password (SRP) protocol.
--
--
--     * @REFRESH_TOKEN_AUTH@ /@REFRESH_TOKEN@ : Authentication flow for refreshing the access token and ID token by supplying a valid refresh token.
--
--
--     * @CUSTOM_AUTH@ : Custom authentication flow.
--
--
--     * @USER_PASSWORD_AUTH@ : Non-SRP authentication flow; USERNAME and PASSWORD are passed directly. If a user migration Lambda trigger is set, this flow will invoke the user migration Lambda if the USERNAME is not found in the user pool.
--
--
--     * @ADMIN_USER_PASSWORD_AUTH@ : Admin-based user password authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication flow. In this flow, Cognito receives the password in the request instead of using the SRP process to verify passwords.
--
--
-- @ADMIN_NO_SRP_AUTH@ is not a valid value.
--
-- /Note:/ Consider using 'authFlow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAuthFlow :: Lens.Lens' InitiateAuth AuthFlowType
iaAuthFlow = Lens.lens (authFlow :: InitiateAuth -> AuthFlowType) (\s a -> s {authFlow = a} :: InitiateAuth)
{-# DEPRECATED iaAuthFlow "Use generic-lens or generic-optics with 'authFlow' instead." #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @InitiateAuth@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAnalyticsMetadata :: Lens.Lens' InitiateAuth (Lude.Maybe AnalyticsMetadataType)
iaAnalyticsMetadata = Lens.lens (analyticsMetadata :: InitiateAuth -> Lude.Maybe AnalyticsMetadataType) (\s a -> s {analyticsMetadata = a} :: InitiateAuth)
{-# DEPRECATED iaAnalyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaUserContextData :: Lens.Lens' InitiateAuth (Lude.Maybe UserContextDataType)
iaUserContextData = Lens.lens (userContextData :: InitiateAuth -> Lude.Maybe UserContextDataType) (\s a -> s {userContextData = a} :: InitiateAuth)
{-# DEPRECATED iaUserContextData "Use generic-lens or generic-optics with 'userContextData' instead." #-}

-- | The authentication parameters. These are inputs corresponding to the @AuthFlow@ that you are invoking. The required values depend on the value of @AuthFlow@ :
--
--
--     * For @USER_SRP_AUTH@ : @USERNAME@ (required), @SRP_A@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
--
--
--     * For @REFRESH_TOKEN_AUTH/REFRESH_TOKEN@ : @REFRESH_TOKEN@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
--
--
--     * For @CUSTOM_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @DEVICE_KEY@ . To start the authentication flow with password verification, include @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@ .
--
--
--
-- /Note:/ Consider using 'authParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAuthParameters :: Lens.Lens' InitiateAuth (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
iaAuthParameters = Lens.lens (authParameters :: InitiateAuth -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {authParameters = a} :: InitiateAuth)
{-# DEPRECATED iaAuthParameters "Use generic-lens or generic-optics with 'authParameters' instead." #-}

instance Lude.AWSRequest InitiateAuth where
  type Rs InitiateAuth = InitiateAuthResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          InitiateAuthResponse'
            Lude.<$> (x Lude..?> "ChallengeName")
            Lude.<*> (x Lude..?> "ChallengeParameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "AuthenticationResult")
            Lude.<*> (x Lude..?> "Session")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders InitiateAuth where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.InitiateAuth" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON InitiateAuth where
  toJSON InitiateAuth' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("ClientId" Lude..= clientId),
            Lude.Just ("AuthFlow" Lude..= authFlow),
            ("AnalyticsMetadata" Lude..=) Lude.<$> analyticsMetadata,
            ("UserContextData" Lude..=) Lude.<$> userContextData,
            ("AuthParameters" Lude..=) Lude.<$> authParameters
          ]
      )

instance Lude.ToPath InitiateAuth where
  toPath = Lude.const "/"

instance Lude.ToQuery InitiateAuth where
  toQuery = Lude.const Lude.mempty

-- | Initiates the authentication response.
--
-- /See:/ 'mkInitiateAuthResponse' smart constructor.
data InitiateAuthResponse = InitiateAuthResponse'
  { -- | The name of the challenge which you are responding to with this call. This is returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge.
    --
    -- Valid values include the following. Note that all of these challenges require @USERNAME@ and @SECRET_HASH@ (if applicable) in the parameters.
    --
    --     * @SMS_MFA@ : Next challenge is to supply an @SMS_MFA_CODE@ , delivered via SMS.
    --
    --
    --     * @PASSWORD_VERIFIER@ : Next challenge is to supply @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , and @TIMESTAMP@ after the client-side SRP calculations.
    --
    --
    --     * @CUSTOM_CHALLENGE@ : This is returned if your custom authentication flow determines that the user should pass another challenge before tokens are issued.
    --
    --
    --     * @DEVICE_SRP_AUTH@ : If device tracking was enabled on your user pool and the previous challenges were passed, this challenge is returned so that Amazon Cognito can start tracking this device.
    --
    --
    --     * @DEVICE_PASSWORD_VERIFIER@ : Similar to @PASSWORD_VERIFIER@ , but for devices only.
    --
    --
    --     * @NEW_PASSWORD_REQUIRED@ : For users which are required to change their passwords after successful first login. This challenge should be passed with @NEW_PASSWORD@ and any other required attributes.
    challengeName :: Lude.Maybe ChallengeNameType,
    -- | The challenge parameters. These are returned to you in the @InitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@RespondToAuthChallenge@ ).
    --
    -- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
    challengeParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
    authenticationResult :: Lude.Maybe AuthenticationResultType,
    -- | The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
    session :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateAuthResponse' with the minimum fields required to make a request.
--
-- * 'challengeName' - The name of the challenge which you are responding to with this call. This is returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge.
--
-- Valid values include the following. Note that all of these challenges require @USERNAME@ and @SECRET_HASH@ (if applicable) in the parameters.
--
--     * @SMS_MFA@ : Next challenge is to supply an @SMS_MFA_CODE@ , delivered via SMS.
--
--
--     * @PASSWORD_VERIFIER@ : Next challenge is to supply @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , and @TIMESTAMP@ after the client-side SRP calculations.
--
--
--     * @CUSTOM_CHALLENGE@ : This is returned if your custom authentication flow determines that the user should pass another challenge before tokens are issued.
--
--
--     * @DEVICE_SRP_AUTH@ : If device tracking was enabled on your user pool and the previous challenges were passed, this challenge is returned so that Amazon Cognito can start tracking this device.
--
--
--     * @DEVICE_PASSWORD_VERIFIER@ : Similar to @PASSWORD_VERIFIER@ , but for devices only.
--
--
--     * @NEW_PASSWORD_REQUIRED@ : For users which are required to change their passwords after successful first login. This challenge should be passed with @NEW_PASSWORD@ and any other required attributes.
--
--
-- * 'challengeParameters' - The challenge parameters. These are returned to you in the @InitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@RespondToAuthChallenge@ ).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
-- * 'authenticationResult' - The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
-- * 'session' - The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
-- * 'responseStatus' - The response status code.
mkInitiateAuthResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  InitiateAuthResponse
mkInitiateAuthResponse pResponseStatus_ =
  InitiateAuthResponse'
    { challengeName = Lude.Nothing,
      challengeParameters = Lude.Nothing,
      authenticationResult = Lude.Nothing,
      session = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the challenge which you are responding to with this call. This is returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge.
--
-- Valid values include the following. Note that all of these challenges require @USERNAME@ and @SECRET_HASH@ (if applicable) in the parameters.
--
--     * @SMS_MFA@ : Next challenge is to supply an @SMS_MFA_CODE@ , delivered via SMS.
--
--
--     * @PASSWORD_VERIFIER@ : Next challenge is to supply @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , and @TIMESTAMP@ after the client-side SRP calculations.
--
--
--     * @CUSTOM_CHALLENGE@ : This is returned if your custom authentication flow determines that the user should pass another challenge before tokens are issued.
--
--
--     * @DEVICE_SRP_AUTH@ : If device tracking was enabled on your user pool and the previous challenges were passed, this challenge is returned so that Amazon Cognito can start tracking this device.
--
--
--     * @DEVICE_PASSWORD_VERIFIER@ : Similar to @PASSWORD_VERIFIER@ , but for devices only.
--
--
--     * @NEW_PASSWORD_REQUIRED@ : For users which are required to change their passwords after successful first login. This challenge should be passed with @NEW_PASSWORD@ and any other required attributes.
--
--
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iarsChallengeName :: Lens.Lens' InitiateAuthResponse (Lude.Maybe ChallengeNameType)
iarsChallengeName = Lens.lens (challengeName :: InitiateAuthResponse -> Lude.Maybe ChallengeNameType) (\s a -> s {challengeName = a} :: InitiateAuthResponse)
{-# DEPRECATED iarsChallengeName "Use generic-lens or generic-optics with 'challengeName' instead." #-}

-- | The challenge parameters. These are returned to you in the @InitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@RespondToAuthChallenge@ ).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
--
-- /Note:/ Consider using 'challengeParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iarsChallengeParameters :: Lens.Lens' InitiateAuthResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
iarsChallengeParameters = Lens.lens (challengeParameters :: InitiateAuthResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {challengeParameters = a} :: InitiateAuthResponse)
{-# DEPRECATED iarsChallengeParameters "Use generic-lens or generic-optics with 'challengeParameters' instead." #-}

-- | The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
--
-- /Note:/ Consider using 'authenticationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iarsAuthenticationResult :: Lens.Lens' InitiateAuthResponse (Lude.Maybe AuthenticationResultType)
iarsAuthenticationResult = Lens.lens (authenticationResult :: InitiateAuthResponse -> Lude.Maybe AuthenticationResultType) (\s a -> s {authenticationResult = a} :: InitiateAuthResponse)
{-# DEPRECATED iarsAuthenticationResult "Use generic-lens or generic-optics with 'authenticationResult' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iarsSession :: Lens.Lens' InitiateAuthResponse (Lude.Maybe Lude.Text)
iarsSession = Lens.lens (session :: InitiateAuthResponse -> Lude.Maybe Lude.Text) (\s a -> s {session = a} :: InitiateAuthResponse)
{-# DEPRECATED iarsSession "Use generic-lens or generic-optics with 'session' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iarsResponseStatus :: Lens.Lens' InitiateAuthResponse Lude.Int
iarsResponseStatus = Lens.lens (responseStatus :: InitiateAuthResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InitiateAuthResponse)
{-# DEPRECATED iarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
