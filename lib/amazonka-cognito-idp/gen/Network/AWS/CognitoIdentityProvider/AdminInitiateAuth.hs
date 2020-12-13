{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the authentication flow, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
  ( -- * Creating a request
    AdminInitiateAuth (..),
    mkAdminInitiateAuth,

    -- ** Request lenses
    aiaClientMetadata,
    aiaClientId,
    aiaContextData,
    aiaAuthFlow,
    aiaUserPoolId,
    aiaAnalyticsMetadata,
    aiaAuthParameters,

    -- * Destructuring the response
    AdminInitiateAuthResponse (..),
    mkAdminInitiateAuthResponse,

    -- ** Response lenses
    aiarsChallengeName,
    aiarsChallengeParameters,
    aiarsAuthenticationResult,
    aiarsSession,
    aiarsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Initiates the authorization request, as an administrator.
--
-- /See:/ 'mkAdminInitiateAuth' smart constructor.
data AdminInitiateAuth = AdminInitiateAuth'
  { -- | A map of custom key-value pairs that you can provide as input for certain custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminInitiateAuth API action, Amazon Cognito invokes the AWS Lambda functions that are specified for various triggers. The ClientMetadata value is passed as input to the functions for only the following triggers:
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
    -- When Amazon Cognito invokes the functions for these triggers, it passes a JSON payload, which the function receives as input. This payload contains a @validationData@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminInitiateAuth request. In your function code in AWS Lambda, you can process the @validationData@ value to enhance your workflow for your specific needs.
    -- When you use the AdminInitiateAuth API action, Amazon Cognito also invokes the functions for the following triggers, but it does not provide the ClientMetadata value as input:
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
    -- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
    contextData :: Lude.Maybe ContextDataType,
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
    --     * @ADMIN_NO_SRP_AUTH@ : Non-SRP authentication flow; you can pass in the USERNAME and PASSWORD directly if the flow is enabled for calling the app client.
    --
    --
    --     * @USER_PASSWORD_AUTH@ : Non-SRP authentication flow; USERNAME and PASSWORD are passed directly. If a user migration Lambda trigger is set, this flow will invoke the user migration Lambda if the USERNAME is not found in the user pool.
    --
    --
    --     * @ADMIN_USER_PASSWORD_AUTH@ : Admin-based user password authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication flow. In this flow, Cognito receives the password in the request instead of using the SRP process to verify passwords.
    authFlow :: AuthFlowType,
    -- | The ID of the Amazon Cognito user pool.
    userPoolId :: Lude.Text,
    -- | The analytics metadata for collecting Amazon Pinpoint metrics for @AdminInitiateAuth@ calls.
    analyticsMetadata :: Lude.Maybe AnalyticsMetadataType,
    -- | The authentication parameters. These are inputs corresponding to the @AuthFlow@ that you are invoking. The required values depend on the value of @AuthFlow@ :
    --
    --
    --     * For @USER_SRP_AUTH@ : @USERNAME@ (required), @SRP_A@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
    --
    --
    --     * For @REFRESH_TOKEN_AUTH/REFRESH_TOKEN@ : @REFRESH_TOKEN@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
    --
    --
    --     * For @ADMIN_NO_SRP_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @PASSWORD@ (required), @DEVICE_KEY@ .
    --
    --
    --     * For @CUSTOM_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @DEVICE_KEY@ . To start the authentication flow with password verification, include @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@ .
    authParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminInitiateAuth' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminInitiateAuth API action, Amazon Cognito invokes the AWS Lambda functions that are specified for various triggers. The ClientMetadata value is passed as input to the functions for only the following triggers:
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
-- When Amazon Cognito invokes the functions for these triggers, it passes a JSON payload, which the function receives as input. This payload contains a @validationData@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminInitiateAuth request. In your function code in AWS Lambda, you can process the @validationData@ value to enhance your workflow for your specific needs.
-- When you use the AdminInitiateAuth API action, Amazon Cognito also invokes the functions for the following triggers, but it does not provide the ClientMetadata value as input:
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
-- * 'contextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
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
--     * @ADMIN_NO_SRP_AUTH@ : Non-SRP authentication flow; you can pass in the USERNAME and PASSWORD directly if the flow is enabled for calling the app client.
--
--
--     * @USER_PASSWORD_AUTH@ : Non-SRP authentication flow; USERNAME and PASSWORD are passed directly. If a user migration Lambda trigger is set, this flow will invoke the user migration Lambda if the USERNAME is not found in the user pool.
--
--
--     * @ADMIN_USER_PASSWORD_AUTH@ : Admin-based user password authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication flow. In this flow, Cognito receives the password in the request instead of using the SRP process to verify passwords.
--
--
-- * 'userPoolId' - The ID of the Amazon Cognito user pool.
-- * 'analyticsMetadata' - The analytics metadata for collecting Amazon Pinpoint metrics for @AdminInitiateAuth@ calls.
-- * 'authParameters' - The authentication parameters. These are inputs corresponding to the @AuthFlow@ that you are invoking. The required values depend on the value of @AuthFlow@ :
--
--
--     * For @USER_SRP_AUTH@ : @USERNAME@ (required), @SRP_A@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
--
--
--     * For @REFRESH_TOKEN_AUTH/REFRESH_TOKEN@ : @REFRESH_TOKEN@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
--
--
--     * For @ADMIN_NO_SRP_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @PASSWORD@ (required), @DEVICE_KEY@ .
--
--
--     * For @CUSTOM_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @DEVICE_KEY@ . To start the authentication flow with password verification, include @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@ .
mkAdminInitiateAuth ::
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  -- | 'authFlow'
  AuthFlowType ->
  -- | 'userPoolId'
  Lude.Text ->
  AdminInitiateAuth
mkAdminInitiateAuth pClientId_ pAuthFlow_ pUserPoolId_ =
  AdminInitiateAuth'
    { clientMetadata = Lude.Nothing,
      clientId = pClientId_,
      contextData = Lude.Nothing,
      authFlow = pAuthFlow_,
      userPoolId = pUserPoolId_,
      analyticsMetadata = Lude.Nothing,
      authParameters = Lude.Nothing
    }

-- | A map of custom key-value pairs that you can provide as input for certain custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminInitiateAuth API action, Amazon Cognito invokes the AWS Lambda functions that are specified for various triggers. The ClientMetadata value is passed as input to the functions for only the following triggers:
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
-- When Amazon Cognito invokes the functions for these triggers, it passes a JSON payload, which the function receives as input. This payload contains a @validationData@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminInitiateAuth request. In your function code in AWS Lambda, you can process the @validationData@ value to enhance your workflow for your specific needs.
-- When you use the AdminInitiateAuth API action, Amazon Cognito also invokes the functions for the following triggers, but it does not provide the ClientMetadata value as input:
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
aiaClientMetadata :: Lens.Lens' AdminInitiateAuth (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
aiaClientMetadata = Lens.lens (clientMetadata :: AdminInitiateAuth -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: AdminInitiateAuth)
{-# DEPRECATED aiaClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaClientId :: Lens.Lens' AdminInitiateAuth (Lude.Sensitive Lude.Text)
aiaClientId = Lens.lens (clientId :: AdminInitiateAuth -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: AdminInitiateAuth)
{-# DEPRECATED aiaClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'contextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaContextData :: Lens.Lens' AdminInitiateAuth (Lude.Maybe ContextDataType)
aiaContextData = Lens.lens (contextData :: AdminInitiateAuth -> Lude.Maybe ContextDataType) (\s a -> s {contextData = a} :: AdminInitiateAuth)
{-# DEPRECATED aiaContextData "Use generic-lens or generic-optics with 'contextData' instead." #-}

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
--     * @ADMIN_NO_SRP_AUTH@ : Non-SRP authentication flow; you can pass in the USERNAME and PASSWORD directly if the flow is enabled for calling the app client.
--
--
--     * @USER_PASSWORD_AUTH@ : Non-SRP authentication flow; USERNAME and PASSWORD are passed directly. If a user migration Lambda trigger is set, this flow will invoke the user migration Lambda if the USERNAME is not found in the user pool.
--
--
--     * @ADMIN_USER_PASSWORD_AUTH@ : Admin-based user password authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication flow. In this flow, Cognito receives the password in the request instead of using the SRP process to verify passwords.
--
--
--
-- /Note:/ Consider using 'authFlow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaAuthFlow :: Lens.Lens' AdminInitiateAuth AuthFlowType
aiaAuthFlow = Lens.lens (authFlow :: AdminInitiateAuth -> AuthFlowType) (\s a -> s {authFlow = a} :: AdminInitiateAuth)
{-# DEPRECATED aiaAuthFlow "Use generic-lens or generic-optics with 'authFlow' instead." #-}

-- | The ID of the Amazon Cognito user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaUserPoolId :: Lens.Lens' AdminInitiateAuth Lude.Text
aiaUserPoolId = Lens.lens (userPoolId :: AdminInitiateAuth -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminInitiateAuth)
{-# DEPRECATED aiaUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The analytics metadata for collecting Amazon Pinpoint metrics for @AdminInitiateAuth@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaAnalyticsMetadata :: Lens.Lens' AdminInitiateAuth (Lude.Maybe AnalyticsMetadataType)
aiaAnalyticsMetadata = Lens.lens (analyticsMetadata :: AdminInitiateAuth -> Lude.Maybe AnalyticsMetadataType) (\s a -> s {analyticsMetadata = a} :: AdminInitiateAuth)
{-# DEPRECATED aiaAnalyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead." #-}

-- | The authentication parameters. These are inputs corresponding to the @AuthFlow@ that you are invoking. The required values depend on the value of @AuthFlow@ :
--
--
--     * For @USER_SRP_AUTH@ : @USERNAME@ (required), @SRP_A@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
--
--
--     * For @REFRESH_TOKEN_AUTH/REFRESH_TOKEN@ : @REFRESH_TOKEN@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@ .
--
--
--     * For @ADMIN_NO_SRP_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @PASSWORD@ (required), @DEVICE_KEY@ .
--
--
--     * For @CUSTOM_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @DEVICE_KEY@ . To start the authentication flow with password verification, include @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@ .
--
--
--
-- /Note:/ Consider using 'authParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaAuthParameters :: Lens.Lens' AdminInitiateAuth (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
aiaAuthParameters = Lens.lens (authParameters :: AdminInitiateAuth -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {authParameters = a} :: AdminInitiateAuth)
{-# DEPRECATED aiaAuthParameters "Use generic-lens or generic-optics with 'authParameters' instead." #-}

instance Lude.AWSRequest AdminInitiateAuth where
  type Rs AdminInitiateAuth = AdminInitiateAuthResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          AdminInitiateAuthResponse'
            Lude.<$> (x Lude..?> "ChallengeName")
            Lude.<*> (x Lude..?> "ChallengeParameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "AuthenticationResult")
            Lude.<*> (x Lude..?> "Session")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminInitiateAuth where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminInitiateAuth" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminInitiateAuth where
  toJSON AdminInitiateAuth' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("ClientId" Lude..= clientId),
            ("ContextData" Lude..=) Lude.<$> contextData,
            Lude.Just ("AuthFlow" Lude..= authFlow),
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            ("AnalyticsMetadata" Lude..=) Lude.<$> analyticsMetadata,
            ("AuthParameters" Lude..=) Lude.<$> authParameters
          ]
      )

instance Lude.ToPath AdminInitiateAuth where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminInitiateAuth where
  toQuery = Lude.const Lude.mempty

-- | Initiates the authentication response, as an administrator.
--
-- /See:/ 'mkAdminInitiateAuthResponse' smart constructor.
data AdminInitiateAuthResponse = AdminInitiateAuthResponse'
  { -- | The name of the challenge which you are responding to with this call. This is returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge.
    --
    --
    --     * @MFA_SETUP@ : If MFA is required, users who do not have at least one of the MFA methods set up are presented with an @MFA_SETUP@ challenge. The user must set up at least one MFA type to continue to authenticate.
    --
    --
    --     * @SELECT_MFA_TYPE@ : Selects the MFA type. Valid MFA options are @SMS_MFA@ for text SMS MFA, and @SOFTWARE_TOKEN_MFA@ for TOTP software token MFA.
    --
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
    --     * @ADMIN_NO_SRP_AUTH@ : This is returned if you need to authenticate with @USERNAME@ and @PASSWORD@ directly. An app client must be enabled to use this flow.
    --
    --
    --     * @NEW_PASSWORD_REQUIRED@ : For users which are required to change their passwords after successful first login. This challenge should be passed with @NEW_PASSWORD@ and any other required attributes.
    challengeName :: Lude.Maybe ChallengeNameType,
    -- | The challenge parameters. These are returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@AdminRespondToAuthChallenge@ ).
    --
    -- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
    -- The value of the @USER_ID_FOR_SRP@ attribute will be the user's actual username, not an alias (such as email address or phone number), even if you specified an alias in your call to @AdminInitiateAuth@ . This is because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@ , the @USERNAME@ attribute cannot be an alias.
    challengeParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
    authenticationResult :: Lude.Maybe AuthenticationResultType,
    -- | The session which should be passed both ways in challenge-response calls to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @AdminRespondToAuthChallenge@ API call.
    session :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminInitiateAuthResponse' with the minimum fields required to make a request.
--
-- * 'challengeName' - The name of the challenge which you are responding to with this call. This is returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge.
--
--
--     * @MFA_SETUP@ : If MFA is required, users who do not have at least one of the MFA methods set up are presented with an @MFA_SETUP@ challenge. The user must set up at least one MFA type to continue to authenticate.
--
--
--     * @SELECT_MFA_TYPE@ : Selects the MFA type. Valid MFA options are @SMS_MFA@ for text SMS MFA, and @SOFTWARE_TOKEN_MFA@ for TOTP software token MFA.
--
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
--     * @ADMIN_NO_SRP_AUTH@ : This is returned if you need to authenticate with @USERNAME@ and @PASSWORD@ directly. An app client must be enabled to use this flow.
--
--
--     * @NEW_PASSWORD_REQUIRED@ : For users which are required to change their passwords after successful first login. This challenge should be passed with @NEW_PASSWORD@ and any other required attributes.
--
--
-- * 'challengeParameters' - The challenge parameters. These are returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@AdminRespondToAuthChallenge@ ).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
-- The value of the @USER_ID_FOR_SRP@ attribute will be the user's actual username, not an alias (such as email address or phone number), even if you specified an alias in your call to @AdminInitiateAuth@ . This is because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@ , the @USERNAME@ attribute cannot be an alias.
-- * 'authenticationResult' - The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
-- * 'session' - The session which should be passed both ways in challenge-response calls to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @AdminRespondToAuthChallenge@ API call.
-- * 'responseStatus' - The response status code.
mkAdminInitiateAuthResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminInitiateAuthResponse
mkAdminInitiateAuthResponse pResponseStatus_ =
  AdminInitiateAuthResponse'
    { challengeName = Lude.Nothing,
      challengeParameters = Lude.Nothing,
      authenticationResult = Lude.Nothing,
      session = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the challenge which you are responding to with this call. This is returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge.
--
--
--     * @MFA_SETUP@ : If MFA is required, users who do not have at least one of the MFA methods set up are presented with an @MFA_SETUP@ challenge. The user must set up at least one MFA type to continue to authenticate.
--
--
--     * @SELECT_MFA_TYPE@ : Selects the MFA type. Valid MFA options are @SMS_MFA@ for text SMS MFA, and @SOFTWARE_TOKEN_MFA@ for TOTP software token MFA.
--
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
--     * @ADMIN_NO_SRP_AUTH@ : This is returned if you need to authenticate with @USERNAME@ and @PASSWORD@ directly. An app client must be enabled to use this flow.
--
--
--     * @NEW_PASSWORD_REQUIRED@ : For users which are required to change their passwords after successful first login. This challenge should be passed with @NEW_PASSWORD@ and any other required attributes.
--
--
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarsChallengeName :: Lens.Lens' AdminInitiateAuthResponse (Lude.Maybe ChallengeNameType)
aiarsChallengeName = Lens.lens (challengeName :: AdminInitiateAuthResponse -> Lude.Maybe ChallengeNameType) (\s a -> s {challengeName = a} :: AdminInitiateAuthResponse)
{-# DEPRECATED aiarsChallengeName "Use generic-lens or generic-optics with 'challengeName' instead." #-}

-- | The challenge parameters. These are returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@AdminRespondToAuthChallenge@ ).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
-- The value of the @USER_ID_FOR_SRP@ attribute will be the user's actual username, not an alias (such as email address or phone number), even if you specified an alias in your call to @AdminInitiateAuth@ . This is because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@ , the @USERNAME@ attribute cannot be an alias.
--
-- /Note:/ Consider using 'challengeParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarsChallengeParameters :: Lens.Lens' AdminInitiateAuthResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
aiarsChallengeParameters = Lens.lens (challengeParameters :: AdminInitiateAuthResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {challengeParameters = a} :: AdminInitiateAuthResponse)
{-# DEPRECATED aiarsChallengeParameters "Use generic-lens or generic-optics with 'challengeParameters' instead." #-}

-- | The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
--
-- /Note:/ Consider using 'authenticationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarsAuthenticationResult :: Lens.Lens' AdminInitiateAuthResponse (Lude.Maybe AuthenticationResultType)
aiarsAuthenticationResult = Lens.lens (authenticationResult :: AdminInitiateAuthResponse -> Lude.Maybe AuthenticationResultType) (\s a -> s {authenticationResult = a} :: AdminInitiateAuthResponse)
{-# DEPRECATED aiarsAuthenticationResult "Use generic-lens or generic-optics with 'authenticationResult' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @AdminRespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarsSession :: Lens.Lens' AdminInitiateAuthResponse (Lude.Maybe Lude.Text)
aiarsSession = Lens.lens (session :: AdminInitiateAuthResponse -> Lude.Maybe Lude.Text) (\s a -> s {session = a} :: AdminInitiateAuthResponse)
{-# DEPRECATED aiarsSession "Use generic-lens or generic-optics with 'session' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarsResponseStatus :: Lens.Lens' AdminInitiateAuthResponse Lude.Int
aiarsResponseStatus = Lens.lens (responseStatus :: AdminInitiateAuthResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminInitiateAuthResponse)
{-# DEPRECATED aiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
