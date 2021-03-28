{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AdminInitiateAuth (..)
    , mkAdminInitiateAuth
    -- ** Request lenses
    , aiaUserPoolId
    , aiaClientId
    , aiaAuthFlow
    , aiaAnalyticsMetadata
    , aiaAuthParameters
    , aiaClientMetadata
    , aiaContextData

    -- * Destructuring the response
    , AdminInitiateAuthResponse (..)
    , mkAdminInitiateAuthResponse
    -- ** Response lenses
    , aiarrsAuthenticationResult
    , aiarrsChallengeName
    , aiarrsChallengeParameters
    , aiarrsSession
    , aiarrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Initiates the authorization request, as an administrator.
--
-- /See:/ 'mkAdminInitiateAuth' smart constructor.
data AdminInitiateAuth = AdminInitiateAuth'
  { userPoolId :: Types.UserPoolId
    -- ^ The ID of the Amazon Cognito user pool.
  , clientId :: Types.ClientIdType
    -- ^ The app client ID.
  , authFlow :: Types.AuthFlowType
    -- ^ The authentication flow for this call to execute. The API action will depend on this value. For example:
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
  , analyticsMetadata :: Core.Maybe Types.AnalyticsMetadataType
    -- ^ The analytics metadata for collecting Amazon Pinpoint metrics for @AdminInitiateAuth@ calls.
  , authParameters :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ The authentication parameters. These are inputs corresponding to the @AuthFlow@ that you are invoking. The required values depend on the value of @AuthFlow@ :
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
  , clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ A map of custom key-value pairs that you can provide as input for certain custom workflows that this action triggers.
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
  , contextData :: Core.Maybe Types.ContextDataType
    -- ^ Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminInitiateAuth' value with any optional fields omitted.
mkAdminInitiateAuth
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.ClientIdType -- ^ 'clientId'
    -> Types.AuthFlowType -- ^ 'authFlow'
    -> AdminInitiateAuth
mkAdminInitiateAuth userPoolId clientId authFlow
  = AdminInitiateAuth'{userPoolId, clientId, authFlow,
                       analyticsMetadata = Core.Nothing, authParameters = Core.Nothing,
                       clientMetadata = Core.Nothing, contextData = Core.Nothing}

-- | The ID of the Amazon Cognito user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaUserPoolId :: Lens.Lens' AdminInitiateAuth Types.UserPoolId
aiaUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE aiaUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaClientId :: Lens.Lens' AdminInitiateAuth Types.ClientIdType
aiaClientId = Lens.field @"clientId"
{-# INLINEABLE aiaClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

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
aiaAuthFlow :: Lens.Lens' AdminInitiateAuth Types.AuthFlowType
aiaAuthFlow = Lens.field @"authFlow"
{-# INLINEABLE aiaAuthFlow #-}
{-# DEPRECATED authFlow "Use generic-lens or generic-optics with 'authFlow' instead"  #-}

-- | The analytics metadata for collecting Amazon Pinpoint metrics for @AdminInitiateAuth@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaAnalyticsMetadata :: Lens.Lens' AdminInitiateAuth (Core.Maybe Types.AnalyticsMetadataType)
aiaAnalyticsMetadata = Lens.field @"analyticsMetadata"
{-# INLINEABLE aiaAnalyticsMetadata #-}
{-# DEPRECATED analyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead"  #-}

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
aiaAuthParameters :: Lens.Lens' AdminInitiateAuth (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
aiaAuthParameters = Lens.field @"authParameters"
{-# INLINEABLE aiaAuthParameters #-}
{-# DEPRECATED authParameters "Use generic-lens or generic-optics with 'authParameters' instead"  #-}

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
aiaClientMetadata :: Lens.Lens' AdminInitiateAuth (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
aiaClientMetadata = Lens.field @"clientMetadata"
{-# INLINEABLE aiaClientMetadata #-}
{-# DEPRECATED clientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead"  #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'contextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaContextData :: Lens.Lens' AdminInitiateAuth (Core.Maybe Types.ContextDataType)
aiaContextData = Lens.field @"contextData"
{-# INLINEABLE aiaContextData #-}
{-# DEPRECATED contextData "Use generic-lens or generic-optics with 'contextData' instead"  #-}

instance Core.ToQuery AdminInitiateAuth where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminInitiateAuth where
        toHeaders AdminInitiateAuth{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminInitiateAuth")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminInitiateAuth where
        toJSON AdminInitiateAuth{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("ClientId" Core..= clientId),
                  Core.Just ("AuthFlow" Core..= authFlow),
                  ("AnalyticsMetadata" Core..=) Core.<$> analyticsMetadata,
                  ("AuthParameters" Core..=) Core.<$> authParameters,
                  ("ClientMetadata" Core..=) Core.<$> clientMetadata,
                  ("ContextData" Core..=) Core.<$> contextData])

instance Core.AWSRequest AdminInitiateAuth where
        type Rs AdminInitiateAuth = AdminInitiateAuthResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AdminInitiateAuthResponse' Core.<$>
                   (x Core..:? "AuthenticationResult") Core.<*>
                     x Core..:? "ChallengeName"
                     Core.<*> x Core..:? "ChallengeParameters"
                     Core.<*> x Core..:? "Session"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Initiates the authentication response, as an administrator.
--
-- /See:/ 'mkAdminInitiateAuthResponse' smart constructor.
data AdminInitiateAuthResponse = AdminInitiateAuthResponse'
  { authenticationResult :: Core.Maybe Types.AuthenticationResultType
    -- ^ The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
  , challengeName :: Core.Maybe Types.ChallengeNameType
    -- ^ The name of the challenge which you are responding to with this call. This is returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge.
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
  , challengeParameters :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ The challenge parameters. These are returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@AdminRespondToAuthChallenge@ ).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
-- The value of the @USER_ID_FOR_SRP@ attribute will be the user's actual username, not an alias (such as email address or phone number), even if you specified an alias in your call to @AdminInitiateAuth@ . This is because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@ , the @USERNAME@ attribute cannot be an alias.
  , session :: Core.Maybe Types.Session
    -- ^ The session which should be passed both ways in challenge-response calls to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @AdminRespondToAuthChallenge@ API call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminInitiateAuthResponse' value with any optional fields omitted.
mkAdminInitiateAuthResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminInitiateAuthResponse
mkAdminInitiateAuthResponse responseStatus
  = AdminInitiateAuthResponse'{authenticationResult = Core.Nothing,
                               challengeName = Core.Nothing, challengeParameters = Core.Nothing,
                               session = Core.Nothing, responseStatus}

-- | The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
--
-- /Note:/ Consider using 'authenticationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarrsAuthenticationResult :: Lens.Lens' AdminInitiateAuthResponse (Core.Maybe Types.AuthenticationResultType)
aiarrsAuthenticationResult = Lens.field @"authenticationResult"
{-# INLINEABLE aiarrsAuthenticationResult #-}
{-# DEPRECATED authenticationResult "Use generic-lens or generic-optics with 'authenticationResult' instead"  #-}

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
aiarrsChallengeName :: Lens.Lens' AdminInitiateAuthResponse (Core.Maybe Types.ChallengeNameType)
aiarrsChallengeName = Lens.field @"challengeName"
{-# INLINEABLE aiarrsChallengeName #-}
{-# DEPRECATED challengeName "Use generic-lens or generic-optics with 'challengeName' instead"  #-}

-- | The challenge parameters. These are returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@AdminRespondToAuthChallenge@ ).
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
-- The value of the @USER_ID_FOR_SRP@ attribute will be the user's actual username, not an alias (such as email address or phone number), even if you specified an alias in your call to @AdminInitiateAuth@ . This is because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@ , the @USERNAME@ attribute cannot be an alias.
--
-- /Note:/ Consider using 'challengeParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarrsChallengeParameters :: Lens.Lens' AdminInitiateAuthResponse (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
aiarrsChallengeParameters = Lens.field @"challengeParameters"
{-# INLINEABLE aiarrsChallengeParameters #-}
{-# DEPRECATED challengeParameters "Use generic-lens or generic-optics with 'challengeParameters' instead"  #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @AdminRespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarrsSession :: Lens.Lens' AdminInitiateAuthResponse (Core.Maybe Types.Session)
aiarrsSession = Lens.field @"session"
{-# INLINEABLE aiarrsSession #-}
{-# DEPRECATED session "Use generic-lens or generic-optics with 'session' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiarrsResponseStatus :: Lens.Lens' AdminInitiateAuthResponse Core.Int
aiarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aiarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
