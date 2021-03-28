{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      InitiateAuth (..)
    , mkInitiateAuth
    -- ** Request lenses
    , iaAuthFlow
    , iaClientId
    , iaAnalyticsMetadata
    , iaAuthParameters
    , iaClientMetadata
    , iaUserContextData

    -- * Destructuring the response
    , InitiateAuthResponse (..)
    , mkInitiateAuthResponse
    -- ** Response lenses
    , iarrsAuthenticationResult
    , iarrsChallengeName
    , iarrsChallengeParameters
    , iarrsSession
    , iarrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Initiates the authentication request.
--
-- /See:/ 'mkInitiateAuth' smart constructor.
data InitiateAuth = InitiateAuth'
  { authFlow :: Types.AuthFlowType
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
--     * @USER_PASSWORD_AUTH@ : Non-SRP authentication flow; USERNAME and PASSWORD are passed directly. If a user migration Lambda trigger is set, this flow will invoke the user migration Lambda if the USERNAME is not found in the user pool. 
--
--
--     * @ADMIN_USER_PASSWORD_AUTH@ : Admin-based user password authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication flow. In this flow, Cognito receives the password in the request instead of using the SRP process to verify passwords.
--
--
-- @ADMIN_NO_SRP_AUTH@ is not a valid value.
  , clientId :: Types.ClientId
    -- ^ The app client ID.
  , analyticsMetadata :: Core.Maybe Types.AnalyticsMetadataType
    -- ^ The Amazon Pinpoint analytics metadata for collecting metrics for @InitiateAuth@ calls.
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
--     * For @CUSTOM_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @DEVICE_KEY@ . To start the authentication flow with password verification, include @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@ .
--
--
  , clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ A map of custom key-value pairs that you can provide as input for certain custom workflows that this action triggers.
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
  , userContextData :: Core.Maybe Types.UserContextDataType
    -- ^ Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateAuth' value with any optional fields omitted.
mkInitiateAuth
    :: Types.AuthFlowType -- ^ 'authFlow'
    -> Types.ClientId -- ^ 'clientId'
    -> InitiateAuth
mkInitiateAuth authFlow clientId
  = InitiateAuth'{authFlow, clientId,
                  analyticsMetadata = Core.Nothing, authParameters = Core.Nothing,
                  clientMetadata = Core.Nothing, userContextData = Core.Nothing}

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
iaAuthFlow :: Lens.Lens' InitiateAuth Types.AuthFlowType
iaAuthFlow = Lens.field @"authFlow"
{-# INLINEABLE iaAuthFlow #-}
{-# DEPRECATED authFlow "Use generic-lens or generic-optics with 'authFlow' instead"  #-}

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaClientId :: Lens.Lens' InitiateAuth Types.ClientId
iaClientId = Lens.field @"clientId"
{-# INLINEABLE iaClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @InitiateAuth@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAnalyticsMetadata :: Lens.Lens' InitiateAuth (Core.Maybe Types.AnalyticsMetadataType)
iaAnalyticsMetadata = Lens.field @"analyticsMetadata"
{-# INLINEABLE iaAnalyticsMetadata #-}
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
--     * For @CUSTOM_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @DEVICE_KEY@ . To start the authentication flow with password verification, include @ChallengeName: SRP_A@ and @SRP_A: (The SRP_A Value)@ .
--
--
--
-- /Note:/ Consider using 'authParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAuthParameters :: Lens.Lens' InitiateAuth (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
iaAuthParameters = Lens.field @"authParameters"
{-# INLINEABLE iaAuthParameters #-}
{-# DEPRECATED authParameters "Use generic-lens or generic-optics with 'authParameters' instead"  #-}

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
iaClientMetadata :: Lens.Lens' InitiateAuth (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
iaClientMetadata = Lens.field @"clientMetadata"
{-# INLINEABLE iaClientMetadata #-}
{-# DEPRECATED clientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead"  #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaUserContextData :: Lens.Lens' InitiateAuth (Core.Maybe Types.UserContextDataType)
iaUserContextData = Lens.field @"userContextData"
{-# INLINEABLE iaUserContextData #-}
{-# DEPRECATED userContextData "Use generic-lens or generic-optics with 'userContextData' instead"  #-}

instance Core.ToQuery InitiateAuth where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders InitiateAuth where
        toHeaders InitiateAuth{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityProviderService.InitiateAuth")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON InitiateAuth where
        toJSON InitiateAuth{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AuthFlow" Core..= authFlow),
                  Core.Just ("ClientId" Core..= clientId),
                  ("AnalyticsMetadata" Core..=) Core.<$> analyticsMetadata,
                  ("AuthParameters" Core..=) Core.<$> authParameters,
                  ("ClientMetadata" Core..=) Core.<$> clientMetadata,
                  ("UserContextData" Core..=) Core.<$> userContextData])

instance Core.AWSRequest InitiateAuth where
        type Rs InitiateAuth = InitiateAuthResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 InitiateAuthResponse' Core.<$>
                   (x Core..:? "AuthenticationResult") Core.<*>
                     x Core..:? "ChallengeName"
                     Core.<*> x Core..:? "ChallengeParameters"
                     Core.<*> x Core..:? "Session"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Initiates the authentication response.
--
-- /See:/ 'mkInitiateAuthResponse' smart constructor.
data InitiateAuthResponse = InitiateAuthResponse'
  { authenticationResult :: Core.Maybe Types.AuthenticationResultType
    -- ^ The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
  , challengeName :: Core.Maybe Types.ChallengeNameType
    -- ^ The name of the challenge which you are responding to with this call. This is returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge.
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
  , challengeParameters :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ The challenge parameters. These are returned to you in the @InitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@RespondToAuthChallenge@ ). 
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
  , session :: Core.Maybe Types.Session
    -- ^ The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateAuthResponse' value with any optional fields omitted.
mkInitiateAuthResponse
    :: Core.Int -- ^ 'responseStatus'
    -> InitiateAuthResponse
mkInitiateAuthResponse responseStatus
  = InitiateAuthResponse'{authenticationResult = Core.Nothing,
                          challengeName = Core.Nothing, challengeParameters = Core.Nothing,
                          session = Core.Nothing, responseStatus}

-- | The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
--
-- /Note:/ Consider using 'authenticationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iarrsAuthenticationResult :: Lens.Lens' InitiateAuthResponse (Core.Maybe Types.AuthenticationResultType)
iarrsAuthenticationResult = Lens.field @"authenticationResult"
{-# INLINEABLE iarrsAuthenticationResult #-}
{-# DEPRECATED authenticationResult "Use generic-lens or generic-optics with 'authenticationResult' instead"  #-}

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
iarrsChallengeName :: Lens.Lens' InitiateAuthResponse (Core.Maybe Types.ChallengeNameType)
iarrsChallengeName = Lens.field @"challengeName"
{-# INLINEABLE iarrsChallengeName #-}
{-# DEPRECATED challengeName "Use generic-lens or generic-optics with 'challengeName' instead"  #-}

-- | The challenge parameters. These are returned to you in the @InitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@RespondToAuthChallenge@ ). 
--
-- All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable).
--
-- /Note:/ Consider using 'challengeParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iarrsChallengeParameters :: Lens.Lens' InitiateAuthResponse (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
iarrsChallengeParameters = Lens.field @"challengeParameters"
{-# INLINEABLE iarrsChallengeParameters #-}
{-# DEPRECATED challengeParameters "Use generic-lens or generic-optics with 'challengeParameters' instead"  #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iarrsSession :: Lens.Lens' InitiateAuthResponse (Core.Maybe Types.Session)
iarrsSession = Lens.field @"session"
{-# INLINEABLE iarrsSession #-}
{-# DEPRECATED session "Use generic-lens or generic-optics with 'session' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iarrsResponseStatus :: Lens.Lens' InitiateAuthResponse Core.Int
iarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE iarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
