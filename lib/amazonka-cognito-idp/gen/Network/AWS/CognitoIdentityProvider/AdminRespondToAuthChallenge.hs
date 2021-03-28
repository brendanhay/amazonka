{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AdminRespondToAuthChallenge (..)
    , mkAdminRespondToAuthChallenge
    -- ** Request lenses
    , artacUserPoolId
    , artacClientId
    , artacChallengeName
    , artacAnalyticsMetadata
    , artacChallengeResponses
    , artacClientMetadata
    , artacContextData
    , artacSession

    -- * Destructuring the response
    , AdminRespondToAuthChallengeResponse (..)
    , mkAdminRespondToAuthChallengeResponse
    -- ** Response lenses
    , artacrrsAuthenticationResult
    , artacrrsChallengeName
    , artacrrsChallengeParameters
    , artacrrsSession
    , artacrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to respond to the authentication challenge, as an administrator.
--
-- /See:/ 'mkAdminRespondToAuthChallenge' smart constructor.
data AdminRespondToAuthChallenge = AdminRespondToAuthChallenge'
  { userPoolId :: Types.UserPoolId
    -- ^ The ID of the Amazon Cognito user pool.
  , clientId :: Types.ClientIdType
    -- ^ The app client ID.
  , challengeName :: Types.ChallengeNameType
    -- ^ The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
  , analyticsMetadata :: Core.Maybe Types.AnalyticsMetadataType
    -- ^ The analytics metadata for collecting Amazon Pinpoint metrics for @AdminRespondToAuthChallenge@ calls.
  , challengeResponses :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ The challenge responses. These are inputs corresponding to the value of @ChallengeName@ , for example:
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
  , clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminRespondToAuthChallenge API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /pre sign-up/ , /custom message/ , /post authentication/ , /user migration/ , /pre token generation/ , /define auth challenge/ , /create auth challenge/ , and /verify auth challenge response/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminRespondToAuthChallenge request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
  , contextData :: Core.Maybe Types.ContextDataType
    -- ^ Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
  , session :: Core.Maybe Types.SessionType
    -- ^ The session which should be passed both ways in challenge-response calls to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminRespondToAuthChallenge' value with any optional fields omitted.
mkAdminRespondToAuthChallenge
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.ClientIdType -- ^ 'clientId'
    -> Types.ChallengeNameType -- ^ 'challengeName'
    -> AdminRespondToAuthChallenge
mkAdminRespondToAuthChallenge userPoolId clientId challengeName
  = AdminRespondToAuthChallenge'{userPoolId, clientId, challengeName,
                                 analyticsMetadata = Core.Nothing,
                                 challengeResponses = Core.Nothing, clientMetadata = Core.Nothing,
                                 contextData = Core.Nothing, session = Core.Nothing}

-- | The ID of the Amazon Cognito user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacUserPoolId :: Lens.Lens' AdminRespondToAuthChallenge Types.UserPoolId
artacUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE artacUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacClientId :: Lens.Lens' AdminRespondToAuthChallenge Types.ClientIdType
artacClientId = Lens.field @"clientId"
{-# INLINEABLE artacClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacChallengeName :: Lens.Lens' AdminRespondToAuthChallenge Types.ChallengeNameType
artacChallengeName = Lens.field @"challengeName"
{-# INLINEABLE artacChallengeName #-}
{-# DEPRECATED challengeName "Use generic-lens or generic-optics with 'challengeName' instead"  #-}

-- | The analytics metadata for collecting Amazon Pinpoint metrics for @AdminRespondToAuthChallenge@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacAnalyticsMetadata :: Lens.Lens' AdminRespondToAuthChallenge (Core.Maybe Types.AnalyticsMetadataType)
artacAnalyticsMetadata = Lens.field @"analyticsMetadata"
{-# INLINEABLE artacAnalyticsMetadata #-}
{-# DEPRECATED analyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead"  #-}

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
artacChallengeResponses :: Lens.Lens' AdminRespondToAuthChallenge (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
artacChallengeResponses = Lens.field @"challengeResponses"
{-# INLINEABLE artacChallengeResponses #-}
{-# DEPRECATED challengeResponses "Use generic-lens or generic-optics with 'challengeResponses' instead"  #-}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminRespondToAuthChallenge API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /pre sign-up/ , /custom message/ , /post authentication/ , /user migration/ , /pre token generation/ , /define auth challenge/ , /create auth challenge/ , and /verify auth challenge response/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminRespondToAuthChallenge request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacClientMetadata :: Lens.Lens' AdminRespondToAuthChallenge (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
artacClientMetadata = Lens.field @"clientMetadata"
{-# INLINEABLE artacClientMetadata #-}
{-# DEPRECATED clientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead"  #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'contextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacContextData :: Lens.Lens' AdminRespondToAuthChallenge (Core.Maybe Types.ContextDataType)
artacContextData = Lens.field @"contextData"
{-# INLINEABLE artacContextData #-}
{-# DEPRECATED contextData "Use generic-lens or generic-optics with 'contextData' instead"  #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacSession :: Lens.Lens' AdminRespondToAuthChallenge (Core.Maybe Types.SessionType)
artacSession = Lens.field @"session"
{-# INLINEABLE artacSession #-}
{-# DEPRECATED session "Use generic-lens or generic-optics with 'session' instead"  #-}

instance Core.ToQuery AdminRespondToAuthChallenge where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminRespondToAuthChallenge where
        toHeaders AdminRespondToAuthChallenge{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminRespondToAuthChallenge")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminRespondToAuthChallenge where
        toJSON AdminRespondToAuthChallenge{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("ClientId" Core..= clientId),
                  Core.Just ("ChallengeName" Core..= challengeName),
                  ("AnalyticsMetadata" Core..=) Core.<$> analyticsMetadata,
                  ("ChallengeResponses" Core..=) Core.<$> challengeResponses,
                  ("ClientMetadata" Core..=) Core.<$> clientMetadata,
                  ("ContextData" Core..=) Core.<$> contextData,
                  ("Session" Core..=) Core.<$> session])

instance Core.AWSRequest AdminRespondToAuthChallenge where
        type Rs AdminRespondToAuthChallenge =
             AdminRespondToAuthChallengeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AdminRespondToAuthChallengeResponse' Core.<$>
                   (x Core..:? "AuthenticationResult") Core.<*>
                     x Core..:? "ChallengeName"
                     Core.<*> x Core..:? "ChallengeParameters"
                     Core.<*> x Core..:? "Session"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Responds to the authentication challenge, as an administrator.
--
-- /See:/ 'mkAdminRespondToAuthChallengeResponse' smart constructor.
data AdminRespondToAuthChallengeResponse = AdminRespondToAuthChallengeResponse'
  { authenticationResult :: Core.Maybe Types.AuthenticationResultType
    -- ^ The result returned by the server in response to the authentication request.
  , challengeName :: Core.Maybe Types.ChallengeNameType
    -- ^ The name of the challenge. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
  , challengeParameters :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ The challenge parameters. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
  , session :: Core.Maybe Types.Session
    -- ^ The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminRespondToAuthChallengeResponse' value with any optional fields omitted.
mkAdminRespondToAuthChallengeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminRespondToAuthChallengeResponse
mkAdminRespondToAuthChallengeResponse responseStatus
  = AdminRespondToAuthChallengeResponse'{authenticationResult =
                                           Core.Nothing,
                                         challengeName = Core.Nothing,
                                         challengeParameters = Core.Nothing, session = Core.Nothing,
                                         responseStatus}

-- | The result returned by the server in response to the authentication request.
--
-- /Note:/ Consider using 'authenticationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacrrsAuthenticationResult :: Lens.Lens' AdminRespondToAuthChallengeResponse (Core.Maybe Types.AuthenticationResultType)
artacrrsAuthenticationResult = Lens.field @"authenticationResult"
{-# INLINEABLE artacrrsAuthenticationResult #-}
{-# DEPRECATED authenticationResult "Use generic-lens or generic-optics with 'authenticationResult' instead"  #-}

-- | The name of the challenge. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacrrsChallengeName :: Lens.Lens' AdminRespondToAuthChallengeResponse (Core.Maybe Types.ChallengeNameType)
artacrrsChallengeName = Lens.field @"challengeName"
{-# INLINEABLE artacrrsChallengeName #-}
{-# DEPRECATED challengeName "Use generic-lens or generic-optics with 'challengeName' instead"  #-}

-- | The challenge parameters. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminInitiateAuth.html AdminInitiateAuth> .
--
-- /Note:/ Consider using 'challengeParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacrrsChallengeParameters :: Lens.Lens' AdminRespondToAuthChallengeResponse (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
artacrrsChallengeParameters = Lens.field @"challengeParameters"
{-# INLINEABLE artacrrsChallengeParameters #-}
{-# DEPRECATED challengeParameters "Use generic-lens or generic-optics with 'challengeParameters' instead"  #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacrrsSession :: Lens.Lens' AdminRespondToAuthChallengeResponse (Core.Maybe Types.Session)
artacrrsSession = Lens.field @"session"
{-# INLINEABLE artacrrsSession #-}
{-# DEPRECATED session "Use generic-lens or generic-optics with 'session' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artacrrsResponseStatus :: Lens.Lens' AdminRespondToAuthChallengeResponse Core.Int
artacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE artacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
