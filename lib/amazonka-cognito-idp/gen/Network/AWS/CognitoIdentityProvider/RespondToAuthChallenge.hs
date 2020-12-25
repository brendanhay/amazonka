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
    rtacClientId,
    rtacChallengeName,
    rtacAnalyticsMetadata,
    rtacChallengeResponses,
    rtacClientMetadata,
    rtacSession,
    rtacUserContextData,

    -- * Destructuring the response
    RespondToAuthChallengeResponse (..),
    mkRespondToAuthChallengeResponse,

    -- ** Response lenses
    rtacrrsAuthenticationResult,
    rtacrrsChallengeName,
    rtacrrsChallengeParameters,
    rtacrrsSession,
    rtacrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to respond to an authentication challenge.
--
-- /See:/ 'mkRespondToAuthChallenge' smart constructor.
data RespondToAuthChallenge = RespondToAuthChallenge'
  { -- | The app client ID.
    clientId :: Types.ClientId,
    -- | The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
    --
    -- @ADMIN_NO_SRP_AUTH@ is not a valid value.
    challengeName :: Types.ChallengeNameType,
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for @RespondToAuthChallenge@ calls.
    analyticsMetadata :: Core.Maybe Types.AnalyticsMetadataType,
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
    challengeResponses :: Core.Maybe (Core.HashMap Types.StringType Types.StringType),
    -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the RespondToAuthChallenge API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /post authentication/ , /pre token generation/ , /define auth challenge/ , /create auth challenge/ , and /verify auth challenge/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your RespondToAuthChallenge request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType),
    -- | The session which should be passed both ways in challenge-response calls to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
    session :: Core.Maybe Types.Session,
    -- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
    userContextData :: Core.Maybe Types.UserContextDataType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RespondToAuthChallenge' value with any optional fields omitted.
mkRespondToAuthChallenge ::
  -- | 'clientId'
  Types.ClientId ->
  -- | 'challengeName'
  Types.ChallengeNameType ->
  RespondToAuthChallenge
mkRespondToAuthChallenge clientId challengeName =
  RespondToAuthChallenge'
    { clientId,
      challengeName,
      analyticsMetadata = Core.Nothing,
      challengeResponses = Core.Nothing,
      clientMetadata = Core.Nothing,
      session = Core.Nothing,
      userContextData = Core.Nothing
    }

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacClientId :: Lens.Lens' RespondToAuthChallenge Types.ClientId
rtacClientId = Lens.field @"clientId"
{-# DEPRECATED rtacClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
--
-- @ADMIN_NO_SRP_AUTH@ is not a valid value.
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacChallengeName :: Lens.Lens' RespondToAuthChallenge Types.ChallengeNameType
rtacChallengeName = Lens.field @"challengeName"
{-# DEPRECATED rtacChallengeName "Use generic-lens or generic-optics with 'challengeName' instead." #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @RespondToAuthChallenge@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacAnalyticsMetadata :: Lens.Lens' RespondToAuthChallenge (Core.Maybe Types.AnalyticsMetadataType)
rtacAnalyticsMetadata = Lens.field @"analyticsMetadata"
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
rtacChallengeResponses :: Lens.Lens' RespondToAuthChallenge (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
rtacChallengeResponses = Lens.field @"challengeResponses"
{-# DEPRECATED rtacChallengeResponses "Use generic-lens or generic-optics with 'challengeResponses' instead." #-}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the RespondToAuthChallenge API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /post authentication/ , /pre token generation/ , /define auth challenge/ , /create auth challenge/ , and /verify auth challenge/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your RespondToAuthChallenge request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacClientMetadata :: Lens.Lens' RespondToAuthChallenge (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
rtacClientMetadata = Lens.field @"clientMetadata"
{-# DEPRECATED rtacClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacSession :: Lens.Lens' RespondToAuthChallenge (Core.Maybe Types.Session)
rtacSession = Lens.field @"session"
{-# DEPRECATED rtacSession "Use generic-lens or generic-optics with 'session' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacUserContextData :: Lens.Lens' RespondToAuthChallenge (Core.Maybe Types.UserContextDataType)
rtacUserContextData = Lens.field @"userContextData"
{-# DEPRECATED rtacUserContextData "Use generic-lens or generic-optics with 'userContextData' instead." #-}

instance Core.FromJSON RespondToAuthChallenge where
  toJSON RespondToAuthChallenge {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClientId" Core..= clientId),
            Core.Just ("ChallengeName" Core..= challengeName),
            ("AnalyticsMetadata" Core..=) Core.<$> analyticsMetadata,
            ("ChallengeResponses" Core..=) Core.<$> challengeResponses,
            ("ClientMetadata" Core..=) Core.<$> clientMetadata,
            ("Session" Core..=) Core.<$> session,
            ("UserContextData" Core..=) Core.<$> userContextData
          ]
      )

instance Core.AWSRequest RespondToAuthChallenge where
  type Rs RespondToAuthChallenge = RespondToAuthChallengeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.RespondToAuthChallenge"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RespondToAuthChallengeResponse'
            Core.<$> (x Core..:? "AuthenticationResult")
            Core.<*> (x Core..:? "ChallengeName")
            Core.<*> (x Core..:? "ChallengeParameters")
            Core.<*> (x Core..:? "Session")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The response to respond to the authentication challenge.
--
-- /See:/ 'mkRespondToAuthChallengeResponse' smart constructor.
data RespondToAuthChallengeResponse = RespondToAuthChallengeResponse'
  { -- | The result returned by the server in response to the request to respond to the authentication challenge.
    authenticationResult :: Core.Maybe Types.AuthenticationResultType,
    -- | The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
    challengeName :: Core.Maybe Types.ChallengeNameType,
    -- | The challenge parameters. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
    challengeParameters :: Core.Maybe (Core.HashMap Types.StringType Types.StringType),
    -- | The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
    session :: Core.Maybe Types.Session,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RespondToAuthChallengeResponse' value with any optional fields omitted.
mkRespondToAuthChallengeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RespondToAuthChallengeResponse
mkRespondToAuthChallengeResponse responseStatus =
  RespondToAuthChallengeResponse'
    { authenticationResult =
        Core.Nothing,
      challengeName = Core.Nothing,
      challengeParameters = Core.Nothing,
      session = Core.Nothing,
      responseStatus
    }

-- | The result returned by the server in response to the request to respond to the authentication challenge.
--
-- /Note:/ Consider using 'authenticationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrrsAuthenticationResult :: Lens.Lens' RespondToAuthChallengeResponse (Core.Maybe Types.AuthenticationResultType)
rtacrrsAuthenticationResult = Lens.field @"authenticationResult"
{-# DEPRECATED rtacrrsAuthenticationResult "Use generic-lens or generic-optics with 'authenticationResult' instead." #-}

-- | The challenge name. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrrsChallengeName :: Lens.Lens' RespondToAuthChallengeResponse (Core.Maybe Types.ChallengeNameType)
rtacrrsChallengeName = Lens.field @"challengeName"
{-# DEPRECATED rtacrrsChallengeName "Use generic-lens or generic-optics with 'challengeName' instead." #-}

-- | The challenge parameters. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_InitiateAuth.html InitiateAuth> .
--
-- /Note:/ Consider using 'challengeParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrrsChallengeParameters :: Lens.Lens' RespondToAuthChallengeResponse (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
rtacrrsChallengeParameters = Lens.field @"challengeParameters"
{-# DEPRECATED rtacrrsChallengeParameters "Use generic-lens or generic-optics with 'challengeParameters' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. If the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrrsSession :: Lens.Lens' RespondToAuthChallengeResponse (Core.Maybe Types.Session)
rtacrrsSession = Lens.field @"session"
{-# DEPRECATED rtacrrsSession "Use generic-lens or generic-optics with 'session' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrrsResponseStatus :: Lens.Lens' RespondToAuthChallengeResponse Core.Int
rtacrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtacrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
