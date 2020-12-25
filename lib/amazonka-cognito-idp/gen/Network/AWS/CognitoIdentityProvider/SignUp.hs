{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.SignUp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the user in the specified user pool and creates a user name, password, and user attributes.
module Network.AWS.CognitoIdentityProvider.SignUp
  ( -- * Creating a request
    SignUp (..),
    mkSignUp,

    -- ** Request lenses
    suClientId,
    suUsername,
    suPassword,
    suAnalyticsMetadata,
    suClientMetadata,
    suSecretHash,
    suUserAttributes,
    suUserContextData,
    suValidationData,

    -- * Destructuring the response
    SignUpResponse (..),
    mkSignUpResponse,

    -- ** Response lenses
    surrsUserConfirmed,
    surrsUserSub,
    surrsCodeDeliveryDetails,
    surrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to register a user.
--
-- /See:/ 'mkSignUp' smart constructor.
data SignUp = SignUp'
  { -- | The ID of the client associated with the user pool.
    clientId :: Types.ClientId,
    -- | The user name of the user you wish to register.
    username :: Types.Username,
    -- | The password of the user you wish to register.
    password :: Types.PasswordType,
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for @SignUp@ calls.
    analyticsMetadata :: Core.Maybe Types.AnalyticsMetadataType,
    -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the SignUp API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /pre sign-up/ , /custom message/ , and /post confirmation/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your SignUp request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType),
    -- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
    secretHash :: Core.Maybe Types.SecretHashType,
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
    userAttributes :: Core.Maybe [Types.AttributeType],
    -- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
    userContextData :: Core.Maybe Types.UserContextDataType,
    -- | The validation data in the request to register a user.
    validationData :: Core.Maybe [Types.AttributeType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SignUp' value with any optional fields omitted.
mkSignUp ::
  -- | 'clientId'
  Types.ClientId ->
  -- | 'username'
  Types.Username ->
  -- | 'password'
  Types.PasswordType ->
  SignUp
mkSignUp clientId username password =
  SignUp'
    { clientId,
      username,
      password,
      analyticsMetadata = Core.Nothing,
      clientMetadata = Core.Nothing,
      secretHash = Core.Nothing,
      userAttributes = Core.Nothing,
      userContextData = Core.Nothing,
      validationData = Core.Nothing
    }

-- | The ID of the client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suClientId :: Lens.Lens' SignUp Types.ClientId
suClientId = Lens.field @"clientId"
{-# DEPRECATED suClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The user name of the user you wish to register.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suUsername :: Lens.Lens' SignUp Types.Username
suUsername = Lens.field @"username"
{-# DEPRECATED suUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The password of the user you wish to register.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suPassword :: Lens.Lens' SignUp Types.PasswordType
suPassword = Lens.field @"password"
{-# DEPRECATED suPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @SignUp@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suAnalyticsMetadata :: Lens.Lens' SignUp (Core.Maybe Types.AnalyticsMetadataType)
suAnalyticsMetadata = Lens.field @"analyticsMetadata"
{-# DEPRECATED suAnalyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead." #-}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the SignUp API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /pre sign-up/ , /custom message/ , and /post confirmation/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your SignUp request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suClientMetadata :: Lens.Lens' SignUp (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
suClientMetadata = Lens.field @"clientMetadata"
{-# DEPRECATED suClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- /Note:/ Consider using 'secretHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suSecretHash :: Lens.Lens' SignUp (Core.Maybe Types.SecretHashType)
suSecretHash = Lens.field @"secretHash"
{-# DEPRECATED suSecretHash "Use generic-lens or generic-optics with 'secretHash' instead." #-}

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suUserAttributes :: Lens.Lens' SignUp (Core.Maybe [Types.AttributeType])
suUserAttributes = Lens.field @"userAttributes"
{-# DEPRECATED suUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suUserContextData :: Lens.Lens' SignUp (Core.Maybe Types.UserContextDataType)
suUserContextData = Lens.field @"userContextData"
{-# DEPRECATED suUserContextData "Use generic-lens or generic-optics with 'userContextData' instead." #-}

-- | The validation data in the request to register a user.
--
-- /Note:/ Consider using 'validationData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suValidationData :: Lens.Lens' SignUp (Core.Maybe [Types.AttributeType])
suValidationData = Lens.field @"validationData"
{-# DEPRECATED suValidationData "Use generic-lens or generic-optics with 'validationData' instead." #-}

instance Core.FromJSON SignUp where
  toJSON SignUp {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClientId" Core..= clientId),
            Core.Just ("Username" Core..= username),
            Core.Just ("Password" Core..= password),
            ("AnalyticsMetadata" Core..=) Core.<$> analyticsMetadata,
            ("ClientMetadata" Core..=) Core.<$> clientMetadata,
            ("SecretHash" Core..=) Core.<$> secretHash,
            ("UserAttributes" Core..=) Core.<$> userAttributes,
            ("UserContextData" Core..=) Core.<$> userContextData,
            ("ValidationData" Core..=) Core.<$> validationData
          ]
      )

instance Core.AWSRequest SignUp where
  type Rs SignUp = SignUpResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityProviderService.SignUp")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SignUpResponse'
            Core.<$> (x Core..: "UserConfirmed")
            Core.<*> (x Core..: "UserSub")
            Core.<*> (x Core..:? "CodeDeliveryDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The response from the server for a registration request.
--
-- /See:/ 'mkSignUpResponse' smart constructor.
data SignUpResponse = SignUpResponse'
  { -- | A response from the server indicating that a user registration has been confirmed.
    userConfirmed :: Core.Bool,
    -- | The UUID of the authenticated user. This is not the same as @username@ .
    userSub :: Types.StringType,
    -- | The code delivery details returned by the server response to the user registration request.
    codeDeliveryDetails :: Core.Maybe Types.CodeDeliveryDetailsType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SignUpResponse' value with any optional fields omitted.
mkSignUpResponse ::
  -- | 'userConfirmed'
  Core.Bool ->
  -- | 'userSub'
  Types.StringType ->
  -- | 'responseStatus'
  Core.Int ->
  SignUpResponse
mkSignUpResponse userConfirmed userSub responseStatus =
  SignUpResponse'
    { userConfirmed,
      userSub,
      codeDeliveryDetails = Core.Nothing,
      responseStatus
    }

-- | A response from the server indicating that a user registration has been confirmed.
--
-- /Note:/ Consider using 'userConfirmed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
surrsUserConfirmed :: Lens.Lens' SignUpResponse Core.Bool
surrsUserConfirmed = Lens.field @"userConfirmed"
{-# DEPRECATED surrsUserConfirmed "Use generic-lens or generic-optics with 'userConfirmed' instead." #-}

-- | The UUID of the authenticated user. This is not the same as @username@ .
--
-- /Note:/ Consider using 'userSub' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
surrsUserSub :: Lens.Lens' SignUpResponse Types.StringType
surrsUserSub = Lens.field @"userSub"
{-# DEPRECATED surrsUserSub "Use generic-lens or generic-optics with 'userSub' instead." #-}

-- | The code delivery details returned by the server response to the user registration request.
--
-- /Note:/ Consider using 'codeDeliveryDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
surrsCodeDeliveryDetails :: Lens.Lens' SignUpResponse (Core.Maybe Types.CodeDeliveryDetailsType)
surrsCodeDeliveryDetails = Lens.field @"codeDeliveryDetails"
{-# DEPRECATED surrsCodeDeliveryDetails "Use generic-lens or generic-optics with 'codeDeliveryDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
surrsResponseStatus :: Lens.Lens' SignUpResponse Core.Int
surrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED surrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
