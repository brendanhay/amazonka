{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to enter a confirmation code to reset a forgotten password.
module Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
  ( -- * Creating a request
    ConfirmForgotPassword (..),
    mkConfirmForgotPassword,

    -- ** Request lenses
    cfpClientId,
    cfpUsername,
    cfpConfirmationCode,
    cfpPassword,
    cfpAnalyticsMetadata,
    cfpClientMetadata,
    cfpSecretHash,
    cfpUserContextData,

    -- * Destructuring the response
    ConfirmForgotPasswordResponse (..),
    mkConfirmForgotPasswordResponse,

    -- ** Response lenses
    cfprrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request representing the confirmation for a password reset.
--
-- /See:/ 'mkConfirmForgotPassword' smart constructor.
data ConfirmForgotPassword = ConfirmForgotPassword'
  { -- | The app client ID of the app associated with the user pool.
    clientId :: Types.ClientIdType,
    -- | The user name of the user for whom you want to enter a code to retrieve a forgotten password.
    username :: Types.Username,
    -- | The confirmation code sent by a user's request to retrieve a forgotten password. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword> .
    confirmationCode :: Types.ConfirmationCodeType,
    -- | The password sent by a user's request to retrieve a forgotten password.
    password :: Types.PasswordType,
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmForgotPassword@ calls.
    analyticsMetadata :: Core.Maybe Types.AnalyticsMetadataType,
    -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ConfirmForgotPassword API action, Amazon Cognito invokes the function that is assigned to the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ConfirmForgotPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType),
    -- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
    secretHash :: Core.Maybe Types.SecretHashType,
    -- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
    userContextData :: Core.Maybe Types.UserContextDataType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmForgotPassword' value with any optional fields omitted.
mkConfirmForgotPassword ::
  -- | 'clientId'
  Types.ClientIdType ->
  -- | 'username'
  Types.Username ->
  -- | 'confirmationCode'
  Types.ConfirmationCodeType ->
  -- | 'password'
  Types.PasswordType ->
  ConfirmForgotPassword
mkConfirmForgotPassword clientId username confirmationCode password =
  ConfirmForgotPassword'
    { clientId,
      username,
      confirmationCode,
      password,
      analyticsMetadata = Core.Nothing,
      clientMetadata = Core.Nothing,
      secretHash = Core.Nothing,
      userContextData = Core.Nothing
    }

-- | The app client ID of the app associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpClientId :: Lens.Lens' ConfirmForgotPassword Types.ClientIdType
cfpClientId = Lens.field @"clientId"
{-# DEPRECATED cfpClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The user name of the user for whom you want to enter a code to retrieve a forgotten password.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpUsername :: Lens.Lens' ConfirmForgotPassword Types.Username
cfpUsername = Lens.field @"username"
{-# DEPRECATED cfpUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The confirmation code sent by a user's request to retrieve a forgotten password. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword> .
--
-- /Note:/ Consider using 'confirmationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpConfirmationCode :: Lens.Lens' ConfirmForgotPassword Types.ConfirmationCodeType
cfpConfirmationCode = Lens.field @"confirmationCode"
{-# DEPRECATED cfpConfirmationCode "Use generic-lens or generic-optics with 'confirmationCode' instead." #-}

-- | The password sent by a user's request to retrieve a forgotten password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpPassword :: Lens.Lens' ConfirmForgotPassword Types.PasswordType
cfpPassword = Lens.field @"password"
{-# DEPRECATED cfpPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmForgotPassword@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpAnalyticsMetadata :: Lens.Lens' ConfirmForgotPassword (Core.Maybe Types.AnalyticsMetadataType)
cfpAnalyticsMetadata = Lens.field @"analyticsMetadata"
{-# DEPRECATED cfpAnalyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead." #-}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ConfirmForgotPassword API action, Amazon Cognito invokes the function that is assigned to the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ConfirmForgotPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpClientMetadata :: Lens.Lens' ConfirmForgotPassword (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
cfpClientMetadata = Lens.field @"clientMetadata"
{-# DEPRECATED cfpClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- /Note:/ Consider using 'secretHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpSecretHash :: Lens.Lens' ConfirmForgotPassword (Core.Maybe Types.SecretHashType)
cfpSecretHash = Lens.field @"secretHash"
{-# DEPRECATED cfpSecretHash "Use generic-lens or generic-optics with 'secretHash' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpUserContextData :: Lens.Lens' ConfirmForgotPassword (Core.Maybe Types.UserContextDataType)
cfpUserContextData = Lens.field @"userContextData"
{-# DEPRECATED cfpUserContextData "Use generic-lens or generic-optics with 'userContextData' instead." #-}

instance Core.FromJSON ConfirmForgotPassword where
  toJSON ConfirmForgotPassword {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClientId" Core..= clientId),
            Core.Just ("Username" Core..= username),
            Core.Just ("ConfirmationCode" Core..= confirmationCode),
            Core.Just ("Password" Core..= password),
            ("AnalyticsMetadata" Core..=) Core.<$> analyticsMetadata,
            ("ClientMetadata" Core..=) Core.<$> clientMetadata,
            ("SecretHash" Core..=) Core.<$> secretHash,
            ("UserContextData" Core..=) Core.<$> userContextData
          ]
      )

instance Core.AWSRequest ConfirmForgotPassword where
  type Rs ConfirmForgotPassword = ConfirmForgotPasswordResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.ConfirmForgotPassword"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ConfirmForgotPasswordResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The response from the server that results from a user's request to retrieve a forgotten password.
--
-- /See:/ 'mkConfirmForgotPasswordResponse' smart constructor.
newtype ConfirmForgotPasswordResponse = ConfirmForgotPasswordResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmForgotPasswordResponse' value with any optional fields omitted.
mkConfirmForgotPasswordResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ConfirmForgotPasswordResponse
mkConfirmForgotPasswordResponse responseStatus =
  ConfirmForgotPasswordResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprrsResponseStatus :: Lens.Lens' ConfirmForgotPasswordResponse Core.Int
cfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
