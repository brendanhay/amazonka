{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ConfirmForgotPassword (..)
    , mkConfirmForgotPassword
    -- ** Request lenses
    , cfpClientId
    , cfpUsername
    , cfpConfirmationCode
    , cfpPassword
    , cfpAnalyticsMetadata
    , cfpClientMetadata
    , cfpSecretHash
    , cfpUserContextData

    -- * Destructuring the response
    , ConfirmForgotPasswordResponse (..)
    , mkConfirmForgotPasswordResponse
    -- ** Response lenses
    , cfprrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request representing the confirmation for a password reset.
--
-- /See:/ 'mkConfirmForgotPassword' smart constructor.
data ConfirmForgotPassword = ConfirmForgotPassword'
  { clientId :: Types.ClientIdType
    -- ^ The app client ID of the app associated with the user pool.
  , username :: Types.Username
    -- ^ The user name of the user for whom you want to enter a code to retrieve a forgotten password.
  , confirmationCode :: Types.ConfirmationCodeType
    -- ^ The confirmation code sent by a user's request to retrieve a forgotten password. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword> .
  , password :: Types.PasswordType
    -- ^ The password sent by a user's request to retrieve a forgotten password.
  , analyticsMetadata :: Core.Maybe Types.AnalyticsMetadataType
    -- ^ The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmForgotPassword@ calls.
  , clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ConfirmForgotPassword API action, Amazon Cognito invokes the function that is assigned to the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ConfirmForgotPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
  , secretHash :: Core.Maybe Types.SecretHashType
    -- ^ A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
  , userContextData :: Core.Maybe Types.UserContextDataType
    -- ^ Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmForgotPassword' value with any optional fields omitted.
mkConfirmForgotPassword
    :: Types.ClientIdType -- ^ 'clientId'
    -> Types.Username -- ^ 'username'
    -> Types.ConfirmationCodeType -- ^ 'confirmationCode'
    -> Types.PasswordType -- ^ 'password'
    -> ConfirmForgotPassword
mkConfirmForgotPassword clientId username confirmationCode password
  = ConfirmForgotPassword'{clientId, username, confirmationCode,
                           password, analyticsMetadata = Core.Nothing,
                           clientMetadata = Core.Nothing, secretHash = Core.Nothing,
                           userContextData = Core.Nothing}

-- | The app client ID of the app associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpClientId :: Lens.Lens' ConfirmForgotPassword Types.ClientIdType
cfpClientId = Lens.field @"clientId"
{-# INLINEABLE cfpClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The user name of the user for whom you want to enter a code to retrieve a forgotten password.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpUsername :: Lens.Lens' ConfirmForgotPassword Types.Username
cfpUsername = Lens.field @"username"
{-# INLINEABLE cfpUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The confirmation code sent by a user's request to retrieve a forgotten password. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword> .
--
-- /Note:/ Consider using 'confirmationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpConfirmationCode :: Lens.Lens' ConfirmForgotPassword Types.ConfirmationCodeType
cfpConfirmationCode = Lens.field @"confirmationCode"
{-# INLINEABLE cfpConfirmationCode #-}
{-# DEPRECATED confirmationCode "Use generic-lens or generic-optics with 'confirmationCode' instead"  #-}

-- | The password sent by a user's request to retrieve a forgotten password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpPassword :: Lens.Lens' ConfirmForgotPassword Types.PasswordType
cfpPassword = Lens.field @"password"
{-# INLINEABLE cfpPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmForgotPassword@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpAnalyticsMetadata :: Lens.Lens' ConfirmForgotPassword (Core.Maybe Types.AnalyticsMetadataType)
cfpAnalyticsMetadata = Lens.field @"analyticsMetadata"
{-# INLINEABLE cfpAnalyticsMetadata #-}
{-# DEPRECATED analyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead"  #-}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ConfirmForgotPassword API action, Amazon Cognito invokes the function that is assigned to the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ConfirmForgotPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpClientMetadata :: Lens.Lens' ConfirmForgotPassword (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
cfpClientMetadata = Lens.field @"clientMetadata"
{-# INLINEABLE cfpClientMetadata #-}
{-# DEPRECATED clientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead"  #-}

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- /Note:/ Consider using 'secretHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpSecretHash :: Lens.Lens' ConfirmForgotPassword (Core.Maybe Types.SecretHashType)
cfpSecretHash = Lens.field @"secretHash"
{-# INLINEABLE cfpSecretHash #-}
{-# DEPRECATED secretHash "Use generic-lens or generic-optics with 'secretHash' instead"  #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpUserContextData :: Lens.Lens' ConfirmForgotPassword (Core.Maybe Types.UserContextDataType)
cfpUserContextData = Lens.field @"userContextData"
{-# INLINEABLE cfpUserContextData #-}
{-# DEPRECATED userContextData "Use generic-lens or generic-optics with 'userContextData' instead"  #-}

instance Core.ToQuery ConfirmForgotPassword where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ConfirmForgotPassword where
        toHeaders ConfirmForgotPassword{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.ConfirmForgotPassword")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ConfirmForgotPassword where
        toJSON ConfirmForgotPassword{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClientId" Core..= clientId),
                  Core.Just ("Username" Core..= username),
                  Core.Just ("ConfirmationCode" Core..= confirmationCode),
                  Core.Just ("Password" Core..= password),
                  ("AnalyticsMetadata" Core..=) Core.<$> analyticsMetadata,
                  ("ClientMetadata" Core..=) Core.<$> clientMetadata,
                  ("SecretHash" Core..=) Core.<$> secretHash,
                  ("UserContextData" Core..=) Core.<$> userContextData])

instance Core.AWSRequest ConfirmForgotPassword where
        type Rs ConfirmForgotPassword = ConfirmForgotPasswordResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ConfirmForgotPasswordResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The response from the server that results from a user's request to retrieve a forgotten password.
--
-- /See:/ 'mkConfirmForgotPasswordResponse' smart constructor.
newtype ConfirmForgotPasswordResponse = ConfirmForgotPasswordResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmForgotPasswordResponse' value with any optional fields omitted.
mkConfirmForgotPasswordResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ConfirmForgotPasswordResponse
mkConfirmForgotPasswordResponse responseStatus
  = ConfirmForgotPasswordResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprrsResponseStatus :: Lens.Lens' ConfirmForgotPasswordResponse Core.Int
cfprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cfprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
