{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resends the confirmation (for confirmation of registration) to a specific user in the user pool.
module Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
    (
    -- * Creating a request
      ResendConfirmationCode (..)
    , mkResendConfirmationCode
    -- ** Request lenses
    , rccClientId
    , rccUsername
    , rccAnalyticsMetadata
    , rccClientMetadata
    , rccSecretHash
    , rccUserContextData

    -- * Destructuring the response
    , ResendConfirmationCodeResponse (..)
    , mkResendConfirmationCodeResponse
    -- ** Response lenses
    , rccrrsCodeDeliveryDetails
    , rccrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to resend the confirmation code.
--
-- /See:/ 'mkResendConfirmationCode' smart constructor.
data ResendConfirmationCode = ResendConfirmationCode'
  { clientId :: Types.ClientIdType
    -- ^ The ID of the client associated with the user pool.
  , username :: Types.Username
    -- ^ The user name of the user to whom you wish to resend a confirmation code.
  , analyticsMetadata :: Core.Maybe Types.AnalyticsMetadataType
    -- ^ The Amazon Pinpoint analytics metadata for collecting metrics for @ResendConfirmationCode@ calls.
  , clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ResendConfirmationCode API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ResendConfirmationCode request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
  , secretHash :: Core.Maybe Types.SecretHashType
    -- ^ A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
  , userContextData :: Core.Maybe Types.UserContextDataType
    -- ^ Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResendConfirmationCode' value with any optional fields omitted.
mkResendConfirmationCode
    :: Types.ClientIdType -- ^ 'clientId'
    -> Types.Username -- ^ 'username'
    -> ResendConfirmationCode
mkResendConfirmationCode clientId username
  = ResendConfirmationCode'{clientId, username,
                            analyticsMetadata = Core.Nothing, clientMetadata = Core.Nothing,
                            secretHash = Core.Nothing, userContextData = Core.Nothing}

-- | The ID of the client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccClientId :: Lens.Lens' ResendConfirmationCode Types.ClientIdType
rccClientId = Lens.field @"clientId"
{-# INLINEABLE rccClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The user name of the user to whom you wish to resend a confirmation code.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccUsername :: Lens.Lens' ResendConfirmationCode Types.Username
rccUsername = Lens.field @"username"
{-# INLINEABLE rccUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @ResendConfirmationCode@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccAnalyticsMetadata :: Lens.Lens' ResendConfirmationCode (Core.Maybe Types.AnalyticsMetadataType)
rccAnalyticsMetadata = Lens.field @"analyticsMetadata"
{-# INLINEABLE rccAnalyticsMetadata #-}
{-# DEPRECATED analyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead"  #-}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ResendConfirmationCode API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ResendConfirmationCode request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccClientMetadata :: Lens.Lens' ResendConfirmationCode (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
rccClientMetadata = Lens.field @"clientMetadata"
{-# INLINEABLE rccClientMetadata #-}
{-# DEPRECATED clientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead"  #-}

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- /Note:/ Consider using 'secretHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccSecretHash :: Lens.Lens' ResendConfirmationCode (Core.Maybe Types.SecretHashType)
rccSecretHash = Lens.field @"secretHash"
{-# INLINEABLE rccSecretHash #-}
{-# DEPRECATED secretHash "Use generic-lens or generic-optics with 'secretHash' instead"  #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccUserContextData :: Lens.Lens' ResendConfirmationCode (Core.Maybe Types.UserContextDataType)
rccUserContextData = Lens.field @"userContextData"
{-# INLINEABLE rccUserContextData #-}
{-# DEPRECATED userContextData "Use generic-lens or generic-optics with 'userContextData' instead"  #-}

instance Core.ToQuery ResendConfirmationCode where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResendConfirmationCode where
        toHeaders ResendConfirmationCode{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.ResendConfirmationCode")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResendConfirmationCode where
        toJSON ResendConfirmationCode{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClientId" Core..= clientId),
                  Core.Just ("Username" Core..= username),
                  ("AnalyticsMetadata" Core..=) Core.<$> analyticsMetadata,
                  ("ClientMetadata" Core..=) Core.<$> clientMetadata,
                  ("SecretHash" Core..=) Core.<$> secretHash,
                  ("UserContextData" Core..=) Core.<$> userContextData])

instance Core.AWSRequest ResendConfirmationCode where
        type Rs ResendConfirmationCode = ResendConfirmationCodeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ResendConfirmationCodeResponse' Core.<$>
                   (x Core..:? "CodeDeliveryDetails") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The response from the server when the Amazon Cognito Your User Pools service makes the request to resend a confirmation code.
--
-- /See:/ 'mkResendConfirmationCodeResponse' smart constructor.
data ResendConfirmationCodeResponse = ResendConfirmationCodeResponse'
  { codeDeliveryDetails :: Core.Maybe Types.CodeDeliveryDetailsType
    -- ^ The code delivery details returned by the server in response to the request to resend the confirmation code.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResendConfirmationCodeResponse' value with any optional fields omitted.
mkResendConfirmationCodeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResendConfirmationCodeResponse
mkResendConfirmationCodeResponse responseStatus
  = ResendConfirmationCodeResponse'{codeDeliveryDetails =
                                      Core.Nothing,
                                    responseStatus}

-- | The code delivery details returned by the server in response to the request to resend the confirmation code.
--
-- /Note:/ Consider using 'codeDeliveryDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccrrsCodeDeliveryDetails :: Lens.Lens' ResendConfirmationCodeResponse (Core.Maybe Types.CodeDeliveryDetailsType)
rccrrsCodeDeliveryDetails = Lens.field @"codeDeliveryDetails"
{-# INLINEABLE rccrrsCodeDeliveryDetails #-}
{-# DEPRECATED codeDeliveryDetails "Use generic-lens or generic-optics with 'codeDeliveryDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccrrsResponseStatus :: Lens.Lens' ResendConfirmationCodeResponse Core.Int
rccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
