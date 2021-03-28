{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user attribute verification code for the specified attribute name.
module Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
    (
    -- * Creating a request
      GetUserAttributeVerificationCode (..)
    , mkGetUserAttributeVerificationCode
    -- ** Request lenses
    , guavcAccessToken
    , guavcAttributeName
    , guavcClientMetadata

    -- * Destructuring the response
    , GetUserAttributeVerificationCodeResponse (..)
    , mkGetUserAttributeVerificationCodeResponse
    -- ** Response lenses
    , guavcrrsCodeDeliveryDetails
    , guavcrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get user attribute verification.
--
-- /See:/ 'mkGetUserAttributeVerificationCode' smart constructor.
data GetUserAttributeVerificationCode = GetUserAttributeVerificationCode'
  { accessToken :: Types.TokenModelType
    -- ^ The access token returned by the server response to get the user attribute verification code.
  , attributeName :: Types.AttributeNameType
    -- ^ The attribute name returned by the server response to get the user attribute verification code.
  , clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the GetUserAttributeVerificationCode API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your GetUserAttributeVerificationCode request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserAttributeVerificationCode' value with any optional fields omitted.
mkGetUserAttributeVerificationCode
    :: Types.TokenModelType -- ^ 'accessToken'
    -> Types.AttributeNameType -- ^ 'attributeName'
    -> GetUserAttributeVerificationCode
mkGetUserAttributeVerificationCode accessToken attributeName
  = GetUserAttributeVerificationCode'{accessToken, attributeName,
                                      clientMetadata = Core.Nothing}

-- | The access token returned by the server response to get the user attribute verification code.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guavcAccessToken :: Lens.Lens' GetUserAttributeVerificationCode Types.TokenModelType
guavcAccessToken = Lens.field @"accessToken"
{-# INLINEABLE guavcAccessToken #-}
{-# DEPRECATED accessToken "Use generic-lens or generic-optics with 'accessToken' instead"  #-}

-- | The attribute name returned by the server response to get the user attribute verification code.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guavcAttributeName :: Lens.Lens' GetUserAttributeVerificationCode Types.AttributeNameType
guavcAttributeName = Lens.field @"attributeName"
{-# INLINEABLE guavcAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the GetUserAttributeVerificationCode API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your GetUserAttributeVerificationCode request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guavcClientMetadata :: Lens.Lens' GetUserAttributeVerificationCode (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
guavcClientMetadata = Lens.field @"clientMetadata"
{-# INLINEABLE guavcClientMetadata #-}
{-# DEPRECATED clientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead"  #-}

instance Core.ToQuery GetUserAttributeVerificationCode where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetUserAttributeVerificationCode where
        toHeaders GetUserAttributeVerificationCode{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.GetUserAttributeVerificationCode")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetUserAttributeVerificationCode where
        toJSON GetUserAttributeVerificationCode{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccessToken" Core..= accessToken),
                  Core.Just ("AttributeName" Core..= attributeName),
                  ("ClientMetadata" Core..=) Core.<$> clientMetadata])

instance Core.AWSRequest GetUserAttributeVerificationCode where
        type Rs GetUserAttributeVerificationCode =
             GetUserAttributeVerificationCodeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetUserAttributeVerificationCodeResponse' Core.<$>
                   (x Core..:? "CodeDeliveryDetails") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The verification code response returned by the server response to get the user attribute verification code.
--
-- /See:/ 'mkGetUserAttributeVerificationCodeResponse' smart constructor.
data GetUserAttributeVerificationCodeResponse = GetUserAttributeVerificationCodeResponse'
  { codeDeliveryDetails :: Core.Maybe Types.CodeDeliveryDetailsType
    -- ^ The code delivery details returned by the server in response to the request to get the user attribute verification code.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserAttributeVerificationCodeResponse' value with any optional fields omitted.
mkGetUserAttributeVerificationCodeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetUserAttributeVerificationCodeResponse
mkGetUserAttributeVerificationCodeResponse responseStatus
  = GetUserAttributeVerificationCodeResponse'{codeDeliveryDetails =
                                                Core.Nothing,
                                              responseStatus}

-- | The code delivery details returned by the server in response to the request to get the user attribute verification code.
--
-- /Note:/ Consider using 'codeDeliveryDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guavcrrsCodeDeliveryDetails :: Lens.Lens' GetUserAttributeVerificationCodeResponse (Core.Maybe Types.CodeDeliveryDetailsType)
guavcrrsCodeDeliveryDetails = Lens.field @"codeDeliveryDetails"
{-# INLINEABLE guavcrrsCodeDeliveryDetails #-}
{-# DEPRECATED codeDeliveryDetails "Use generic-lens or generic-optics with 'codeDeliveryDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guavcrrsResponseStatus :: Lens.Lens' GetUserAttributeVerificationCodeResponse Core.Int
guavcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE guavcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
