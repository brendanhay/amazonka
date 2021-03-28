{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserPoolDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Secure Sockets Layer (SSL) certificate for the custom domain for your user pool.
--
-- You can use this operation to provide the Amazon Resource Name (ARN) of a new certificate to Amazon Cognito. You cannot use it to change the domain for a user pool.
-- A custom domain is used to host the Amazon Cognito hosted UI, which provides sign-up and sign-in pages for your application. When you set up a custom domain, you provide a certificate that you manage with AWS Certificate Manager (ACM). When necessary, you can use this operation to change the certificate that you applied to your custom domain.
-- Usually, this is unnecessary following routine certificate renewal with ACM. When you renew your existing certificate in ACM, the ARN for your certificate remains the same, and your custom domain uses the new certificate automatically.
-- However, if you replace your existing certificate with a new one, ACM gives the new certificate a new ARN. To apply the new certificate to your custom domain, you must provide this ARN to Amazon Cognito.
-- When you add your new certificate in ACM, you must choose US East (N. Virginia) as the AWS Region.
-- After you submit your request, Amazon Cognito requires up to 1 hour to distribute your new certificate to your custom domain.
-- For more information about adding a custom domain to your user pool, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI> .
module Network.AWS.CognitoIdentityProvider.UpdateUserPoolDomain
    (
    -- * Creating a request
      UpdateUserPoolDomain (..)
    , mkUpdateUserPoolDomain
    -- ** Request lenses
    , uupdDomain
    , uupdUserPoolId
    , uupdCustomDomainConfig

    -- * Destructuring the response
    , UpdateUserPoolDomainResponse (..)
    , mkUpdateUserPoolDomainResponse
    -- ** Response lenses
    , uupdrrsCloudFrontDomain
    , uupdrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The UpdateUserPoolDomain request input.
--
-- /See:/ 'mkUpdateUserPoolDomain' smart constructor.
data UpdateUserPoolDomain = UpdateUserPoolDomain'
  { domain :: Types.Domain
    -- ^ The domain name for the custom domain that hosts the sign-up and sign-in pages for your application. For example: @auth.example.com@ . 
--
-- This string can include only lowercase letters, numbers, and hyphens. Do not use a hyphen for the first or last character. Use periods to separate subdomain names.
  , userPoolId :: Types.UserPoolId
    -- ^ The ID of the user pool that is associated with the custom domain that you are updating the certificate for.
  , customDomainConfig :: Types.CustomDomainConfigType
    -- ^ The configuration for a custom domain that hosts the sign-up and sign-in pages for your application. Use this object to specify an SSL certificate that is managed by ACM.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserPoolDomain' value with any optional fields omitted.
mkUpdateUserPoolDomain
    :: Types.Domain -- ^ 'domain'
    -> Types.UserPoolId -- ^ 'userPoolId'
    -> Types.CustomDomainConfigType -- ^ 'customDomainConfig'
    -> UpdateUserPoolDomain
mkUpdateUserPoolDomain domain userPoolId customDomainConfig
  = UpdateUserPoolDomain'{domain, userPoolId, customDomainConfig}

-- | The domain name for the custom domain that hosts the sign-up and sign-in pages for your application. For example: @auth.example.com@ . 
--
-- This string can include only lowercase letters, numbers, and hyphens. Do not use a hyphen for the first or last character. Use periods to separate subdomain names.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupdDomain :: Lens.Lens' UpdateUserPoolDomain Types.Domain
uupdDomain = Lens.field @"domain"
{-# INLINEABLE uupdDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The ID of the user pool that is associated with the custom domain that you are updating the certificate for.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupdUserPoolId :: Lens.Lens' UpdateUserPoolDomain Types.UserPoolId
uupdUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE uupdUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The configuration for a custom domain that hosts the sign-up and sign-in pages for your application. Use this object to specify an SSL certificate that is managed by ACM.
--
-- /Note:/ Consider using 'customDomainConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupdCustomDomainConfig :: Lens.Lens' UpdateUserPoolDomain Types.CustomDomainConfigType
uupdCustomDomainConfig = Lens.field @"customDomainConfig"
{-# INLINEABLE uupdCustomDomainConfig #-}
{-# DEPRECATED customDomainConfig "Use generic-lens or generic-optics with 'customDomainConfig' instead"  #-}

instance Core.ToQuery UpdateUserPoolDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateUserPoolDomain where
        toHeaders UpdateUserPoolDomain{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.UpdateUserPoolDomain")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateUserPoolDomain where
        toJSON UpdateUserPoolDomain{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Domain" Core..= domain),
                  Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("CustomDomainConfig" Core..= customDomainConfig)])

instance Core.AWSRequest UpdateUserPoolDomain where
        type Rs UpdateUserPoolDomain = UpdateUserPoolDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateUserPoolDomainResponse' Core.<$>
                   (x Core..:? "CloudFrontDomain") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The UpdateUserPoolDomain response output.
--
-- /See:/ 'mkUpdateUserPoolDomainResponse' smart constructor.
data UpdateUserPoolDomainResponse = UpdateUserPoolDomainResponse'
  { cloudFrontDomain :: Core.Maybe Types.CloudFrontDomain
    -- ^ The Amazon CloudFront endpoint that Amazon Cognito set up when you added the custom domain to your user pool.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserPoolDomainResponse' value with any optional fields omitted.
mkUpdateUserPoolDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateUserPoolDomainResponse
mkUpdateUserPoolDomainResponse responseStatus
  = UpdateUserPoolDomainResponse'{cloudFrontDomain = Core.Nothing,
                                  responseStatus}

-- | The Amazon CloudFront endpoint that Amazon Cognito set up when you added the custom domain to your user pool.
--
-- /Note:/ Consider using 'cloudFrontDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupdrrsCloudFrontDomain :: Lens.Lens' UpdateUserPoolDomainResponse (Core.Maybe Types.CloudFrontDomain)
uupdrrsCloudFrontDomain = Lens.field @"cloudFrontDomain"
{-# INLINEABLE uupdrrsCloudFrontDomain #-}
{-# DEPRECATED cloudFrontDomain "Use generic-lens or generic-optics with 'cloudFrontDomain' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupdrrsResponseStatus :: Lens.Lens' UpdateUserPoolDomainResponse Core.Int
uupdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uupdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
