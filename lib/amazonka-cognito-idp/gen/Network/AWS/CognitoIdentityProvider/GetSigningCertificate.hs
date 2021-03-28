{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetSigningCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This method takes a user pool ID, and returns the signing certificate.
module Network.AWS.CognitoIdentityProvider.GetSigningCertificate
    (
    -- * Creating a request
      GetSigningCertificate (..)
    , mkGetSigningCertificate
    -- ** Request lenses
    , gscUserPoolId

    -- * Destructuring the response
    , GetSigningCertificateResponse (..)
    , mkGetSigningCertificateResponse
    -- ** Response lenses
    , gscrrsCertificate
    , gscrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to get a signing certificate from Cognito.
--
-- /See:/ 'mkGetSigningCertificate' smart constructor.
newtype GetSigningCertificate = GetSigningCertificate'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSigningCertificate' value with any optional fields omitted.
mkGetSigningCertificate
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> GetSigningCertificate
mkGetSigningCertificate userPoolId
  = GetSigningCertificate'{userPoolId}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscUserPoolId :: Lens.Lens' GetSigningCertificate Types.UserPoolId
gscUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE gscUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

instance Core.ToQuery GetSigningCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSigningCertificate where
        toHeaders GetSigningCertificate{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.GetSigningCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSigningCertificate where
        toJSON GetSigningCertificate{..}
          = Core.object
              (Core.catMaybes [Core.Just ("UserPoolId" Core..= userPoolId)])

instance Core.AWSRequest GetSigningCertificate where
        type Rs GetSigningCertificate = GetSigningCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSigningCertificateResponse' Core.<$>
                   (x Core..:? "Certificate") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response from Cognito for a signing certificate request.
--
-- /See:/ 'mkGetSigningCertificateResponse' smart constructor.
data GetSigningCertificateResponse = GetSigningCertificateResponse'
  { certificate :: Core.Maybe Types.StringType
    -- ^ The signing certificate.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSigningCertificateResponse' value with any optional fields omitted.
mkGetSigningCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSigningCertificateResponse
mkGetSigningCertificateResponse responseStatus
  = GetSigningCertificateResponse'{certificate = Core.Nothing,
                                   responseStatus}

-- | The signing certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrrsCertificate :: Lens.Lens' GetSigningCertificateResponse (Core.Maybe Types.StringType)
gscrrsCertificate = Lens.field @"certificate"
{-# INLINEABLE gscrrsCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrrsResponseStatus :: Lens.Lens' GetSigningCertificateResponse Core.Int
gscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
