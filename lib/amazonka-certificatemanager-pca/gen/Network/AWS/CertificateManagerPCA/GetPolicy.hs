{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.GetPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource-based policy attached to a private CA. If either the private CA resource or the policy cannot be found, this action returns a @ResourceNotFoundException@ . 
--
-- The policy can be attached or updated with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_PutPolicy.html PutPolicy> and removed with <acm-pca/latest/APIReference/API_DeletePolicy.html DeletePolicy> .
-- __About Policies__ 
--
--     * A policy grants access on a private CA to an AWS customer account, to AWS Organizations, or to an AWS Organizations unit. Policies are under the control of a CA administrator. For more information, see <acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA> .
--
--
--     * A policy permits a user of AWS Certificate Manager (ACM) to issue ACM certificates signed by a CA in another account.
--
--
--     * For ACM to manage automatic renewal of these certificates, the ACM user must configure a Service Linked Role (SLR). The SLR allows the ACM service to assume the identity of the user, subject to confirmation against the ACM Private CA policy. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-slr.html Using a Service Linked Role with ACM> .
--
--
--     * Updates made in AWS Resource Manager (RAM) are reflected in policies. For more information, see <acm-pca/latest/userguide/pca-ram.html Using AWS Resource Access Manager (RAM) with ACM Private CA> .
--
--
module Network.AWS.CertificateManagerPCA.GetPolicy
    (
    -- * Creating a request
      GetPolicy (..)
    , mkGetPolicy
    -- ** Request lenses
    , gpResourceArn

    -- * Destructuring the response
    , GetPolicyResponse (..)
    , mkGetPolicyResponse
    -- ** Response lenses
    , gprrsPolicy
    , gprrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPolicy' smart constructor.
newtype GetPolicy = GetPolicy'
  { resourceArn :: Types.Arn
    -- ^ The Amazon Resource Number (ARN) of the private CA that will have its policy retrieved. You can find the CA's ARN by calling the ListCertificateAuthorities action. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicy' value with any optional fields omitted.
mkGetPolicy
    :: Types.Arn -- ^ 'resourceArn'
    -> GetPolicy
mkGetPolicy resourceArn = GetPolicy'{resourceArn}

-- | The Amazon Resource Number (ARN) of the private CA that will have its policy retrieved. You can find the CA's ARN by calling the ListCertificateAuthorities action. 
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpResourceArn :: Lens.Lens' GetPolicy Types.Arn
gpResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE gpResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery GetPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPolicy where
        toHeaders GetPolicy{..}
          = Core.pure ("X-Amz-Target", "ACMPrivateCA.GetPolicy") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetPolicy where
        toJSON GetPolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest GetPolicy where
        type Rs GetPolicy = GetPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPolicyResponse' Core.<$>
                   (x Core..:? "Policy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { policy :: Core.Maybe Types.Policy
    -- ^ The policy attached to the private CA as a JSON document.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicyResponse' value with any optional fields omitted.
mkGetPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPolicyResponse
mkGetPolicyResponse responseStatus
  = GetPolicyResponse'{policy = Core.Nothing, responseStatus}

-- | The policy attached to the private CA as a JSON document.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPolicy :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.Policy)
gprrsPolicy = Lens.field @"policy"
{-# INLINEABLE gprrsPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetPolicyResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
