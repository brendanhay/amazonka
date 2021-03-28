{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.PutPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a resource-based policy to a private CA. 
--
-- A policy can also be applied by <https://docs.aws.amazon.com/acm-pca/latest/userguide/pca-ram.html sharing> a private CA through AWS Resource Access Manager (RAM).
-- The policy can be displayed with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetPolicy.html GetPolicy> and removed with <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePolicy.html DeletePolicy> .
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
module Network.AWS.CertificateManagerPCA.PutPolicy
    (
    -- * Creating a request
      PutPolicy (..)
    , mkPutPolicy
    -- ** Request lenses
    , ppResourceArn
    , ppPolicy

    -- * Destructuring the response
    , PutPolicyResponse (..)
    , mkPutPolicyResponse
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutPolicy' smart constructor.
data PutPolicy = PutPolicy'
  { resourceArn :: Types.Arn
    -- ^ The Amazon Resource Number (ARN) of the private CA to associate with the policy. The ARN of the CA can be found by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
--
--
  , policy :: Types.Policy
    -- ^ The path and filename of a JSON-formatted IAM policy to attach to the specified private CA resource. If this policy does not contain all required statements or if it includes any statement that is not allowed, the @PutPolicy@ action returns an @InvalidPolicyException@ . For information about IAM policy and statement structure, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutPolicy' value with any optional fields omitted.
mkPutPolicy
    :: Types.Arn -- ^ 'resourceArn'
    -> Types.Policy -- ^ 'policy'
    -> PutPolicy
mkPutPolicy resourceArn policy = PutPolicy'{resourceArn, policy}

-- | The Amazon Resource Number (ARN) of the private CA to associate with the policy. The ARN of the CA can be found by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> action.
--
--
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppResourceArn :: Lens.Lens' PutPolicy Types.Arn
ppResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE ppResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | The path and filename of a JSON-formatted IAM policy to attach to the specified private CA resource. If this policy does not contain all required statements or if it includes any statement that is not allowed, the @PutPolicy@ action returns an @InvalidPolicyException@ . For information about IAM policy and statement structure, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies> .
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPolicy :: Lens.Lens' PutPolicy Types.Policy
ppPolicy = Lens.field @"policy"
{-# INLINEABLE ppPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

instance Core.ToQuery PutPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutPolicy where
        toHeaders PutPolicy{..}
          = Core.pure ("X-Amz-Target", "ACMPrivateCA.PutPolicy") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutPolicy where
        toJSON PutPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceArn" Core..= resourceArn),
                  Core.Just ("Policy" Core..= policy)])

instance Core.AWSRequest PutPolicy where
        type Rs PutPolicy = PutPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PutPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutPolicyResponse' smart constructor.
data PutPolicyResponse = PutPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutPolicyResponse' value with any optional fields omitted.
mkPutPolicyResponse
    :: PutPolicyResponse
mkPutPolicyResponse = PutPolicyResponse'
