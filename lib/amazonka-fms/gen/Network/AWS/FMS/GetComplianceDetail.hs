{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetComplianceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed compliance information about the specified member account. Details include resources that are in and out of compliance with the specified policy. Resources are considered noncompliant for AWS WAF and Shield Advanced policies if the specified policy has not been applied to them. Resources are considered noncompliant for security group policies if they are in scope of the policy, they violate one or more of the policy rules, and remediation is disabled or not possible. Resources are considered noncompliant for Network Firewall policies if a firewall is missing in the VPC, if the firewall endpoint isn't set up in an expected Availability Zone and subnet, if a subnet created by the Firewall Manager doesn't have the expected route table, and for modifications to a firewall policy that violate the Firewall Manager policy's rules. 
module Network.AWS.FMS.GetComplianceDetail
    (
    -- * Creating a request
      GetComplianceDetail (..)
    , mkGetComplianceDetail
    -- ** Request lenses
    , gcdPolicyId
    , gcdMemberAccount

    -- * Destructuring the response
    , GetComplianceDetailResponse (..)
    , mkGetComplianceDetailResponse
    -- ** Response lenses
    , gcdrrsPolicyComplianceDetail
    , gcdrrsResponseStatus
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetComplianceDetail' smart constructor.
data GetComplianceDetail = GetComplianceDetail'
  { policyId :: Types.PolicyId
    -- ^ The ID of the policy that you want to get the details for. @PolicyId@ is returned by @PutPolicy@ and by @ListPolicies@ .
  , memberAccount :: Types.AWSAccountId
    -- ^ The AWS account that owns the resources that you want to get the details for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetComplianceDetail' value with any optional fields omitted.
mkGetComplianceDetail
    :: Types.PolicyId -- ^ 'policyId'
    -> Types.AWSAccountId -- ^ 'memberAccount'
    -> GetComplianceDetail
mkGetComplianceDetail policyId memberAccount
  = GetComplianceDetail'{policyId, memberAccount}

-- | The ID of the policy that you want to get the details for. @PolicyId@ is returned by @PutPolicy@ and by @ListPolicies@ .
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdPolicyId :: Lens.Lens' GetComplianceDetail Types.PolicyId
gcdPolicyId = Lens.field @"policyId"
{-# INLINEABLE gcdPolicyId #-}
{-# DEPRECATED policyId "Use generic-lens or generic-optics with 'policyId' instead"  #-}

-- | The AWS account that owns the resources that you want to get the details for.
--
-- /Note:/ Consider using 'memberAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdMemberAccount :: Lens.Lens' GetComplianceDetail Types.AWSAccountId
gcdMemberAccount = Lens.field @"memberAccount"
{-# INLINEABLE gcdMemberAccount #-}
{-# DEPRECATED memberAccount "Use generic-lens or generic-optics with 'memberAccount' instead"  #-}

instance Core.ToQuery GetComplianceDetail where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetComplianceDetail where
        toHeaders GetComplianceDetail{..}
          = Core.pure ("X-Amz-Target", "AWSFMS_20180101.GetComplianceDetail")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetComplianceDetail where
        toJSON GetComplianceDetail{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PolicyId" Core..= policyId),
                  Core.Just ("MemberAccount" Core..= memberAccount)])

instance Core.AWSRequest GetComplianceDetail where
        type Rs GetComplianceDetail = GetComplianceDetailResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetComplianceDetailResponse' Core.<$>
                   (x Core..:? "PolicyComplianceDetail") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetComplianceDetailResponse' smart constructor.
data GetComplianceDetailResponse = GetComplianceDetailResponse'
  { policyComplianceDetail :: Core.Maybe Types.PolicyComplianceDetail
    -- ^ Information about the resources and the policy that you specified in the @GetComplianceDetail@ request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetComplianceDetailResponse' value with any optional fields omitted.
mkGetComplianceDetailResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetComplianceDetailResponse
mkGetComplianceDetailResponse responseStatus
  = GetComplianceDetailResponse'{policyComplianceDetail =
                                   Core.Nothing,
                                 responseStatus}

-- | Information about the resources and the policy that you specified in the @GetComplianceDetail@ request.
--
-- /Note:/ Consider using 'policyComplianceDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrrsPolicyComplianceDetail :: Lens.Lens' GetComplianceDetailResponse (Core.Maybe Types.PolicyComplianceDetail)
gcdrrsPolicyComplianceDetail = Lens.field @"policyComplianceDetail"
{-# INLINEABLE gcdrrsPolicyComplianceDetail #-}
{-# DEPRECATED policyComplianceDetail "Use generic-lens or generic-optics with 'policyComplianceDetail' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrrsResponseStatus :: Lens.Lens' GetComplianceDetailResponse Core.Int
gcdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
