{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a policy.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribePolicy
    (
    -- * Creating a request
      DescribePolicy (..)
    , mkDescribePolicy
    -- ** Request lenses
    , dpPolicyId

    -- * Destructuring the response
    , DescribePolicyResponse (..)
    , mkDescribePolicyResponse
    -- ** Response lenses
    , dprrsPolicy
    , dprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePolicy' smart constructor.
newtype DescribePolicy = DescribePolicy'
  { policyId :: Types.PolicyId
    -- ^ The unique identifier (ID) of the policy that you want details about. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePolicy' value with any optional fields omitted.
mkDescribePolicy
    :: Types.PolicyId -- ^ 'policyId'
    -> DescribePolicy
mkDescribePolicy policyId = DescribePolicy'{policyId}

-- | The unique identifier (ID) of the policy that you want details about. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyId :: Lens.Lens' DescribePolicy Types.PolicyId
dpPolicyId = Lens.field @"policyId"
{-# INLINEABLE dpPolicyId #-}
{-# DEPRECATED policyId "Use generic-lens or generic-optics with 'policyId' instead"  #-}

instance Core.ToQuery DescribePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribePolicy where
        toHeaders DescribePolicy{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.DescribePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribePolicy where
        toJSON DescribePolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("PolicyId" Core..= policyId)])

instance Core.AWSRequest DescribePolicy where
        type Rs DescribePolicy = DescribePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribePolicyResponse' Core.<$>
                   (x Core..:? "Policy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribePolicyResponse' smart constructor.
data DescribePolicyResponse = DescribePolicyResponse'
  { policy :: Core.Maybe Types.Policy
    -- ^ A structure that contains details about the specified policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePolicyResponse' value with any optional fields omitted.
mkDescribePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePolicyResponse
mkDescribePolicyResponse responseStatus
  = DescribePolicyResponse'{policy = Core.Nothing, responseStatus}

-- | A structure that contains details about the specified policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsPolicy :: Lens.Lens' DescribePolicyResponse (Core.Maybe Types.Policy)
dprrsPolicy = Lens.field @"policy"
{-# INLINEABLE dprrsPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DescribePolicyResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
