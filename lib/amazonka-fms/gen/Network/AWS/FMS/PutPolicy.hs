{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.PutPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager policy.
--
-- Firewall Manager provides the following types of policies:
--
--     * An AWS WAF policy (type WAFV2), which defines rule groups to run first in the corresponding AWS WAF web ACL and rule groups to run last in the web ACL.
--
--
--     * An AWS WAF Classic policy (type WAF), which defines a rule group.
--
--
--     * A Shield Advanced policy, which applies Shield Advanced protection to specified accounts and resources.
--
--
--     * A security group policy, which manages VPC security groups across your AWS organization.
--
--
--     * An AWS Network Firewall policy, which provides firewall rules to filter network traffic in specified Amazon VPCs.
--
--
-- Each policy is specific to one of the types. If you want to enforce more than one policy type across accounts, create multiple policies. You can create multiple policies for each type.
-- You must be subscribed to Shield Advanced to create a Shield Advanced policy. For more information about subscribing to Shield Advanced, see <https://docs.aws.amazon.com/waf/latest/DDOSAPIReference/API_CreateSubscription.html CreateSubscription> .
module Network.AWS.FMS.PutPolicy
  ( -- * Creating a request
    PutPolicy (..),
    mkPutPolicy,

    -- ** Request lenses
    ppPolicy,
    ppTagList,

    -- * Destructuring the response
    PutPolicyResponse (..),
    mkPutPolicyResponse,

    -- ** Response lenses
    pprrsPolicy,
    pprrsPolicyArn,
    pprrsResponseStatus,
  )
where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutPolicy' smart constructor.
data PutPolicy = PutPolicy'
  { -- | The details of the AWS Firewall Manager policy to be created.
    policy :: Types.Policy,
    -- | The tags to add to the AWS resource.
    tagList :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutPolicy' value with any optional fields omitted.
mkPutPolicy ::
  -- | 'policy'
  Types.Policy ->
  PutPolicy
mkPutPolicy policy = PutPolicy' {policy, tagList = Core.Nothing}

-- | The details of the AWS Firewall Manager policy to be created.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPolicy :: Lens.Lens' PutPolicy Types.Policy
ppPolicy = Lens.field @"policy"
{-# DEPRECATED ppPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The tags to add to the AWS resource.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppTagList :: Lens.Lens' PutPolicy (Core.Maybe [Types.Tag])
ppTagList = Lens.field @"tagList"
{-# DEPRECATED ppTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

instance Core.FromJSON PutPolicy where
  toJSON PutPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Policy" Core..= policy),
            ("TagList" Core..=) Core.<$> tagList
          ]
      )

instance Core.AWSRequest PutPolicy where
  type Rs PutPolicy = PutPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSFMS_20180101.PutPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutPolicyResponse'
            Core.<$> (x Core..:? "Policy")
            Core.<*> (x Core..:? "PolicyArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutPolicyResponse' smart constructor.
data PutPolicyResponse = PutPolicyResponse'
  { -- | The details of the AWS Firewall Manager policy.
    policy :: Core.Maybe Types.Policy,
    -- | The Amazon Resource Name (ARN) of the policy.
    policyArn :: Core.Maybe Types.PolicyArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutPolicyResponse' value with any optional fields omitted.
mkPutPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutPolicyResponse
mkPutPolicyResponse responseStatus =
  PutPolicyResponse'
    { policy = Core.Nothing,
      policyArn = Core.Nothing,
      responseStatus
    }

-- | The details of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprrsPolicy :: Lens.Lens' PutPolicyResponse (Core.Maybe Types.Policy)
pprrsPolicy = Lens.field @"policy"
{-# DEPRECATED pprrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The Amazon Resource Name (ARN) of the policy.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprrsPolicyArn :: Lens.Lens' PutPolicyResponse (Core.Maybe Types.PolicyArn)
pprrsPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED pprrsPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprrsResponseStatus :: Lens.Lens' PutPolicyResponse Core.Int
pprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
