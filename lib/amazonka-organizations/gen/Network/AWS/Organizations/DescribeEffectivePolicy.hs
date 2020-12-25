{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeEffectivePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the contents of the effective policy for specified policy type and account. The effective policy is the aggregation of any policies of the specified type that the account inherits, plus any policy of that type that is directly attached to the account.
--
-- This operation applies only to policy types /other/ than service control policies (SCPs).
-- For more information about policy inheritance, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies-inheritance.html How Policy Inheritance Works> in the /AWS Organizations User Guide/ .
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribeEffectivePolicy
  ( -- * Creating a request
    DescribeEffectivePolicy (..),
    mkDescribeEffectivePolicy,

    -- ** Request lenses
    depPolicyType,
    depTargetId,

    -- * Destructuring the response
    DescribeEffectivePolicyResponse (..),
    mkDescribeEffectivePolicyResponse,

    -- ** Response lenses
    deprrsEffectivePolicy,
    deprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEffectivePolicy' smart constructor.
data DescribeEffectivePolicy = DescribeEffectivePolicy'
  { -- | The type of policy that you want information about. You can specify one of the following values:
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
    policyType :: Types.EffectivePolicyType,
    -- | When you're signed in as the management account, specify the ID of the account that you want details about. Specifying an organization root or organizational unit (OU) as the target is not supported.
    targetId :: Core.Maybe Types.PolicyTargetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEffectivePolicy' value with any optional fields omitted.
mkDescribeEffectivePolicy ::
  -- | 'policyType'
  Types.EffectivePolicyType ->
  DescribeEffectivePolicy
mkDescribeEffectivePolicy policyType =
  DescribeEffectivePolicy' {policyType, targetId = Core.Nothing}

-- | The type of policy that you want information about. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
--
--
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depPolicyType :: Lens.Lens' DescribeEffectivePolicy Types.EffectivePolicyType
depPolicyType = Lens.field @"policyType"
{-# DEPRECATED depPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | When you're signed in as the management account, specify the ID of the account that you want details about. Specifying an organization root or organizational unit (OU) as the target is not supported.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depTargetId :: Lens.Lens' DescribeEffectivePolicy (Core.Maybe Types.PolicyTargetId)
depTargetId = Lens.field @"targetId"
{-# DEPRECATED depTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

instance Core.FromJSON DescribeEffectivePolicy where
  toJSON DescribeEffectivePolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyType" Core..= policyType),
            ("TargetId" Core..=) Core.<$> targetId
          ]
      )

instance Core.AWSRequest DescribeEffectivePolicy where
  type Rs DescribeEffectivePolicy = DescribeEffectivePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrganizationsV20161128.DescribeEffectivePolicy"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEffectivePolicyResponse'
            Core.<$> (x Core..:? "EffectivePolicy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeEffectivePolicyResponse' smart constructor.
data DescribeEffectivePolicyResponse = DescribeEffectivePolicyResponse'
  { -- | The contents of the effective policy.
    effectivePolicy :: Core.Maybe Types.EffectivePolicy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEffectivePolicyResponse' value with any optional fields omitted.
mkDescribeEffectivePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEffectivePolicyResponse
mkDescribeEffectivePolicyResponse responseStatus =
  DescribeEffectivePolicyResponse'
    { effectivePolicy = Core.Nothing,
      responseStatus
    }

-- | The contents of the effective policy.
--
-- /Note:/ Consider using 'effectivePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deprrsEffectivePolicy :: Lens.Lens' DescribeEffectivePolicyResponse (Core.Maybe Types.EffectivePolicy)
deprrsEffectivePolicy = Lens.field @"effectivePolicy"
{-# DEPRECATED deprrsEffectivePolicy "Use generic-lens or generic-optics with 'effectivePolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deprrsResponseStatus :: Lens.Lens' DescribeEffectivePolicyResponse Core.Int
deprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED deprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
