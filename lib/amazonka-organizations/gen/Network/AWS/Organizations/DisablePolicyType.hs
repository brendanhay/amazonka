{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DisablePolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an organizational policy type in a root. A policy of a certain type can be attached to entities in a root only if that type is enabled in the root. After you perform this operation, you no longer can attach policies of the specified type to that root or to any organizational unit (OU) or account in that root. You can undo this by using the 'EnablePolicyType' operation.
--
-- This is an asynchronous request that AWS performs in the background. If you disable a policy type for a root, it still appears enabled for the organization if <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features> are enabled for the organization. AWS recommends that you first use 'ListRoots' to see the status of policy types for a specified root, and then use this operation.
-- This operation can be called only from the organization's management account.
-- To view the status of available policy types in the organization, use 'DescribeOrganization' .
module Network.AWS.Organizations.DisablePolicyType
  ( -- * Creating a request
    DisablePolicyType (..),
    mkDisablePolicyType,

    -- ** Request lenses
    dptRootId,
    dptPolicyType,

    -- * Destructuring the response
    DisablePolicyTypeResponse (..),
    mkDisablePolicyTypeResponse,

    -- ** Response lenses
    dptrrsRoot,
    dptrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisablePolicyType' smart constructor.
data DisablePolicyType = DisablePolicyType'
  { -- | The unique identifier (ID) of the root in which you want to disable a policy type. You can get the ID from the 'ListRoots' operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
    rootId :: Types.RootId,
    -- | The policy type that you want to disable in this root. You can specify one of the following values:
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
    policyType :: Types.PolicyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisablePolicyType' value with any optional fields omitted.
mkDisablePolicyType ::
  -- | 'rootId'
  Types.RootId ->
  -- | 'policyType'
  Types.PolicyType ->
  DisablePolicyType
mkDisablePolicyType rootId policyType =
  DisablePolicyType' {rootId, policyType}

-- | The unique identifier (ID) of the root in which you want to disable a policy type. You can get the ID from the 'ListRoots' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'rootId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptRootId :: Lens.Lens' DisablePolicyType Types.RootId
dptRootId = Lens.field @"rootId"
{-# DEPRECATED dptRootId "Use generic-lens or generic-optics with 'rootId' instead." #-}

-- | The policy type that you want to disable in this root. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
--
--
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptPolicyType :: Lens.Lens' DisablePolicyType Types.PolicyType
dptPolicyType = Lens.field @"policyType"
{-# DEPRECATED dptPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

instance Core.FromJSON DisablePolicyType where
  toJSON DisablePolicyType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RootId" Core..= rootId),
            Core.Just ("PolicyType" Core..= policyType)
          ]
      )

instance Core.AWSRequest DisablePolicyType where
  type Rs DisablePolicyType = DisablePolicyTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSOrganizationsV20161128.DisablePolicyType")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DisablePolicyTypeResponse'
            Core.<$> (x Core..:? "Root") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisablePolicyTypeResponse' smart constructor.
data DisablePolicyTypeResponse = DisablePolicyTypeResponse'
  { -- | A structure that shows the root with the updated list of enabled policy types.
    root :: Core.Maybe Types.Root,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisablePolicyTypeResponse' value with any optional fields omitted.
mkDisablePolicyTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisablePolicyTypeResponse
mkDisablePolicyTypeResponse responseStatus =
  DisablePolicyTypeResponse' {root = Core.Nothing, responseStatus}

-- | A structure that shows the root with the updated list of enabled policy types.
--
-- /Note:/ Consider using 'root' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsRoot :: Lens.Lens' DisablePolicyTypeResponse (Core.Maybe Types.Root)
dptrrsRoot = Lens.field @"root"
{-# DEPRECATED dptrrsRoot "Use generic-lens or generic-optics with 'root' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsResponseStatus :: Lens.Lens' DisablePolicyTypeResponse Core.Int
dptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
