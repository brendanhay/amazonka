{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of an ingress (inbound) security group rule. You can replace an existing description, or add a description to a rule that did not have one previously.
--
-- You specify the description as part of the IP permissions structure. You can remove a description for a security group rule by omitting the description parameter in the request.
module Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress
  ( -- * Creating a request
    UpdateSecurityGroupRuleDescriptionsIngress (..),
    mkUpdateSecurityGroupRuleDescriptionsIngress,

    -- ** Request lenses
    usgrdiIpPermissions,
    usgrdiDryRun,
    usgrdiGroupId,
    usgrdiGroupName,

    -- * Destructuring the response
    UpdateSecurityGroupRuleDescriptionsIngressResponse (..),
    mkUpdateSecurityGroupRuleDescriptionsIngressResponse,

    -- ** Response lenses
    usgrdirrsReturn,
    usgrdirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSecurityGroupRuleDescriptionsIngress' smart constructor.
data UpdateSecurityGroupRuleDescriptionsIngress = UpdateSecurityGroupRuleDescriptionsIngress'
  { -- | The IP permissions for the security group rule.
    ipPermissions :: [Types.IpPermission],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
    groupId :: Core.Maybe Types.SecurityGroupId,
    -- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
    groupName :: Core.Maybe Types.SecurityGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSecurityGroupRuleDescriptionsIngress' value with any optional fields omitted.
mkUpdateSecurityGroupRuleDescriptionsIngress ::
  UpdateSecurityGroupRuleDescriptionsIngress
mkUpdateSecurityGroupRuleDescriptionsIngress =
  UpdateSecurityGroupRuleDescriptionsIngress'
    { ipPermissions =
        Core.mempty,
      dryRun = Core.Nothing,
      groupId = Core.Nothing,
      groupName = Core.Nothing
    }

-- | The IP permissions for the security group rule.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdiIpPermissions :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress [Types.IpPermission]
usgrdiIpPermissions = Lens.field @"ipPermissions"
{-# DEPRECATED usgrdiIpPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdiDryRun :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress (Core.Maybe Core.Bool)
usgrdiDryRun = Lens.field @"dryRun"
{-# DEPRECATED usgrdiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdiGroupId :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress (Core.Maybe Types.SecurityGroupId)
usgrdiGroupId = Lens.field @"groupId"
{-# DEPRECATED usgrdiGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdiGroupName :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress (Core.Maybe Types.SecurityGroupName)
usgrdiGroupName = Lens.field @"groupName"
{-# DEPRECATED usgrdiGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Core.AWSRequest UpdateSecurityGroupRuleDescriptionsIngress where
  type
    Rs UpdateSecurityGroupRuleDescriptionsIngress =
      UpdateSecurityGroupRuleDescriptionsIngressResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "UpdateSecurityGroupRuleDescriptionsIngress")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "IpPermissions" ipPermissions)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "GroupId" Core.<$> groupId)
                Core.<> (Core.toQueryValue "GroupName" Core.<$> groupName)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateSecurityGroupRuleDescriptionsIngressResponse'
            Core.<$> (x Core..@? "return") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSecurityGroupRuleDescriptionsIngressResponse' smart constructor.
data UpdateSecurityGroupRuleDescriptionsIngressResponse = UpdateSecurityGroupRuleDescriptionsIngressResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSecurityGroupRuleDescriptionsIngressResponse' value with any optional fields omitted.
mkUpdateSecurityGroupRuleDescriptionsIngressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSecurityGroupRuleDescriptionsIngressResponse
mkUpdateSecurityGroupRuleDescriptionsIngressResponse responseStatus =
  UpdateSecurityGroupRuleDescriptionsIngressResponse'
    { return =
        Core.Nothing,
      responseStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdirrsReturn :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngressResponse (Core.Maybe Core.Bool)
usgrdirrsReturn = Lens.field @"return"
{-# DEPRECATED usgrdirrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdirrsResponseStatus :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngressResponse Core.Int
usgrdirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usgrdirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
