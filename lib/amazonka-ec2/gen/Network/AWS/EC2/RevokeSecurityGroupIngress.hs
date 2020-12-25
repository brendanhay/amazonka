{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RevokeSecurityGroupIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified ingress rules from a security group. To remove a rule, the values that you specify (for example, ports) must match the existing rule's values exactly.
--
-- Each rule consists of the protocol and the CIDR range or source security group. For the TCP and UDP protocols, you must also specify the destination port or range of ports. For the ICMP protocol, you must also specify the ICMP type and code. If the security group rule has a description, you do not have to specify the description to revoke the rule.
-- Rule changes are propagated to instances within the security group as quickly as possible. However, a small delay might occur.
module Network.AWS.EC2.RevokeSecurityGroupIngress
  ( -- * Creating a request
    RevokeSecurityGroupIngress (..),
    mkRevokeSecurityGroupIngress,

    -- ** Request lenses
    rsgiCidrIp,
    rsgiDryRun,
    rsgiFromPort,
    rsgiGroupId,
    rsgiGroupName,
    rsgiIpPermissions,
    rsgiIpProtocol,
    rsgiSourceSecurityGroupName,
    rsgiSourceSecurityGroupOwnerId,
    rsgiToPort,

    -- * Destructuring the response
    RevokeSecurityGroupIngressResponse (..),
    mkRevokeSecurityGroupIngressResponse,

    -- ** Response lenses
    rsgirrsReturn,
    rsgirrsUnknownIpPermissions,
    rsgirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRevokeSecurityGroupIngress' smart constructor.
data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress'
  { -- | The CIDR IP address range. You can't specify this parameter when specifying a source security group.
    cidrIp :: Core.Maybe Types.CidrIp,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all ICMP types.
    fromPort :: Core.Maybe Core.Int,
    -- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
    groupId :: Core.Maybe Types.GroupId,
    -- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
    groupName :: Core.Maybe Types.GroupName,
    -- | The sets of IP permissions. You can't specify a source security group and a CIDR IP address range in the same set of permissions.
    ipPermissions :: Core.Maybe [Types.IpPermission],
    -- | The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). Use @-1@ to specify all.
    ipProtocol :: Core.Maybe Types.IpProtocol,
    -- | [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. For EC2-VPC, the source security group must be in the same VPC. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
    sourceSecurityGroupName :: Core.Maybe Types.SourceSecurityGroupName,
    -- | [EC2-Classic] The AWS account ID of the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
    sourceSecurityGroupOwnerId :: Core.Maybe Types.SourceSecurityGroupOwnerId,
    -- | The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all ICMP codes for the ICMP type.
    toPort :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeSecurityGroupIngress' value with any optional fields omitted.
mkRevokeSecurityGroupIngress ::
  RevokeSecurityGroupIngress
mkRevokeSecurityGroupIngress =
  RevokeSecurityGroupIngress'
    { cidrIp = Core.Nothing,
      dryRun = Core.Nothing,
      fromPort = Core.Nothing,
      groupId = Core.Nothing,
      groupName = Core.Nothing,
      ipPermissions = Core.Nothing,
      ipProtocol = Core.Nothing,
      sourceSecurityGroupName = Core.Nothing,
      sourceSecurityGroupOwnerId = Core.Nothing,
      toPort = Core.Nothing
    }

-- | The CIDR IP address range. You can't specify this parameter when specifying a source security group.
--
-- /Note:/ Consider using 'cidrIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiCidrIp :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Types.CidrIp)
rsgiCidrIp = Lens.field @"cidrIp"
{-# DEPRECATED rsgiCidrIp "Use generic-lens or generic-optics with 'cidrIp' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiDryRun :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Core.Bool)
rsgiDryRun = Lens.field @"dryRun"
{-# DEPRECATED rsgiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all ICMP types.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiFromPort :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Core.Int)
rsgiFromPort = Lens.field @"fromPort"
{-# DEPRECATED rsgiFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiGroupId :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Types.GroupId)
rsgiGroupId = Lens.field @"groupId"
{-# DEPRECATED rsgiGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiGroupName :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Types.GroupName)
rsgiGroupName = Lens.field @"groupName"
{-# DEPRECATED rsgiGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The sets of IP permissions. You can't specify a source security group and a CIDR IP address range in the same set of permissions.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiIpPermissions :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe [Types.IpPermission])
rsgiIpPermissions = Lens.field @"ipPermissions"
{-# DEPRECATED rsgiIpPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). Use @-1@ to specify all.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiIpProtocol :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Types.IpProtocol)
rsgiIpProtocol = Lens.field @"ipProtocol"
{-# DEPRECATED rsgiIpProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead." #-}

-- | [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. For EC2-VPC, the source security group must be in the same VPC. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
--
-- /Note:/ Consider using 'sourceSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiSourceSecurityGroupName :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Types.SourceSecurityGroupName)
rsgiSourceSecurityGroupName = Lens.field @"sourceSecurityGroupName"
{-# DEPRECATED rsgiSourceSecurityGroupName "Use generic-lens or generic-optics with 'sourceSecurityGroupName' instead." #-}

-- | [EC2-Classic] The AWS account ID of the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
--
-- /Note:/ Consider using 'sourceSecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiSourceSecurityGroupOwnerId :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Types.SourceSecurityGroupOwnerId)
rsgiSourceSecurityGroupOwnerId = Lens.field @"sourceSecurityGroupOwnerId"
{-# DEPRECATED rsgiSourceSecurityGroupOwnerId "Use generic-lens or generic-optics with 'sourceSecurityGroupOwnerId' instead." #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all ICMP codes for the ICMP type.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiToPort :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Core.Int)
rsgiToPort = Lens.field @"toPort"
{-# DEPRECATED rsgiToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

instance Core.AWSRequest RevokeSecurityGroupIngress where
  type
    Rs RevokeSecurityGroupIngress =
      RevokeSecurityGroupIngressResponse
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
            ( Core.pure ("Action", "RevokeSecurityGroupIngress")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "CidrIp" Core.<$> cidrIp)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "FromPort" Core.<$> fromPort)
                Core.<> (Core.toQueryValue "GroupId" Core.<$> groupId)
                Core.<> (Core.toQueryValue "GroupName" Core.<$> groupName)
                Core.<> (Core.toQueryList "IpPermissions" Core.<$> ipPermissions)
                Core.<> (Core.toQueryValue "IpProtocol" Core.<$> ipProtocol)
                Core.<> ( Core.toQueryValue "SourceSecurityGroupName"
                            Core.<$> sourceSecurityGroupName
                        )
                Core.<> ( Core.toQueryValue "SourceSecurityGroupOwnerId"
                            Core.<$> sourceSecurityGroupOwnerId
                        )
                Core.<> (Core.toQueryValue "ToPort" Core.<$> toPort)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          RevokeSecurityGroupIngressResponse'
            Core.<$> (x Core..@? "return")
            Core.<*> ( x Core..@? "unknownIpPermissionSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRevokeSecurityGroupIngressResponse' smart constructor.
data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return :: Core.Maybe Core.Bool,
    -- | The inbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter.
    unknownIpPermissions :: Core.Maybe [Types.IpPermission],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeSecurityGroupIngressResponse' value with any optional fields omitted.
mkRevokeSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RevokeSecurityGroupIngressResponse
mkRevokeSecurityGroupIngressResponse responseStatus =
  RevokeSecurityGroupIngressResponse'
    { return = Core.Nothing,
      unknownIpPermissions = Core.Nothing,
      responseStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgirrsReturn :: Lens.Lens' RevokeSecurityGroupIngressResponse (Core.Maybe Core.Bool)
rsgirrsReturn = Lens.field @"return"
{-# DEPRECATED rsgirrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The inbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter.
--
-- /Note:/ Consider using 'unknownIpPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgirrsUnknownIpPermissions :: Lens.Lens' RevokeSecurityGroupIngressResponse (Core.Maybe [Types.IpPermission])
rsgirrsUnknownIpPermissions = Lens.field @"unknownIpPermissions"
{-# DEPRECATED rsgirrsUnknownIpPermissions "Use generic-lens or generic-optics with 'unknownIpPermissions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgirrsResponseStatus :: Lens.Lens' RevokeSecurityGroupIngressResponse Core.Int
rsgirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsgirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
