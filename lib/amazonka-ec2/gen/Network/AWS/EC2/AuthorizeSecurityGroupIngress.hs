{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified ingress rules to a security group.
--
-- An inbound rule permits instances to receive traffic from the specified IPv4 or IPv6 CIDR address ranges, or from the instances associated with the specified destination security groups.
-- You specify a protocol for each rule (for example, TCP). For TCP and UDP, you must also specify the destination port or port range. For ICMP/ICMPv6, you must also specify the ICMP/ICMPv6 type and code. You can use -1 to mean all types or all codes.
-- Rule changes are propagated to instances within the security group as quickly as possible. However, a small delay might occur.
-- For more information about VPC security group limits, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html Amazon VPC Limits> .
module Network.AWS.EC2.AuthorizeSecurityGroupIngress
  ( -- * Creating a request
    AuthorizeSecurityGroupIngress (..),
    mkAuthorizeSecurityGroupIngress,

    -- ** Request lenses
    asgiCidrIp,
    asgiDryRun,
    asgiFromPort,
    asgiGroupId,
    asgiGroupName,
    asgiIpPermissions,
    asgiIpProtocol,
    asgiSourceSecurityGroupName,
    asgiSourceSecurityGroupOwnerId,
    asgiToPort,

    -- * Destructuring the response
    AuthorizeSecurityGroupIngressResponse (..),
    mkAuthorizeSecurityGroupIngressResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAuthorizeSecurityGroupIngress' smart constructor.
data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress'
  { -- | The IPv4 address range, in CIDR format. You can't specify this parameter when specifying a source security group. To specify an IPv6 address range, use a set of IP permissions.
    --
    -- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
    cidrIp :: Core.Maybe Types.CidrIp,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all types. If you specify all ICMP types, you must specify all codes.
    --
    -- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
    fromPort :: Core.Maybe Core.Int,
    -- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
    groupId :: Core.Maybe Types.GroupId,
    -- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
    groupName :: Core.Maybe Types.GroupName,
    -- | The sets of IP permissions.
    ipPermissions :: Core.Maybe [Types.IpPermission],
    -- | The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). To specify @icmpv6@ , use a set of IP permissions.
    --
    -- [VPC only] Use @-1@ to specify all protocols. If you specify @-1@ or a protocol other than @tcp@ , @udp@ , or @icmp@ , traffic on all ports is allowed, regardless of any ports you specify.
    -- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
    ipProtocol :: Core.Maybe Types.IpProtocol,
    -- | [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead. For EC2-VPC, the source security group must be in the same VPC.
    sourceSecurityGroupName :: Core.Maybe Types.SourceSecurityGroupName,
    -- | [nondefault VPC] The AWS account ID for the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead.
    sourceSecurityGroupOwnerId :: Core.Maybe Types.SourceSecurityGroupOwnerId,
    -- | The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all codes. If you specify all ICMP types, you must specify all codes.
    --
    -- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
    toPort :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeSecurityGroupIngress' value with any optional fields omitted.
mkAuthorizeSecurityGroupIngress ::
  AuthorizeSecurityGroupIngress
mkAuthorizeSecurityGroupIngress =
  AuthorizeSecurityGroupIngress'
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

-- | The IPv4 address range, in CIDR format. You can't specify this parameter when specifying a source security group. To specify an IPv6 address range, use a set of IP permissions.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'cidrIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiCidrIp :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Types.CidrIp)
asgiCidrIp = Lens.field @"cidrIp"
{-# DEPRECATED asgiCidrIp "Use generic-lens or generic-optics with 'cidrIp' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiDryRun :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Core.Bool)
asgiDryRun = Lens.field @"dryRun"
{-# DEPRECATED asgiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all types. If you specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiFromPort :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Core.Int)
asgiFromPort = Lens.field @"fromPort"
{-# DEPRECATED asgiFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiGroupId :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Types.GroupId)
asgiGroupId = Lens.field @"groupId"
{-# DEPRECATED asgiGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiGroupName :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Types.GroupName)
asgiGroupName = Lens.field @"groupName"
{-# DEPRECATED asgiGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The sets of IP permissions.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiIpPermissions :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe [Types.IpPermission])
asgiIpPermissions = Lens.field @"ipPermissions"
{-# DEPRECATED asgiIpPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). To specify @icmpv6@ , use a set of IP permissions.
--
-- [VPC only] Use @-1@ to specify all protocols. If you specify @-1@ or a protocol other than @tcp@ , @udp@ , or @icmp@ , traffic on all ports is allowed, regardless of any ports you specify.
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiIpProtocol :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Types.IpProtocol)
asgiIpProtocol = Lens.field @"ipProtocol"
{-# DEPRECATED asgiIpProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead." #-}

-- | [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead. For EC2-VPC, the source security group must be in the same VPC.
--
-- /Note:/ Consider using 'sourceSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiSourceSecurityGroupName :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Types.SourceSecurityGroupName)
asgiSourceSecurityGroupName = Lens.field @"sourceSecurityGroupName"
{-# DEPRECATED asgiSourceSecurityGroupName "Use generic-lens or generic-optics with 'sourceSecurityGroupName' instead." #-}

-- | [nondefault VPC] The AWS account ID for the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead.
--
-- /Note:/ Consider using 'sourceSecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiSourceSecurityGroupOwnerId :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Types.SourceSecurityGroupOwnerId)
asgiSourceSecurityGroupOwnerId = Lens.field @"sourceSecurityGroupOwnerId"
{-# DEPRECATED asgiSourceSecurityGroupOwnerId "Use generic-lens or generic-optics with 'sourceSecurityGroupOwnerId' instead." #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all codes. If you specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiToPort :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Core.Int)
asgiToPort = Lens.field @"toPort"
{-# DEPRECATED asgiToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

instance Core.AWSRequest AuthorizeSecurityGroupIngress where
  type
    Rs AuthorizeSecurityGroupIngress =
      AuthorizeSecurityGroupIngressResponse
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
            ( Core.pure ("Action", "AuthorizeSecurityGroupIngress")
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
    Response.receiveNull AuthorizeSecurityGroupIngressResponse'

-- | /See:/ 'mkAuthorizeSecurityGroupIngressResponse' smart constructor.
data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeSecurityGroupIngressResponse' value with any optional fields omitted.
mkAuthorizeSecurityGroupIngressResponse ::
  AuthorizeSecurityGroupIngressResponse
mkAuthorizeSecurityGroupIngressResponse =
  AuthorizeSecurityGroupIngressResponse'
