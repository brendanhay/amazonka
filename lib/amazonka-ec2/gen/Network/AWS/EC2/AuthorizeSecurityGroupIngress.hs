{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AuthorizeSecurityGroupIngress (..)
    , mkAuthorizeSecurityGroupIngress
    -- ** Request lenses
    , asgiCidrIp
    , asgiDryRun
    , asgiFromPort
    , asgiGroupId
    , asgiGroupName
    , asgiIpPermissions
    , asgiIpProtocol
    , asgiSourceSecurityGroupName
    , asgiSourceSecurityGroupOwnerId
    , asgiToPort

    -- * Destructuring the response
    , AuthorizeSecurityGroupIngressResponse (..)
    , mkAuthorizeSecurityGroupIngressResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAuthorizeSecurityGroupIngress' smart constructor.
data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress'
  { cidrIp :: Core.Maybe Core.Text
    -- ^ The IPv4 address range, in CIDR format. You can't specify this parameter when specifying a source security group. To specify an IPv6 address range, use a set of IP permissions.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , fromPort :: Core.Maybe Core.Int
    -- ^ The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all types. If you specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
  , groupId :: Core.Maybe Types.GroupId
    -- ^ The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
  , ipPermissions :: Core.Maybe [Types.IpPermission]
    -- ^ The sets of IP permissions.
  , ipProtocol :: Core.Maybe Core.Text
    -- ^ The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). To specify @icmpv6@ , use a set of IP permissions.
--
-- [VPC only] Use @-1@ to specify all protocols. If you specify @-1@ or a protocol other than @tcp@ , @udp@ , or @icmp@ , traffic on all ports is allowed, regardless of any ports you specify.
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
  , sourceSecurityGroupName :: Core.Maybe Core.Text
    -- ^ [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead. For EC2-VPC, the source security group must be in the same VPC.
  , sourceSecurityGroupOwnerId :: Core.Maybe Core.Text
    -- ^ [nondefault VPC] The AWS account ID for the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead.
  , toPort :: Core.Maybe Core.Int
    -- ^ The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all codes. If you specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeSecurityGroupIngress' value with any optional fields omitted.
mkAuthorizeSecurityGroupIngress
    :: AuthorizeSecurityGroupIngress
mkAuthorizeSecurityGroupIngress
  = AuthorizeSecurityGroupIngress'{cidrIp = Core.Nothing,
                                   dryRun = Core.Nothing, fromPort = Core.Nothing,
                                   groupId = Core.Nothing, groupName = Core.Nothing,
                                   ipPermissions = Core.Nothing, ipProtocol = Core.Nothing,
                                   sourceSecurityGroupName = Core.Nothing,
                                   sourceSecurityGroupOwnerId = Core.Nothing, toPort = Core.Nothing}

-- | The IPv4 address range, in CIDR format. You can't specify this parameter when specifying a source security group. To specify an IPv6 address range, use a set of IP permissions.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'cidrIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiCidrIp :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Core.Text)
asgiCidrIp = Lens.field @"cidrIp"
{-# INLINEABLE asgiCidrIp #-}
{-# DEPRECATED cidrIp "Use generic-lens or generic-optics with 'cidrIp' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiDryRun :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Core.Bool)
asgiDryRun = Lens.field @"dryRun"
{-# INLINEABLE asgiDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all types. If you specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiFromPort :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Core.Int)
asgiFromPort = Lens.field @"fromPort"
{-# INLINEABLE asgiFromPort #-}
{-# DEPRECATED fromPort "Use generic-lens or generic-optics with 'fromPort' instead"  #-}

-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiGroupId :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Types.GroupId)
asgiGroupId = Lens.field @"groupId"
{-# INLINEABLE asgiGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiGroupName :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Types.GroupName)
asgiGroupName = Lens.field @"groupName"
{-# INLINEABLE asgiGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The sets of IP permissions.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiIpPermissions :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe [Types.IpPermission])
asgiIpPermissions = Lens.field @"ipPermissions"
{-# INLINEABLE asgiIpPermissions #-}
{-# DEPRECATED ipPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead"  #-}

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). To specify @icmpv6@ , use a set of IP permissions.
--
-- [VPC only] Use @-1@ to specify all protocols. If you specify @-1@ or a protocol other than @tcp@ , @udp@ , or @icmp@ , traffic on all ports is allowed, regardless of any ports you specify.
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiIpProtocol :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Core.Text)
asgiIpProtocol = Lens.field @"ipProtocol"
{-# INLINEABLE asgiIpProtocol #-}
{-# DEPRECATED ipProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead"  #-}

-- | [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead. For EC2-VPC, the source security group must be in the same VPC.
--
-- /Note:/ Consider using 'sourceSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiSourceSecurityGroupName :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Core.Text)
asgiSourceSecurityGroupName = Lens.field @"sourceSecurityGroupName"
{-# INLINEABLE asgiSourceSecurityGroupName #-}
{-# DEPRECATED sourceSecurityGroupName "Use generic-lens or generic-optics with 'sourceSecurityGroupName' instead"  #-}

-- | [nondefault VPC] The AWS account ID for the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead.
--
-- /Note:/ Consider using 'sourceSecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiSourceSecurityGroupOwnerId :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Core.Text)
asgiSourceSecurityGroupOwnerId = Lens.field @"sourceSecurityGroupOwnerId"
{-# INLINEABLE asgiSourceSecurityGroupOwnerId #-}
{-# DEPRECATED sourceSecurityGroupOwnerId "Use generic-lens or generic-optics with 'sourceSecurityGroupOwnerId' instead"  #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all codes. If you specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiToPort :: Lens.Lens' AuthorizeSecurityGroupIngress (Core.Maybe Core.Int)
asgiToPort = Lens.field @"toPort"
{-# INLINEABLE asgiToPort #-}
{-# DEPRECATED toPort "Use generic-lens or generic-optics with 'toPort' instead"  #-}

instance Core.ToQuery AuthorizeSecurityGroupIngress where
        toQuery AuthorizeSecurityGroupIngress{..}
          = Core.toQueryPair "Action"
              ("AuthorizeSecurityGroupIngress" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "CidrIp") cidrIp
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FromPort") fromPort
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "GroupId") groupId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GroupName") groupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "IpPermissions")
                ipPermissions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IpProtocol") ipProtocol
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceSecurityGroupName")
                sourceSecurityGroupName
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SourceSecurityGroupOwnerId")
                sourceSecurityGroupOwnerId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "ToPort") toPort

instance Core.ToHeaders AuthorizeSecurityGroupIngress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AuthorizeSecurityGroupIngress where
        type Rs AuthorizeSecurityGroupIngress =
             AuthorizeSecurityGroupIngressResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull AuthorizeSecurityGroupIngressResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAuthorizeSecurityGroupIngressResponse' smart constructor.
data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeSecurityGroupIngressResponse' value with any optional fields omitted.
mkAuthorizeSecurityGroupIngressResponse
    :: AuthorizeSecurityGroupIngressResponse
mkAuthorizeSecurityGroupIngressResponse
  = AuthorizeSecurityGroupIngressResponse'
