{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RevokeSecurityGroupIngress (..)
    , mkRevokeSecurityGroupIngress
    -- ** Request lenses
    , rsgiCidrIp
    , rsgiDryRun
    , rsgiFromPort
    , rsgiGroupId
    , rsgiGroupName
    , rsgiIpPermissions
    , rsgiIpProtocol
    , rsgiSourceSecurityGroupName
    , rsgiSourceSecurityGroupOwnerId
    , rsgiToPort

    -- * Destructuring the response
    , RevokeSecurityGroupIngressResponse (..)
    , mkRevokeSecurityGroupIngressResponse
    -- ** Response lenses
    , rsgirrsReturn
    , rsgirrsUnknownIpPermissions
    , rsgirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRevokeSecurityGroupIngress' smart constructor.
data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress'
  { cidrIp :: Core.Maybe Core.Text
    -- ^ The CIDR IP address range. You can't specify this parameter when specifying a source security group.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , fromPort :: Core.Maybe Core.Int
    -- ^ The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all ICMP types.
  , groupId :: Core.Maybe Types.GroupId
    -- ^ The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
  , ipPermissions :: Core.Maybe [Types.IpPermission]
    -- ^ The sets of IP permissions. You can't specify a source security group and a CIDR IP address range in the same set of permissions.
  , ipProtocol :: Core.Maybe Core.Text
    -- ^ The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). Use @-1@ to specify all.
  , sourceSecurityGroupName :: Core.Maybe Core.Text
    -- ^ [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. For EC2-VPC, the source security group must be in the same VPC. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
  , sourceSecurityGroupOwnerId :: Core.Maybe Core.Text
    -- ^ [EC2-Classic] The AWS account ID of the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
  , toPort :: Core.Maybe Core.Int
    -- ^ The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all ICMP codes for the ICMP type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeSecurityGroupIngress' value with any optional fields omitted.
mkRevokeSecurityGroupIngress
    :: RevokeSecurityGroupIngress
mkRevokeSecurityGroupIngress
  = RevokeSecurityGroupIngress'{cidrIp = Core.Nothing,
                                dryRun = Core.Nothing, fromPort = Core.Nothing,
                                groupId = Core.Nothing, groupName = Core.Nothing,
                                ipPermissions = Core.Nothing, ipProtocol = Core.Nothing,
                                sourceSecurityGroupName = Core.Nothing,
                                sourceSecurityGroupOwnerId = Core.Nothing, toPort = Core.Nothing}

-- | The CIDR IP address range. You can't specify this parameter when specifying a source security group.
--
-- /Note:/ Consider using 'cidrIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiCidrIp :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Core.Text)
rsgiCidrIp = Lens.field @"cidrIp"
{-# INLINEABLE rsgiCidrIp #-}
{-# DEPRECATED cidrIp "Use generic-lens or generic-optics with 'cidrIp' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiDryRun :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Core.Bool)
rsgiDryRun = Lens.field @"dryRun"
{-# INLINEABLE rsgiDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all ICMP types.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiFromPort :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Core.Int)
rsgiFromPort = Lens.field @"fromPort"
{-# INLINEABLE rsgiFromPort #-}
{-# DEPRECATED fromPort "Use generic-lens or generic-optics with 'fromPort' instead"  #-}

-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiGroupId :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Types.GroupId)
rsgiGroupId = Lens.field @"groupId"
{-# INLINEABLE rsgiGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiGroupName :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Types.GroupName)
rsgiGroupName = Lens.field @"groupName"
{-# INLINEABLE rsgiGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The sets of IP permissions. You can't specify a source security group and a CIDR IP address range in the same set of permissions.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiIpPermissions :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe [Types.IpPermission])
rsgiIpPermissions = Lens.field @"ipPermissions"
{-# INLINEABLE rsgiIpPermissions #-}
{-# DEPRECATED ipPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead"  #-}

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). Use @-1@ to specify all.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiIpProtocol :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Core.Text)
rsgiIpProtocol = Lens.field @"ipProtocol"
{-# INLINEABLE rsgiIpProtocol #-}
{-# DEPRECATED ipProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead"  #-}

-- | [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. For EC2-VPC, the source security group must be in the same VPC. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
--
-- /Note:/ Consider using 'sourceSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiSourceSecurityGroupName :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Core.Text)
rsgiSourceSecurityGroupName = Lens.field @"sourceSecurityGroupName"
{-# INLINEABLE rsgiSourceSecurityGroupName #-}
{-# DEPRECATED sourceSecurityGroupName "Use generic-lens or generic-optics with 'sourceSecurityGroupName' instead"  #-}

-- | [EC2-Classic] The AWS account ID of the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
--
-- /Note:/ Consider using 'sourceSecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiSourceSecurityGroupOwnerId :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Core.Text)
rsgiSourceSecurityGroupOwnerId = Lens.field @"sourceSecurityGroupOwnerId"
{-# INLINEABLE rsgiSourceSecurityGroupOwnerId #-}
{-# DEPRECATED sourceSecurityGroupOwnerId "Use generic-lens or generic-optics with 'sourceSecurityGroupOwnerId' instead"  #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all ICMP codes for the ICMP type.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiToPort :: Lens.Lens' RevokeSecurityGroupIngress (Core.Maybe Core.Int)
rsgiToPort = Lens.field @"toPort"
{-# INLINEABLE rsgiToPort #-}
{-# DEPRECATED toPort "Use generic-lens or generic-optics with 'toPort' instead"  #-}

instance Core.ToQuery RevokeSecurityGroupIngress where
        toQuery RevokeSecurityGroupIngress{..}
          = Core.toQueryPair "Action"
              ("RevokeSecurityGroupIngress" :: Core.Text)
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

instance Core.ToHeaders RevokeSecurityGroupIngress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RevokeSecurityGroupIngress where
        type Rs RevokeSecurityGroupIngress =
             RevokeSecurityGroupIngressResponse
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
          = Response.receiveXML
              (\ s h x ->
                 RevokeSecurityGroupIngressResponse' Core.<$>
                   (x Core..@? "return") Core.<*>
                     x Core..@? "unknownIpPermissionSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRevokeSecurityGroupIngressResponse' smart constructor.
data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, returns an error.
  , unknownIpPermissions :: Core.Maybe [Types.IpPermission]
    -- ^ The inbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeSecurityGroupIngressResponse' value with any optional fields omitted.
mkRevokeSecurityGroupIngressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RevokeSecurityGroupIngressResponse
mkRevokeSecurityGroupIngressResponse responseStatus
  = RevokeSecurityGroupIngressResponse'{return = Core.Nothing,
                                        unknownIpPermissions = Core.Nothing, responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgirrsReturn :: Lens.Lens' RevokeSecurityGroupIngressResponse (Core.Maybe Core.Bool)
rsgirrsReturn = Lens.field @"return"
{-# INLINEABLE rsgirrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The inbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter. 
--
-- /Note:/ Consider using 'unknownIpPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgirrsUnknownIpPermissions :: Lens.Lens' RevokeSecurityGroupIngressResponse (Core.Maybe [Types.IpPermission])
rsgirrsUnknownIpPermissions = Lens.field @"unknownIpPermissions"
{-# INLINEABLE rsgirrsUnknownIpPermissions #-}
{-# DEPRECATED unknownIpPermissions "Use generic-lens or generic-optics with 'unknownIpPermissions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgirrsResponseStatus :: Lens.Lens' RevokeSecurityGroupIngressResponse Core.Int
rsgirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rsgirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
