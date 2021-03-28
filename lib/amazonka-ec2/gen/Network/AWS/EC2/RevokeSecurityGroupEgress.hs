{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RevokeSecurityGroupEgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Removes the specified egress rules from a security group for EC2-VPC. This action does not apply to security groups for use in EC2-Classic. To remove a rule, the values that you specify (for example, ports) must match the existing rule's values exactly.
--
-- Each rule consists of the protocol and the IPv4 or IPv6 CIDR range or source security group. For the TCP and UDP protocols, you must also specify the destination port or range of ports. For the ICMP protocol, you must also specify the ICMP type and code. If the security group rule has a description, you do not have to specify the description to revoke the rule.
-- Rule changes are propagated to instances within the security group as quickly as possible. However, a small delay might occur.
module Network.AWS.EC2.RevokeSecurityGroupEgress
    (
    -- * Creating a request
      RevokeSecurityGroupEgress (..)
    , mkRevokeSecurityGroupEgress
    -- ** Request lenses
    , rsgeGroupId
    , rsgeCidrIp
    , rsgeDryRun
    , rsgeFromPort
    , rsgeIpPermissions
    , rsgeIpProtocol
    , rsgeSourceSecurityGroupName
    , rsgeSourceSecurityGroupOwnerId
    , rsgeToPort

    -- * Destructuring the response
    , RevokeSecurityGroupEgressResponse (..)
    , mkRevokeSecurityGroupEgressResponse
    -- ** Response lenses
    , rsgerrsReturn
    , rsgerrsUnknownIpPermissions
    , rsgerrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRevokeSecurityGroupEgress' smart constructor.
data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress'
  { groupId :: Types.SecurityGroupId
    -- ^ The ID of the security group.
  , cidrIp :: Core.Maybe Core.Text
    -- ^ Not supported. Use a set of IP permissions to specify the CIDR.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , fromPort :: Core.Maybe Core.Int
    -- ^ Not supported. Use a set of IP permissions to specify the port.
  , ipPermissions :: Core.Maybe [Types.IpPermission]
    -- ^ The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
  , ipProtocol :: Core.Maybe Core.Text
    -- ^ Not supported. Use a set of IP permissions to specify the protocol name or number.
  , sourceSecurityGroupName :: Core.Maybe Core.Text
    -- ^ Not supported. Use a set of IP permissions to specify a destination security group.
  , sourceSecurityGroupOwnerId :: Core.Maybe Core.Text
    -- ^ Not supported. Use a set of IP permissions to specify a destination security group.
  , toPort :: Core.Maybe Core.Int
    -- ^ Not supported. Use a set of IP permissions to specify the port.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeSecurityGroupEgress' value with any optional fields omitted.
mkRevokeSecurityGroupEgress
    :: Types.SecurityGroupId -- ^ 'groupId'
    -> RevokeSecurityGroupEgress
mkRevokeSecurityGroupEgress groupId
  = RevokeSecurityGroupEgress'{groupId, cidrIp = Core.Nothing,
                               dryRun = Core.Nothing, fromPort = Core.Nothing,
                               ipPermissions = Core.Nothing, ipProtocol = Core.Nothing,
                               sourceSecurityGroupName = Core.Nothing,
                               sourceSecurityGroupOwnerId = Core.Nothing, toPort = Core.Nothing}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeGroupId :: Lens.Lens' RevokeSecurityGroupEgress Types.SecurityGroupId
rsgeGroupId = Lens.field @"groupId"
{-# INLINEABLE rsgeGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify the CIDR.
--
-- /Note:/ Consider using 'cidrIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeCidrIp :: Lens.Lens' RevokeSecurityGroupEgress (Core.Maybe Core.Text)
rsgeCidrIp = Lens.field @"cidrIp"
{-# INLINEABLE rsgeCidrIp #-}
{-# DEPRECATED cidrIp "Use generic-lens or generic-optics with 'cidrIp' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeDryRun :: Lens.Lens' RevokeSecurityGroupEgress (Core.Maybe Core.Bool)
rsgeDryRun = Lens.field @"dryRun"
{-# INLINEABLE rsgeDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify the port.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeFromPort :: Lens.Lens' RevokeSecurityGroupEgress (Core.Maybe Core.Int)
rsgeFromPort = Lens.field @"fromPort"
{-# INLINEABLE rsgeFromPort #-}
{-# DEPRECATED fromPort "Use generic-lens or generic-optics with 'fromPort' instead"  #-}

-- | The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeIpPermissions :: Lens.Lens' RevokeSecurityGroupEgress (Core.Maybe [Types.IpPermission])
rsgeIpPermissions = Lens.field @"ipPermissions"
{-# INLINEABLE rsgeIpPermissions #-}
{-# DEPRECATED ipPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify the protocol name or number.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeIpProtocol :: Lens.Lens' RevokeSecurityGroupEgress (Core.Maybe Core.Text)
rsgeIpProtocol = Lens.field @"ipProtocol"
{-# INLINEABLE rsgeIpProtocol #-}
{-# DEPRECATED ipProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify a destination security group.
--
-- /Note:/ Consider using 'sourceSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeSourceSecurityGroupName :: Lens.Lens' RevokeSecurityGroupEgress (Core.Maybe Core.Text)
rsgeSourceSecurityGroupName = Lens.field @"sourceSecurityGroupName"
{-# INLINEABLE rsgeSourceSecurityGroupName #-}
{-# DEPRECATED sourceSecurityGroupName "Use generic-lens or generic-optics with 'sourceSecurityGroupName' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify a destination security group.
--
-- /Note:/ Consider using 'sourceSecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeSourceSecurityGroupOwnerId :: Lens.Lens' RevokeSecurityGroupEgress (Core.Maybe Core.Text)
rsgeSourceSecurityGroupOwnerId = Lens.field @"sourceSecurityGroupOwnerId"
{-# INLINEABLE rsgeSourceSecurityGroupOwnerId #-}
{-# DEPRECATED sourceSecurityGroupOwnerId "Use generic-lens or generic-optics with 'sourceSecurityGroupOwnerId' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify the port.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeToPort :: Lens.Lens' RevokeSecurityGroupEgress (Core.Maybe Core.Int)
rsgeToPort = Lens.field @"toPort"
{-# INLINEABLE rsgeToPort #-}
{-# DEPRECATED toPort "Use generic-lens or generic-optics with 'toPort' instead"  #-}

instance Core.ToQuery RevokeSecurityGroupEgress where
        toQuery RevokeSecurityGroupEgress{..}
          = Core.toQueryPair "Action"
              ("RevokeSecurityGroupEgress" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "GroupId" groupId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "CidrIp") cidrIp
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FromPort") fromPort
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

instance Core.ToHeaders RevokeSecurityGroupEgress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RevokeSecurityGroupEgress where
        type Rs RevokeSecurityGroupEgress =
             RevokeSecurityGroupEgressResponse
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
                 RevokeSecurityGroupEgressResponse' Core.<$>
                   (x Core..@? "return") Core.<*>
                     x Core..@? "unknownIpPermissionSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRevokeSecurityGroupEgressResponse' smart constructor.
data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, returns an error.
  , unknownIpPermissions :: Core.Maybe [Types.IpPermission]
    -- ^ The outbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeSecurityGroupEgressResponse' value with any optional fields omitted.
mkRevokeSecurityGroupEgressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RevokeSecurityGroupEgressResponse
mkRevokeSecurityGroupEgressResponse responseStatus
  = RevokeSecurityGroupEgressResponse'{return = Core.Nothing,
                                       unknownIpPermissions = Core.Nothing, responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgerrsReturn :: Lens.Lens' RevokeSecurityGroupEgressResponse (Core.Maybe Core.Bool)
rsgerrsReturn = Lens.field @"return"
{-# INLINEABLE rsgerrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The outbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter. 
--
-- /Note:/ Consider using 'unknownIpPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgerrsUnknownIpPermissions :: Lens.Lens' RevokeSecurityGroupEgressResponse (Core.Maybe [Types.IpPermission])
rsgerrsUnknownIpPermissions = Lens.field @"unknownIpPermissions"
{-# INLINEABLE rsgerrsUnknownIpPermissions #-}
{-# DEPRECATED unknownIpPermissions "Use generic-lens or generic-optics with 'unknownIpPermissions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgerrsResponseStatus :: Lens.Lens' RevokeSecurityGroupEgressResponse Core.Int
rsgerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rsgerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
