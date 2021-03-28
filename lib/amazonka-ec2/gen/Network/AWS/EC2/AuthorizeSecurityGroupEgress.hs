{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupEgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Adds the specified egress rules to a security group for use with a VPC.
--
-- An outbound rule permits instances to send traffic to the specified IPv4 or IPv6 CIDR address ranges, or to the instances associated with the specified destination security groups.
-- You specify a protocol for each rule (for example, TCP). For the TCP and UDP protocols, you must also specify the destination port or port range. For the ICMP protocol, you must also specify the ICMP type and code. You can use -1 for the type or code to mean all types or all codes.
-- Rule changes are propagated to affected instances as quickly as possible. However, a small delay might occur.
-- For more information about VPC security group limits, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html Amazon VPC Limits> .
module Network.AWS.EC2.AuthorizeSecurityGroupEgress
    (
    -- * Creating a request
      AuthorizeSecurityGroupEgress (..)
    , mkAuthorizeSecurityGroupEgress
    -- ** Request lenses
    , asgeGroupId
    , asgeCidrIp
    , asgeDryRun
    , asgeFromPort
    , asgeIpPermissions
    , asgeIpProtocol
    , asgeSourceSecurityGroupName
    , asgeSourceSecurityGroupOwnerId
    , asgeToPort

    -- * Destructuring the response
    , AuthorizeSecurityGroupEgressResponse (..)
    , mkAuthorizeSecurityGroupEgressResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAuthorizeSecurityGroupEgress' smart constructor.
data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress'
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

-- | Creates a 'AuthorizeSecurityGroupEgress' value with any optional fields omitted.
mkAuthorizeSecurityGroupEgress
    :: Types.SecurityGroupId -- ^ 'groupId'
    -> AuthorizeSecurityGroupEgress
mkAuthorizeSecurityGroupEgress groupId
  = AuthorizeSecurityGroupEgress'{groupId, cidrIp = Core.Nothing,
                                  dryRun = Core.Nothing, fromPort = Core.Nothing,
                                  ipPermissions = Core.Nothing, ipProtocol = Core.Nothing,
                                  sourceSecurityGroupName = Core.Nothing,
                                  sourceSecurityGroupOwnerId = Core.Nothing, toPort = Core.Nothing}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeGroupId :: Lens.Lens' AuthorizeSecurityGroupEgress Types.SecurityGroupId
asgeGroupId = Lens.field @"groupId"
{-# INLINEABLE asgeGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify the CIDR.
--
-- /Note:/ Consider using 'cidrIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeCidrIp :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Core.Text)
asgeCidrIp = Lens.field @"cidrIp"
{-# INLINEABLE asgeCidrIp #-}
{-# DEPRECATED cidrIp "Use generic-lens or generic-optics with 'cidrIp' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeDryRun :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Core.Bool)
asgeDryRun = Lens.field @"dryRun"
{-# INLINEABLE asgeDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify the port.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeFromPort :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Core.Int)
asgeFromPort = Lens.field @"fromPort"
{-# INLINEABLE asgeFromPort #-}
{-# DEPRECATED fromPort "Use generic-lens or generic-optics with 'fromPort' instead"  #-}

-- | The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeIpPermissions :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe [Types.IpPermission])
asgeIpPermissions = Lens.field @"ipPermissions"
{-# INLINEABLE asgeIpPermissions #-}
{-# DEPRECATED ipPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify the protocol name or number.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeIpProtocol :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Core.Text)
asgeIpProtocol = Lens.field @"ipProtocol"
{-# INLINEABLE asgeIpProtocol #-}
{-# DEPRECATED ipProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify a destination security group.
--
-- /Note:/ Consider using 'sourceSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeSourceSecurityGroupName :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Core.Text)
asgeSourceSecurityGroupName = Lens.field @"sourceSecurityGroupName"
{-# INLINEABLE asgeSourceSecurityGroupName #-}
{-# DEPRECATED sourceSecurityGroupName "Use generic-lens or generic-optics with 'sourceSecurityGroupName' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify a destination security group.
--
-- /Note:/ Consider using 'sourceSecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeSourceSecurityGroupOwnerId :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Core.Text)
asgeSourceSecurityGroupOwnerId = Lens.field @"sourceSecurityGroupOwnerId"
{-# INLINEABLE asgeSourceSecurityGroupOwnerId #-}
{-# DEPRECATED sourceSecurityGroupOwnerId "Use generic-lens or generic-optics with 'sourceSecurityGroupOwnerId' instead"  #-}

-- | Not supported. Use a set of IP permissions to specify the port.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeToPort :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Core.Int)
asgeToPort = Lens.field @"toPort"
{-# INLINEABLE asgeToPort #-}
{-# DEPRECATED toPort "Use generic-lens or generic-optics with 'toPort' instead"  #-}

instance Core.ToQuery AuthorizeSecurityGroupEgress where
        toQuery AuthorizeSecurityGroupEgress{..}
          = Core.toQueryPair "Action"
              ("AuthorizeSecurityGroupEgress" :: Core.Text)
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

instance Core.ToHeaders AuthorizeSecurityGroupEgress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AuthorizeSecurityGroupEgress where
        type Rs AuthorizeSecurityGroupEgress =
             AuthorizeSecurityGroupEgressResponse
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
          = Response.receiveNull AuthorizeSecurityGroupEgressResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAuthorizeSecurityGroupEgressResponse' smart constructor.
data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeSecurityGroupEgressResponse' value with any optional fields omitted.
mkAuthorizeSecurityGroupEgressResponse
    :: AuthorizeSecurityGroupEgressResponse
mkAuthorizeSecurityGroupEgressResponse
  = AuthorizeSecurityGroupEgressResponse'
