{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AuthorizeSecurityGroupEgress (..),
    mkAuthorizeSecurityGroupEgress,

    -- ** Request lenses
    asgeGroupId,
    asgeCidrIp,
    asgeDryRun,
    asgeFromPort,
    asgeIpPermissions,
    asgeIpProtocol,
    asgeSourceSecurityGroupName,
    asgeSourceSecurityGroupOwnerId,
    asgeToPort,

    -- * Destructuring the response
    AuthorizeSecurityGroupEgressResponse (..),
    mkAuthorizeSecurityGroupEgressResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAuthorizeSecurityGroupEgress' smart constructor.
data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress'
  { -- | The ID of the security group.
    groupId :: Types.SecurityGroupId,
    -- | Not supported. Use a set of IP permissions to specify the CIDR.
    cidrIp :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Not supported. Use a set of IP permissions to specify the port.
    fromPort :: Core.Maybe Core.Int,
    -- | The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
    ipPermissions :: Core.Maybe [Types.IpPermission],
    -- | Not supported. Use a set of IP permissions to specify the protocol name or number.
    ipProtocol :: Core.Maybe Types.String,
    -- | Not supported. Use a set of IP permissions to specify a destination security group.
    sourceSecurityGroupName :: Core.Maybe Types.String,
    -- | Not supported. Use a set of IP permissions to specify a destination security group.
    sourceSecurityGroupOwnerId :: Core.Maybe Types.String,
    -- | Not supported. Use a set of IP permissions to specify the port.
    toPort :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeSecurityGroupEgress' value with any optional fields omitted.
mkAuthorizeSecurityGroupEgress ::
  -- | 'groupId'
  Types.SecurityGroupId ->
  AuthorizeSecurityGroupEgress
mkAuthorizeSecurityGroupEgress groupId =
  AuthorizeSecurityGroupEgress'
    { groupId,
      cidrIp = Core.Nothing,
      dryRun = Core.Nothing,
      fromPort = Core.Nothing,
      ipPermissions = Core.Nothing,
      ipProtocol = Core.Nothing,
      sourceSecurityGroupName = Core.Nothing,
      sourceSecurityGroupOwnerId = Core.Nothing,
      toPort = Core.Nothing
    }

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeGroupId :: Lens.Lens' AuthorizeSecurityGroupEgress Types.SecurityGroupId
asgeGroupId = Lens.field @"groupId"
{-# DEPRECATED asgeGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | Not supported. Use a set of IP permissions to specify the CIDR.
--
-- /Note:/ Consider using 'cidrIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeCidrIp :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Types.String)
asgeCidrIp = Lens.field @"cidrIp"
{-# DEPRECATED asgeCidrIp "Use generic-lens or generic-optics with 'cidrIp' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeDryRun :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Core.Bool)
asgeDryRun = Lens.field @"dryRun"
{-# DEPRECATED asgeDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Not supported. Use a set of IP permissions to specify the port.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeFromPort :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Core.Int)
asgeFromPort = Lens.field @"fromPort"
{-# DEPRECATED asgeFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeIpPermissions :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe [Types.IpPermission])
asgeIpPermissions = Lens.field @"ipPermissions"
{-# DEPRECATED asgeIpPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

-- | Not supported. Use a set of IP permissions to specify the protocol name or number.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeIpProtocol :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Types.String)
asgeIpProtocol = Lens.field @"ipProtocol"
{-# DEPRECATED asgeIpProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead." #-}

-- | Not supported. Use a set of IP permissions to specify a destination security group.
--
-- /Note:/ Consider using 'sourceSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeSourceSecurityGroupName :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Types.String)
asgeSourceSecurityGroupName = Lens.field @"sourceSecurityGroupName"
{-# DEPRECATED asgeSourceSecurityGroupName "Use generic-lens or generic-optics with 'sourceSecurityGroupName' instead." #-}

-- | Not supported. Use a set of IP permissions to specify a destination security group.
--
-- /Note:/ Consider using 'sourceSecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeSourceSecurityGroupOwnerId :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Types.String)
asgeSourceSecurityGroupOwnerId = Lens.field @"sourceSecurityGroupOwnerId"
{-# DEPRECATED asgeSourceSecurityGroupOwnerId "Use generic-lens or generic-optics with 'sourceSecurityGroupOwnerId' instead." #-}

-- | Not supported. Use a set of IP permissions to specify the port.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeToPort :: Lens.Lens' AuthorizeSecurityGroupEgress (Core.Maybe Core.Int)
asgeToPort = Lens.field @"toPort"
{-# DEPRECATED asgeToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

instance Core.AWSRequest AuthorizeSecurityGroupEgress where
  type
    Rs AuthorizeSecurityGroupEgress =
      AuthorizeSecurityGroupEgressResponse
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
            ( Core.pure ("Action", "AuthorizeSecurityGroupEgress")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "GroupId" groupId)
                Core.<> (Core.toQueryValue "CidrIp" Core.<$> cidrIp)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "FromPort" Core.<$> fromPort)
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
    Response.receiveNull AuthorizeSecurityGroupEgressResponse'

-- | /See:/ 'mkAuthorizeSecurityGroupEgressResponse' smart constructor.
data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeSecurityGroupEgressResponse' value with any optional fields omitted.
mkAuthorizeSecurityGroupEgressResponse ::
  AuthorizeSecurityGroupEgressResponse
mkAuthorizeSecurityGroupEgressResponse =
  AuthorizeSecurityGroupEgressResponse'
