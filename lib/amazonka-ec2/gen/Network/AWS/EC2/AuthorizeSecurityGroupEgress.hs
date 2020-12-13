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
    asgeFromPort,
    asgeIPPermissions,
    asgeIPProtocol,
    asgeGroupId,
    asgeToPort,
    asgeCidrIP,
    asgeSourceSecurityGroupOwnerId,
    asgeSourceSecurityGroupName,
    asgeDryRun,

    -- * Destructuring the response
    AuthorizeSecurityGroupEgressResponse (..),
    mkAuthorizeSecurityGroupEgressResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAuthorizeSecurityGroupEgress' smart constructor.
data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress'
  { -- | Not supported. Use a set of IP permissions to specify the port.
    fromPort :: Lude.Maybe Lude.Int,
    -- | The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
    ipPermissions :: Lude.Maybe [IPPermission],
    -- | Not supported. Use a set of IP permissions to specify the protocol name or number.
    ipProtocol :: Lude.Maybe Lude.Text,
    -- | The ID of the security group.
    groupId :: Lude.Text,
    -- | Not supported. Use a set of IP permissions to specify the port.
    toPort :: Lude.Maybe Lude.Int,
    -- | Not supported. Use a set of IP permissions to specify the CIDR.
    cidrIP :: Lude.Maybe Lude.Text,
    -- | Not supported. Use a set of IP permissions to specify a destination security group.
    sourceSecurityGroupOwnerId :: Lude.Maybe Lude.Text,
    -- | Not supported. Use a set of IP permissions to specify a destination security group.
    sourceSecurityGroupName :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeSecurityGroupEgress' with the minimum fields required to make a request.
--
-- * 'fromPort' - Not supported. Use a set of IP permissions to specify the port.
-- * 'ipPermissions' - The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
-- * 'ipProtocol' - Not supported. Use a set of IP permissions to specify the protocol name or number.
-- * 'groupId' - The ID of the security group.
-- * 'toPort' - Not supported. Use a set of IP permissions to specify the port.
-- * 'cidrIP' - Not supported. Use a set of IP permissions to specify the CIDR.
-- * 'sourceSecurityGroupOwnerId' - Not supported. Use a set of IP permissions to specify a destination security group.
-- * 'sourceSecurityGroupName' - Not supported. Use a set of IP permissions to specify a destination security group.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkAuthorizeSecurityGroupEgress ::
  -- | 'groupId'
  Lude.Text ->
  AuthorizeSecurityGroupEgress
mkAuthorizeSecurityGroupEgress pGroupId_ =
  AuthorizeSecurityGroupEgress'
    { fromPort = Lude.Nothing,
      ipPermissions = Lude.Nothing,
      ipProtocol = Lude.Nothing,
      groupId = pGroupId_,
      toPort = Lude.Nothing,
      cidrIP = Lude.Nothing,
      sourceSecurityGroupOwnerId = Lude.Nothing,
      sourceSecurityGroupName = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | Not supported. Use a set of IP permissions to specify the port.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeFromPort :: Lens.Lens' AuthorizeSecurityGroupEgress (Lude.Maybe Lude.Int)
asgeFromPort = Lens.lens (fromPort :: AuthorizeSecurityGroupEgress -> Lude.Maybe Lude.Int) (\s a -> s {fromPort = a} :: AuthorizeSecurityGroupEgress)
{-# DEPRECATED asgeFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeIPPermissions :: Lens.Lens' AuthorizeSecurityGroupEgress (Lude.Maybe [IPPermission])
asgeIPPermissions = Lens.lens (ipPermissions :: AuthorizeSecurityGroupEgress -> Lude.Maybe [IPPermission]) (\s a -> s {ipPermissions = a} :: AuthorizeSecurityGroupEgress)
{-# DEPRECATED asgeIPPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

-- | Not supported. Use a set of IP permissions to specify the protocol name or number.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeIPProtocol :: Lens.Lens' AuthorizeSecurityGroupEgress (Lude.Maybe Lude.Text)
asgeIPProtocol = Lens.lens (ipProtocol :: AuthorizeSecurityGroupEgress -> Lude.Maybe Lude.Text) (\s a -> s {ipProtocol = a} :: AuthorizeSecurityGroupEgress)
{-# DEPRECATED asgeIPProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead." #-}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeGroupId :: Lens.Lens' AuthorizeSecurityGroupEgress Lude.Text
asgeGroupId = Lens.lens (groupId :: AuthorizeSecurityGroupEgress -> Lude.Text) (\s a -> s {groupId = a} :: AuthorizeSecurityGroupEgress)
{-# DEPRECATED asgeGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | Not supported. Use a set of IP permissions to specify the port.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeToPort :: Lens.Lens' AuthorizeSecurityGroupEgress (Lude.Maybe Lude.Int)
asgeToPort = Lens.lens (toPort :: AuthorizeSecurityGroupEgress -> Lude.Maybe Lude.Int) (\s a -> s {toPort = a} :: AuthorizeSecurityGroupEgress)
{-# DEPRECATED asgeToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

-- | Not supported. Use a set of IP permissions to specify the CIDR.
--
-- /Note:/ Consider using 'cidrIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeCidrIP :: Lens.Lens' AuthorizeSecurityGroupEgress (Lude.Maybe Lude.Text)
asgeCidrIP = Lens.lens (cidrIP :: AuthorizeSecurityGroupEgress -> Lude.Maybe Lude.Text) (\s a -> s {cidrIP = a} :: AuthorizeSecurityGroupEgress)
{-# DEPRECATED asgeCidrIP "Use generic-lens or generic-optics with 'cidrIP' instead." #-}

-- | Not supported. Use a set of IP permissions to specify a destination security group.
--
-- /Note:/ Consider using 'sourceSecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeSourceSecurityGroupOwnerId :: Lens.Lens' AuthorizeSecurityGroupEgress (Lude.Maybe Lude.Text)
asgeSourceSecurityGroupOwnerId = Lens.lens (sourceSecurityGroupOwnerId :: AuthorizeSecurityGroupEgress -> Lude.Maybe Lude.Text) (\s a -> s {sourceSecurityGroupOwnerId = a} :: AuthorizeSecurityGroupEgress)
{-# DEPRECATED asgeSourceSecurityGroupOwnerId "Use generic-lens or generic-optics with 'sourceSecurityGroupOwnerId' instead." #-}

-- | Not supported. Use a set of IP permissions to specify a destination security group.
--
-- /Note:/ Consider using 'sourceSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeSourceSecurityGroupName :: Lens.Lens' AuthorizeSecurityGroupEgress (Lude.Maybe Lude.Text)
asgeSourceSecurityGroupName = Lens.lens (sourceSecurityGroupName :: AuthorizeSecurityGroupEgress -> Lude.Maybe Lude.Text) (\s a -> s {sourceSecurityGroupName = a} :: AuthorizeSecurityGroupEgress)
{-# DEPRECATED asgeSourceSecurityGroupName "Use generic-lens or generic-optics with 'sourceSecurityGroupName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgeDryRun :: Lens.Lens' AuthorizeSecurityGroupEgress (Lude.Maybe Lude.Bool)
asgeDryRun = Lens.lens (dryRun :: AuthorizeSecurityGroupEgress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AuthorizeSecurityGroupEgress)
{-# DEPRECATED asgeDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AuthorizeSecurityGroupEgress where
  type
    Rs AuthorizeSecurityGroupEgress =
      AuthorizeSecurityGroupEgressResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull AuthorizeSecurityGroupEgressResponse'

instance Lude.ToHeaders AuthorizeSecurityGroupEgress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AuthorizeSecurityGroupEgress where
  toPath = Lude.const "/"

instance Lude.ToQuery AuthorizeSecurityGroupEgress where
  toQuery AuthorizeSecurityGroupEgress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AuthorizeSecurityGroupEgress" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "FromPort" Lude.=: fromPort,
        Lude.toQuery
          (Lude.toQueryList "IpPermissions" Lude.<$> ipPermissions),
        "IpProtocol" Lude.=: ipProtocol,
        "GroupId" Lude.=: groupId,
        "ToPort" Lude.=: toPort,
        "CidrIp" Lude.=: cidrIP,
        "SourceSecurityGroupOwnerId" Lude.=: sourceSecurityGroupOwnerId,
        "SourceSecurityGroupName" Lude.=: sourceSecurityGroupName,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkAuthorizeSecurityGroupEgressResponse' smart constructor.
data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeSecurityGroupEgressResponse' with the minimum fields required to make a request.
mkAuthorizeSecurityGroupEgressResponse ::
  AuthorizeSecurityGroupEgressResponse
mkAuthorizeSecurityGroupEgressResponse =
  AuthorizeSecurityGroupEgressResponse'
