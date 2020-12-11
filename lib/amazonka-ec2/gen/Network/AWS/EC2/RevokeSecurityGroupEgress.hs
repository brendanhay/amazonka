{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RevokeSecurityGroupEgress (..),
    mkRevokeSecurityGroupEgress,

    -- ** Request lenses
    rsgeFromPort,
    rsgeIPPermissions,
    rsgeIPProtocol,
    rsgeToPort,
    rsgeCidrIP,
    rsgeSourceSecurityGroupOwnerId,
    rsgeSourceSecurityGroupName,
    rsgeDryRun,
    rsgeGroupId,

    -- * Destructuring the response
    RevokeSecurityGroupEgressResponse (..),
    mkRevokeSecurityGroupEgressResponse,

    -- ** Response lenses
    rsgersReturn,
    rsgersUnknownIPPermissions,
    rsgersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRevokeSecurityGroupEgress' smart constructor.
data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress'
  { fromPort ::
      Lude.Maybe Lude.Int,
    ipPermissions ::
      Lude.Maybe [IPPermission],
    ipProtocol :: Lude.Maybe Lude.Text,
    toPort :: Lude.Maybe Lude.Int,
    cidrIP :: Lude.Maybe Lude.Text,
    sourceSecurityGroupOwnerId ::
      Lude.Maybe Lude.Text,
    sourceSecurityGroupName ::
      Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    groupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeSecurityGroupEgress' with the minimum fields required to make a request.
--
-- * 'cidrIP' - Not supported. Use a set of IP permissions to specify the CIDR.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'fromPort' - Not supported. Use a set of IP permissions to specify the port.
-- * 'groupId' - The ID of the security group.
-- * 'ipPermissions' - The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
-- * 'ipProtocol' - Not supported. Use a set of IP permissions to specify the protocol name or number.
-- * 'sourceSecurityGroupName' - Not supported. Use a set of IP permissions to specify a destination security group.
-- * 'sourceSecurityGroupOwnerId' - Not supported. Use a set of IP permissions to specify a destination security group.
-- * 'toPort' - Not supported. Use a set of IP permissions to specify the port.
mkRevokeSecurityGroupEgress ::
  -- | 'groupId'
  Lude.Text ->
  RevokeSecurityGroupEgress
mkRevokeSecurityGroupEgress pGroupId_ =
  RevokeSecurityGroupEgress'
    { fromPort = Lude.Nothing,
      ipPermissions = Lude.Nothing,
      ipProtocol = Lude.Nothing,
      toPort = Lude.Nothing,
      cidrIP = Lude.Nothing,
      sourceSecurityGroupOwnerId = Lude.Nothing,
      sourceSecurityGroupName = Lude.Nothing,
      dryRun = Lude.Nothing,
      groupId = pGroupId_
    }

-- | Not supported. Use a set of IP permissions to specify the port.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeFromPort :: Lens.Lens' RevokeSecurityGroupEgress (Lude.Maybe Lude.Int)
rsgeFromPort = Lens.lens (fromPort :: RevokeSecurityGroupEgress -> Lude.Maybe Lude.Int) (\s a -> s {fromPort = a} :: RevokeSecurityGroupEgress)
{-# DEPRECATED rsgeFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The sets of IP permissions. You can't specify a destination security group and a CIDR IP address range in the same set of permissions.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeIPPermissions :: Lens.Lens' RevokeSecurityGroupEgress (Lude.Maybe [IPPermission])
rsgeIPPermissions = Lens.lens (ipPermissions :: RevokeSecurityGroupEgress -> Lude.Maybe [IPPermission]) (\s a -> s {ipPermissions = a} :: RevokeSecurityGroupEgress)
{-# DEPRECATED rsgeIPPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

-- | Not supported. Use a set of IP permissions to specify the protocol name or number.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeIPProtocol :: Lens.Lens' RevokeSecurityGroupEgress (Lude.Maybe Lude.Text)
rsgeIPProtocol = Lens.lens (ipProtocol :: RevokeSecurityGroupEgress -> Lude.Maybe Lude.Text) (\s a -> s {ipProtocol = a} :: RevokeSecurityGroupEgress)
{-# DEPRECATED rsgeIPProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead." #-}

-- | Not supported. Use a set of IP permissions to specify the port.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeToPort :: Lens.Lens' RevokeSecurityGroupEgress (Lude.Maybe Lude.Int)
rsgeToPort = Lens.lens (toPort :: RevokeSecurityGroupEgress -> Lude.Maybe Lude.Int) (\s a -> s {toPort = a} :: RevokeSecurityGroupEgress)
{-# DEPRECATED rsgeToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

-- | Not supported. Use a set of IP permissions to specify the CIDR.
--
-- /Note:/ Consider using 'cidrIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeCidrIP :: Lens.Lens' RevokeSecurityGroupEgress (Lude.Maybe Lude.Text)
rsgeCidrIP = Lens.lens (cidrIP :: RevokeSecurityGroupEgress -> Lude.Maybe Lude.Text) (\s a -> s {cidrIP = a} :: RevokeSecurityGroupEgress)
{-# DEPRECATED rsgeCidrIP "Use generic-lens or generic-optics with 'cidrIP' instead." #-}

-- | Not supported. Use a set of IP permissions to specify a destination security group.
--
-- /Note:/ Consider using 'sourceSecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeSourceSecurityGroupOwnerId :: Lens.Lens' RevokeSecurityGroupEgress (Lude.Maybe Lude.Text)
rsgeSourceSecurityGroupOwnerId = Lens.lens (sourceSecurityGroupOwnerId :: RevokeSecurityGroupEgress -> Lude.Maybe Lude.Text) (\s a -> s {sourceSecurityGroupOwnerId = a} :: RevokeSecurityGroupEgress)
{-# DEPRECATED rsgeSourceSecurityGroupOwnerId "Use generic-lens or generic-optics with 'sourceSecurityGroupOwnerId' instead." #-}

-- | Not supported. Use a set of IP permissions to specify a destination security group.
--
-- /Note:/ Consider using 'sourceSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeSourceSecurityGroupName :: Lens.Lens' RevokeSecurityGroupEgress (Lude.Maybe Lude.Text)
rsgeSourceSecurityGroupName = Lens.lens (sourceSecurityGroupName :: RevokeSecurityGroupEgress -> Lude.Maybe Lude.Text) (\s a -> s {sourceSecurityGroupName = a} :: RevokeSecurityGroupEgress)
{-# DEPRECATED rsgeSourceSecurityGroupName "Use generic-lens or generic-optics with 'sourceSecurityGroupName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeDryRun :: Lens.Lens' RevokeSecurityGroupEgress (Lude.Maybe Lude.Bool)
rsgeDryRun = Lens.lens (dryRun :: RevokeSecurityGroupEgress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RevokeSecurityGroupEgress)
{-# DEPRECATED rsgeDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgeGroupId :: Lens.Lens' RevokeSecurityGroupEgress Lude.Text
rsgeGroupId = Lens.lens (groupId :: RevokeSecurityGroupEgress -> Lude.Text) (\s a -> s {groupId = a} :: RevokeSecurityGroupEgress)
{-# DEPRECATED rsgeGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest RevokeSecurityGroupEgress where
  type
    Rs RevokeSecurityGroupEgress =
      RevokeSecurityGroupEgressResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RevokeSecurityGroupEgressResponse'
            Lude.<$> (x Lude..@? "return")
            Lude.<*> ( x Lude..@? "unknownIpPermissionSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RevokeSecurityGroupEgress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RevokeSecurityGroupEgress where
  toPath = Lude.const "/"

instance Lude.ToQuery RevokeSecurityGroupEgress where
  toQuery RevokeSecurityGroupEgress' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RevokeSecurityGroupEgress" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "FromPort" Lude.=: fromPort,
        Lude.toQuery
          (Lude.toQueryList "IpPermissions" Lude.<$> ipPermissions),
        "IpProtocol" Lude.=: ipProtocol,
        "ToPort" Lude.=: toPort,
        "CidrIp" Lude.=: cidrIP,
        "SourceSecurityGroupOwnerId" Lude.=: sourceSecurityGroupOwnerId,
        "SourceSecurityGroupName" Lude.=: sourceSecurityGroupName,
        "DryRun" Lude.=: dryRun,
        "GroupId" Lude.=: groupId
      ]

-- | /See:/ 'mkRevokeSecurityGroupEgressResponse' smart constructor.
data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse'
  { return ::
      Lude.Maybe Lude.Bool,
    unknownIPPermissions ::
      Lude.Maybe
        [IPPermission],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeSecurityGroupEgressResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, returns an error.
-- * 'unknownIPPermissions' - The outbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter.
mkRevokeSecurityGroupEgressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RevokeSecurityGroupEgressResponse
mkRevokeSecurityGroupEgressResponse pResponseStatus_ =
  RevokeSecurityGroupEgressResponse'
    { return = Lude.Nothing,
      unknownIPPermissions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgersReturn :: Lens.Lens' RevokeSecurityGroupEgressResponse (Lude.Maybe Lude.Bool)
rsgersReturn = Lens.lens (return :: RevokeSecurityGroupEgressResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: RevokeSecurityGroupEgressResponse)
{-# DEPRECATED rsgersReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The outbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter.
--
-- /Note:/ Consider using 'unknownIPPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgersUnknownIPPermissions :: Lens.Lens' RevokeSecurityGroupEgressResponse (Lude.Maybe [IPPermission])
rsgersUnknownIPPermissions = Lens.lens (unknownIPPermissions :: RevokeSecurityGroupEgressResponse -> Lude.Maybe [IPPermission]) (\s a -> s {unknownIPPermissions = a} :: RevokeSecurityGroupEgressResponse)
{-# DEPRECATED rsgersUnknownIPPermissions "Use generic-lens or generic-optics with 'unknownIPPermissions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgersResponseStatus :: Lens.Lens' RevokeSecurityGroupEgressResponse Lude.Int
rsgersResponseStatus = Lens.lens (responseStatus :: RevokeSecurityGroupEgressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RevokeSecurityGroupEgressResponse)
{-# DEPRECATED rsgersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
