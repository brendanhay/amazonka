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
    rsgiFromPort,
    rsgiIPPermissions,
    rsgiIPProtocol,
    rsgiGroupId,
    rsgiToPort,
    rsgiCidrIP,
    rsgiSourceSecurityGroupOwnerId,
    rsgiGroupName,
    rsgiSourceSecurityGroupName,
    rsgiDryRun,

    -- * Destructuring the response
    RevokeSecurityGroupIngressResponse (..),
    mkRevokeSecurityGroupIngressResponse,

    -- ** Response lenses
    rsgirsReturn,
    rsgirsUnknownIPPermissions,
    rsgirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRevokeSecurityGroupIngress' smart constructor.
data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress'
  { -- | The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all ICMP types.
    fromPort :: Lude.Maybe Lude.Int,
    -- | The sets of IP permissions. You can't specify a source security group and a CIDR IP address range in the same set of permissions.
    ipPermissions :: Lude.Maybe [IPPermission],
    -- | The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). Use @-1@ to specify all.
    ipProtocol :: Lude.Maybe Lude.Text,
    -- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
    groupId :: Lude.Maybe Lude.Text,
    -- | The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all ICMP codes for the ICMP type.
    toPort :: Lude.Maybe Lude.Int,
    -- | The CIDR IP address range. You can't specify this parameter when specifying a source security group.
    cidrIP :: Lude.Maybe Lude.Text,
    -- | [EC2-Classic] The AWS account ID of the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
    sourceSecurityGroupOwnerId :: Lude.Maybe Lude.Text,
    -- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
    groupName :: Lude.Maybe Lude.Text,
    -- | [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. For EC2-VPC, the source security group must be in the same VPC. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
    sourceSecurityGroupName :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeSecurityGroupIngress' with the minimum fields required to make a request.
--
-- * 'fromPort' - The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all ICMP types.
-- * 'ipPermissions' - The sets of IP permissions. You can't specify a source security group and a CIDR IP address range in the same set of permissions.
-- * 'ipProtocol' - The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). Use @-1@ to specify all.
-- * 'groupId' - The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
-- * 'toPort' - The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all ICMP codes for the ICMP type.
-- * 'cidrIP' - The CIDR IP address range. You can't specify this parameter when specifying a source security group.
-- * 'sourceSecurityGroupOwnerId' - [EC2-Classic] The AWS account ID of the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
-- * 'groupName' - [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
-- * 'sourceSecurityGroupName' - [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. For EC2-VPC, the source security group must be in the same VPC. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkRevokeSecurityGroupIngress ::
  RevokeSecurityGroupIngress
mkRevokeSecurityGroupIngress =
  RevokeSecurityGroupIngress'
    { fromPort = Lude.Nothing,
      ipPermissions = Lude.Nothing,
      ipProtocol = Lude.Nothing,
      groupId = Lude.Nothing,
      toPort = Lude.Nothing,
      cidrIP = Lude.Nothing,
      sourceSecurityGroupOwnerId = Lude.Nothing,
      groupName = Lude.Nothing,
      sourceSecurityGroupName = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all ICMP types.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiFromPort :: Lens.Lens' RevokeSecurityGroupIngress (Lude.Maybe Lude.Int)
rsgiFromPort = Lens.lens (fromPort :: RevokeSecurityGroupIngress -> Lude.Maybe Lude.Int) (\s a -> s {fromPort = a} :: RevokeSecurityGroupIngress)
{-# DEPRECATED rsgiFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The sets of IP permissions. You can't specify a source security group and a CIDR IP address range in the same set of permissions.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiIPPermissions :: Lens.Lens' RevokeSecurityGroupIngress (Lude.Maybe [IPPermission])
rsgiIPPermissions = Lens.lens (ipPermissions :: RevokeSecurityGroupIngress -> Lude.Maybe [IPPermission]) (\s a -> s {ipPermissions = a} :: RevokeSecurityGroupIngress)
{-# DEPRECATED rsgiIPPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). Use @-1@ to specify all.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiIPProtocol :: Lens.Lens' RevokeSecurityGroupIngress (Lude.Maybe Lude.Text)
rsgiIPProtocol = Lens.lens (ipProtocol :: RevokeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ipProtocol = a} :: RevokeSecurityGroupIngress)
{-# DEPRECATED rsgiIPProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead." #-}

-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiGroupId :: Lens.Lens' RevokeSecurityGroupIngress (Lude.Maybe Lude.Text)
rsgiGroupId = Lens.lens (groupId :: RevokeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: RevokeSecurityGroupIngress)
{-# DEPRECATED rsgiGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all ICMP codes for the ICMP type.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiToPort :: Lens.Lens' RevokeSecurityGroupIngress (Lude.Maybe Lude.Int)
rsgiToPort = Lens.lens (toPort :: RevokeSecurityGroupIngress -> Lude.Maybe Lude.Int) (\s a -> s {toPort = a} :: RevokeSecurityGroupIngress)
{-# DEPRECATED rsgiToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

-- | The CIDR IP address range. You can't specify this parameter when specifying a source security group.
--
-- /Note:/ Consider using 'cidrIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiCidrIP :: Lens.Lens' RevokeSecurityGroupIngress (Lude.Maybe Lude.Text)
rsgiCidrIP = Lens.lens (cidrIP :: RevokeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {cidrIP = a} :: RevokeSecurityGroupIngress)
{-# DEPRECATED rsgiCidrIP "Use generic-lens or generic-optics with 'cidrIP' instead." #-}

-- | [EC2-Classic] The AWS account ID of the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
--
-- /Note:/ Consider using 'sourceSecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiSourceSecurityGroupOwnerId :: Lens.Lens' RevokeSecurityGroupIngress (Lude.Maybe Lude.Text)
rsgiSourceSecurityGroupOwnerId = Lens.lens (sourceSecurityGroupOwnerId :: RevokeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {sourceSecurityGroupOwnerId = a} :: RevokeSecurityGroupIngress)
{-# DEPRECATED rsgiSourceSecurityGroupOwnerId "Use generic-lens or generic-optics with 'sourceSecurityGroupOwnerId' instead." #-}

-- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiGroupName :: Lens.Lens' RevokeSecurityGroupIngress (Lude.Maybe Lude.Text)
rsgiGroupName = Lens.lens (groupName :: RevokeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: RevokeSecurityGroupIngress)
{-# DEPRECATED rsgiGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. For EC2-VPC, the source security group must be in the same VPC. To revoke a specific rule for an IP protocol and port range, use a set of IP permissions instead.
--
-- /Note:/ Consider using 'sourceSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiSourceSecurityGroupName :: Lens.Lens' RevokeSecurityGroupIngress (Lude.Maybe Lude.Text)
rsgiSourceSecurityGroupName = Lens.lens (sourceSecurityGroupName :: RevokeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {sourceSecurityGroupName = a} :: RevokeSecurityGroupIngress)
{-# DEPRECATED rsgiSourceSecurityGroupName "Use generic-lens or generic-optics with 'sourceSecurityGroupName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgiDryRun :: Lens.Lens' RevokeSecurityGroupIngress (Lude.Maybe Lude.Bool)
rsgiDryRun = Lens.lens (dryRun :: RevokeSecurityGroupIngress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RevokeSecurityGroupIngress)
{-# DEPRECATED rsgiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest RevokeSecurityGroupIngress where
  type
    Rs RevokeSecurityGroupIngress =
      RevokeSecurityGroupIngressResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RevokeSecurityGroupIngressResponse'
            Lude.<$> (x Lude..@? "return")
            Lude.<*> ( x Lude..@? "unknownIpPermissionSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RevokeSecurityGroupIngress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RevokeSecurityGroupIngress where
  toPath = Lude.const "/"

instance Lude.ToQuery RevokeSecurityGroupIngress where
  toQuery RevokeSecurityGroupIngress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RevokeSecurityGroupIngress" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "FromPort" Lude.=: fromPort,
        Lude.toQuery
          (Lude.toQueryList "IpPermissions" Lude.<$> ipPermissions),
        "IpProtocol" Lude.=: ipProtocol,
        "GroupId" Lude.=: groupId,
        "ToPort" Lude.=: toPort,
        "CidrIp" Lude.=: cidrIP,
        "SourceSecurityGroupOwnerId" Lude.=: sourceSecurityGroupOwnerId,
        "GroupName" Lude.=: groupName,
        "SourceSecurityGroupName" Lude.=: sourceSecurityGroupName,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkRevokeSecurityGroupIngressResponse' smart constructor.
data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return :: Lude.Maybe Lude.Bool,
    -- | The inbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter.
    unknownIPPermissions :: Lude.Maybe [IPPermission],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeSecurityGroupIngressResponse' with the minimum fields required to make a request.
--
-- * 'return' - Returns @true@ if the request succeeds; otherwise, returns an error.
-- * 'unknownIPPermissions' - The inbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter.
-- * 'responseStatus' - The response status code.
mkRevokeSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RevokeSecurityGroupIngressResponse
mkRevokeSecurityGroupIngressResponse pResponseStatus_ =
  RevokeSecurityGroupIngressResponse'
    { return = Lude.Nothing,
      unknownIPPermissions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgirsReturn :: Lens.Lens' RevokeSecurityGroupIngressResponse (Lude.Maybe Lude.Bool)
rsgirsReturn = Lens.lens (return :: RevokeSecurityGroupIngressResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: RevokeSecurityGroupIngressResponse)
{-# DEPRECATED rsgirsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The inbound rules that were unknown to the service. In some cases, @unknownIpPermissionSet@ might be in a different format from the request parameter.
--
-- /Note:/ Consider using 'unknownIPPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgirsUnknownIPPermissions :: Lens.Lens' RevokeSecurityGroupIngressResponse (Lude.Maybe [IPPermission])
rsgirsUnknownIPPermissions = Lens.lens (unknownIPPermissions :: RevokeSecurityGroupIngressResponse -> Lude.Maybe [IPPermission]) (\s a -> s {unknownIPPermissions = a} :: RevokeSecurityGroupIngressResponse)
{-# DEPRECATED rsgirsUnknownIPPermissions "Use generic-lens or generic-optics with 'unknownIPPermissions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgirsResponseStatus :: Lens.Lens' RevokeSecurityGroupIngressResponse Lude.Int
rsgirsResponseStatus = Lens.lens (responseStatus :: RevokeSecurityGroupIngressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RevokeSecurityGroupIngressResponse)
{-# DEPRECATED rsgirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
