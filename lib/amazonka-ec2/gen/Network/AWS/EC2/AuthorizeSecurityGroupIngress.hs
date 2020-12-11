{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    asgiFromPort,
    asgiIPPermissions,
    asgiIPProtocol,
    asgiGroupId,
    asgiToPort,
    asgiCidrIP,
    asgiSourceSecurityGroupOwnerId,
    asgiGroupName,
    asgiSourceSecurityGroupName,
    asgiDryRun,

    -- * Destructuring the response
    AuthorizeSecurityGroupIngressResponse (..),
    mkAuthorizeSecurityGroupIngressResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAuthorizeSecurityGroupIngress' smart constructor.
data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress'
  { fromPort ::
      Lude.Maybe Lude.Int,
    ipPermissions ::
      Lude.Maybe [IPPermission],
    ipProtocol ::
      Lude.Maybe Lude.Text,
    groupId :: Lude.Maybe Lude.Text,
    toPort :: Lude.Maybe Lude.Int,
    cidrIP :: Lude.Maybe Lude.Text,
    sourceSecurityGroupOwnerId ::
      Lude.Maybe Lude.Text,
    groupName ::
      Lude.Maybe Lude.Text,
    sourceSecurityGroupName ::
      Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeSecurityGroupIngress' with the minimum fields required to make a request.
--
-- * 'cidrIP' - The IPv4 address range, in CIDR format. You can't specify this parameter when specifying a source security group. To specify an IPv6 address range, use a set of IP permissions.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'fromPort' - The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all types. If you specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
-- * 'groupId' - The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
-- * 'groupName' - [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
-- * 'ipPermissions' - The sets of IP permissions.
-- * 'ipProtocol' - The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). To specify @icmpv6@ , use a set of IP permissions.
--
-- [VPC only] Use @-1@ to specify all protocols. If you specify @-1@ or a protocol other than @tcp@ , @udp@ , or @icmp@ , traffic on all ports is allowed, regardless of any ports you specify.
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
-- * 'sourceSecurityGroupName' - [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead. For EC2-VPC, the source security group must be in the same VPC.
-- * 'sourceSecurityGroupOwnerId' - [nondefault VPC] The AWS account ID for the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead.
-- * 'toPort' - The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all codes. If you specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
mkAuthorizeSecurityGroupIngress ::
  AuthorizeSecurityGroupIngress
mkAuthorizeSecurityGroupIngress =
  AuthorizeSecurityGroupIngress'
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

-- | The start of port range for the TCP and UDP protocols, or an ICMP type number. For the ICMP type number, use @-1@ to specify all types. If you specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiFromPort :: Lens.Lens' AuthorizeSecurityGroupIngress (Lude.Maybe Lude.Int)
asgiFromPort = Lens.lens (fromPort :: AuthorizeSecurityGroupIngress -> Lude.Maybe Lude.Int) (\s a -> s {fromPort = a} :: AuthorizeSecurityGroupIngress)
{-# DEPRECATED asgiFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The sets of IP permissions.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiIPPermissions :: Lens.Lens' AuthorizeSecurityGroupIngress (Lude.Maybe [IPPermission])
asgiIPPermissions = Lens.lens (ipPermissions :: AuthorizeSecurityGroupIngress -> Lude.Maybe [IPPermission]) (\s a -> s {ipPermissions = a} :: AuthorizeSecurityGroupIngress)
{-# DEPRECATED asgiIPPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). To specify @icmpv6@ , use a set of IP permissions.
--
-- [VPC only] Use @-1@ to specify all protocols. If you specify @-1@ or a protocol other than @tcp@ , @udp@ , or @icmp@ , traffic on all ports is allowed, regardless of any ports you specify.
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiIPProtocol :: Lens.Lens' AuthorizeSecurityGroupIngress (Lude.Maybe Lude.Text)
asgiIPProtocol = Lens.lens (ipProtocol :: AuthorizeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ipProtocol = a} :: AuthorizeSecurityGroupIngress)
{-# DEPRECATED asgiIPProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead." #-}

-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiGroupId :: Lens.Lens' AuthorizeSecurityGroupIngress (Lude.Maybe Lude.Text)
asgiGroupId = Lens.lens (groupId :: AuthorizeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: AuthorizeSecurityGroupIngress)
{-# DEPRECATED asgiGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP code number. For the ICMP code number, use @-1@ to specify all codes. If you specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiToPort :: Lens.Lens' AuthorizeSecurityGroupIngress (Lude.Maybe Lude.Int)
asgiToPort = Lens.lens (toPort :: AuthorizeSecurityGroupIngress -> Lude.Maybe Lude.Int) (\s a -> s {toPort = a} :: AuthorizeSecurityGroupIngress)
{-# DEPRECATED asgiToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

-- | The IPv4 address range, in CIDR format. You can't specify this parameter when specifying a source security group. To specify an IPv6 address range, use a set of IP permissions.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and a description for the rule.
--
-- /Note:/ Consider using 'cidrIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiCidrIP :: Lens.Lens' AuthorizeSecurityGroupIngress (Lude.Maybe Lude.Text)
asgiCidrIP = Lens.lens (cidrIP :: AuthorizeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {cidrIP = a} :: AuthorizeSecurityGroupIngress)
{-# DEPRECATED asgiCidrIP "Use generic-lens or generic-optics with 'cidrIP' instead." #-}

-- | [nondefault VPC] The AWS account ID for the source security group, if the source security group is in a different account. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the IP protocol, the start of the port range, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead.
--
-- /Note:/ Consider using 'sourceSecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiSourceSecurityGroupOwnerId :: Lens.Lens' AuthorizeSecurityGroupIngress (Lude.Maybe Lude.Text)
asgiSourceSecurityGroupOwnerId = Lens.lens (sourceSecurityGroupOwnerId :: AuthorizeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {sourceSecurityGroupOwnerId = a} :: AuthorizeSecurityGroupIngress)
{-# DEPRECATED asgiSourceSecurityGroupOwnerId "Use generic-lens or generic-optics with 'sourceSecurityGroupOwnerId' instead." #-}

-- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiGroupName :: Lens.Lens' AuthorizeSecurityGroupIngress (Lude.Maybe Lude.Text)
asgiGroupName = Lens.lens (groupName :: AuthorizeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: AuthorizeSecurityGroupIngress)
{-# DEPRECATED asgiGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | [EC2-Classic, default VPC] The name of the source security group. You can't specify this parameter in combination with the following parameters: the CIDR IP address range, the start of the port range, the IP protocol, and the end of the port range. Creates rules that grant full ICMP, UDP, and TCP access. To create a rule with a specific IP protocol and port range, use a set of IP permissions instead. For EC2-VPC, the source security group must be in the same VPC.
--
-- /Note:/ Consider using 'sourceSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiSourceSecurityGroupName :: Lens.Lens' AuthorizeSecurityGroupIngress (Lude.Maybe Lude.Text)
asgiSourceSecurityGroupName = Lens.lens (sourceSecurityGroupName :: AuthorizeSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {sourceSecurityGroupName = a} :: AuthorizeSecurityGroupIngress)
{-# DEPRECATED asgiSourceSecurityGroupName "Use generic-lens or generic-optics with 'sourceSecurityGroupName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgiDryRun :: Lens.Lens' AuthorizeSecurityGroupIngress (Lude.Maybe Lude.Bool)
asgiDryRun = Lens.lens (dryRun :: AuthorizeSecurityGroupIngress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AuthorizeSecurityGroupIngress)
{-# DEPRECATED asgiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AuthorizeSecurityGroupIngress where
  type
    Rs AuthorizeSecurityGroupIngress =
      AuthorizeSecurityGroupIngressResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull AuthorizeSecurityGroupIngressResponse'

instance Lude.ToHeaders AuthorizeSecurityGroupIngress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AuthorizeSecurityGroupIngress where
  toPath = Lude.const "/"

instance Lude.ToQuery AuthorizeSecurityGroupIngress where
  toQuery AuthorizeSecurityGroupIngress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AuthorizeSecurityGroupIngress" :: Lude.ByteString),
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

-- | /See:/ 'mkAuthorizeSecurityGroupIngressResponse' smart constructor.
data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeSecurityGroupIngressResponse' with the minimum fields required to make a request.
mkAuthorizeSecurityGroupIngressResponse ::
  AuthorizeSecurityGroupIngressResponse
mkAuthorizeSecurityGroupIngressResponse =
  AuthorizeSecurityGroupIngressResponse'
