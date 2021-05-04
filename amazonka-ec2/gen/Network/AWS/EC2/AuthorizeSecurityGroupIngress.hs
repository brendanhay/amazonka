{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupIngress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified ingress rules to a security group.
--
-- An inbound rule permits instances to receive traffic from the specified
-- IPv4 or IPv6 CIDR address ranges, or from the instances associated with
-- the specified destination security groups.
--
-- You specify a protocol for each rule (for example, TCP). For TCP and
-- UDP, you must also specify the destination port or port range. For
-- ICMP\/ICMPv6, you must also specify the ICMP\/ICMPv6 type and code. You
-- can use -1 to mean all types or all codes.
--
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur.
--
-- For more information about VPC security group limits, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html Amazon VPC Limits>.
module Network.AWS.EC2.AuthorizeSecurityGroupIngress
  ( -- * Creating a Request
    AuthorizeSecurityGroupIngress (..),
    newAuthorizeSecurityGroupIngress,

    -- * Request Lenses
    authorizeSecurityGroupIngress_fromPort,
    authorizeSecurityGroupIngress_dryRun,
    authorizeSecurityGroupIngress_sourceSecurityGroupName,
    authorizeSecurityGroupIngress_groupName,
    authorizeSecurityGroupIngress_cidrIp,
    authorizeSecurityGroupIngress_groupId,
    authorizeSecurityGroupIngress_ipProtocol,
    authorizeSecurityGroupIngress_ipPermissions,
    authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupIngress_toPort,

    -- * Destructuring the Response
    AuthorizeSecurityGroupIngressResponse (..),
    newAuthorizeSecurityGroupIngressResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAuthorizeSecurityGroupIngress' smart constructor.
data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress'
  { -- | The start of port range for the TCP and UDP protocols, or an ICMP type
    -- number. For the ICMP type number, use @-1@ to specify all types. If you
    -- specify all ICMP types, you must specify all codes.
    --
    -- Alternatively, use a set of IP permissions to specify multiple rules and
    -- a description for the rule.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | [EC2-Classic, default VPC] The name of the source security group. You
    -- can\'t specify this parameter in combination with the following
    -- parameters: the CIDR IP address range, the start of the port range, the
    -- IP protocol, and the end of the port range. Creates rules that grant
    -- full ICMP, UDP, and TCP access. To create a rule with a specific IP
    -- protocol and port range, use a set of IP permissions instead. For
    -- EC2-VPC, the source security group must be in the same VPC.
    sourceSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | [EC2-Classic, default VPC] The name of the security group. You must
    -- specify either the security group ID or the security group name in the
    -- request.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 address range, in CIDR format. You can\'t specify this
    -- parameter when specifying a source security group. To specify an IPv6
    -- address range, use a set of IP permissions.
    --
    -- Alternatively, use a set of IP permissions to specify multiple rules and
    -- a description for the rule.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group. You must specify either the security group
    -- ID or the security group name in the request. For security groups in a
    -- nondefault VPC, you must specify the security group ID.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
    -- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
    -- To specify @icmpv6@, use a set of IP permissions.
    --
    -- [VPC only] Use @-1@ to specify all protocols. If you specify @-1@ or a
    -- protocol other than @tcp@, @udp@, or @icmp@, traffic on all ports is
    -- allowed, regardless of any ports you specify.
    --
    -- Alternatively, use a set of IP permissions to specify multiple rules and
    -- a description for the rule.
    ipProtocol :: Prelude.Maybe Prelude.Text,
    -- | The sets of IP permissions.
    ipPermissions :: Prelude.Maybe [IpPermission],
    -- | [nondefault VPC] The AWS account ID for the source security group, if
    -- the source security group is in a different account. You can\'t specify
    -- this parameter in combination with the following parameters: the CIDR IP
    -- address range, the IP protocol, the start of the port range, and the end
    -- of the port range. Creates rules that grant full ICMP, UDP, and TCP
    -- access. To create a rule with a specific IP protocol and port range, use
    -- a set of IP permissions instead.
    sourceSecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The end of port range for the TCP and UDP protocols, or an ICMP code
    -- number. For the ICMP code number, use @-1@ to specify all codes. If you
    -- specify all ICMP types, you must specify all codes.
    --
    -- Alternatively, use a set of IP permissions to specify multiple rules and
    -- a description for the rule.
    toPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeSecurityGroupIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'authorizeSecurityGroupIngress_fromPort' - The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all types. If you
-- specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
--
-- 'dryRun', 'authorizeSecurityGroupIngress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'sourceSecurityGroupName', 'authorizeSecurityGroupIngress_sourceSecurityGroupName' - [EC2-Classic, default VPC] The name of the source security group. You
-- can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the start of the port range, the
-- IP protocol, and the end of the port range. Creates rules that grant
-- full ICMP, UDP, and TCP access. To create a rule with a specific IP
-- protocol and port range, use a set of IP permissions instead. For
-- EC2-VPC, the source security group must be in the same VPC.
--
-- 'groupName', 'authorizeSecurityGroupIngress_groupName' - [EC2-Classic, default VPC] The name of the security group. You must
-- specify either the security group ID or the security group name in the
-- request.
--
-- 'cidrIp', 'authorizeSecurityGroupIngress_cidrIp' - The IPv4 address range, in CIDR format. You can\'t specify this
-- parameter when specifying a source security group. To specify an IPv6
-- address range, use a set of IP permissions.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
--
-- 'groupId', 'authorizeSecurityGroupIngress_groupId' - The ID of the security group. You must specify either the security group
-- ID or the security group name in the request. For security groups in a
-- nondefault VPC, you must specify the security group ID.
--
-- 'ipProtocol', 'authorizeSecurityGroupIngress_ipProtocol' - The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- To specify @icmpv6@, use a set of IP permissions.
--
-- [VPC only] Use @-1@ to specify all protocols. If you specify @-1@ or a
-- protocol other than @tcp@, @udp@, or @icmp@, traffic on all ports is
-- allowed, regardless of any ports you specify.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
--
-- 'ipPermissions', 'authorizeSecurityGroupIngress_ipPermissions' - The sets of IP permissions.
--
-- 'sourceSecurityGroupOwnerId', 'authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId' - [nondefault VPC] The AWS account ID for the source security group, if
-- the source security group is in a different account. You can\'t specify
-- this parameter in combination with the following parameters: the CIDR IP
-- address range, the IP protocol, the start of the port range, and the end
-- of the port range. Creates rules that grant full ICMP, UDP, and TCP
-- access. To create a rule with a specific IP protocol and port range, use
-- a set of IP permissions instead.
--
-- 'toPort', 'authorizeSecurityGroupIngress_toPort' - The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all codes. If you
-- specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
newAuthorizeSecurityGroupIngress ::
  AuthorizeSecurityGroupIngress
newAuthorizeSecurityGroupIngress =
  AuthorizeSecurityGroupIngress'
    { fromPort =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      sourceSecurityGroupName = Prelude.Nothing,
      groupName = Prelude.Nothing,
      cidrIp = Prelude.Nothing,
      groupId = Prelude.Nothing,
      ipProtocol = Prelude.Nothing,
      ipPermissions = Prelude.Nothing,
      sourceSecurityGroupOwnerId = Prelude.Nothing,
      toPort = Prelude.Nothing
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all types. If you
-- specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
authorizeSecurityGroupIngress_fromPort :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Int)
authorizeSecurityGroupIngress_fromPort = Lens.lens (\AuthorizeSecurityGroupIngress' {fromPort} -> fromPort) (\s@AuthorizeSecurityGroupIngress' {} a -> s {fromPort = a} :: AuthorizeSecurityGroupIngress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
authorizeSecurityGroupIngress_dryRun :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Bool)
authorizeSecurityGroupIngress_dryRun = Lens.lens (\AuthorizeSecurityGroupIngress' {dryRun} -> dryRun) (\s@AuthorizeSecurityGroupIngress' {} a -> s {dryRun = a} :: AuthorizeSecurityGroupIngress)

-- | [EC2-Classic, default VPC] The name of the source security group. You
-- can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the start of the port range, the
-- IP protocol, and the end of the port range. Creates rules that grant
-- full ICMP, UDP, and TCP access. To create a rule with a specific IP
-- protocol and port range, use a set of IP permissions instead. For
-- EC2-VPC, the source security group must be in the same VPC.
authorizeSecurityGroupIngress_sourceSecurityGroupName :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupIngress_sourceSecurityGroupName = Lens.lens (\AuthorizeSecurityGroupIngress' {sourceSecurityGroupName} -> sourceSecurityGroupName) (\s@AuthorizeSecurityGroupIngress' {} a -> s {sourceSecurityGroupName = a} :: AuthorizeSecurityGroupIngress)

-- | [EC2-Classic, default VPC] The name of the security group. You must
-- specify either the security group ID or the security group name in the
-- request.
authorizeSecurityGroupIngress_groupName :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupIngress_groupName = Lens.lens (\AuthorizeSecurityGroupIngress' {groupName} -> groupName) (\s@AuthorizeSecurityGroupIngress' {} a -> s {groupName = a} :: AuthorizeSecurityGroupIngress)

-- | The IPv4 address range, in CIDR format. You can\'t specify this
-- parameter when specifying a source security group. To specify an IPv6
-- address range, use a set of IP permissions.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
authorizeSecurityGroupIngress_cidrIp :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupIngress_cidrIp = Lens.lens (\AuthorizeSecurityGroupIngress' {cidrIp} -> cidrIp) (\s@AuthorizeSecurityGroupIngress' {} a -> s {cidrIp = a} :: AuthorizeSecurityGroupIngress)

-- | The ID of the security group. You must specify either the security group
-- ID or the security group name in the request. For security groups in a
-- nondefault VPC, you must specify the security group ID.
authorizeSecurityGroupIngress_groupId :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupIngress_groupId = Lens.lens (\AuthorizeSecurityGroupIngress' {groupId} -> groupId) (\s@AuthorizeSecurityGroupIngress' {} a -> s {groupId = a} :: AuthorizeSecurityGroupIngress)

-- | The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- To specify @icmpv6@, use a set of IP permissions.
--
-- [VPC only] Use @-1@ to specify all protocols. If you specify @-1@ or a
-- protocol other than @tcp@, @udp@, or @icmp@, traffic on all ports is
-- allowed, regardless of any ports you specify.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
authorizeSecurityGroupIngress_ipProtocol :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupIngress_ipProtocol = Lens.lens (\AuthorizeSecurityGroupIngress' {ipProtocol} -> ipProtocol) (\s@AuthorizeSecurityGroupIngress' {} a -> s {ipProtocol = a} :: AuthorizeSecurityGroupIngress)

-- | The sets of IP permissions.
authorizeSecurityGroupIngress_ipPermissions :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe [IpPermission])
authorizeSecurityGroupIngress_ipPermissions = Lens.lens (\AuthorizeSecurityGroupIngress' {ipPermissions} -> ipPermissions) (\s@AuthorizeSecurityGroupIngress' {} a -> s {ipPermissions = a} :: AuthorizeSecurityGroupIngress) Prelude.. Lens.mapping Prelude._Coerce

-- | [nondefault VPC] The AWS account ID for the source security group, if
-- the source security group is in a different account. You can\'t specify
-- this parameter in combination with the following parameters: the CIDR IP
-- address range, the IP protocol, the start of the port range, and the end
-- of the port range. Creates rules that grant full ICMP, UDP, and TCP
-- access. To create a rule with a specific IP protocol and port range, use
-- a set of IP permissions instead.
authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId = Lens.lens (\AuthorizeSecurityGroupIngress' {sourceSecurityGroupOwnerId} -> sourceSecurityGroupOwnerId) (\s@AuthorizeSecurityGroupIngress' {} a -> s {sourceSecurityGroupOwnerId = a} :: AuthorizeSecurityGroupIngress)

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all codes. If you
-- specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
authorizeSecurityGroupIngress_toPort :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Int)
authorizeSecurityGroupIngress_toPort = Lens.lens (\AuthorizeSecurityGroupIngress' {toPort} -> toPort) (\s@AuthorizeSecurityGroupIngress' {} a -> s {toPort = a} :: AuthorizeSecurityGroupIngress)

instance
  Prelude.AWSRequest
    AuthorizeSecurityGroupIngress
  where
  type
    Rs AuthorizeSecurityGroupIngress =
      AuthorizeSecurityGroupIngressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      AuthorizeSecurityGroupIngressResponse'

instance
  Prelude.Hashable
    AuthorizeSecurityGroupIngress

instance Prelude.NFData AuthorizeSecurityGroupIngress

instance
  Prelude.ToHeaders
    AuthorizeSecurityGroupIngress
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AuthorizeSecurityGroupIngress where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    AuthorizeSecurityGroupIngress
  where
  toQuery AuthorizeSecurityGroupIngress' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "AuthorizeSecurityGroupIngress" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "FromPort" Prelude.=: fromPort,
        "DryRun" Prelude.=: dryRun,
        "SourceSecurityGroupName"
          Prelude.=: sourceSecurityGroupName,
        "GroupName" Prelude.=: groupName,
        "CidrIp" Prelude.=: cidrIp,
        "GroupId" Prelude.=: groupId,
        "IpProtocol" Prelude.=: ipProtocol,
        Prelude.toQuery
          ( Prelude.toQueryList "IpPermissions"
              Prelude.<$> ipPermissions
          ),
        "SourceSecurityGroupOwnerId"
          Prelude.=: sourceSecurityGroupOwnerId,
        "ToPort" Prelude.=: toPort
      ]

-- | /See:/ 'newAuthorizeSecurityGroupIngressResponse' smart constructor.
data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeSecurityGroupIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAuthorizeSecurityGroupIngressResponse ::
  AuthorizeSecurityGroupIngressResponse
newAuthorizeSecurityGroupIngressResponse =
  AuthorizeSecurityGroupIngressResponse'

instance
  Prelude.NFData
    AuthorizeSecurityGroupIngressResponse
