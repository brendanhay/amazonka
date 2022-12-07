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
-- Module      : Amazonka.EC2.AuthorizeSecurityGroupIngress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified inbound (ingress) rules to a security group.
--
-- An inbound rule permits instances to receive traffic from the specified
-- IPv4 or IPv6 CIDR address range, or from the instances that are
-- associated with the specified destination security groups. When
-- specifying an inbound rule for your security group in a VPC, the
-- @IpPermissions@ must include a source for the traffic.
--
-- You specify a protocol for each rule (for example, TCP). For TCP and
-- UDP, you must also specify the destination port or port range. For
-- ICMP\/ICMPv6, you must also specify the ICMP\/ICMPv6 type and code. You
-- can use -1 to mean all types or all codes.
--
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur.
--
-- For more information about VPC security group quotas, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html Amazon VPC quotas>.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.AuthorizeSecurityGroupIngress
  ( -- * Creating a Request
    AuthorizeSecurityGroupIngress (..),
    newAuthorizeSecurityGroupIngress,

    -- * Request Lenses
    authorizeSecurityGroupIngress_sourceSecurityGroupName,
    authorizeSecurityGroupIngress_toPort,
    authorizeSecurityGroupIngress_ipPermissions,
    authorizeSecurityGroupIngress_ipProtocol,
    authorizeSecurityGroupIngress_groupName,
    authorizeSecurityGroupIngress_dryRun,
    authorizeSecurityGroupIngress_cidrIp,
    authorizeSecurityGroupIngress_tagSpecifications,
    authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupIngress_fromPort,
    authorizeSecurityGroupIngress_groupId,

    -- * Destructuring the Response
    AuthorizeSecurityGroupIngressResponse (..),
    newAuthorizeSecurityGroupIngressResponse,

    -- * Response Lenses
    authorizeSecurityGroupIngressResponse_securityGroupRules,
    authorizeSecurityGroupIngressResponse_return,
    authorizeSecurityGroupIngressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAuthorizeSecurityGroupIngress' smart constructor.
data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress'
  { -- | [EC2-Classic, default VPC] The name of the source security group. You
    -- can\'t specify this parameter in combination with the following
    -- parameters: the CIDR IP address range, the start of the port range, the
    -- IP protocol, and the end of the port range. Creates rules that grant
    -- full ICMP, UDP, and TCP access. To create a rule with a specific IP
    -- protocol and port range, use a set of IP permissions instead. For
    -- EC2-VPC, the source security group must be in the same VPC.
    sourceSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | The end of port range for the TCP and UDP protocols, or an ICMP code
    -- number. For the ICMP code number, use @-1@ to specify all codes. If you
    -- specify all ICMP types, you must specify all codes.
    --
    -- Alternatively, use a set of IP permissions to specify multiple rules and
    -- a description for the rule.
    toPort :: Prelude.Maybe Prelude.Int,
    -- | The sets of IP permissions.
    ipPermissions :: Prelude.Maybe [IpPermission],
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
    -- | [EC2-Classic, default VPC] The name of the security group. You must
    -- specify either the security group ID or the security group name in the
    -- request. For security groups in a nondefault VPC, you must specify the
    -- security group ID.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IPv4 address range, in CIDR format. You can\'t specify this
    -- parameter when specifying a source security group. To specify an IPv6
    -- address range, use a set of IP permissions.
    --
    -- Alternatively, use a set of IP permissions to specify multiple rules and
    -- a description for the rule.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | [VPC Only] The tags applied to the security group rule.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | [nondefault VPC] The Amazon Web Services account ID for the source
    -- security group, if the source security group is in a different account.
    -- You can\'t specify this parameter in combination with the following
    -- parameters: the CIDR IP address range, the IP protocol, the start of the
    -- port range, and the end of the port range. Creates rules that grant full
    -- ICMP, UDP, and TCP access. To create a rule with a specific IP protocol
    -- and port range, use a set of IP permissions instead.
    sourceSecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The start of port range for the TCP and UDP protocols, or an ICMP type
    -- number. For the ICMP type number, use @-1@ to specify all types. If you
    -- specify all ICMP types, you must specify all codes.
    --
    -- Alternatively, use a set of IP permissions to specify multiple rules and
    -- a description for the rule.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The ID of the security group. You must specify either the security group
    -- ID or the security group name in the request. For security groups in a
    -- nondefault VPC, you must specify the security group ID.
    groupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeSecurityGroupIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceSecurityGroupName', 'authorizeSecurityGroupIngress_sourceSecurityGroupName' - [EC2-Classic, default VPC] The name of the source security group. You
-- can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the start of the port range, the
-- IP protocol, and the end of the port range. Creates rules that grant
-- full ICMP, UDP, and TCP access. To create a rule with a specific IP
-- protocol and port range, use a set of IP permissions instead. For
-- EC2-VPC, the source security group must be in the same VPC.
--
-- 'toPort', 'authorizeSecurityGroupIngress_toPort' - The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all codes. If you
-- specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
--
-- 'ipPermissions', 'authorizeSecurityGroupIngress_ipPermissions' - The sets of IP permissions.
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
-- 'groupName', 'authorizeSecurityGroupIngress_groupName' - [EC2-Classic, default VPC] The name of the security group. You must
-- specify either the security group ID or the security group name in the
-- request. For security groups in a nondefault VPC, you must specify the
-- security group ID.
--
-- 'dryRun', 'authorizeSecurityGroupIngress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'cidrIp', 'authorizeSecurityGroupIngress_cidrIp' - The IPv4 address range, in CIDR format. You can\'t specify this
-- parameter when specifying a source security group. To specify an IPv6
-- address range, use a set of IP permissions.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
--
-- 'tagSpecifications', 'authorizeSecurityGroupIngress_tagSpecifications' - [VPC Only] The tags applied to the security group rule.
--
-- 'sourceSecurityGroupOwnerId', 'authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId' - [nondefault VPC] The Amazon Web Services account ID for the source
-- security group, if the source security group is in a different account.
-- You can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the IP protocol, the start of the
-- port range, and the end of the port range. Creates rules that grant full
-- ICMP, UDP, and TCP access. To create a rule with a specific IP protocol
-- and port range, use a set of IP permissions instead.
--
-- 'fromPort', 'authorizeSecurityGroupIngress_fromPort' - The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all types. If you
-- specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
--
-- 'groupId', 'authorizeSecurityGroupIngress_groupId' - The ID of the security group. You must specify either the security group
-- ID or the security group name in the request. For security groups in a
-- nondefault VPC, you must specify the security group ID.
newAuthorizeSecurityGroupIngress ::
  AuthorizeSecurityGroupIngress
newAuthorizeSecurityGroupIngress =
  AuthorizeSecurityGroupIngress'
    { sourceSecurityGroupName =
        Prelude.Nothing,
      toPort = Prelude.Nothing,
      ipPermissions = Prelude.Nothing,
      ipProtocol = Prelude.Nothing,
      groupName = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      cidrIp = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      sourceSecurityGroupOwnerId = Prelude.Nothing,
      fromPort = Prelude.Nothing,
      groupId = Prelude.Nothing
    }

-- | [EC2-Classic, default VPC] The name of the source security group. You
-- can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the start of the port range, the
-- IP protocol, and the end of the port range. Creates rules that grant
-- full ICMP, UDP, and TCP access. To create a rule with a specific IP
-- protocol and port range, use a set of IP permissions instead. For
-- EC2-VPC, the source security group must be in the same VPC.
authorizeSecurityGroupIngress_sourceSecurityGroupName :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupIngress_sourceSecurityGroupName = Lens.lens (\AuthorizeSecurityGroupIngress' {sourceSecurityGroupName} -> sourceSecurityGroupName) (\s@AuthorizeSecurityGroupIngress' {} a -> s {sourceSecurityGroupName = a} :: AuthorizeSecurityGroupIngress)

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all codes. If you
-- specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
authorizeSecurityGroupIngress_toPort :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Int)
authorizeSecurityGroupIngress_toPort = Lens.lens (\AuthorizeSecurityGroupIngress' {toPort} -> toPort) (\s@AuthorizeSecurityGroupIngress' {} a -> s {toPort = a} :: AuthorizeSecurityGroupIngress)

-- | The sets of IP permissions.
authorizeSecurityGroupIngress_ipPermissions :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe [IpPermission])
authorizeSecurityGroupIngress_ipPermissions = Lens.lens (\AuthorizeSecurityGroupIngress' {ipPermissions} -> ipPermissions) (\s@AuthorizeSecurityGroupIngress' {} a -> s {ipPermissions = a} :: AuthorizeSecurityGroupIngress) Prelude.. Lens.mapping Lens.coerced

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

-- | [EC2-Classic, default VPC] The name of the security group. You must
-- specify either the security group ID or the security group name in the
-- request. For security groups in a nondefault VPC, you must specify the
-- security group ID.
authorizeSecurityGroupIngress_groupName :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupIngress_groupName = Lens.lens (\AuthorizeSecurityGroupIngress' {groupName} -> groupName) (\s@AuthorizeSecurityGroupIngress' {} a -> s {groupName = a} :: AuthorizeSecurityGroupIngress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
authorizeSecurityGroupIngress_dryRun :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Bool)
authorizeSecurityGroupIngress_dryRun = Lens.lens (\AuthorizeSecurityGroupIngress' {dryRun} -> dryRun) (\s@AuthorizeSecurityGroupIngress' {} a -> s {dryRun = a} :: AuthorizeSecurityGroupIngress)

-- | The IPv4 address range, in CIDR format. You can\'t specify this
-- parameter when specifying a source security group. To specify an IPv6
-- address range, use a set of IP permissions.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
authorizeSecurityGroupIngress_cidrIp :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupIngress_cidrIp = Lens.lens (\AuthorizeSecurityGroupIngress' {cidrIp} -> cidrIp) (\s@AuthorizeSecurityGroupIngress' {} a -> s {cidrIp = a} :: AuthorizeSecurityGroupIngress)

-- | [VPC Only] The tags applied to the security group rule.
authorizeSecurityGroupIngress_tagSpecifications :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe [TagSpecification])
authorizeSecurityGroupIngress_tagSpecifications = Lens.lens (\AuthorizeSecurityGroupIngress' {tagSpecifications} -> tagSpecifications) (\s@AuthorizeSecurityGroupIngress' {} a -> s {tagSpecifications = a} :: AuthorizeSecurityGroupIngress) Prelude.. Lens.mapping Lens.coerced

-- | [nondefault VPC] The Amazon Web Services account ID for the source
-- security group, if the source security group is in a different account.
-- You can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the IP protocol, the start of the
-- port range, and the end of the port range. Creates rules that grant full
-- ICMP, UDP, and TCP access. To create a rule with a specific IP protocol
-- and port range, use a set of IP permissions instead.
authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId = Lens.lens (\AuthorizeSecurityGroupIngress' {sourceSecurityGroupOwnerId} -> sourceSecurityGroupOwnerId) (\s@AuthorizeSecurityGroupIngress' {} a -> s {sourceSecurityGroupOwnerId = a} :: AuthorizeSecurityGroupIngress)

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all types. If you
-- specify all ICMP types, you must specify all codes.
--
-- Alternatively, use a set of IP permissions to specify multiple rules and
-- a description for the rule.
authorizeSecurityGroupIngress_fromPort :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Int)
authorizeSecurityGroupIngress_fromPort = Lens.lens (\AuthorizeSecurityGroupIngress' {fromPort} -> fromPort) (\s@AuthorizeSecurityGroupIngress' {} a -> s {fromPort = a} :: AuthorizeSecurityGroupIngress)

-- | The ID of the security group. You must specify either the security group
-- ID or the security group name in the request. For security groups in a
-- nondefault VPC, you must specify the security group ID.
authorizeSecurityGroupIngress_groupId :: Lens.Lens' AuthorizeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupIngress_groupId = Lens.lens (\AuthorizeSecurityGroupIngress' {groupId} -> groupId) (\s@AuthorizeSecurityGroupIngress' {} a -> s {groupId = a} :: AuthorizeSecurityGroupIngress)

instance
  Core.AWSRequest
    AuthorizeSecurityGroupIngress
  where
  type
    AWSResponse AuthorizeSecurityGroupIngress =
      AuthorizeSecurityGroupIngressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AuthorizeSecurityGroupIngressResponse'
            Prelude.<$> ( x Data..@? "securityGroupRuleSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AuthorizeSecurityGroupIngress
  where
  hashWithSalt _salt AuthorizeSecurityGroupIngress' {..} =
    _salt
      `Prelude.hashWithSalt` sourceSecurityGroupName
      `Prelude.hashWithSalt` toPort
      `Prelude.hashWithSalt` ipPermissions
      `Prelude.hashWithSalt` ipProtocol
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` cidrIp
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` sourceSecurityGroupOwnerId
      `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData AuthorizeSecurityGroupIngress where
  rnf AuthorizeSecurityGroupIngress' {..} =
    Prelude.rnf sourceSecurityGroupName
      `Prelude.seq` Prelude.rnf toPort
      `Prelude.seq` Prelude.rnf ipPermissions
      `Prelude.seq` Prelude.rnf ipProtocol
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf cidrIp
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf sourceSecurityGroupOwnerId
      `Prelude.seq` Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf groupId

instance Data.ToHeaders AuthorizeSecurityGroupIngress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AuthorizeSecurityGroupIngress where
  toPath = Prelude.const "/"

instance Data.ToQuery AuthorizeSecurityGroupIngress where
  toQuery AuthorizeSecurityGroupIngress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AuthorizeSecurityGroupIngress" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "SourceSecurityGroupName"
          Data.=: sourceSecurityGroupName,
        "ToPort" Data.=: toPort,
        Data.toQuery
          ( Data.toQueryList "IpPermissions"
              Prelude.<$> ipPermissions
          ),
        "IpProtocol" Data.=: ipProtocol,
        "GroupName" Data.=: groupName,
        "DryRun" Data.=: dryRun,
        "CidrIp" Data.=: cidrIp,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "SourceSecurityGroupOwnerId"
          Data.=: sourceSecurityGroupOwnerId,
        "FromPort" Data.=: fromPort,
        "GroupId" Data.=: groupId
      ]

-- | /See:/ 'newAuthorizeSecurityGroupIngressResponse' smart constructor.
data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse'
  { -- | Information about the inbound (ingress) security group rules that were
    -- added.
    securityGroupRules :: Prelude.Maybe [SecurityGroupRule],
    -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeSecurityGroupIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupRules', 'authorizeSecurityGroupIngressResponse_securityGroupRules' - Information about the inbound (ingress) security group rules that were
-- added.
--
-- 'return'', 'authorizeSecurityGroupIngressResponse_return' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- 'httpStatus', 'authorizeSecurityGroupIngressResponse_httpStatus' - The response's http status code.
newAuthorizeSecurityGroupIngressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AuthorizeSecurityGroupIngressResponse
newAuthorizeSecurityGroupIngressResponse pHttpStatus_ =
  AuthorizeSecurityGroupIngressResponse'
    { securityGroupRules =
        Prelude.Nothing,
      return' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the inbound (ingress) security group rules that were
-- added.
authorizeSecurityGroupIngressResponse_securityGroupRules :: Lens.Lens' AuthorizeSecurityGroupIngressResponse (Prelude.Maybe [SecurityGroupRule])
authorizeSecurityGroupIngressResponse_securityGroupRules = Lens.lens (\AuthorizeSecurityGroupIngressResponse' {securityGroupRules} -> securityGroupRules) (\s@AuthorizeSecurityGroupIngressResponse' {} a -> s {securityGroupRules = a} :: AuthorizeSecurityGroupIngressResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
authorizeSecurityGroupIngressResponse_return :: Lens.Lens' AuthorizeSecurityGroupIngressResponse (Prelude.Maybe Prelude.Bool)
authorizeSecurityGroupIngressResponse_return = Lens.lens (\AuthorizeSecurityGroupIngressResponse' {return'} -> return') (\s@AuthorizeSecurityGroupIngressResponse' {} a -> s {return' = a} :: AuthorizeSecurityGroupIngressResponse)

-- | The response's http status code.
authorizeSecurityGroupIngressResponse_httpStatus :: Lens.Lens' AuthorizeSecurityGroupIngressResponse Prelude.Int
authorizeSecurityGroupIngressResponse_httpStatus = Lens.lens (\AuthorizeSecurityGroupIngressResponse' {httpStatus} -> httpStatus) (\s@AuthorizeSecurityGroupIngressResponse' {} a -> s {httpStatus = a} :: AuthorizeSecurityGroupIngressResponse)

instance
  Prelude.NFData
    AuthorizeSecurityGroupIngressResponse
  where
  rnf AuthorizeSecurityGroupIngressResponse' {..} =
    Prelude.rnf securityGroupRules
      `Prelude.seq` Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
