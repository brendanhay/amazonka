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
-- Module      : Amazonka.EC2.RevokeSecurityGroupIngress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified inbound (ingress) rules from a security group.
--
-- You can specify rules using either rule IDs or security group rule
-- properties. If you use rule properties, the values that you specify (for
-- example, ports) must match the existing rule\'s values exactly. Each
-- rule has a protocol, from and to ports, and source (CIDR range, security
-- group, or prefix list). For the TCP and UDP protocols, you must also
-- specify the destination port or range of ports. For the ICMP protocol,
-- you must also specify the ICMP type and code. If the security group rule
-- has a description, you do not need to specify the description to revoke
-- the rule.
--
-- [EC2-Classic, default VPC] If the values you specify do not match the
-- existing rule\'s values, no error is returned, and the output describes
-- the security group rules that were not revoked.
--
-- Amazon Web Services recommends that you describe the security group to
-- verify that the rules were removed.
--
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur.
module Amazonka.EC2.RevokeSecurityGroupIngress
  ( -- * Creating a Request
    RevokeSecurityGroupIngress (..),
    newRevokeSecurityGroupIngress,

    -- * Request Lenses
    revokeSecurityGroupIngress_fromPort,
    revokeSecurityGroupIngress_ipPermissions,
    revokeSecurityGroupIngress_ipProtocol,
    revokeSecurityGroupIngress_groupId,
    revokeSecurityGroupIngress_toPort,
    revokeSecurityGroupIngress_cidrIp,
    revokeSecurityGroupIngress_sourceSecurityGroupOwnerId,
    revokeSecurityGroupIngress_groupName,
    revokeSecurityGroupIngress_sourceSecurityGroupName,
    revokeSecurityGroupIngress_dryRun,
    revokeSecurityGroupIngress_securityGroupRuleIds,

    -- * Destructuring the Response
    RevokeSecurityGroupIngressResponse (..),
    newRevokeSecurityGroupIngressResponse,

    -- * Response Lenses
    revokeSecurityGroupIngressResponse_return,
    revokeSecurityGroupIngressResponse_unknownIpPermissions,
    revokeSecurityGroupIngressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRevokeSecurityGroupIngress' smart constructor.
data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress'
  { -- | The start of port range for the TCP and UDP protocols, or an ICMP type
    -- number. For the ICMP type number, use @-1@ to specify all ICMP types.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The sets of IP permissions. You can\'t specify a source security group
    -- and a CIDR IP address range in the same set of permissions.
    ipPermissions :: Prelude.Maybe [IpPermission],
    -- | The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
    -- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
    -- Use @-1@ to specify all.
    ipProtocol :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group. You must specify either the security group
    -- ID or the security group name in the request. For security groups in a
    -- nondefault VPC, you must specify the security group ID.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The end of port range for the TCP and UDP protocols, or an ICMP code
    -- number. For the ICMP code number, use @-1@ to specify all ICMP codes for
    -- the ICMP type.
    toPort :: Prelude.Maybe Prelude.Int,
    -- | The CIDR IP address range. You can\'t specify this parameter when
    -- specifying a source security group.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | [EC2-Classic] The Amazon Web Services account ID of the source security
    -- group, if the source security group is in a different account. You
    -- can\'t specify this parameter in combination with the following
    -- parameters: the CIDR IP address range, the IP protocol, the start of the
    -- port range, and the end of the port range. To revoke a specific rule for
    -- an IP protocol and port range, use a set of IP permissions instead.
    sourceSecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | [EC2-Classic, default VPC] The name of the security group. You must
    -- specify either the security group ID or the security group name in the
    -- request.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | [EC2-Classic, default VPC] The name of the source security group. You
    -- can\'t specify this parameter in combination with the following
    -- parameters: the CIDR IP address range, the start of the port range, the
    -- IP protocol, and the end of the port range. For EC2-VPC, the source
    -- security group must be in the same VPC. To revoke a specific rule for an
    -- IP protocol and port range, use a set of IP permissions instead.
    sourceSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the security group rules.
    securityGroupRuleIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeSecurityGroupIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'revokeSecurityGroupIngress_fromPort' - The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all ICMP types.
--
-- 'ipPermissions', 'revokeSecurityGroupIngress_ipPermissions' - The sets of IP permissions. You can\'t specify a source security group
-- and a CIDR IP address range in the same set of permissions.
--
-- 'ipProtocol', 'revokeSecurityGroupIngress_ipProtocol' - The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- Use @-1@ to specify all.
--
-- 'groupId', 'revokeSecurityGroupIngress_groupId' - The ID of the security group. You must specify either the security group
-- ID or the security group name in the request. For security groups in a
-- nondefault VPC, you must specify the security group ID.
--
-- 'toPort', 'revokeSecurityGroupIngress_toPort' - The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all ICMP codes for
-- the ICMP type.
--
-- 'cidrIp', 'revokeSecurityGroupIngress_cidrIp' - The CIDR IP address range. You can\'t specify this parameter when
-- specifying a source security group.
--
-- 'sourceSecurityGroupOwnerId', 'revokeSecurityGroupIngress_sourceSecurityGroupOwnerId' - [EC2-Classic] The Amazon Web Services account ID of the source security
-- group, if the source security group is in a different account. You
-- can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the IP protocol, the start of the
-- port range, and the end of the port range. To revoke a specific rule for
-- an IP protocol and port range, use a set of IP permissions instead.
--
-- 'groupName', 'revokeSecurityGroupIngress_groupName' - [EC2-Classic, default VPC] The name of the security group. You must
-- specify either the security group ID or the security group name in the
-- request.
--
-- 'sourceSecurityGroupName', 'revokeSecurityGroupIngress_sourceSecurityGroupName' - [EC2-Classic, default VPC] The name of the source security group. You
-- can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the start of the port range, the
-- IP protocol, and the end of the port range. For EC2-VPC, the source
-- security group must be in the same VPC. To revoke a specific rule for an
-- IP protocol and port range, use a set of IP permissions instead.
--
-- 'dryRun', 'revokeSecurityGroupIngress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'securityGroupRuleIds', 'revokeSecurityGroupIngress_securityGroupRuleIds' - The IDs of the security group rules.
newRevokeSecurityGroupIngress ::
  RevokeSecurityGroupIngress
newRevokeSecurityGroupIngress =
  RevokeSecurityGroupIngress'
    { fromPort =
        Prelude.Nothing,
      ipPermissions = Prelude.Nothing,
      ipProtocol = Prelude.Nothing,
      groupId = Prelude.Nothing,
      toPort = Prelude.Nothing,
      cidrIp = Prelude.Nothing,
      sourceSecurityGroupOwnerId = Prelude.Nothing,
      groupName = Prelude.Nothing,
      sourceSecurityGroupName = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      securityGroupRuleIds = Prelude.Nothing
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. For the ICMP type number, use @-1@ to specify all ICMP types.
revokeSecurityGroupIngress_fromPort :: Lens.Lens' RevokeSecurityGroupIngress (Prelude.Maybe Prelude.Int)
revokeSecurityGroupIngress_fromPort = Lens.lens (\RevokeSecurityGroupIngress' {fromPort} -> fromPort) (\s@RevokeSecurityGroupIngress' {} a -> s {fromPort = a} :: RevokeSecurityGroupIngress)

-- | The sets of IP permissions. You can\'t specify a source security group
-- and a CIDR IP address range in the same set of permissions.
revokeSecurityGroupIngress_ipPermissions :: Lens.Lens' RevokeSecurityGroupIngress (Prelude.Maybe [IpPermission])
revokeSecurityGroupIngress_ipPermissions = Lens.lens (\RevokeSecurityGroupIngress' {ipPermissions} -> ipPermissions) (\s@RevokeSecurityGroupIngress' {} a -> s {ipPermissions = a} :: RevokeSecurityGroupIngress) Prelude.. Lens.mapping Lens.coerced

-- | The IP protocol name (@tcp@, @udp@, @icmp@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
-- Use @-1@ to specify all.
revokeSecurityGroupIngress_ipProtocol :: Lens.Lens' RevokeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupIngress_ipProtocol = Lens.lens (\RevokeSecurityGroupIngress' {ipProtocol} -> ipProtocol) (\s@RevokeSecurityGroupIngress' {} a -> s {ipProtocol = a} :: RevokeSecurityGroupIngress)

-- | The ID of the security group. You must specify either the security group
-- ID or the security group name in the request. For security groups in a
-- nondefault VPC, you must specify the security group ID.
revokeSecurityGroupIngress_groupId :: Lens.Lens' RevokeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupIngress_groupId = Lens.lens (\RevokeSecurityGroupIngress' {groupId} -> groupId) (\s@RevokeSecurityGroupIngress' {} a -> s {groupId = a} :: RevokeSecurityGroupIngress)

-- | The end of port range for the TCP and UDP protocols, or an ICMP code
-- number. For the ICMP code number, use @-1@ to specify all ICMP codes for
-- the ICMP type.
revokeSecurityGroupIngress_toPort :: Lens.Lens' RevokeSecurityGroupIngress (Prelude.Maybe Prelude.Int)
revokeSecurityGroupIngress_toPort = Lens.lens (\RevokeSecurityGroupIngress' {toPort} -> toPort) (\s@RevokeSecurityGroupIngress' {} a -> s {toPort = a} :: RevokeSecurityGroupIngress)

-- | The CIDR IP address range. You can\'t specify this parameter when
-- specifying a source security group.
revokeSecurityGroupIngress_cidrIp :: Lens.Lens' RevokeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupIngress_cidrIp = Lens.lens (\RevokeSecurityGroupIngress' {cidrIp} -> cidrIp) (\s@RevokeSecurityGroupIngress' {} a -> s {cidrIp = a} :: RevokeSecurityGroupIngress)

-- | [EC2-Classic] The Amazon Web Services account ID of the source security
-- group, if the source security group is in a different account. You
-- can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the IP protocol, the start of the
-- port range, and the end of the port range. To revoke a specific rule for
-- an IP protocol and port range, use a set of IP permissions instead.
revokeSecurityGroupIngress_sourceSecurityGroupOwnerId :: Lens.Lens' RevokeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupIngress_sourceSecurityGroupOwnerId = Lens.lens (\RevokeSecurityGroupIngress' {sourceSecurityGroupOwnerId} -> sourceSecurityGroupOwnerId) (\s@RevokeSecurityGroupIngress' {} a -> s {sourceSecurityGroupOwnerId = a} :: RevokeSecurityGroupIngress)

-- | [EC2-Classic, default VPC] The name of the security group. You must
-- specify either the security group ID or the security group name in the
-- request.
revokeSecurityGroupIngress_groupName :: Lens.Lens' RevokeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupIngress_groupName = Lens.lens (\RevokeSecurityGroupIngress' {groupName} -> groupName) (\s@RevokeSecurityGroupIngress' {} a -> s {groupName = a} :: RevokeSecurityGroupIngress)

-- | [EC2-Classic, default VPC] The name of the source security group. You
-- can\'t specify this parameter in combination with the following
-- parameters: the CIDR IP address range, the start of the port range, the
-- IP protocol, and the end of the port range. For EC2-VPC, the source
-- security group must be in the same VPC. To revoke a specific rule for an
-- IP protocol and port range, use a set of IP permissions instead.
revokeSecurityGroupIngress_sourceSecurityGroupName :: Lens.Lens' RevokeSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupIngress_sourceSecurityGroupName = Lens.lens (\RevokeSecurityGroupIngress' {sourceSecurityGroupName} -> sourceSecurityGroupName) (\s@RevokeSecurityGroupIngress' {} a -> s {sourceSecurityGroupName = a} :: RevokeSecurityGroupIngress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
revokeSecurityGroupIngress_dryRun :: Lens.Lens' RevokeSecurityGroupIngress (Prelude.Maybe Prelude.Bool)
revokeSecurityGroupIngress_dryRun = Lens.lens (\RevokeSecurityGroupIngress' {dryRun} -> dryRun) (\s@RevokeSecurityGroupIngress' {} a -> s {dryRun = a} :: RevokeSecurityGroupIngress)

-- | The IDs of the security group rules.
revokeSecurityGroupIngress_securityGroupRuleIds :: Lens.Lens' RevokeSecurityGroupIngress (Prelude.Maybe [Prelude.Text])
revokeSecurityGroupIngress_securityGroupRuleIds = Lens.lens (\RevokeSecurityGroupIngress' {securityGroupRuleIds} -> securityGroupRuleIds) (\s@RevokeSecurityGroupIngress' {} a -> s {securityGroupRuleIds = a} :: RevokeSecurityGroupIngress) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest RevokeSecurityGroupIngress where
  type
    AWSResponse RevokeSecurityGroupIngress =
      RevokeSecurityGroupIngressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RevokeSecurityGroupIngressResponse'
            Prelude.<$> (x Core..@? "return")
            Prelude.<*> ( x Core..@? "unknownIpPermissionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RevokeSecurityGroupIngress where
  hashWithSalt _salt RevokeSecurityGroupIngress' {..} =
    _salt `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` ipPermissions
      `Prelude.hashWithSalt` ipProtocol
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` toPort
      `Prelude.hashWithSalt` cidrIp
      `Prelude.hashWithSalt` sourceSecurityGroupOwnerId
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` sourceSecurityGroupName
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` securityGroupRuleIds

instance Prelude.NFData RevokeSecurityGroupIngress where
  rnf RevokeSecurityGroupIngress' {..} =
    Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf ipPermissions
      `Prelude.seq` Prelude.rnf ipProtocol
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf toPort
      `Prelude.seq` Prelude.rnf cidrIp
      `Prelude.seq` Prelude.rnf sourceSecurityGroupOwnerId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf sourceSecurityGroupName
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf securityGroupRuleIds

instance Core.ToHeaders RevokeSecurityGroupIngress where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RevokeSecurityGroupIngress where
  toPath = Prelude.const "/"

instance Core.ToQuery RevokeSecurityGroupIngress where
  toQuery RevokeSecurityGroupIngress' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RevokeSecurityGroupIngress" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "FromPort" Core.=: fromPort,
        Core.toQuery
          ( Core.toQueryList "IpPermissions"
              Prelude.<$> ipPermissions
          ),
        "IpProtocol" Core.=: ipProtocol,
        "GroupId" Core.=: groupId,
        "ToPort" Core.=: toPort,
        "CidrIp" Core.=: cidrIp,
        "SourceSecurityGroupOwnerId"
          Core.=: sourceSecurityGroupOwnerId,
        "GroupName" Core.=: groupName,
        "SourceSecurityGroupName"
          Core.=: sourceSecurityGroupName,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "SecurityGroupRuleId"
              Prelude.<$> securityGroupRuleIds
          )
      ]

-- | /See:/ 'newRevokeSecurityGroupIngressResponse' smart constructor.
data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The inbound rules that were unknown to the service. In some cases,
    -- @unknownIpPermissionSet@ might be in a different format from the request
    -- parameter.
    unknownIpPermissions :: Prelude.Maybe [IpPermission],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeSecurityGroupIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'revokeSecurityGroupIngressResponse_return' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- 'unknownIpPermissions', 'revokeSecurityGroupIngressResponse_unknownIpPermissions' - The inbound rules that were unknown to the service. In some cases,
-- @unknownIpPermissionSet@ might be in a different format from the request
-- parameter.
--
-- 'httpStatus', 'revokeSecurityGroupIngressResponse_httpStatus' - The response's http status code.
newRevokeSecurityGroupIngressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RevokeSecurityGroupIngressResponse
newRevokeSecurityGroupIngressResponse pHttpStatus_ =
  RevokeSecurityGroupIngressResponse'
    { return' =
        Prelude.Nothing,
      unknownIpPermissions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
revokeSecurityGroupIngressResponse_return :: Lens.Lens' RevokeSecurityGroupIngressResponse (Prelude.Maybe Prelude.Bool)
revokeSecurityGroupIngressResponse_return = Lens.lens (\RevokeSecurityGroupIngressResponse' {return'} -> return') (\s@RevokeSecurityGroupIngressResponse' {} a -> s {return' = a} :: RevokeSecurityGroupIngressResponse)

-- | The inbound rules that were unknown to the service. In some cases,
-- @unknownIpPermissionSet@ might be in a different format from the request
-- parameter.
revokeSecurityGroupIngressResponse_unknownIpPermissions :: Lens.Lens' RevokeSecurityGroupIngressResponse (Prelude.Maybe [IpPermission])
revokeSecurityGroupIngressResponse_unknownIpPermissions = Lens.lens (\RevokeSecurityGroupIngressResponse' {unknownIpPermissions} -> unknownIpPermissions) (\s@RevokeSecurityGroupIngressResponse' {} a -> s {unknownIpPermissions = a} :: RevokeSecurityGroupIngressResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
revokeSecurityGroupIngressResponse_httpStatus :: Lens.Lens' RevokeSecurityGroupIngressResponse Prelude.Int
revokeSecurityGroupIngressResponse_httpStatus = Lens.lens (\RevokeSecurityGroupIngressResponse' {httpStatus} -> httpStatus) (\s@RevokeSecurityGroupIngressResponse' {} a -> s {httpStatus = a} :: RevokeSecurityGroupIngressResponse)

instance
  Prelude.NFData
    RevokeSecurityGroupIngressResponse
  where
  rnf RevokeSecurityGroupIngressResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf unknownIpPermissions
      `Prelude.seq` Prelude.rnf httpStatus
