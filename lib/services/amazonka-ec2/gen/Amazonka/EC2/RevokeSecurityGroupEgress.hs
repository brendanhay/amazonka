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
-- Module      : Amazonka.EC2.RevokeSecurityGroupEgress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Removes the specified outbound (egress) rules from a security
-- group for EC2-VPC. This action does not apply to security groups for use
-- in EC2-Classic.
--
-- You can specify rules using either rule IDs or security group rule
-- properties. If you use rule properties, the values that you specify (for
-- example, ports) must match the existing rule\'s values exactly. Each
-- rule has a protocol, from and to ports, and destination (CIDR range,
-- security group, or prefix list). For the TCP and UDP protocols, you must
-- also specify the destination port or range of ports. For the ICMP
-- protocol, you must also specify the ICMP type and code. If the security
-- group rule has a description, you do not need to specify the description
-- to revoke the rule.
--
-- [Default VPC] If the values you specify do not match the existing
-- rule\'s values, no error is returned, and the output describes the
-- security group rules that were not revoked.
--
-- Amazon Web Services recommends that you describe the security group to
-- verify that the rules were removed.
--
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur.
module Amazonka.EC2.RevokeSecurityGroupEgress
  ( -- * Creating a Request
    RevokeSecurityGroupEgress (..),
    newRevokeSecurityGroupEgress,

    -- * Request Lenses
    revokeSecurityGroupEgress_cidrIp,
    revokeSecurityGroupEgress_dryRun,
    revokeSecurityGroupEgress_fromPort,
    revokeSecurityGroupEgress_ipPermissions,
    revokeSecurityGroupEgress_ipProtocol,
    revokeSecurityGroupEgress_securityGroupRuleIds,
    revokeSecurityGroupEgress_sourceSecurityGroupName,
    revokeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    revokeSecurityGroupEgress_toPort,
    revokeSecurityGroupEgress_groupId,

    -- * Destructuring the Response
    RevokeSecurityGroupEgressResponse (..),
    newRevokeSecurityGroupEgressResponse,

    -- * Response Lenses
    revokeSecurityGroupEgressResponse_return,
    revokeSecurityGroupEgressResponse_unknownIpPermissions,
    revokeSecurityGroupEgressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRevokeSecurityGroupEgress' smart constructor.
data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress'
  { -- | Not supported. Use a set of IP permissions to specify the CIDR.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Not supported. Use a set of IP permissions to specify the port.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The sets of IP permissions. You can\'t specify a destination security
    -- group and a CIDR IP address range in the same set of permissions.
    ipPermissions :: Prelude.Maybe [IpPermission],
    -- | Not supported. Use a set of IP permissions to specify the protocol name
    -- or number.
    ipProtocol :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the security group rules.
    securityGroupRuleIds :: Prelude.Maybe [Prelude.Text],
    -- | Not supported. Use a set of IP permissions to specify a destination
    -- security group.
    sourceSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | Not supported. Use a set of IP permissions to specify a destination
    -- security group.
    sourceSecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | Not supported. Use a set of IP permissions to specify the port.
    toPort :: Prelude.Maybe Prelude.Int,
    -- | The ID of the security group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeSecurityGroupEgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrIp', 'revokeSecurityGroupEgress_cidrIp' - Not supported. Use a set of IP permissions to specify the CIDR.
--
-- 'dryRun', 'revokeSecurityGroupEgress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'fromPort', 'revokeSecurityGroupEgress_fromPort' - Not supported. Use a set of IP permissions to specify the port.
--
-- 'ipPermissions', 'revokeSecurityGroupEgress_ipPermissions' - The sets of IP permissions. You can\'t specify a destination security
-- group and a CIDR IP address range in the same set of permissions.
--
-- 'ipProtocol', 'revokeSecurityGroupEgress_ipProtocol' - Not supported. Use a set of IP permissions to specify the protocol name
-- or number.
--
-- 'securityGroupRuleIds', 'revokeSecurityGroupEgress_securityGroupRuleIds' - The IDs of the security group rules.
--
-- 'sourceSecurityGroupName', 'revokeSecurityGroupEgress_sourceSecurityGroupName' - Not supported. Use a set of IP permissions to specify a destination
-- security group.
--
-- 'sourceSecurityGroupOwnerId', 'revokeSecurityGroupEgress_sourceSecurityGroupOwnerId' - Not supported. Use a set of IP permissions to specify a destination
-- security group.
--
-- 'toPort', 'revokeSecurityGroupEgress_toPort' - Not supported. Use a set of IP permissions to specify the port.
--
-- 'groupId', 'revokeSecurityGroupEgress_groupId' - The ID of the security group.
newRevokeSecurityGroupEgress ::
  -- | 'groupId'
  Prelude.Text ->
  RevokeSecurityGroupEgress
newRevokeSecurityGroupEgress pGroupId_ =
  RevokeSecurityGroupEgress'
    { cidrIp =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      fromPort = Prelude.Nothing,
      ipPermissions = Prelude.Nothing,
      ipProtocol = Prelude.Nothing,
      securityGroupRuleIds = Prelude.Nothing,
      sourceSecurityGroupName = Prelude.Nothing,
      sourceSecurityGroupOwnerId = Prelude.Nothing,
      toPort = Prelude.Nothing,
      groupId = pGroupId_
    }

-- | Not supported. Use a set of IP permissions to specify the CIDR.
revokeSecurityGroupEgress_cidrIp :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupEgress_cidrIp = Lens.lens (\RevokeSecurityGroupEgress' {cidrIp} -> cidrIp) (\s@RevokeSecurityGroupEgress' {} a -> s {cidrIp = a} :: RevokeSecurityGroupEgress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
revokeSecurityGroupEgress_dryRun :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Bool)
revokeSecurityGroupEgress_dryRun = Lens.lens (\RevokeSecurityGroupEgress' {dryRun} -> dryRun) (\s@RevokeSecurityGroupEgress' {} a -> s {dryRun = a} :: RevokeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify the port.
revokeSecurityGroupEgress_fromPort :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Int)
revokeSecurityGroupEgress_fromPort = Lens.lens (\RevokeSecurityGroupEgress' {fromPort} -> fromPort) (\s@RevokeSecurityGroupEgress' {} a -> s {fromPort = a} :: RevokeSecurityGroupEgress)

-- | The sets of IP permissions. You can\'t specify a destination security
-- group and a CIDR IP address range in the same set of permissions.
revokeSecurityGroupEgress_ipPermissions :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe [IpPermission])
revokeSecurityGroupEgress_ipPermissions = Lens.lens (\RevokeSecurityGroupEgress' {ipPermissions} -> ipPermissions) (\s@RevokeSecurityGroupEgress' {} a -> s {ipPermissions = a} :: RevokeSecurityGroupEgress) Prelude.. Lens.mapping Lens.coerced

-- | Not supported. Use a set of IP permissions to specify the protocol name
-- or number.
revokeSecurityGroupEgress_ipProtocol :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupEgress_ipProtocol = Lens.lens (\RevokeSecurityGroupEgress' {ipProtocol} -> ipProtocol) (\s@RevokeSecurityGroupEgress' {} a -> s {ipProtocol = a} :: RevokeSecurityGroupEgress)

-- | The IDs of the security group rules.
revokeSecurityGroupEgress_securityGroupRuleIds :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe [Prelude.Text])
revokeSecurityGroupEgress_securityGroupRuleIds = Lens.lens (\RevokeSecurityGroupEgress' {securityGroupRuleIds} -> securityGroupRuleIds) (\s@RevokeSecurityGroupEgress' {} a -> s {securityGroupRuleIds = a} :: RevokeSecurityGroupEgress) Prelude.. Lens.mapping Lens.coerced

-- | Not supported. Use a set of IP permissions to specify a destination
-- security group.
revokeSecurityGroupEgress_sourceSecurityGroupName :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupEgress_sourceSecurityGroupName = Lens.lens (\RevokeSecurityGroupEgress' {sourceSecurityGroupName} -> sourceSecurityGroupName) (\s@RevokeSecurityGroupEgress' {} a -> s {sourceSecurityGroupName = a} :: RevokeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify a destination
-- security group.
revokeSecurityGroupEgress_sourceSecurityGroupOwnerId :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupEgress_sourceSecurityGroupOwnerId = Lens.lens (\RevokeSecurityGroupEgress' {sourceSecurityGroupOwnerId} -> sourceSecurityGroupOwnerId) (\s@RevokeSecurityGroupEgress' {} a -> s {sourceSecurityGroupOwnerId = a} :: RevokeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify the port.
revokeSecurityGroupEgress_toPort :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Int)
revokeSecurityGroupEgress_toPort = Lens.lens (\RevokeSecurityGroupEgress' {toPort} -> toPort) (\s@RevokeSecurityGroupEgress' {} a -> s {toPort = a} :: RevokeSecurityGroupEgress)

-- | The ID of the security group.
revokeSecurityGroupEgress_groupId :: Lens.Lens' RevokeSecurityGroupEgress Prelude.Text
revokeSecurityGroupEgress_groupId = Lens.lens (\RevokeSecurityGroupEgress' {groupId} -> groupId) (\s@RevokeSecurityGroupEgress' {} a -> s {groupId = a} :: RevokeSecurityGroupEgress)

instance Core.AWSRequest RevokeSecurityGroupEgress where
  type
    AWSResponse RevokeSecurityGroupEgress =
      RevokeSecurityGroupEgressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RevokeSecurityGroupEgressResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> ( x
                            Data..@? "unknownIpPermissionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RevokeSecurityGroupEgress where
  hashWithSalt _salt RevokeSecurityGroupEgress' {..} =
    _salt
      `Prelude.hashWithSalt` cidrIp
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` ipPermissions
      `Prelude.hashWithSalt` ipProtocol
      `Prelude.hashWithSalt` securityGroupRuleIds
      `Prelude.hashWithSalt` sourceSecurityGroupName
      `Prelude.hashWithSalt` sourceSecurityGroupOwnerId
      `Prelude.hashWithSalt` toPort
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData RevokeSecurityGroupEgress where
  rnf RevokeSecurityGroupEgress' {..} =
    Prelude.rnf cidrIp
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf ipPermissions
      `Prelude.seq` Prelude.rnf ipProtocol
      `Prelude.seq` Prelude.rnf securityGroupRuleIds
      `Prelude.seq` Prelude.rnf sourceSecurityGroupName
      `Prelude.seq` Prelude.rnf sourceSecurityGroupOwnerId
      `Prelude.seq` Prelude.rnf toPort
      `Prelude.seq` Prelude.rnf groupId

instance Data.ToHeaders RevokeSecurityGroupEgress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RevokeSecurityGroupEgress where
  toPath = Prelude.const "/"

instance Data.ToQuery RevokeSecurityGroupEgress where
  toQuery RevokeSecurityGroupEgress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RevokeSecurityGroupEgress" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "CidrIp" Data.=: cidrIp,
        "DryRun" Data.=: dryRun,
        "FromPort" Data.=: fromPort,
        Data.toQuery
          ( Data.toQueryList "IpPermissions"
              Prelude.<$> ipPermissions
          ),
        "IpProtocol" Data.=: ipProtocol,
        Data.toQuery
          ( Data.toQueryList "SecurityGroupRuleId"
              Prelude.<$> securityGroupRuleIds
          ),
        "SourceSecurityGroupName"
          Data.=: sourceSecurityGroupName,
        "SourceSecurityGroupOwnerId"
          Data.=: sourceSecurityGroupOwnerId,
        "ToPort" Data.=: toPort,
        "GroupId" Data.=: groupId
      ]

-- | /See:/ 'newRevokeSecurityGroupEgressResponse' smart constructor.
data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The outbound rules that were unknown to the service. In some cases,
    -- @unknownIpPermissionSet@ might be in a different format from the request
    -- parameter.
    unknownIpPermissions :: Prelude.Maybe [IpPermission],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeSecurityGroupEgressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'revokeSecurityGroupEgressResponse_return' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- 'unknownIpPermissions', 'revokeSecurityGroupEgressResponse_unknownIpPermissions' - The outbound rules that were unknown to the service. In some cases,
-- @unknownIpPermissionSet@ might be in a different format from the request
-- parameter.
--
-- 'httpStatus', 'revokeSecurityGroupEgressResponse_httpStatus' - The response's http status code.
newRevokeSecurityGroupEgressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RevokeSecurityGroupEgressResponse
newRevokeSecurityGroupEgressResponse pHttpStatus_ =
  RevokeSecurityGroupEgressResponse'
    { return' =
        Prelude.Nothing,
      unknownIpPermissions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
revokeSecurityGroupEgressResponse_return :: Lens.Lens' RevokeSecurityGroupEgressResponse (Prelude.Maybe Prelude.Bool)
revokeSecurityGroupEgressResponse_return = Lens.lens (\RevokeSecurityGroupEgressResponse' {return'} -> return') (\s@RevokeSecurityGroupEgressResponse' {} a -> s {return' = a} :: RevokeSecurityGroupEgressResponse)

-- | The outbound rules that were unknown to the service. In some cases,
-- @unknownIpPermissionSet@ might be in a different format from the request
-- parameter.
revokeSecurityGroupEgressResponse_unknownIpPermissions :: Lens.Lens' RevokeSecurityGroupEgressResponse (Prelude.Maybe [IpPermission])
revokeSecurityGroupEgressResponse_unknownIpPermissions = Lens.lens (\RevokeSecurityGroupEgressResponse' {unknownIpPermissions} -> unknownIpPermissions) (\s@RevokeSecurityGroupEgressResponse' {} a -> s {unknownIpPermissions = a} :: RevokeSecurityGroupEgressResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
revokeSecurityGroupEgressResponse_httpStatus :: Lens.Lens' RevokeSecurityGroupEgressResponse Prelude.Int
revokeSecurityGroupEgressResponse_httpStatus = Lens.lens (\RevokeSecurityGroupEgressResponse' {httpStatus} -> httpStatus) (\s@RevokeSecurityGroupEgressResponse' {} a -> s {httpStatus = a} :: RevokeSecurityGroupEgressResponse)

instance
  Prelude.NFData
    RevokeSecurityGroupEgressResponse
  where
  rnf RevokeSecurityGroupEgressResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf unknownIpPermissions
      `Prelude.seq` Prelude.rnf httpStatus
