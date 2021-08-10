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
-- Module      : Network.AWS.EC2.RevokeSecurityGroupEgress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Removes the specified egress rules from a security group for
-- EC2-VPC. This action does not apply to security groups for use in
-- EC2-Classic. To remove a rule, the values that you specify (for example,
-- ports) must match the existing rule\'s values exactly.
--
-- [Default VPC] If the values you specify do not match the existing
-- rule\'s values, no error is returned, and the output describes the
-- security group rules that were not revoked.
--
-- AWS recommends that you use DescribeSecurityGroups to verify that the
-- rule has been removed.
--
-- Each rule consists of the protocol and the IPv4 or IPv6 CIDR range or
-- source security group. For the TCP and UDP protocols, you must also
-- specify the destination port or range of ports. For the ICMP protocol,
-- you must also specify the ICMP type and code. If the security group rule
-- has a description, you do not have to specify the description to revoke
-- the rule.
--
-- Rule changes are propagated to instances within the security group as
-- quickly as possible. However, a small delay might occur.
module Network.AWS.EC2.RevokeSecurityGroupEgress
  ( -- * Creating a Request
    RevokeSecurityGroupEgress (..),
    newRevokeSecurityGroupEgress,

    -- * Request Lenses
    revokeSecurityGroupEgress_fromPort,
    revokeSecurityGroupEgress_dryRun,
    revokeSecurityGroupEgress_sourceSecurityGroupName,
    revokeSecurityGroupEgress_cidrIp,
    revokeSecurityGroupEgress_ipProtocol,
    revokeSecurityGroupEgress_ipPermissions,
    revokeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    revokeSecurityGroupEgress_toPort,
    revokeSecurityGroupEgress_groupId,

    -- * Destructuring the Response
    RevokeSecurityGroupEgressResponse (..),
    newRevokeSecurityGroupEgressResponse,

    -- * Response Lenses
    revokeSecurityGroupEgressResponse_unknownIpPermissions,
    revokeSecurityGroupEgressResponse_return,
    revokeSecurityGroupEgressResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRevokeSecurityGroupEgress' smart constructor.
data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress'
  { -- | Not supported. Use a set of IP permissions to specify the port.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Not supported. Use a set of IP permissions to specify a destination
    -- security group.
    sourceSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | Not supported. Use a set of IP permissions to specify the CIDR.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | Not supported. Use a set of IP permissions to specify the protocol name
    -- or number.
    ipProtocol :: Prelude.Maybe Prelude.Text,
    -- | The sets of IP permissions. You can\'t specify a destination security
    -- group and a CIDR IP address range in the same set of permissions.
    ipPermissions :: Prelude.Maybe [IpPermission],
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
-- 'fromPort', 'revokeSecurityGroupEgress_fromPort' - Not supported. Use a set of IP permissions to specify the port.
--
-- 'dryRun', 'revokeSecurityGroupEgress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'sourceSecurityGroupName', 'revokeSecurityGroupEgress_sourceSecurityGroupName' - Not supported. Use a set of IP permissions to specify a destination
-- security group.
--
-- 'cidrIp', 'revokeSecurityGroupEgress_cidrIp' - Not supported. Use a set of IP permissions to specify the CIDR.
--
-- 'ipProtocol', 'revokeSecurityGroupEgress_ipProtocol' - Not supported. Use a set of IP permissions to specify the protocol name
-- or number.
--
-- 'ipPermissions', 'revokeSecurityGroupEgress_ipPermissions' - The sets of IP permissions. You can\'t specify a destination security
-- group and a CIDR IP address range in the same set of permissions.
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
    { fromPort =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      sourceSecurityGroupName = Prelude.Nothing,
      cidrIp = Prelude.Nothing,
      ipProtocol = Prelude.Nothing,
      ipPermissions = Prelude.Nothing,
      sourceSecurityGroupOwnerId = Prelude.Nothing,
      toPort = Prelude.Nothing,
      groupId = pGroupId_
    }

-- | Not supported. Use a set of IP permissions to specify the port.
revokeSecurityGroupEgress_fromPort :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Int)
revokeSecurityGroupEgress_fromPort = Lens.lens (\RevokeSecurityGroupEgress' {fromPort} -> fromPort) (\s@RevokeSecurityGroupEgress' {} a -> s {fromPort = a} :: RevokeSecurityGroupEgress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
revokeSecurityGroupEgress_dryRun :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Bool)
revokeSecurityGroupEgress_dryRun = Lens.lens (\RevokeSecurityGroupEgress' {dryRun} -> dryRun) (\s@RevokeSecurityGroupEgress' {} a -> s {dryRun = a} :: RevokeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify a destination
-- security group.
revokeSecurityGroupEgress_sourceSecurityGroupName :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupEgress_sourceSecurityGroupName = Lens.lens (\RevokeSecurityGroupEgress' {sourceSecurityGroupName} -> sourceSecurityGroupName) (\s@RevokeSecurityGroupEgress' {} a -> s {sourceSecurityGroupName = a} :: RevokeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify the CIDR.
revokeSecurityGroupEgress_cidrIp :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupEgress_cidrIp = Lens.lens (\RevokeSecurityGroupEgress' {cidrIp} -> cidrIp) (\s@RevokeSecurityGroupEgress' {} a -> s {cidrIp = a} :: RevokeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify the protocol name
-- or number.
revokeSecurityGroupEgress_ipProtocol :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
revokeSecurityGroupEgress_ipProtocol = Lens.lens (\RevokeSecurityGroupEgress' {ipProtocol} -> ipProtocol) (\s@RevokeSecurityGroupEgress' {} a -> s {ipProtocol = a} :: RevokeSecurityGroupEgress)

-- | The sets of IP permissions. You can\'t specify a destination security
-- group and a CIDR IP address range in the same set of permissions.
revokeSecurityGroupEgress_ipPermissions :: Lens.Lens' RevokeSecurityGroupEgress (Prelude.Maybe [IpPermission])
revokeSecurityGroupEgress_ipPermissions = Lens.lens (\RevokeSecurityGroupEgress' {ipPermissions} -> ipPermissions) (\s@RevokeSecurityGroupEgress' {} a -> s {ipPermissions = a} :: RevokeSecurityGroupEgress) Prelude.. Lens.mapping Lens._Coerce

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RevokeSecurityGroupEgressResponse'
            Prelude.<$> ( x Core..@? "unknownIpPermissionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RevokeSecurityGroupEgress

instance Prelude.NFData RevokeSecurityGroupEgress

instance Core.ToHeaders RevokeSecurityGroupEgress where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RevokeSecurityGroupEgress where
  toPath = Prelude.const "/"

instance Core.ToQuery RevokeSecurityGroupEgress where
  toQuery RevokeSecurityGroupEgress' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RevokeSecurityGroupEgress" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "FromPort" Core.=: fromPort,
        "DryRun" Core.=: dryRun,
        "SourceSecurityGroupName"
          Core.=: sourceSecurityGroupName,
        "CidrIp" Core.=: cidrIp,
        "IpProtocol" Core.=: ipProtocol,
        Core.toQuery
          ( Core.toQueryList "IpPermissions"
              Prelude.<$> ipPermissions
          ),
        "SourceSecurityGroupOwnerId"
          Core.=: sourceSecurityGroupOwnerId,
        "ToPort" Core.=: toPort,
        "GroupId" Core.=: groupId
      ]

-- | /See:/ 'newRevokeSecurityGroupEgressResponse' smart constructor.
data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse'
  { -- | The outbound rules that were unknown to the service. In some cases,
    -- @unknownIpPermissionSet@ might be in a different format from the request
    -- parameter.
    unknownIpPermissions :: Prelude.Maybe [IpPermission],
    -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
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
-- 'unknownIpPermissions', 'revokeSecurityGroupEgressResponse_unknownIpPermissions' - The outbound rules that were unknown to the service. In some cases,
-- @unknownIpPermissionSet@ might be in a different format from the request
-- parameter.
--
-- 'return'', 'revokeSecurityGroupEgressResponse_return' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- 'httpStatus', 'revokeSecurityGroupEgressResponse_httpStatus' - The response's http status code.
newRevokeSecurityGroupEgressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RevokeSecurityGroupEgressResponse
newRevokeSecurityGroupEgressResponse pHttpStatus_ =
  RevokeSecurityGroupEgressResponse'
    { unknownIpPermissions =
        Prelude.Nothing,
      return' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The outbound rules that were unknown to the service. In some cases,
-- @unknownIpPermissionSet@ might be in a different format from the request
-- parameter.
revokeSecurityGroupEgressResponse_unknownIpPermissions :: Lens.Lens' RevokeSecurityGroupEgressResponse (Prelude.Maybe [IpPermission])
revokeSecurityGroupEgressResponse_unknownIpPermissions = Lens.lens (\RevokeSecurityGroupEgressResponse' {unknownIpPermissions} -> unknownIpPermissions) (\s@RevokeSecurityGroupEgressResponse' {} a -> s {unknownIpPermissions = a} :: RevokeSecurityGroupEgressResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
revokeSecurityGroupEgressResponse_return :: Lens.Lens' RevokeSecurityGroupEgressResponse (Prelude.Maybe Prelude.Bool)
revokeSecurityGroupEgressResponse_return = Lens.lens (\RevokeSecurityGroupEgressResponse' {return'} -> return') (\s@RevokeSecurityGroupEgressResponse' {} a -> s {return' = a} :: RevokeSecurityGroupEgressResponse)

-- | The response's http status code.
revokeSecurityGroupEgressResponse_httpStatus :: Lens.Lens' RevokeSecurityGroupEgressResponse Prelude.Int
revokeSecurityGroupEgressResponse_httpStatus = Lens.lens (\RevokeSecurityGroupEgressResponse' {httpStatus} -> httpStatus) (\s@RevokeSecurityGroupEgressResponse' {} a -> s {httpStatus = a} :: RevokeSecurityGroupEgressResponse)

instance
  Prelude.NFData
    RevokeSecurityGroupEgressResponse
