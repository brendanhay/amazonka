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
-- Module      : Amazonka.EC2.AuthorizeSecurityGroupEgress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Adds the specified outbound (egress) rules to a security
-- group for use with a VPC.
--
-- An outbound rule permits instances to send traffic to the specified IPv4
-- or IPv6 CIDR address ranges, or to the instances that are associated
-- with the specified source security groups. When specifying an outbound
-- rule for your security group in a VPC, the @IpPermissions@ must include
-- a destination for the traffic.
--
-- You specify a protocol for each rule (for example, TCP). For the TCP and
-- UDP protocols, you must also specify the destination port or port range.
-- For the ICMP protocol, you must also specify the ICMP type and code. You
-- can use -1 for the type or code to mean all types or all codes.
--
-- Rule changes are propagated to affected instances as quickly as
-- possible. However, a small delay might occur.
--
-- For information about VPC security group quotas, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html Amazon VPC quotas>.
module Amazonka.EC2.AuthorizeSecurityGroupEgress
  ( -- * Creating a Request
    AuthorizeSecurityGroupEgress (..),
    newAuthorizeSecurityGroupEgress,

    -- * Request Lenses
    authorizeSecurityGroupEgress_sourceSecurityGroupName,
    authorizeSecurityGroupEgress_toPort,
    authorizeSecurityGroupEgress_ipPermissions,
    authorizeSecurityGroupEgress_ipProtocol,
    authorizeSecurityGroupEgress_dryRun,
    authorizeSecurityGroupEgress_cidrIp,
    authorizeSecurityGroupEgress_tagSpecifications,
    authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupEgress_fromPort,
    authorizeSecurityGroupEgress_groupId,

    -- * Destructuring the Response
    AuthorizeSecurityGroupEgressResponse (..),
    newAuthorizeSecurityGroupEgressResponse,

    -- * Response Lenses
    authorizeSecurityGroupEgressResponse_securityGroupRules,
    authorizeSecurityGroupEgressResponse_return,
    authorizeSecurityGroupEgressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAuthorizeSecurityGroupEgress' smart constructor.
data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress'
  { -- | Not supported. Use a set of IP permissions to specify a destination
    -- security group.
    sourceSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | Not supported. Use a set of IP permissions to specify the port.
    toPort :: Prelude.Maybe Prelude.Int,
    -- | The sets of IP permissions. You can\'t specify a destination security
    -- group and a CIDR IP address range in the same set of permissions.
    ipPermissions :: Prelude.Maybe [IpPermission],
    -- | Not supported. Use a set of IP permissions to specify the protocol name
    -- or number.
    ipProtocol :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Not supported. Use a set of IP permissions to specify the CIDR.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | The tags applied to the security group rule.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Not supported. Use a set of IP permissions to specify a destination
    -- security group.
    sourceSecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | Not supported. Use a set of IP permissions to specify the port.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The ID of the security group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeSecurityGroupEgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceSecurityGroupName', 'authorizeSecurityGroupEgress_sourceSecurityGroupName' - Not supported. Use a set of IP permissions to specify a destination
-- security group.
--
-- 'toPort', 'authorizeSecurityGroupEgress_toPort' - Not supported. Use a set of IP permissions to specify the port.
--
-- 'ipPermissions', 'authorizeSecurityGroupEgress_ipPermissions' - The sets of IP permissions. You can\'t specify a destination security
-- group and a CIDR IP address range in the same set of permissions.
--
-- 'ipProtocol', 'authorizeSecurityGroupEgress_ipProtocol' - Not supported. Use a set of IP permissions to specify the protocol name
-- or number.
--
-- 'dryRun', 'authorizeSecurityGroupEgress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'cidrIp', 'authorizeSecurityGroupEgress_cidrIp' - Not supported. Use a set of IP permissions to specify the CIDR.
--
-- 'tagSpecifications', 'authorizeSecurityGroupEgress_tagSpecifications' - The tags applied to the security group rule.
--
-- 'sourceSecurityGroupOwnerId', 'authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId' - Not supported. Use a set of IP permissions to specify a destination
-- security group.
--
-- 'fromPort', 'authorizeSecurityGroupEgress_fromPort' - Not supported. Use a set of IP permissions to specify the port.
--
-- 'groupId', 'authorizeSecurityGroupEgress_groupId' - The ID of the security group.
newAuthorizeSecurityGroupEgress ::
  -- | 'groupId'
  Prelude.Text ->
  AuthorizeSecurityGroupEgress
newAuthorizeSecurityGroupEgress pGroupId_ =
  AuthorizeSecurityGroupEgress'
    { sourceSecurityGroupName =
        Prelude.Nothing,
      toPort = Prelude.Nothing,
      ipPermissions = Prelude.Nothing,
      ipProtocol = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      cidrIp = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      sourceSecurityGroupOwnerId = Prelude.Nothing,
      fromPort = Prelude.Nothing,
      groupId = pGroupId_
    }

-- | Not supported. Use a set of IP permissions to specify a destination
-- security group.
authorizeSecurityGroupEgress_sourceSecurityGroupName :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupEgress_sourceSecurityGroupName = Lens.lens (\AuthorizeSecurityGroupEgress' {sourceSecurityGroupName} -> sourceSecurityGroupName) (\s@AuthorizeSecurityGroupEgress' {} a -> s {sourceSecurityGroupName = a} :: AuthorizeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify the port.
authorizeSecurityGroupEgress_toPort :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Int)
authorizeSecurityGroupEgress_toPort = Lens.lens (\AuthorizeSecurityGroupEgress' {toPort} -> toPort) (\s@AuthorizeSecurityGroupEgress' {} a -> s {toPort = a} :: AuthorizeSecurityGroupEgress)

-- | The sets of IP permissions. You can\'t specify a destination security
-- group and a CIDR IP address range in the same set of permissions.
authorizeSecurityGroupEgress_ipPermissions :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe [IpPermission])
authorizeSecurityGroupEgress_ipPermissions = Lens.lens (\AuthorizeSecurityGroupEgress' {ipPermissions} -> ipPermissions) (\s@AuthorizeSecurityGroupEgress' {} a -> s {ipPermissions = a} :: AuthorizeSecurityGroupEgress) Prelude.. Lens.mapping Lens.coerced

-- | Not supported. Use a set of IP permissions to specify the protocol name
-- or number.
authorizeSecurityGroupEgress_ipProtocol :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupEgress_ipProtocol = Lens.lens (\AuthorizeSecurityGroupEgress' {ipProtocol} -> ipProtocol) (\s@AuthorizeSecurityGroupEgress' {} a -> s {ipProtocol = a} :: AuthorizeSecurityGroupEgress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
authorizeSecurityGroupEgress_dryRun :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Bool)
authorizeSecurityGroupEgress_dryRun = Lens.lens (\AuthorizeSecurityGroupEgress' {dryRun} -> dryRun) (\s@AuthorizeSecurityGroupEgress' {} a -> s {dryRun = a} :: AuthorizeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify the CIDR.
authorizeSecurityGroupEgress_cidrIp :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupEgress_cidrIp = Lens.lens (\AuthorizeSecurityGroupEgress' {cidrIp} -> cidrIp) (\s@AuthorizeSecurityGroupEgress' {} a -> s {cidrIp = a} :: AuthorizeSecurityGroupEgress)

-- | The tags applied to the security group rule.
authorizeSecurityGroupEgress_tagSpecifications :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe [TagSpecification])
authorizeSecurityGroupEgress_tagSpecifications = Lens.lens (\AuthorizeSecurityGroupEgress' {tagSpecifications} -> tagSpecifications) (\s@AuthorizeSecurityGroupEgress' {} a -> s {tagSpecifications = a} :: AuthorizeSecurityGroupEgress) Prelude.. Lens.mapping Lens.coerced

-- | Not supported. Use a set of IP permissions to specify a destination
-- security group.
authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId = Lens.lens (\AuthorizeSecurityGroupEgress' {sourceSecurityGroupOwnerId} -> sourceSecurityGroupOwnerId) (\s@AuthorizeSecurityGroupEgress' {} a -> s {sourceSecurityGroupOwnerId = a} :: AuthorizeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify the port.
authorizeSecurityGroupEgress_fromPort :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Int)
authorizeSecurityGroupEgress_fromPort = Lens.lens (\AuthorizeSecurityGroupEgress' {fromPort} -> fromPort) (\s@AuthorizeSecurityGroupEgress' {} a -> s {fromPort = a} :: AuthorizeSecurityGroupEgress)

-- | The ID of the security group.
authorizeSecurityGroupEgress_groupId :: Lens.Lens' AuthorizeSecurityGroupEgress Prelude.Text
authorizeSecurityGroupEgress_groupId = Lens.lens (\AuthorizeSecurityGroupEgress' {groupId} -> groupId) (\s@AuthorizeSecurityGroupEgress' {} a -> s {groupId = a} :: AuthorizeSecurityGroupEgress)

instance Core.AWSRequest AuthorizeSecurityGroupEgress where
  type
    AWSResponse AuthorizeSecurityGroupEgress =
      AuthorizeSecurityGroupEgressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AuthorizeSecurityGroupEgressResponse'
            Prelude.<$> ( x Core..@? "securityGroupRuleSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AuthorizeSecurityGroupEgress
  where
  hashWithSalt _salt AuthorizeSecurityGroupEgress' {..} =
    _salt
      `Prelude.hashWithSalt` sourceSecurityGroupName
      `Prelude.hashWithSalt` toPort
      `Prelude.hashWithSalt` ipPermissions
      `Prelude.hashWithSalt` ipProtocol
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` cidrIp
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` sourceSecurityGroupOwnerId
      `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData AuthorizeSecurityGroupEgress where
  rnf AuthorizeSecurityGroupEgress' {..} =
    Prelude.rnf sourceSecurityGroupName
      `Prelude.seq` Prelude.rnf toPort
      `Prelude.seq` Prelude.rnf ipPermissions
      `Prelude.seq` Prelude.rnf ipProtocol
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf cidrIp
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf sourceSecurityGroupOwnerId
      `Prelude.seq` Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf groupId

instance Core.ToHeaders AuthorizeSecurityGroupEgress where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AuthorizeSecurityGroupEgress where
  toPath = Prelude.const "/"

instance Core.ToQuery AuthorizeSecurityGroupEgress where
  toQuery AuthorizeSecurityGroupEgress' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "AuthorizeSecurityGroupEgress" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "SourceSecurityGroupName"
          Core.=: sourceSecurityGroupName,
        "ToPort" Core.=: toPort,
        Core.toQuery
          ( Core.toQueryList "IpPermissions"
              Prelude.<$> ipPermissions
          ),
        "IpProtocol" Core.=: ipProtocol,
        "DryRun" Core.=: dryRun,
        "CidrIp" Core.=: cidrIp,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "SourceSecurityGroupOwnerId"
          Core.=: sourceSecurityGroupOwnerId,
        "FromPort" Core.=: fromPort,
        "GroupId" Core.=: groupId
      ]

-- | /See:/ 'newAuthorizeSecurityGroupEgressResponse' smart constructor.
data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse'
  { -- | Information about the outbound (egress) security group rules that were
    -- added.
    securityGroupRules :: Prelude.Maybe [SecurityGroupRule],
    -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeSecurityGroupEgressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupRules', 'authorizeSecurityGroupEgressResponse_securityGroupRules' - Information about the outbound (egress) security group rules that were
-- added.
--
-- 'return'', 'authorizeSecurityGroupEgressResponse_return' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- 'httpStatus', 'authorizeSecurityGroupEgressResponse_httpStatus' - The response's http status code.
newAuthorizeSecurityGroupEgressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AuthorizeSecurityGroupEgressResponse
newAuthorizeSecurityGroupEgressResponse pHttpStatus_ =
  AuthorizeSecurityGroupEgressResponse'
    { securityGroupRules =
        Prelude.Nothing,
      return' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the outbound (egress) security group rules that were
-- added.
authorizeSecurityGroupEgressResponse_securityGroupRules :: Lens.Lens' AuthorizeSecurityGroupEgressResponse (Prelude.Maybe [SecurityGroupRule])
authorizeSecurityGroupEgressResponse_securityGroupRules = Lens.lens (\AuthorizeSecurityGroupEgressResponse' {securityGroupRules} -> securityGroupRules) (\s@AuthorizeSecurityGroupEgressResponse' {} a -> s {securityGroupRules = a} :: AuthorizeSecurityGroupEgressResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
authorizeSecurityGroupEgressResponse_return :: Lens.Lens' AuthorizeSecurityGroupEgressResponse (Prelude.Maybe Prelude.Bool)
authorizeSecurityGroupEgressResponse_return = Lens.lens (\AuthorizeSecurityGroupEgressResponse' {return'} -> return') (\s@AuthorizeSecurityGroupEgressResponse' {} a -> s {return' = a} :: AuthorizeSecurityGroupEgressResponse)

-- | The response's http status code.
authorizeSecurityGroupEgressResponse_httpStatus :: Lens.Lens' AuthorizeSecurityGroupEgressResponse Prelude.Int
authorizeSecurityGroupEgressResponse_httpStatus = Lens.lens (\AuthorizeSecurityGroupEgressResponse' {httpStatus} -> httpStatus) (\s@AuthorizeSecurityGroupEgressResponse' {} a -> s {httpStatus = a} :: AuthorizeSecurityGroupEgressResponse)

instance
  Prelude.NFData
    AuthorizeSecurityGroupEgressResponse
  where
  rnf AuthorizeSecurityGroupEgressResponse' {..} =
    Prelude.rnf securityGroupRules
      `Prelude.seq` Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
