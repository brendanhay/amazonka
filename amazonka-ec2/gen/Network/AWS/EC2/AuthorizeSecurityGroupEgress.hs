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
-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupEgress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Adds the specified egress rules to a security group for use
-- with a VPC.
--
-- An outbound rule permits instances to send traffic to the specified IPv4
-- or IPv6 CIDR address ranges, or to the instances associated with the
-- specified destination security groups.
--
-- You specify a protocol for each rule (for example, TCP). For the TCP and
-- UDP protocols, you must also specify the destination port or port range.
-- For the ICMP protocol, you must also specify the ICMP type and code. You
-- can use -1 for the type or code to mean all types or all codes.
--
-- Rule changes are propagated to affected instances as quickly as
-- possible. However, a small delay might occur.
--
-- For more information about VPC security group limits, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html Amazon VPC Limits>.
module Network.AWS.EC2.AuthorizeSecurityGroupEgress
  ( -- * Creating a Request
    AuthorizeSecurityGroupEgress (..),
    newAuthorizeSecurityGroupEgress,

    -- * Request Lenses
    authorizeSecurityGroupEgress_fromPort,
    authorizeSecurityGroupEgress_dryRun,
    authorizeSecurityGroupEgress_sourceSecurityGroupName,
    authorizeSecurityGroupEgress_cidrIp,
    authorizeSecurityGroupEgress_ipProtocol,
    authorizeSecurityGroupEgress_ipPermissions,
    authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupEgress_toPort,
    authorizeSecurityGroupEgress_groupId,

    -- * Destructuring the Response
    AuthorizeSecurityGroupEgressResponse (..),
    newAuthorizeSecurityGroupEgressResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAuthorizeSecurityGroupEgress' smart constructor.
data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress'
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeSecurityGroupEgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'authorizeSecurityGroupEgress_fromPort' - Not supported. Use a set of IP permissions to specify the port.
--
-- 'dryRun', 'authorizeSecurityGroupEgress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'sourceSecurityGroupName', 'authorizeSecurityGroupEgress_sourceSecurityGroupName' - Not supported. Use a set of IP permissions to specify a destination
-- security group.
--
-- 'cidrIp', 'authorizeSecurityGroupEgress_cidrIp' - Not supported. Use a set of IP permissions to specify the CIDR.
--
-- 'ipProtocol', 'authorizeSecurityGroupEgress_ipProtocol' - Not supported. Use a set of IP permissions to specify the protocol name
-- or number.
--
-- 'ipPermissions', 'authorizeSecurityGroupEgress_ipPermissions' - The sets of IP permissions. You can\'t specify a destination security
-- group and a CIDR IP address range in the same set of permissions.
--
-- 'sourceSecurityGroupOwnerId', 'authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId' - Not supported. Use a set of IP permissions to specify a destination
-- security group.
--
-- 'toPort', 'authorizeSecurityGroupEgress_toPort' - Not supported. Use a set of IP permissions to specify the port.
--
-- 'groupId', 'authorizeSecurityGroupEgress_groupId' - The ID of the security group.
newAuthorizeSecurityGroupEgress ::
  -- | 'groupId'
  Prelude.Text ->
  AuthorizeSecurityGroupEgress
newAuthorizeSecurityGroupEgress pGroupId_ =
  AuthorizeSecurityGroupEgress'
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
authorizeSecurityGroupEgress_fromPort :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Int)
authorizeSecurityGroupEgress_fromPort = Lens.lens (\AuthorizeSecurityGroupEgress' {fromPort} -> fromPort) (\s@AuthorizeSecurityGroupEgress' {} a -> s {fromPort = a} :: AuthorizeSecurityGroupEgress)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
authorizeSecurityGroupEgress_dryRun :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Bool)
authorizeSecurityGroupEgress_dryRun = Lens.lens (\AuthorizeSecurityGroupEgress' {dryRun} -> dryRun) (\s@AuthorizeSecurityGroupEgress' {} a -> s {dryRun = a} :: AuthorizeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify a destination
-- security group.
authorizeSecurityGroupEgress_sourceSecurityGroupName :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupEgress_sourceSecurityGroupName = Lens.lens (\AuthorizeSecurityGroupEgress' {sourceSecurityGroupName} -> sourceSecurityGroupName) (\s@AuthorizeSecurityGroupEgress' {} a -> s {sourceSecurityGroupName = a} :: AuthorizeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify the CIDR.
authorizeSecurityGroupEgress_cidrIp :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupEgress_cidrIp = Lens.lens (\AuthorizeSecurityGroupEgress' {cidrIp} -> cidrIp) (\s@AuthorizeSecurityGroupEgress' {} a -> s {cidrIp = a} :: AuthorizeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify the protocol name
-- or number.
authorizeSecurityGroupEgress_ipProtocol :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupEgress_ipProtocol = Lens.lens (\AuthorizeSecurityGroupEgress' {ipProtocol} -> ipProtocol) (\s@AuthorizeSecurityGroupEgress' {} a -> s {ipProtocol = a} :: AuthorizeSecurityGroupEgress)

-- | The sets of IP permissions. You can\'t specify a destination security
-- group and a CIDR IP address range in the same set of permissions.
authorizeSecurityGroupEgress_ipPermissions :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe [IpPermission])
authorizeSecurityGroupEgress_ipPermissions = Lens.lens (\AuthorizeSecurityGroupEgress' {ipPermissions} -> ipPermissions) (\s@AuthorizeSecurityGroupEgress' {} a -> s {ipPermissions = a} :: AuthorizeSecurityGroupEgress) Prelude.. Lens.mapping Prelude._Coerce

-- | Not supported. Use a set of IP permissions to specify a destination
-- security group.
authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Text)
authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId = Lens.lens (\AuthorizeSecurityGroupEgress' {sourceSecurityGroupOwnerId} -> sourceSecurityGroupOwnerId) (\s@AuthorizeSecurityGroupEgress' {} a -> s {sourceSecurityGroupOwnerId = a} :: AuthorizeSecurityGroupEgress)

-- | Not supported. Use a set of IP permissions to specify the port.
authorizeSecurityGroupEgress_toPort :: Lens.Lens' AuthorizeSecurityGroupEgress (Prelude.Maybe Prelude.Int)
authorizeSecurityGroupEgress_toPort = Lens.lens (\AuthorizeSecurityGroupEgress' {toPort} -> toPort) (\s@AuthorizeSecurityGroupEgress' {} a -> s {toPort = a} :: AuthorizeSecurityGroupEgress)

-- | The ID of the security group.
authorizeSecurityGroupEgress_groupId :: Lens.Lens' AuthorizeSecurityGroupEgress Prelude.Text
authorizeSecurityGroupEgress_groupId = Lens.lens (\AuthorizeSecurityGroupEgress' {groupId} -> groupId) (\s@AuthorizeSecurityGroupEgress' {} a -> s {groupId = a} :: AuthorizeSecurityGroupEgress)

instance
  Prelude.AWSRequest
    AuthorizeSecurityGroupEgress
  where
  type
    Rs AuthorizeSecurityGroupEgress =
      AuthorizeSecurityGroupEgressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      AuthorizeSecurityGroupEgressResponse'

instance
  Prelude.Hashable
    AuthorizeSecurityGroupEgress

instance Prelude.NFData AuthorizeSecurityGroupEgress

instance
  Prelude.ToHeaders
    AuthorizeSecurityGroupEgress
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AuthorizeSecurityGroupEgress where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AuthorizeSecurityGroupEgress where
  toQuery AuthorizeSecurityGroupEgress' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "AuthorizeSecurityGroupEgress" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "FromPort" Prelude.=: fromPort,
        "DryRun" Prelude.=: dryRun,
        "SourceSecurityGroupName"
          Prelude.=: sourceSecurityGroupName,
        "CidrIp" Prelude.=: cidrIp,
        "IpProtocol" Prelude.=: ipProtocol,
        Prelude.toQuery
          ( Prelude.toQueryList "IpPermissions"
              Prelude.<$> ipPermissions
          ),
        "SourceSecurityGroupOwnerId"
          Prelude.=: sourceSecurityGroupOwnerId,
        "ToPort" Prelude.=: toPort,
        "GroupId" Prelude.=: groupId
      ]

-- | /See:/ 'newAuthorizeSecurityGroupEgressResponse' smart constructor.
data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeSecurityGroupEgressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAuthorizeSecurityGroupEgressResponse ::
  AuthorizeSecurityGroupEgressResponse
newAuthorizeSecurityGroupEgressResponse =
  AuthorizeSecurityGroupEgressResponse'

instance
  Prelude.NFData
    AuthorizeSecurityGroupEgressResponse
