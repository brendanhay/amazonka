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
-- Module      : Network.AWS.RDS.RevokeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes ingress from a DBSecurityGroup for previously authorized IP
-- ranges or EC2 or VPC Security Groups. Required parameters for this API
-- are one of CIDRIP, EC2SecurityGroupId for VPC, or
-- (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId).
module Network.AWS.RDS.RevokeDBSecurityGroupIngress
  ( -- * Creating a Request
    RevokeDBSecurityGroupIngress (..),
    newRevokeDBSecurityGroupIngress,

    -- * Request Lenses
    revokeDBSecurityGroupIngress_cidrip,
    revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId,
    revokeDBSecurityGroupIngress_eC2SecurityGroupId,
    revokeDBSecurityGroupIngress_eC2SecurityGroupName,
    revokeDBSecurityGroupIngress_dbSecurityGroupName,

    -- * Destructuring the Response
    RevokeDBSecurityGroupIngressResponse (..),
    newRevokeDBSecurityGroupIngressResponse,

    -- * Response Lenses
    revokeDBSecurityGroupIngressResponse_dbSecurityGroup,
    revokeDBSecurityGroupIngressResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRevokeDBSecurityGroupIngress' smart constructor.
data RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngress'
  { -- | The IP range to revoke access from. Must be a valid CIDR range. If
    -- @CIDRIP@ is specified, @EC2SecurityGroupName@, @EC2SecurityGroupId@ and
    -- @EC2SecurityGroupOwnerId@ can\'t be provided.
    cidrip :: Core.Maybe Core.Text,
    -- | The AWS account number of the owner of the EC2 security group specified
    -- in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn\'t an
    -- acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must
    -- be provided. Otherwise, EC2SecurityGroupOwnerId and either
    -- @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupOwnerId :: Core.Maybe Core.Text,
    -- | The id of the EC2 security group to revoke access from. For VPC DB
    -- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
    -- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
    -- @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupId :: Core.Maybe Core.Text,
    -- | The name of the EC2 security group to revoke access from. For VPC DB
    -- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
    -- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
    -- @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupName :: Core.Maybe Core.Text,
    -- | The name of the DB security group to revoke ingress from.
    dbSecurityGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevokeDBSecurityGroupIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrip', 'revokeDBSecurityGroupIngress_cidrip' - The IP range to revoke access from. Must be a valid CIDR range. If
-- @CIDRIP@ is specified, @EC2SecurityGroupName@, @EC2SecurityGroupId@ and
-- @EC2SecurityGroupOwnerId@ can\'t be provided.
--
-- 'eC2SecurityGroupOwnerId', 'revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId' - The AWS account number of the owner of the EC2 security group specified
-- in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn\'t an
-- acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must
-- be provided. Otherwise, EC2SecurityGroupOwnerId and either
-- @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- 'eC2SecurityGroupId', 'revokeDBSecurityGroupIngress_eC2SecurityGroupId' - The id of the EC2 security group to revoke access from. For VPC DB
-- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
--
-- 'eC2SecurityGroupName', 'revokeDBSecurityGroupIngress_eC2SecurityGroupName' - The name of the EC2 security group to revoke access from. For VPC DB
-- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
--
-- 'dbSecurityGroupName', 'revokeDBSecurityGroupIngress_dbSecurityGroupName' - The name of the DB security group to revoke ingress from.
newRevokeDBSecurityGroupIngress ::
  -- | 'dbSecurityGroupName'
  Core.Text ->
  RevokeDBSecurityGroupIngress
newRevokeDBSecurityGroupIngress pDBSecurityGroupName_ =
  RevokeDBSecurityGroupIngress'
    { cidrip =
        Core.Nothing,
      eC2SecurityGroupOwnerId = Core.Nothing,
      eC2SecurityGroupId = Core.Nothing,
      eC2SecurityGroupName = Core.Nothing,
      dbSecurityGroupName = pDBSecurityGroupName_
    }

-- | The IP range to revoke access from. Must be a valid CIDR range. If
-- @CIDRIP@ is specified, @EC2SecurityGroupName@, @EC2SecurityGroupId@ and
-- @EC2SecurityGroupOwnerId@ can\'t be provided.
revokeDBSecurityGroupIngress_cidrip :: Lens.Lens' RevokeDBSecurityGroupIngress (Core.Maybe Core.Text)
revokeDBSecurityGroupIngress_cidrip = Lens.lens (\RevokeDBSecurityGroupIngress' {cidrip} -> cidrip) (\s@RevokeDBSecurityGroupIngress' {} a -> s {cidrip = a} :: RevokeDBSecurityGroupIngress)

-- | The AWS account number of the owner of the EC2 security group specified
-- in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn\'t an
-- acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must
-- be provided. Otherwise, EC2SecurityGroupOwnerId and either
-- @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId :: Lens.Lens' RevokeDBSecurityGroupIngress (Core.Maybe Core.Text)
revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId = Lens.lens (\RevokeDBSecurityGroupIngress' {eC2SecurityGroupOwnerId} -> eC2SecurityGroupOwnerId) (\s@RevokeDBSecurityGroupIngress' {} a -> s {eC2SecurityGroupOwnerId = a} :: RevokeDBSecurityGroupIngress)

-- | The id of the EC2 security group to revoke access from. For VPC DB
-- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
revokeDBSecurityGroupIngress_eC2SecurityGroupId :: Lens.Lens' RevokeDBSecurityGroupIngress (Core.Maybe Core.Text)
revokeDBSecurityGroupIngress_eC2SecurityGroupId = Lens.lens (\RevokeDBSecurityGroupIngress' {eC2SecurityGroupId} -> eC2SecurityGroupId) (\s@RevokeDBSecurityGroupIngress' {} a -> s {eC2SecurityGroupId = a} :: RevokeDBSecurityGroupIngress)

-- | The name of the EC2 security group to revoke access from. For VPC DB
-- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
revokeDBSecurityGroupIngress_eC2SecurityGroupName :: Lens.Lens' RevokeDBSecurityGroupIngress (Core.Maybe Core.Text)
revokeDBSecurityGroupIngress_eC2SecurityGroupName = Lens.lens (\RevokeDBSecurityGroupIngress' {eC2SecurityGroupName} -> eC2SecurityGroupName) (\s@RevokeDBSecurityGroupIngress' {} a -> s {eC2SecurityGroupName = a} :: RevokeDBSecurityGroupIngress)

-- | The name of the DB security group to revoke ingress from.
revokeDBSecurityGroupIngress_dbSecurityGroupName :: Lens.Lens' RevokeDBSecurityGroupIngress Core.Text
revokeDBSecurityGroupIngress_dbSecurityGroupName = Lens.lens (\RevokeDBSecurityGroupIngress' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@RevokeDBSecurityGroupIngress' {} a -> s {dbSecurityGroupName = a} :: RevokeDBSecurityGroupIngress)

instance Core.AWSRequest RevokeDBSecurityGroupIngress where
  type
    AWSResponse RevokeDBSecurityGroupIngress =
      RevokeDBSecurityGroupIngressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RevokeDBSecurityGroupIngressResult"
      ( \s h x ->
          RevokeDBSecurityGroupIngressResponse'
            Core.<$> (x Core..@? "DBSecurityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RevokeDBSecurityGroupIngress

instance Core.NFData RevokeDBSecurityGroupIngress

instance Core.ToHeaders RevokeDBSecurityGroupIngress where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RevokeDBSecurityGroupIngress where
  toPath = Core.const "/"

instance Core.ToQuery RevokeDBSecurityGroupIngress where
  toQuery RevokeDBSecurityGroupIngress' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RevokeDBSecurityGroupIngress" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "CIDRIP" Core.=: cidrip,
        "EC2SecurityGroupOwnerId"
          Core.=: eC2SecurityGroupOwnerId,
        "EC2SecurityGroupId" Core.=: eC2SecurityGroupId,
        "EC2SecurityGroupName" Core.=: eC2SecurityGroupName,
        "DBSecurityGroupName" Core.=: dbSecurityGroupName
      ]

-- | /See:/ 'newRevokeDBSecurityGroupIngressResponse' smart constructor.
data RevokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse'
  { dbSecurityGroup :: Core.Maybe DBSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevokeDBSecurityGroupIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSecurityGroup', 'revokeDBSecurityGroupIngressResponse_dbSecurityGroup' - Undocumented member.
--
-- 'httpStatus', 'revokeDBSecurityGroupIngressResponse_httpStatus' - The response's http status code.
newRevokeDBSecurityGroupIngressResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RevokeDBSecurityGroupIngressResponse
newRevokeDBSecurityGroupIngressResponse pHttpStatus_ =
  RevokeDBSecurityGroupIngressResponse'
    { dbSecurityGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
revokeDBSecurityGroupIngressResponse_dbSecurityGroup :: Lens.Lens' RevokeDBSecurityGroupIngressResponse (Core.Maybe DBSecurityGroup)
revokeDBSecurityGroupIngressResponse_dbSecurityGroup = Lens.lens (\RevokeDBSecurityGroupIngressResponse' {dbSecurityGroup} -> dbSecurityGroup) (\s@RevokeDBSecurityGroupIngressResponse' {} a -> s {dbSecurityGroup = a} :: RevokeDBSecurityGroupIngressResponse)

-- | The response's http status code.
revokeDBSecurityGroupIngressResponse_httpStatus :: Lens.Lens' RevokeDBSecurityGroupIngressResponse Core.Int
revokeDBSecurityGroupIngressResponse_httpStatus = Lens.lens (\RevokeDBSecurityGroupIngressResponse' {httpStatus} -> httpStatus) (\s@RevokeDBSecurityGroupIngressResponse' {} a -> s {httpStatus = a} :: RevokeDBSecurityGroupIngressResponse)

instance
  Core.NFData
    RevokeDBSecurityGroupIngressResponse
