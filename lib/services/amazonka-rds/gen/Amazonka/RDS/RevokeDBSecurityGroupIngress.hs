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
-- Module      : Amazonka.RDS.RevokeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes ingress from a DBSecurityGroup for previously authorized IP
-- ranges or EC2 or VPC security groups. Required parameters for this API
-- are one of CIDRIP, EC2SecurityGroupId for VPC, or
-- (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId).
--
-- EC2-Classic was retired on August 15, 2022. If you haven\'t migrated
-- from EC2-Classic to a VPC, we recommend that you migrate as soon as
-- possible. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon EC2 User Guide/, the blog
-- <http://aws.amazon.com/blogs/aws/ec2-classic-is-retiring-heres-how-to-prepare/ EC2-Classic Networking is Retiring – Here’s How to Prepare>,
-- and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.Non-VPC2VPC.html Moving a DB instance not in a VPC into a VPC>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.RevokeDBSecurityGroupIngress
  ( -- * Creating a Request
    RevokeDBSecurityGroupIngress (..),
    newRevokeDBSecurityGroupIngress,

    -- * Request Lenses
    revokeDBSecurityGroupIngress_cidrip,
    revokeDBSecurityGroupIngress_eC2SecurityGroupId,
    revokeDBSecurityGroupIngress_eC2SecurityGroupName,
    revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId,
    revokeDBSecurityGroupIngress_dbSecurityGroupName,

    -- * Destructuring the Response
    RevokeDBSecurityGroupIngressResponse (..),
    newRevokeDBSecurityGroupIngressResponse,

    -- * Response Lenses
    revokeDBSecurityGroupIngressResponse_dbSecurityGroup,
    revokeDBSecurityGroupIngressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newRevokeDBSecurityGroupIngress' smart constructor.
data RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngress'
  { -- | The IP range to revoke access from. Must be a valid CIDR range. If
    -- @CIDRIP@ is specified, @EC2SecurityGroupName@, @EC2SecurityGroupId@ and
    -- @EC2SecurityGroupOwnerId@ can\'t be provided.
    cidrip :: Prelude.Maybe Prelude.Text,
    -- | The id of the EC2 security group to revoke access from. For VPC DB
    -- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
    -- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
    -- @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the EC2 security group to revoke access from. For VPC DB
    -- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
    -- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
    -- @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account number of the owner of the EC2 security
    -- group specified in the @EC2SecurityGroupName@ parameter. The Amazon Web
    -- Services access key ID isn\'t an acceptable value. For VPC DB security
    -- groups, @EC2SecurityGroupId@ must be provided. Otherwise,
    -- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
    -- @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB security group to revoke ingress from.
    dbSecurityGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'eC2SecurityGroupOwnerId', 'revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId' - The Amazon Web Services account number of the owner of the EC2 security
-- group specified in the @EC2SecurityGroupName@ parameter. The Amazon Web
-- Services access key ID isn\'t an acceptable value. For VPC DB security
-- groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
--
-- 'dbSecurityGroupName', 'revokeDBSecurityGroupIngress_dbSecurityGroupName' - The name of the DB security group to revoke ingress from.
newRevokeDBSecurityGroupIngress ::
  -- | 'dbSecurityGroupName'
  Prelude.Text ->
  RevokeDBSecurityGroupIngress
newRevokeDBSecurityGroupIngress pDBSecurityGroupName_ =
  RevokeDBSecurityGroupIngress'
    { cidrip =
        Prelude.Nothing,
      eC2SecurityGroupId = Prelude.Nothing,
      eC2SecurityGroupName = Prelude.Nothing,
      eC2SecurityGroupOwnerId = Prelude.Nothing,
      dbSecurityGroupName = pDBSecurityGroupName_
    }

-- | The IP range to revoke access from. Must be a valid CIDR range. If
-- @CIDRIP@ is specified, @EC2SecurityGroupName@, @EC2SecurityGroupId@ and
-- @EC2SecurityGroupOwnerId@ can\'t be provided.
revokeDBSecurityGroupIngress_cidrip :: Lens.Lens' RevokeDBSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeDBSecurityGroupIngress_cidrip = Lens.lens (\RevokeDBSecurityGroupIngress' {cidrip} -> cidrip) (\s@RevokeDBSecurityGroupIngress' {} a -> s {cidrip = a} :: RevokeDBSecurityGroupIngress)

-- | The id of the EC2 security group to revoke access from. For VPC DB
-- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
revokeDBSecurityGroupIngress_eC2SecurityGroupId :: Lens.Lens' RevokeDBSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeDBSecurityGroupIngress_eC2SecurityGroupId = Lens.lens (\RevokeDBSecurityGroupIngress' {eC2SecurityGroupId} -> eC2SecurityGroupId) (\s@RevokeDBSecurityGroupIngress' {} a -> s {eC2SecurityGroupId = a} :: RevokeDBSecurityGroupIngress)

-- | The name of the EC2 security group to revoke access from. For VPC DB
-- security groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
revokeDBSecurityGroupIngress_eC2SecurityGroupName :: Lens.Lens' RevokeDBSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeDBSecurityGroupIngress_eC2SecurityGroupName = Lens.lens (\RevokeDBSecurityGroupIngress' {eC2SecurityGroupName} -> eC2SecurityGroupName) (\s@RevokeDBSecurityGroupIngress' {} a -> s {eC2SecurityGroupName = a} :: RevokeDBSecurityGroupIngress)

-- | The Amazon Web Services account number of the owner of the EC2 security
-- group specified in the @EC2SecurityGroupName@ parameter. The Amazon Web
-- Services access key ID isn\'t an acceptable value. For VPC DB security
-- groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId :: Lens.Lens' RevokeDBSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeDBSecurityGroupIngress_eC2SecurityGroupOwnerId = Lens.lens (\RevokeDBSecurityGroupIngress' {eC2SecurityGroupOwnerId} -> eC2SecurityGroupOwnerId) (\s@RevokeDBSecurityGroupIngress' {} a -> s {eC2SecurityGroupOwnerId = a} :: RevokeDBSecurityGroupIngress)

-- | The name of the DB security group to revoke ingress from.
revokeDBSecurityGroupIngress_dbSecurityGroupName :: Lens.Lens' RevokeDBSecurityGroupIngress Prelude.Text
revokeDBSecurityGroupIngress_dbSecurityGroupName = Lens.lens (\RevokeDBSecurityGroupIngress' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@RevokeDBSecurityGroupIngress' {} a -> s {dbSecurityGroupName = a} :: RevokeDBSecurityGroupIngress)

instance Core.AWSRequest RevokeDBSecurityGroupIngress where
  type
    AWSResponse RevokeDBSecurityGroupIngress =
      RevokeDBSecurityGroupIngressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RevokeDBSecurityGroupIngressResult"
      ( \s h x ->
          RevokeDBSecurityGroupIngressResponse'
            Prelude.<$> (x Data..@? "DBSecurityGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RevokeDBSecurityGroupIngress
  where
  hashWithSalt _salt RevokeDBSecurityGroupIngress' {..} =
    _salt
      `Prelude.hashWithSalt` cidrip
      `Prelude.hashWithSalt` eC2SecurityGroupId
      `Prelude.hashWithSalt` eC2SecurityGroupName
      `Prelude.hashWithSalt` eC2SecurityGroupOwnerId
      `Prelude.hashWithSalt` dbSecurityGroupName

instance Prelude.NFData RevokeDBSecurityGroupIngress where
  rnf RevokeDBSecurityGroupIngress' {..} =
    Prelude.rnf cidrip
      `Prelude.seq` Prelude.rnf eC2SecurityGroupId
      `Prelude.seq` Prelude.rnf eC2SecurityGroupName
      `Prelude.seq` Prelude.rnf eC2SecurityGroupOwnerId
      `Prelude.seq` Prelude.rnf dbSecurityGroupName

instance Data.ToHeaders RevokeDBSecurityGroupIngress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RevokeDBSecurityGroupIngress where
  toPath = Prelude.const "/"

instance Data.ToQuery RevokeDBSecurityGroupIngress where
  toQuery RevokeDBSecurityGroupIngress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RevokeDBSecurityGroupIngress" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "CIDRIP" Data.=: cidrip,
        "EC2SecurityGroupId" Data.=: eC2SecurityGroupId,
        "EC2SecurityGroupName" Data.=: eC2SecurityGroupName,
        "EC2SecurityGroupOwnerId"
          Data.=: eC2SecurityGroupOwnerId,
        "DBSecurityGroupName" Data.=: dbSecurityGroupName
      ]

-- | /See:/ 'newRevokeDBSecurityGroupIngressResponse' smart constructor.
data RevokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse'
  { dbSecurityGroup :: Prelude.Maybe DBSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RevokeDBSecurityGroupIngressResponse
newRevokeDBSecurityGroupIngressResponse pHttpStatus_ =
  RevokeDBSecurityGroupIngressResponse'
    { dbSecurityGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
revokeDBSecurityGroupIngressResponse_dbSecurityGroup :: Lens.Lens' RevokeDBSecurityGroupIngressResponse (Prelude.Maybe DBSecurityGroup)
revokeDBSecurityGroupIngressResponse_dbSecurityGroup = Lens.lens (\RevokeDBSecurityGroupIngressResponse' {dbSecurityGroup} -> dbSecurityGroup) (\s@RevokeDBSecurityGroupIngressResponse' {} a -> s {dbSecurityGroup = a} :: RevokeDBSecurityGroupIngressResponse)

-- | The response's http status code.
revokeDBSecurityGroupIngressResponse_httpStatus :: Lens.Lens' RevokeDBSecurityGroupIngressResponse Prelude.Int
revokeDBSecurityGroupIngressResponse_httpStatus = Lens.lens (\RevokeDBSecurityGroupIngressResponse' {httpStatus} -> httpStatus) (\s@RevokeDBSecurityGroupIngressResponse' {} a -> s {httpStatus = a} :: RevokeDBSecurityGroupIngressResponse)

instance
  Prelude.NFData
    RevokeDBSecurityGroupIngressResponse
  where
  rnf RevokeDBSecurityGroupIngressResponse' {..} =
    Prelude.rnf dbSecurityGroup
      `Prelude.seq` Prelude.rnf httpStatus
