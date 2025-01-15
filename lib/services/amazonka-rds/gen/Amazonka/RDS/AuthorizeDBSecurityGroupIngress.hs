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
-- Module      : Amazonka.RDS.AuthorizeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables ingress to a DBSecurityGroup using one of two forms of
-- authorization. First, EC2 or VPC security groups can be added to the
-- DBSecurityGroup if the application using the database is running on EC2
-- or VPC instances. Second, IP ranges are available if the application
-- accessing your database is running on the internet. Required parameters
-- for this API are one of CIDR range, EC2SecurityGroupId for VPC, or
-- (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId for non-VPC).
--
-- You can\'t authorize ingress from an EC2 security group in one Amazon
-- Web Services Region to an Amazon RDS DB instance in another. You can\'t
-- authorize ingress from a VPC security group in one VPC to an Amazon RDS
-- DB instance in another.
--
-- For an overview of CIDR ranges, go to the
-- <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Wikipedia Tutorial>.
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
module Amazonka.RDS.AuthorizeDBSecurityGroupIngress
  ( -- * Creating a Request
    AuthorizeDBSecurityGroupIngress (..),
    newAuthorizeDBSecurityGroupIngress,

    -- * Request Lenses
    authorizeDBSecurityGroupIngress_cidrip,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupId,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupName,
    authorizeDBSecurityGroupIngress_eC2SecurityGroupOwnerId,
    authorizeDBSecurityGroupIngress_dbSecurityGroupName,

    -- * Destructuring the Response
    AuthorizeDBSecurityGroupIngressResponse (..),
    newAuthorizeDBSecurityGroupIngressResponse,

    -- * Response Lenses
    authorizeDBSecurityGroupIngressResponse_dbSecurityGroup,
    authorizeDBSecurityGroupIngressResponse_httpStatus,
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
-- /See:/ 'newAuthorizeDBSecurityGroupIngress' smart constructor.
data AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngress'
  { -- | The IP range to authorize.
    cidrip :: Prelude.Maybe Prelude.Text,
    -- | Id of the EC2 security group to authorize. For VPC DB security groups,
    -- @EC2SecurityGroupId@ must be provided. Otherwise,
    -- @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or
    -- @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | Name of the EC2 security group to authorize. For VPC DB security groups,
    -- @EC2SecurityGroupId@ must be provided. Otherwise,
    -- @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or
    -- @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | Amazon Web Services account number of the owner of the EC2 security
    -- group specified in the @EC2SecurityGroupName@ parameter. The Amazon Web
    -- Services access key ID isn\'t an acceptable value. For VPC DB security
    -- groups, @EC2SecurityGroupId@ must be provided. Otherwise,
    -- @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or
    -- @EC2SecurityGroupId@ must be provided.
    eC2SecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB security group to add authorization to.
    dbSecurityGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeDBSecurityGroupIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrip', 'authorizeDBSecurityGroupIngress_cidrip' - The IP range to authorize.
--
-- 'eC2SecurityGroupId', 'authorizeDBSecurityGroupIngress_eC2SecurityGroupId' - Id of the EC2 security group to authorize. For VPC DB security groups,
-- @EC2SecurityGroupId@ must be provided. Otherwise,
-- @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
--
-- 'eC2SecurityGroupName', 'authorizeDBSecurityGroupIngress_eC2SecurityGroupName' - Name of the EC2 security group to authorize. For VPC DB security groups,
-- @EC2SecurityGroupId@ must be provided. Otherwise,
-- @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
--
-- 'eC2SecurityGroupOwnerId', 'authorizeDBSecurityGroupIngress_eC2SecurityGroupOwnerId' - Amazon Web Services account number of the owner of the EC2 security
-- group specified in the @EC2SecurityGroupName@ parameter. The Amazon Web
-- Services access key ID isn\'t an acceptable value. For VPC DB security
-- groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
--
-- 'dbSecurityGroupName', 'authorizeDBSecurityGroupIngress_dbSecurityGroupName' - The name of the DB security group to add authorization to.
newAuthorizeDBSecurityGroupIngress ::
  -- | 'dbSecurityGroupName'
  Prelude.Text ->
  AuthorizeDBSecurityGroupIngress
newAuthorizeDBSecurityGroupIngress
  pDBSecurityGroupName_ =
    AuthorizeDBSecurityGroupIngress'
      { cidrip =
          Prelude.Nothing,
        eC2SecurityGroupId = Prelude.Nothing,
        eC2SecurityGroupName = Prelude.Nothing,
        eC2SecurityGroupOwnerId = Prelude.Nothing,
        dbSecurityGroupName =
          pDBSecurityGroupName_
      }

-- | The IP range to authorize.
authorizeDBSecurityGroupIngress_cidrip :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeDBSecurityGroupIngress_cidrip = Lens.lens (\AuthorizeDBSecurityGroupIngress' {cidrip} -> cidrip) (\s@AuthorizeDBSecurityGroupIngress' {} a -> s {cidrip = a} :: AuthorizeDBSecurityGroupIngress)

-- | Id of the EC2 security group to authorize. For VPC DB security groups,
-- @EC2SecurityGroupId@ must be provided. Otherwise,
-- @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
authorizeDBSecurityGroupIngress_eC2SecurityGroupId :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeDBSecurityGroupIngress_eC2SecurityGroupId = Lens.lens (\AuthorizeDBSecurityGroupIngress' {eC2SecurityGroupId} -> eC2SecurityGroupId) (\s@AuthorizeDBSecurityGroupIngress' {} a -> s {eC2SecurityGroupId = a} :: AuthorizeDBSecurityGroupIngress)

-- | Name of the EC2 security group to authorize. For VPC DB security groups,
-- @EC2SecurityGroupId@ must be provided. Otherwise,
-- @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
authorizeDBSecurityGroupIngress_eC2SecurityGroupName :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeDBSecurityGroupIngress_eC2SecurityGroupName = Lens.lens (\AuthorizeDBSecurityGroupIngress' {eC2SecurityGroupName} -> eC2SecurityGroupName) (\s@AuthorizeDBSecurityGroupIngress' {} a -> s {eC2SecurityGroupName = a} :: AuthorizeDBSecurityGroupIngress)

-- | Amazon Web Services account number of the owner of the EC2 security
-- group specified in the @EC2SecurityGroupName@ parameter. The Amazon Web
-- Services access key ID isn\'t an acceptable value. For VPC DB security
-- groups, @EC2SecurityGroupId@ must be provided. Otherwise,
-- @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or
-- @EC2SecurityGroupId@ must be provided.
authorizeDBSecurityGroupIngress_eC2SecurityGroupOwnerId :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Prelude.Maybe Prelude.Text)
authorizeDBSecurityGroupIngress_eC2SecurityGroupOwnerId = Lens.lens (\AuthorizeDBSecurityGroupIngress' {eC2SecurityGroupOwnerId} -> eC2SecurityGroupOwnerId) (\s@AuthorizeDBSecurityGroupIngress' {} a -> s {eC2SecurityGroupOwnerId = a} :: AuthorizeDBSecurityGroupIngress)

-- | The name of the DB security group to add authorization to.
authorizeDBSecurityGroupIngress_dbSecurityGroupName :: Lens.Lens' AuthorizeDBSecurityGroupIngress Prelude.Text
authorizeDBSecurityGroupIngress_dbSecurityGroupName = Lens.lens (\AuthorizeDBSecurityGroupIngress' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@AuthorizeDBSecurityGroupIngress' {} a -> s {dbSecurityGroupName = a} :: AuthorizeDBSecurityGroupIngress)

instance
  Core.AWSRequest
    AuthorizeDBSecurityGroupIngress
  where
  type
    AWSResponse AuthorizeDBSecurityGroupIngress =
      AuthorizeDBSecurityGroupIngressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AuthorizeDBSecurityGroupIngressResult"
      ( \s h x ->
          AuthorizeDBSecurityGroupIngressResponse'
            Prelude.<$> (x Data..@? "DBSecurityGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AuthorizeDBSecurityGroupIngress
  where
  hashWithSalt
    _salt
    AuthorizeDBSecurityGroupIngress' {..} =
      _salt
        `Prelude.hashWithSalt` cidrip
        `Prelude.hashWithSalt` eC2SecurityGroupId
        `Prelude.hashWithSalt` eC2SecurityGroupName
        `Prelude.hashWithSalt` eC2SecurityGroupOwnerId
        `Prelude.hashWithSalt` dbSecurityGroupName

instance
  Prelude.NFData
    AuthorizeDBSecurityGroupIngress
  where
  rnf AuthorizeDBSecurityGroupIngress' {..} =
    Prelude.rnf cidrip `Prelude.seq`
      Prelude.rnf eC2SecurityGroupId `Prelude.seq`
        Prelude.rnf eC2SecurityGroupName `Prelude.seq`
          Prelude.rnf eC2SecurityGroupOwnerId `Prelude.seq`
            Prelude.rnf dbSecurityGroupName

instance
  Data.ToHeaders
    AuthorizeDBSecurityGroupIngress
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AuthorizeDBSecurityGroupIngress where
  toPath = Prelude.const "/"

instance Data.ToQuery AuthorizeDBSecurityGroupIngress where
  toQuery AuthorizeDBSecurityGroupIngress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AuthorizeDBSecurityGroupIngress" ::
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

-- | /See:/ 'newAuthorizeDBSecurityGroupIngressResponse' smart constructor.
data AuthorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse'
  { dbSecurityGroup :: Prelude.Maybe DBSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeDBSecurityGroupIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSecurityGroup', 'authorizeDBSecurityGroupIngressResponse_dbSecurityGroup' - Undocumented member.
--
-- 'httpStatus', 'authorizeDBSecurityGroupIngressResponse_httpStatus' - The response's http status code.
newAuthorizeDBSecurityGroupIngressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AuthorizeDBSecurityGroupIngressResponse
newAuthorizeDBSecurityGroupIngressResponse
  pHttpStatus_ =
    AuthorizeDBSecurityGroupIngressResponse'
      { dbSecurityGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
authorizeDBSecurityGroupIngressResponse_dbSecurityGroup :: Lens.Lens' AuthorizeDBSecurityGroupIngressResponse (Prelude.Maybe DBSecurityGroup)
authorizeDBSecurityGroupIngressResponse_dbSecurityGroup = Lens.lens (\AuthorizeDBSecurityGroupIngressResponse' {dbSecurityGroup} -> dbSecurityGroup) (\s@AuthorizeDBSecurityGroupIngressResponse' {} a -> s {dbSecurityGroup = a} :: AuthorizeDBSecurityGroupIngressResponse)

-- | The response's http status code.
authorizeDBSecurityGroupIngressResponse_httpStatus :: Lens.Lens' AuthorizeDBSecurityGroupIngressResponse Prelude.Int
authorizeDBSecurityGroupIngressResponse_httpStatus = Lens.lens (\AuthorizeDBSecurityGroupIngressResponse' {httpStatus} -> httpStatus) (\s@AuthorizeDBSecurityGroupIngressResponse' {} a -> s {httpStatus = a} :: AuthorizeDBSecurityGroupIngressResponse)

instance
  Prelude.NFData
    AuthorizeDBSecurityGroupIngressResponse
  where
  rnf AuthorizeDBSecurityGroupIngressResponse' {..} =
    Prelude.rnf dbSecurityGroup `Prelude.seq`
      Prelude.rnf httpStatus
