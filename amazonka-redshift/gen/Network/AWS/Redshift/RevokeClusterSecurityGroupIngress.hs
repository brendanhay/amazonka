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
-- Module      : Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes an ingress rule in an Amazon Redshift security group for a
-- previously authorized IP range or Amazon EC2 security group. To add an
-- ingress rule, see AuthorizeClusterSecurityGroupIngress. For information
-- about managing security groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
module Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
  ( -- * Creating a Request
    RevokeClusterSecurityGroupIngress (..),
    newRevokeClusterSecurityGroupIngress,

    -- * Request Lenses
    revokeClusterSecurityGroupIngress_cidrip,
    revokeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId,
    revokeClusterSecurityGroupIngress_eC2SecurityGroupName,
    revokeClusterSecurityGroupIngress_clusterSecurityGroupName,

    -- * Destructuring the Response
    RevokeClusterSecurityGroupIngressResponse (..),
    newRevokeClusterSecurityGroupIngressResponse,

    -- * Response Lenses
    revokeClusterSecurityGroupIngressResponse_clusterSecurityGroup,
    revokeClusterSecurityGroupIngressResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRevokeClusterSecurityGroupIngress' smart constructor.
data RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngress'
  { -- | The IP range for which to revoke access. This range must be a valid
    -- Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@
    -- is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@
    -- cannot be provided.
    cidrip :: Prelude.Maybe Prelude.Text,
    -- | The AWS account number of the owner of the security group specified in
    -- the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an
    -- acceptable value. If @EC2SecurityGroupOwnerId@ is specified,
    -- @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be
    -- provided.
    --
    -- Example: @111122223333@
    eC2SecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The name of the EC2 Security Group whose access is to be revoked. If
    -- @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also
    -- be provided and @CIDRIP@ cannot be provided.
    eC2SecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the security Group from which to revoke the ingress rule.
    clusterSecurityGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeClusterSecurityGroupIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrip', 'revokeClusterSecurityGroupIngress_cidrip' - The IP range for which to revoke access. This range must be a valid
-- Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@
-- is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@
-- cannot be provided.
--
-- 'eC2SecurityGroupOwnerId', 'revokeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId' - The AWS account number of the owner of the security group specified in
-- the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an
-- acceptable value. If @EC2SecurityGroupOwnerId@ is specified,
-- @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be
-- provided.
--
-- Example: @111122223333@
--
-- 'eC2SecurityGroupName', 'revokeClusterSecurityGroupIngress_eC2SecurityGroupName' - The name of the EC2 Security Group whose access is to be revoked. If
-- @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also
-- be provided and @CIDRIP@ cannot be provided.
--
-- 'clusterSecurityGroupName', 'revokeClusterSecurityGroupIngress_clusterSecurityGroupName' - The name of the security Group from which to revoke the ingress rule.
newRevokeClusterSecurityGroupIngress ::
  -- | 'clusterSecurityGroupName'
  Prelude.Text ->
  RevokeClusterSecurityGroupIngress
newRevokeClusterSecurityGroupIngress
  pClusterSecurityGroupName_ =
    RevokeClusterSecurityGroupIngress'
      { cidrip =
          Prelude.Nothing,
        eC2SecurityGroupOwnerId =
          Prelude.Nothing,
        eC2SecurityGroupName = Prelude.Nothing,
        clusterSecurityGroupName =
          pClusterSecurityGroupName_
      }

-- | The IP range for which to revoke access. This range must be a valid
-- Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@
-- is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@
-- cannot be provided.
revokeClusterSecurityGroupIngress_cidrip :: Lens.Lens' RevokeClusterSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeClusterSecurityGroupIngress_cidrip = Lens.lens (\RevokeClusterSecurityGroupIngress' {cidrip} -> cidrip) (\s@RevokeClusterSecurityGroupIngress' {} a -> s {cidrip = a} :: RevokeClusterSecurityGroupIngress)

-- | The AWS account number of the owner of the security group specified in
-- the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an
-- acceptable value. If @EC2SecurityGroupOwnerId@ is specified,
-- @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be
-- provided.
--
-- Example: @111122223333@
revokeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId :: Lens.Lens' RevokeClusterSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId = Lens.lens (\RevokeClusterSecurityGroupIngress' {eC2SecurityGroupOwnerId} -> eC2SecurityGroupOwnerId) (\s@RevokeClusterSecurityGroupIngress' {} a -> s {eC2SecurityGroupOwnerId = a} :: RevokeClusterSecurityGroupIngress)

-- | The name of the EC2 Security Group whose access is to be revoked. If
-- @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also
-- be provided and @CIDRIP@ cannot be provided.
revokeClusterSecurityGroupIngress_eC2SecurityGroupName :: Lens.Lens' RevokeClusterSecurityGroupIngress (Prelude.Maybe Prelude.Text)
revokeClusterSecurityGroupIngress_eC2SecurityGroupName = Lens.lens (\RevokeClusterSecurityGroupIngress' {eC2SecurityGroupName} -> eC2SecurityGroupName) (\s@RevokeClusterSecurityGroupIngress' {} a -> s {eC2SecurityGroupName = a} :: RevokeClusterSecurityGroupIngress)

-- | The name of the security Group from which to revoke the ingress rule.
revokeClusterSecurityGroupIngress_clusterSecurityGroupName :: Lens.Lens' RevokeClusterSecurityGroupIngress Prelude.Text
revokeClusterSecurityGroupIngress_clusterSecurityGroupName = Lens.lens (\RevokeClusterSecurityGroupIngress' {clusterSecurityGroupName} -> clusterSecurityGroupName) (\s@RevokeClusterSecurityGroupIngress' {} a -> s {clusterSecurityGroupName = a} :: RevokeClusterSecurityGroupIngress)

instance
  Core.AWSRequest
    RevokeClusterSecurityGroupIngress
  where
  type
    AWSResponse RevokeClusterSecurityGroupIngress =
      RevokeClusterSecurityGroupIngressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RevokeClusterSecurityGroupIngressResult"
      ( \s h x ->
          RevokeClusterSecurityGroupIngressResponse'
            Prelude.<$> (x Core..@? "ClusterSecurityGroup")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RevokeClusterSecurityGroupIngress

instance
  Prelude.NFData
    RevokeClusterSecurityGroupIngress

instance
  Core.ToHeaders
    RevokeClusterSecurityGroupIngress
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    RevokeClusterSecurityGroupIngress
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    RevokeClusterSecurityGroupIngress
  where
  toQuery RevokeClusterSecurityGroupIngress' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "RevokeClusterSecurityGroupIngress" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "CIDRIP" Core.=: cidrip,
        "EC2SecurityGroupOwnerId"
          Core.=: eC2SecurityGroupOwnerId,
        "EC2SecurityGroupName" Core.=: eC2SecurityGroupName,
        "ClusterSecurityGroupName"
          Core.=: clusterSecurityGroupName
      ]

-- | /See:/ 'newRevokeClusterSecurityGroupIngressResponse' smart constructor.
data RevokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse'
  { clusterSecurityGroup :: Prelude.Maybe ClusterSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeClusterSecurityGroupIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSecurityGroup', 'revokeClusterSecurityGroupIngressResponse_clusterSecurityGroup' - Undocumented member.
--
-- 'httpStatus', 'revokeClusterSecurityGroupIngressResponse_httpStatus' - The response's http status code.
newRevokeClusterSecurityGroupIngressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RevokeClusterSecurityGroupIngressResponse
newRevokeClusterSecurityGroupIngressResponse
  pHttpStatus_ =
    RevokeClusterSecurityGroupIngressResponse'
      { clusterSecurityGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
revokeClusterSecurityGroupIngressResponse_clusterSecurityGroup :: Lens.Lens' RevokeClusterSecurityGroupIngressResponse (Prelude.Maybe ClusterSecurityGroup)
revokeClusterSecurityGroupIngressResponse_clusterSecurityGroup = Lens.lens (\RevokeClusterSecurityGroupIngressResponse' {clusterSecurityGroup} -> clusterSecurityGroup) (\s@RevokeClusterSecurityGroupIngressResponse' {} a -> s {clusterSecurityGroup = a} :: RevokeClusterSecurityGroupIngressResponse)

-- | The response's http status code.
revokeClusterSecurityGroupIngressResponse_httpStatus :: Lens.Lens' RevokeClusterSecurityGroupIngressResponse Prelude.Int
revokeClusterSecurityGroupIngressResponse_httpStatus = Lens.lens (\RevokeClusterSecurityGroupIngressResponse' {httpStatus} -> httpStatus) (\s@RevokeClusterSecurityGroupIngressResponse' {} a -> s {httpStatus = a} :: RevokeClusterSecurityGroupIngressResponse)

instance
  Prelude.NFData
    RevokeClusterSecurityGroupIngressResponse
