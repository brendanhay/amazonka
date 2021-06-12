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
    cidrip :: Core.Maybe Core.Text,
    -- | The AWS account number of the owner of the security group specified in
    -- the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an
    -- acceptable value. If @EC2SecurityGroupOwnerId@ is specified,
    -- @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be
    -- provided.
    --
    -- Example: @111122223333@
    eC2SecurityGroupOwnerId :: Core.Maybe Core.Text,
    -- | The name of the EC2 Security Group whose access is to be revoked. If
    -- @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also
    -- be provided and @CIDRIP@ cannot be provided.
    eC2SecurityGroupName :: Core.Maybe Core.Text,
    -- | The name of the security Group from which to revoke the ingress rule.
    clusterSecurityGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  RevokeClusterSecurityGroupIngress
newRevokeClusterSecurityGroupIngress
  pClusterSecurityGroupName_ =
    RevokeClusterSecurityGroupIngress'
      { cidrip =
          Core.Nothing,
        eC2SecurityGroupOwnerId = Core.Nothing,
        eC2SecurityGroupName = Core.Nothing,
        clusterSecurityGroupName =
          pClusterSecurityGroupName_
      }

-- | The IP range for which to revoke access. This range must be a valid
-- Classless Inter-Domain Routing (CIDR) block of IP addresses. If @CIDRIP@
-- is specified, @EC2SecurityGroupName@ and @EC2SecurityGroupOwnerId@
-- cannot be provided.
revokeClusterSecurityGroupIngress_cidrip :: Lens.Lens' RevokeClusterSecurityGroupIngress (Core.Maybe Core.Text)
revokeClusterSecurityGroupIngress_cidrip = Lens.lens (\RevokeClusterSecurityGroupIngress' {cidrip} -> cidrip) (\s@RevokeClusterSecurityGroupIngress' {} a -> s {cidrip = a} :: RevokeClusterSecurityGroupIngress)

-- | The AWS account number of the owner of the security group specified in
-- the @EC2SecurityGroupName@ parameter. The AWS access key ID is not an
-- acceptable value. If @EC2SecurityGroupOwnerId@ is specified,
-- @EC2SecurityGroupName@ must also be provided. and @CIDRIP@ cannot be
-- provided.
--
-- Example: @111122223333@
revokeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId :: Lens.Lens' RevokeClusterSecurityGroupIngress (Core.Maybe Core.Text)
revokeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId = Lens.lens (\RevokeClusterSecurityGroupIngress' {eC2SecurityGroupOwnerId} -> eC2SecurityGroupOwnerId) (\s@RevokeClusterSecurityGroupIngress' {} a -> s {eC2SecurityGroupOwnerId = a} :: RevokeClusterSecurityGroupIngress)

-- | The name of the EC2 Security Group whose access is to be revoked. If
-- @EC2SecurityGroupName@ is specified, @EC2SecurityGroupOwnerId@ must also
-- be provided and @CIDRIP@ cannot be provided.
revokeClusterSecurityGroupIngress_eC2SecurityGroupName :: Lens.Lens' RevokeClusterSecurityGroupIngress (Core.Maybe Core.Text)
revokeClusterSecurityGroupIngress_eC2SecurityGroupName = Lens.lens (\RevokeClusterSecurityGroupIngress' {eC2SecurityGroupName} -> eC2SecurityGroupName) (\s@RevokeClusterSecurityGroupIngress' {} a -> s {eC2SecurityGroupName = a} :: RevokeClusterSecurityGroupIngress)

-- | The name of the security Group from which to revoke the ingress rule.
revokeClusterSecurityGroupIngress_clusterSecurityGroupName :: Lens.Lens' RevokeClusterSecurityGroupIngress Core.Text
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
            Core.<$> (x Core..@? "ClusterSecurityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    RevokeClusterSecurityGroupIngress

instance
  Core.NFData
    RevokeClusterSecurityGroupIngress

instance
  Core.ToHeaders
    RevokeClusterSecurityGroupIngress
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    RevokeClusterSecurityGroupIngress
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    RevokeClusterSecurityGroupIngress
  where
  toQuery RevokeClusterSecurityGroupIngress' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "RevokeClusterSecurityGroupIngress" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "CIDRIP" Core.=: cidrip,
        "EC2SecurityGroupOwnerId"
          Core.=: eC2SecurityGroupOwnerId,
        "EC2SecurityGroupName" Core.=: eC2SecurityGroupName,
        "ClusterSecurityGroupName"
          Core.=: clusterSecurityGroupName
      ]

-- | /See:/ 'newRevokeClusterSecurityGroupIngressResponse' smart constructor.
data RevokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse'
  { clusterSecurityGroup :: Core.Maybe ClusterSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RevokeClusterSecurityGroupIngressResponse
newRevokeClusterSecurityGroupIngressResponse
  pHttpStatus_ =
    RevokeClusterSecurityGroupIngressResponse'
      { clusterSecurityGroup =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
revokeClusterSecurityGroupIngressResponse_clusterSecurityGroup :: Lens.Lens' RevokeClusterSecurityGroupIngressResponse (Core.Maybe ClusterSecurityGroup)
revokeClusterSecurityGroupIngressResponse_clusterSecurityGroup = Lens.lens (\RevokeClusterSecurityGroupIngressResponse' {clusterSecurityGroup} -> clusterSecurityGroup) (\s@RevokeClusterSecurityGroupIngressResponse' {} a -> s {clusterSecurityGroup = a} :: RevokeClusterSecurityGroupIngressResponse)

-- | The response's http status code.
revokeClusterSecurityGroupIngressResponse_httpStatus :: Lens.Lens' RevokeClusterSecurityGroupIngressResponse Core.Int
revokeClusterSecurityGroupIngressResponse_httpStatus = Lens.lens (\RevokeClusterSecurityGroupIngressResponse' {httpStatus} -> httpStatus) (\s@RevokeClusterSecurityGroupIngressResponse' {} a -> s {httpStatus = a} :: RevokeClusterSecurityGroupIngressResponse)

instance
  Core.NFData
    RevokeClusterSecurityGroupIngressResponse
