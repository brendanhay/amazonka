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
-- Module      : Amazonka.DocDbElastic.CreateCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Elastic DocumentDB cluster and returns its Cluster
-- structure.
module Amazonka.DocDbElastic.CreateCluster
  ( -- * Creating a Request
    CreateCluster (..),
    newCreateCluster,

    -- * Request Lenses
    createCluster_clientToken,
    createCluster_kmsKeyId,
    createCluster_preferredMaintenanceWindow,
    createCluster_subnetIds,
    createCluster_tags,
    createCluster_vpcSecurityGroupIds,
    createCluster_adminUserName,
    createCluster_adminUserPassword,
    createCluster_authType,
    createCluster_clusterName,
    createCluster_shardCapacity,
    createCluster_shardCount,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_httpStatus,
    createClusterResponse_cluster,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | The client token for the Elastic DocumentDB cluster.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The KMS key identifier to use to encrypt the new Elastic DocumentDB
    -- cluster.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
    -- encryption key. If you are creating a cluster using the same Amazon
    -- account that owns this KMS encryption key, you can use the KMS key alias
    -- instead of the ARN as the KMS encryption key.
    --
    -- If an encryption key is not specified, Elastic DocumentDB uses the
    -- default encryption key that KMS creates for your account. Your account
    -- has a different default encryption key for each Amazon Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- /Format/: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- /Default/: a 30-minute window selected at random from an 8-hour block of
    -- time for each Amazon Web Services Region, occurring on a random day of
    -- the week.
    --
    -- /Valid days/: Mon, Tue, Wed, Thu, Fri, Sat, Sun
    --
    -- /Constraints/: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 subnet IDs for the new Elastic DocumentDB cluster.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags to be assigned to the new Elastic DocumentDB cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of EC2 VPC security groups to associate with the new Elastic
    -- DocumentDB cluster.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Elastic DocumentDB cluster administrator.
    --
    -- /Constraints/:
    --
    -- -   Must be from 1 to 63 letters or numbers.
    --
    -- -   The first character must be a letter.
    --
    -- -   Cannot be a reserved word.
    adminUserName :: Prelude.Text,
    -- | The password for the Elastic DocumentDB cluster administrator and can
    -- contain any printable ASCII characters.
    --
    -- /Constraints/:
    --
    -- -   Must contain from 8 to 100 characters.
    --
    -- -   Cannot contain a forward slash (\/), double quote (\"), or the
    --     \"at\" symbol (\@).
    adminUserPassword :: Data.Sensitive Prelude.Text,
    -- | The authentication type for the Elastic DocumentDB cluster.
    authType :: Auth,
    -- | The name of the new Elastic DocumentDB cluster. This parameter is stored
    -- as a lowercase string.
    --
    -- /Constraints/:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- /Example/: @my-cluster@
    clusterName :: Prelude.Text,
    -- | The capacity of each shard in the new Elastic DocumentDB cluster.
    shardCapacity :: Prelude.Int,
    -- | The number of shards to create in the new Elastic DocumentDB cluster.
    shardCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createCluster_clientToken' - The client token for the Elastic DocumentDB cluster.
--
-- 'kmsKeyId', 'createCluster_kmsKeyId' - The KMS key identifier to use to encrypt the new Elastic DocumentDB
-- cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are creating a cluster using the same Amazon
-- account that owns this KMS encryption key, you can use the KMS key alias
-- instead of the ARN as the KMS encryption key.
--
-- If an encryption key is not specified, Elastic DocumentDB uses the
-- default encryption key that KMS creates for your account. Your account
-- has a different default encryption key for each Amazon Region.
--
-- 'preferredMaintenanceWindow', 'createCluster_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- /Format/: @ddd:hh24:mi-ddd:hh24:mi@
--
-- /Default/: a 30-minute window selected at random from an 8-hour block of
-- time for each Amazon Web Services Region, occurring on a random day of
-- the week.
--
-- /Valid days/: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- /Constraints/: Minimum 30-minute window.
--
-- 'subnetIds', 'createCluster_subnetIds' - The Amazon EC2 subnet IDs for the new Elastic DocumentDB cluster.
--
-- 'tags', 'createCluster_tags' - The tags to be assigned to the new Elastic DocumentDB cluster.
--
-- 'vpcSecurityGroupIds', 'createCluster_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with the new Elastic
-- DocumentDB cluster.
--
-- 'adminUserName', 'createCluster_adminUserName' - The name of the Elastic DocumentDB cluster administrator.
--
-- /Constraints/:
--
-- -   Must be from 1 to 63 letters or numbers.
--
-- -   The first character must be a letter.
--
-- -   Cannot be a reserved word.
--
-- 'adminUserPassword', 'createCluster_adminUserPassword' - The password for the Elastic DocumentDB cluster administrator and can
-- contain any printable ASCII characters.
--
-- /Constraints/:
--
-- -   Must contain from 8 to 100 characters.
--
-- -   Cannot contain a forward slash (\/), double quote (\"), or the
--     \"at\" symbol (\@).
--
-- 'authType', 'createCluster_authType' - The authentication type for the Elastic DocumentDB cluster.
--
-- 'clusterName', 'createCluster_clusterName' - The name of the new Elastic DocumentDB cluster. This parameter is stored
-- as a lowercase string.
--
-- /Constraints/:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- /Example/: @my-cluster@
--
-- 'shardCapacity', 'createCluster_shardCapacity' - The capacity of each shard in the new Elastic DocumentDB cluster.
--
-- 'shardCount', 'createCluster_shardCount' - The number of shards to create in the new Elastic DocumentDB cluster.
newCreateCluster ::
  -- | 'adminUserName'
  Prelude.Text ->
  -- | 'adminUserPassword'
  Prelude.Text ->
  -- | 'authType'
  Auth ->
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'shardCapacity'
  Prelude.Int ->
  -- | 'shardCount'
  Prelude.Int ->
  CreateCluster
newCreateCluster
  pAdminUserName_
  pAdminUserPassword_
  pAuthType_
  pClusterName_
  pShardCapacity_
  pShardCount_ =
    CreateCluster'
      { clientToken = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        subnetIds = Prelude.Nothing,
        tags = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        adminUserName = pAdminUserName_,
        adminUserPassword =
          Data._Sensitive Lens.# pAdminUserPassword_,
        authType = pAuthType_,
        clusterName = pClusterName_,
        shardCapacity = pShardCapacity_,
        shardCount = pShardCount_
      }

-- | The client token for the Elastic DocumentDB cluster.
createCluster_clientToken :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_clientToken = Lens.lens (\CreateCluster' {clientToken} -> clientToken) (\s@CreateCluster' {} a -> s {clientToken = a} :: CreateCluster)

-- | The KMS key identifier to use to encrypt the new Elastic DocumentDB
-- cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are creating a cluster using the same Amazon
-- account that owns this KMS encryption key, you can use the KMS key alias
-- instead of the ARN as the KMS encryption key.
--
-- If an encryption key is not specified, Elastic DocumentDB uses the
-- default encryption key that KMS creates for your account. Your account
-- has a different default encryption key for each Amazon Region.
createCluster_kmsKeyId :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_kmsKeyId = Lens.lens (\CreateCluster' {kmsKeyId} -> kmsKeyId) (\s@CreateCluster' {} a -> s {kmsKeyId = a} :: CreateCluster)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- /Format/: @ddd:hh24:mi-ddd:hh24:mi@
--
-- /Default/: a 30-minute window selected at random from an 8-hour block of
-- time for each Amazon Web Services Region, occurring on a random day of
-- the week.
--
-- /Valid days/: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- /Constraints/: Minimum 30-minute window.
createCluster_preferredMaintenanceWindow :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_preferredMaintenanceWindow = Lens.lens (\CreateCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateCluster' {} a -> s {preferredMaintenanceWindow = a} :: CreateCluster)

-- | The Amazon EC2 subnet IDs for the new Elastic DocumentDB cluster.
createCluster_subnetIds :: Lens.Lens' CreateCluster (Prelude.Maybe [Prelude.Text])
createCluster_subnetIds = Lens.lens (\CreateCluster' {subnetIds} -> subnetIds) (\s@CreateCluster' {} a -> s {subnetIds = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The tags to be assigned to the new Elastic DocumentDB cluster.
createCluster_tags :: Lens.Lens' CreateCluster (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCluster_tags = Lens.lens (\CreateCluster' {tags} -> tags) (\s@CreateCluster' {} a -> s {tags = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | A list of EC2 VPC security groups to associate with the new Elastic
-- DocumentDB cluster.
createCluster_vpcSecurityGroupIds :: Lens.Lens' CreateCluster (Prelude.Maybe [Prelude.Text])
createCluster_vpcSecurityGroupIds = Lens.lens (\CreateCluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateCluster' {} a -> s {vpcSecurityGroupIds = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Elastic DocumentDB cluster administrator.
--
-- /Constraints/:
--
-- -   Must be from 1 to 63 letters or numbers.
--
-- -   The first character must be a letter.
--
-- -   Cannot be a reserved word.
createCluster_adminUserName :: Lens.Lens' CreateCluster Prelude.Text
createCluster_adminUserName = Lens.lens (\CreateCluster' {adminUserName} -> adminUserName) (\s@CreateCluster' {} a -> s {adminUserName = a} :: CreateCluster)

-- | The password for the Elastic DocumentDB cluster administrator and can
-- contain any printable ASCII characters.
--
-- /Constraints/:
--
-- -   Must contain from 8 to 100 characters.
--
-- -   Cannot contain a forward slash (\/), double quote (\"), or the
--     \"at\" symbol (\@).
createCluster_adminUserPassword :: Lens.Lens' CreateCluster Prelude.Text
createCluster_adminUserPassword = Lens.lens (\CreateCluster' {adminUserPassword} -> adminUserPassword) (\s@CreateCluster' {} a -> s {adminUserPassword = a} :: CreateCluster) Prelude.. Data._Sensitive

-- | The authentication type for the Elastic DocumentDB cluster.
createCluster_authType :: Lens.Lens' CreateCluster Auth
createCluster_authType = Lens.lens (\CreateCluster' {authType} -> authType) (\s@CreateCluster' {} a -> s {authType = a} :: CreateCluster)

-- | The name of the new Elastic DocumentDB cluster. This parameter is stored
-- as a lowercase string.
--
-- /Constraints/:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- /Example/: @my-cluster@
createCluster_clusterName :: Lens.Lens' CreateCluster Prelude.Text
createCluster_clusterName = Lens.lens (\CreateCluster' {clusterName} -> clusterName) (\s@CreateCluster' {} a -> s {clusterName = a} :: CreateCluster)

-- | The capacity of each shard in the new Elastic DocumentDB cluster.
createCluster_shardCapacity :: Lens.Lens' CreateCluster Prelude.Int
createCluster_shardCapacity = Lens.lens (\CreateCluster' {shardCapacity} -> shardCapacity) (\s@CreateCluster' {} a -> s {shardCapacity = a} :: CreateCluster)

-- | The number of shards to create in the new Elastic DocumentDB cluster.
createCluster_shardCount :: Lens.Lens' CreateCluster Prelude.Int
createCluster_shardCount = Lens.lens (\CreateCluster' {shardCount} -> shardCount) (\s@CreateCluster' {} a -> s {shardCount = a} :: CreateCluster)

instance Core.AWSRequest CreateCluster where
  type
    AWSResponse CreateCluster =
      CreateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "cluster")
      )

instance Prelude.Hashable CreateCluster where
  hashWithSalt _salt CreateCluster' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` adminUserName
      `Prelude.hashWithSalt` adminUserPassword
      `Prelude.hashWithSalt` authType
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` shardCapacity
      `Prelude.hashWithSalt` shardCount

instance Prelude.NFData CreateCluster where
  rnf CreateCluster' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf adminUserName
      `Prelude.seq` Prelude.rnf adminUserPassword
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf shardCapacity
      `Prelude.seq` Prelude.rnf shardCount

instance Data.ToHeaders CreateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("preferredMaintenanceWindow" Data..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("subnetIds" Data..=) Prelude.<$> subnetIds,
            ("tags" Data..=) Prelude.<$> tags,
            ("vpcSecurityGroupIds" Data..=)
              Prelude.<$> vpcSecurityGroupIds,
            Prelude.Just ("adminUserName" Data..= adminUserName),
            Prelude.Just
              ("adminUserPassword" Data..= adminUserPassword),
            Prelude.Just ("authType" Data..= authType),
            Prelude.Just ("clusterName" Data..= clusterName),
            Prelude.Just ("shardCapacity" Data..= shardCapacity),
            Prelude.Just ("shardCount" Data..= shardCount)
          ]
      )

instance Data.ToPath CreateCluster where
  toPath = Prelude.const "/cluster"

instance Data.ToQuery CreateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The new Elastic DocumentDB cluster that has been created.
    cluster :: Cluster
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createClusterResponse_httpStatus' - The response's http status code.
--
-- 'cluster', 'createClusterResponse_cluster' - The new Elastic DocumentDB cluster that has been created.
newCreateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'cluster'
  Cluster ->
  CreateClusterResponse
newCreateClusterResponse pHttpStatus_ pCluster_ =
  CreateClusterResponse'
    { httpStatus = pHttpStatus_,
      cluster = pCluster_
    }

-- | The response's http status code.
createClusterResponse_httpStatus :: Lens.Lens' CreateClusterResponse Prelude.Int
createClusterResponse_httpStatus = Lens.lens (\CreateClusterResponse' {httpStatus} -> httpStatus) (\s@CreateClusterResponse' {} a -> s {httpStatus = a} :: CreateClusterResponse)

-- | The new Elastic DocumentDB cluster that has been created.
createClusterResponse_cluster :: Lens.Lens' CreateClusterResponse Cluster
createClusterResponse_cluster = Lens.lens (\CreateClusterResponse' {cluster} -> cluster) (\s@CreateClusterResponse' {} a -> s {cluster = a} :: CreateClusterResponse)

instance Prelude.NFData CreateClusterResponse where
  rnf CreateClusterResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf cluster
