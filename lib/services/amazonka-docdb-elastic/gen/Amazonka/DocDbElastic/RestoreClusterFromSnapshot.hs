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
-- Module      : Amazonka.DocDbElastic.RestoreClusterFromSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a Elastic DocumentDB cluster from a snapshot.
module Amazonka.DocDbElastic.RestoreClusterFromSnapshot
  ( -- * Creating a Request
    RestoreClusterFromSnapshot (..),
    newRestoreClusterFromSnapshot,

    -- * Request Lenses
    restoreClusterFromSnapshot_kmsKeyId,
    restoreClusterFromSnapshot_subnetIds,
    restoreClusterFromSnapshot_tags,
    restoreClusterFromSnapshot_vpcSecurityGroupIds,
    restoreClusterFromSnapshot_clusterName,
    restoreClusterFromSnapshot_snapshotArn,

    -- * Destructuring the Response
    RestoreClusterFromSnapshotResponse (..),
    newRestoreClusterFromSnapshotResponse,

    -- * Response Lenses
    restoreClusterFromSnapshotResponse_httpStatus,
    restoreClusterFromSnapshotResponse_cluster,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreClusterFromSnapshot' smart constructor.
data RestoreClusterFromSnapshot = RestoreClusterFromSnapshot'
  { -- | The KMS key identifier to use to encrypt the new Elastic DocumentDB
    -- cluster.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
    -- encryption key. If you are creating a cluster using the same Amazon
    -- account that owns this KMS encryption key, you can use the KMS key alias
    -- instead of the ARN as the KMS encryption key.
    --
    -- If an encryption key is not specified here, Elastic DocumentDB uses the
    -- default encryption key that KMS creates for your account. Your account
    -- has a different default encryption key for each Amazon Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 subnet IDs for the Elastic DocumentDB cluster.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of the tag names to be assigned to the restored DB cluster, in
    -- the form of an array of key-value pairs in which the key is the tag name
    -- and the value is the key value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of EC2 VPC security groups to associate with the Elastic
    -- DocumentDB cluster.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Elastic DocumentDB cluster.
    clusterName :: Prelude.Text,
    -- | The arn of the Elastic DocumentDB snapshot.
    snapshotArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreClusterFromSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'restoreClusterFromSnapshot_kmsKeyId' - The KMS key identifier to use to encrypt the new Elastic DocumentDB
-- cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are creating a cluster using the same Amazon
-- account that owns this KMS encryption key, you can use the KMS key alias
-- instead of the ARN as the KMS encryption key.
--
-- If an encryption key is not specified here, Elastic DocumentDB uses the
-- default encryption key that KMS creates for your account. Your account
-- has a different default encryption key for each Amazon Region.
--
-- 'subnetIds', 'restoreClusterFromSnapshot_subnetIds' - The Amazon EC2 subnet IDs for the Elastic DocumentDB cluster.
--
-- 'tags', 'restoreClusterFromSnapshot_tags' - A list of the tag names to be assigned to the restored DB cluster, in
-- the form of an array of key-value pairs in which the key is the tag name
-- and the value is the key value.
--
-- 'vpcSecurityGroupIds', 'restoreClusterFromSnapshot_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with the Elastic
-- DocumentDB cluster.
--
-- 'clusterName', 'restoreClusterFromSnapshot_clusterName' - The name of the Elastic DocumentDB cluster.
--
-- 'snapshotArn', 'restoreClusterFromSnapshot_snapshotArn' - The arn of the Elastic DocumentDB snapshot.
newRestoreClusterFromSnapshot ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'snapshotArn'
  Prelude.Text ->
  RestoreClusterFromSnapshot
newRestoreClusterFromSnapshot
  pClusterName_
  pSnapshotArn_ =
    RestoreClusterFromSnapshot'
      { kmsKeyId =
          Prelude.Nothing,
        subnetIds = Prelude.Nothing,
        tags = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        clusterName = pClusterName_,
        snapshotArn = pSnapshotArn_
      }

-- | The KMS key identifier to use to encrypt the new Elastic DocumentDB
-- cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are creating a cluster using the same Amazon
-- account that owns this KMS encryption key, you can use the KMS key alias
-- instead of the ARN as the KMS encryption key.
--
-- If an encryption key is not specified here, Elastic DocumentDB uses the
-- default encryption key that KMS creates for your account. Your account
-- has a different default encryption key for each Amazon Region.
restoreClusterFromSnapshot_kmsKeyId :: Lens.Lens' RestoreClusterFromSnapshot (Prelude.Maybe Prelude.Text)
restoreClusterFromSnapshot_kmsKeyId = Lens.lens (\RestoreClusterFromSnapshot' {kmsKeyId} -> kmsKeyId) (\s@RestoreClusterFromSnapshot' {} a -> s {kmsKeyId = a} :: RestoreClusterFromSnapshot)

-- | The Amazon EC2 subnet IDs for the Elastic DocumentDB cluster.
restoreClusterFromSnapshot_subnetIds :: Lens.Lens' RestoreClusterFromSnapshot (Prelude.Maybe [Prelude.Text])
restoreClusterFromSnapshot_subnetIds = Lens.lens (\RestoreClusterFromSnapshot' {subnetIds} -> subnetIds) (\s@RestoreClusterFromSnapshot' {} a -> s {subnetIds = a} :: RestoreClusterFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | A list of the tag names to be assigned to the restored DB cluster, in
-- the form of an array of key-value pairs in which the key is the tag name
-- and the value is the key value.
restoreClusterFromSnapshot_tags :: Lens.Lens' RestoreClusterFromSnapshot (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
restoreClusterFromSnapshot_tags = Lens.lens (\RestoreClusterFromSnapshot' {tags} -> tags) (\s@RestoreClusterFromSnapshot' {} a -> s {tags = a} :: RestoreClusterFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | A list of EC2 VPC security groups to associate with the Elastic
-- DocumentDB cluster.
restoreClusterFromSnapshot_vpcSecurityGroupIds :: Lens.Lens' RestoreClusterFromSnapshot (Prelude.Maybe [Prelude.Text])
restoreClusterFromSnapshot_vpcSecurityGroupIds = Lens.lens (\RestoreClusterFromSnapshot' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreClusterFromSnapshot' {} a -> s {vpcSecurityGroupIds = a} :: RestoreClusterFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Elastic DocumentDB cluster.
restoreClusterFromSnapshot_clusterName :: Lens.Lens' RestoreClusterFromSnapshot Prelude.Text
restoreClusterFromSnapshot_clusterName = Lens.lens (\RestoreClusterFromSnapshot' {clusterName} -> clusterName) (\s@RestoreClusterFromSnapshot' {} a -> s {clusterName = a} :: RestoreClusterFromSnapshot)

-- | The arn of the Elastic DocumentDB snapshot.
restoreClusterFromSnapshot_snapshotArn :: Lens.Lens' RestoreClusterFromSnapshot Prelude.Text
restoreClusterFromSnapshot_snapshotArn = Lens.lens (\RestoreClusterFromSnapshot' {snapshotArn} -> snapshotArn) (\s@RestoreClusterFromSnapshot' {} a -> s {snapshotArn = a} :: RestoreClusterFromSnapshot)

instance Core.AWSRequest RestoreClusterFromSnapshot where
  type
    AWSResponse RestoreClusterFromSnapshot =
      RestoreClusterFromSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreClusterFromSnapshotResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "cluster")
      )

instance Prelude.Hashable RestoreClusterFromSnapshot where
  hashWithSalt _salt RestoreClusterFromSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` snapshotArn

instance Prelude.NFData RestoreClusterFromSnapshot where
  rnf RestoreClusterFromSnapshot' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf snapshotArn

instance Data.ToHeaders RestoreClusterFromSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RestoreClusterFromSnapshot where
  toJSON RestoreClusterFromSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("subnetIds" Data..=) Prelude.<$> subnetIds,
            ("tags" Data..=) Prelude.<$> tags,
            ("vpcSecurityGroupIds" Data..=)
              Prelude.<$> vpcSecurityGroupIds,
            Prelude.Just ("clusterName" Data..= clusterName)
          ]
      )

instance Data.ToPath RestoreClusterFromSnapshot where
  toPath RestoreClusterFromSnapshot' {..} =
    Prelude.mconcat
      [ "/cluster-snapshot/",
        Data.toBS snapshotArn,
        "/restore"
      ]

instance Data.ToQuery RestoreClusterFromSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreClusterFromSnapshotResponse' smart constructor.
data RestoreClusterFromSnapshotResponse = RestoreClusterFromSnapshotResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns information about a the restored Elastic DocumentDB cluster.
    cluster :: Cluster
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreClusterFromSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'restoreClusterFromSnapshotResponse_httpStatus' - The response's http status code.
--
-- 'cluster', 'restoreClusterFromSnapshotResponse_cluster' - Returns information about a the restored Elastic DocumentDB cluster.
newRestoreClusterFromSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'cluster'
  Cluster ->
  RestoreClusterFromSnapshotResponse
newRestoreClusterFromSnapshotResponse
  pHttpStatus_
  pCluster_ =
    RestoreClusterFromSnapshotResponse'
      { httpStatus =
          pHttpStatus_,
        cluster = pCluster_
      }

-- | The response's http status code.
restoreClusterFromSnapshotResponse_httpStatus :: Lens.Lens' RestoreClusterFromSnapshotResponse Prelude.Int
restoreClusterFromSnapshotResponse_httpStatus = Lens.lens (\RestoreClusterFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@RestoreClusterFromSnapshotResponse' {} a -> s {httpStatus = a} :: RestoreClusterFromSnapshotResponse)

-- | Returns information about a the restored Elastic DocumentDB cluster.
restoreClusterFromSnapshotResponse_cluster :: Lens.Lens' RestoreClusterFromSnapshotResponse Cluster
restoreClusterFromSnapshotResponse_cluster = Lens.lens (\RestoreClusterFromSnapshotResponse' {cluster} -> cluster) (\s@RestoreClusterFromSnapshotResponse' {} a -> s {cluster = a} :: RestoreClusterFromSnapshotResponse)

instance
  Prelude.NFData
    RestoreClusterFromSnapshotResponse
  where
  rnf RestoreClusterFromSnapshotResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf cluster
