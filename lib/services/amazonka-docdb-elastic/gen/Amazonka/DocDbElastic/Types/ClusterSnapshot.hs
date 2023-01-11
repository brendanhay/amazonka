{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DocDbElastic.Types.ClusterSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocDbElastic.Types.ClusterSnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a specific Elastic DocumentDB snapshot.
--
-- /See:/ 'newClusterSnapshot' smart constructor.
data ClusterSnapshot = ClusterSnapshot'
  { -- | The name of the Elastic DocumentDB cluster administrator.
    adminUserName :: Prelude.Text,
    -- | The arn of the Elastic DocumentDB cluster.
    clusterArn :: Prelude.Text,
    -- | The time when the Elastic DocumentDB cluster was created in Universal
    -- Coordinated Time (UTC).
    clusterCreationTime :: Prelude.Text,
    -- | The KMS key identifier to use to encrypt the Elastic DocumentDB cluster.
    kmsKeyId :: Prelude.Text,
    -- | The arn of the Elastic DocumentDB snapshot
    snapshotArn :: Prelude.Text,
    -- | The time when the Elastic DocumentDB snapshot was created in Universal
    -- Coordinated Time (UTC).
    snapshotCreationTime :: Prelude.Text,
    -- | The name of the Elastic DocumentDB snapshot.
    snapshotName :: Prelude.Text,
    -- | The status of the Elastic DocumentDB snapshot.
    status :: Status,
    -- | A list of the IDs of subnets associated with the DB cluster snapshot.
    subnetIds :: [Prelude.Text],
    -- | A list of the IDs of the VPC security groups associated with the cluster
    -- snapshot.
    vpcSecurityGroupIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminUserName', 'clusterSnapshot_adminUserName' - The name of the Elastic DocumentDB cluster administrator.
--
-- 'clusterArn', 'clusterSnapshot_clusterArn' - The arn of the Elastic DocumentDB cluster.
--
-- 'clusterCreationTime', 'clusterSnapshot_clusterCreationTime' - The time when the Elastic DocumentDB cluster was created in Universal
-- Coordinated Time (UTC).
--
-- 'kmsKeyId', 'clusterSnapshot_kmsKeyId' - The KMS key identifier to use to encrypt the Elastic DocumentDB cluster.
--
-- 'snapshotArn', 'clusterSnapshot_snapshotArn' - The arn of the Elastic DocumentDB snapshot
--
-- 'snapshotCreationTime', 'clusterSnapshot_snapshotCreationTime' - The time when the Elastic DocumentDB snapshot was created in Universal
-- Coordinated Time (UTC).
--
-- 'snapshotName', 'clusterSnapshot_snapshotName' - The name of the Elastic DocumentDB snapshot.
--
-- 'status', 'clusterSnapshot_status' - The status of the Elastic DocumentDB snapshot.
--
-- 'subnetIds', 'clusterSnapshot_subnetIds' - A list of the IDs of subnets associated with the DB cluster snapshot.
--
-- 'vpcSecurityGroupIds', 'clusterSnapshot_vpcSecurityGroupIds' - A list of the IDs of the VPC security groups associated with the cluster
-- snapshot.
newClusterSnapshot ::
  -- | 'adminUserName'
  Prelude.Text ->
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'clusterCreationTime'
  Prelude.Text ->
  -- | 'kmsKeyId'
  Prelude.Text ->
  -- | 'snapshotArn'
  Prelude.Text ->
  -- | 'snapshotCreationTime'
  Prelude.Text ->
  -- | 'snapshotName'
  Prelude.Text ->
  -- | 'status'
  Status ->
  ClusterSnapshot
newClusterSnapshot
  pAdminUserName_
  pClusterArn_
  pClusterCreationTime_
  pKmsKeyId_
  pSnapshotArn_
  pSnapshotCreationTime_
  pSnapshotName_
  pStatus_ =
    ClusterSnapshot'
      { adminUserName = pAdminUserName_,
        clusterArn = pClusterArn_,
        clusterCreationTime = pClusterCreationTime_,
        kmsKeyId = pKmsKeyId_,
        snapshotArn = pSnapshotArn_,
        snapshotCreationTime = pSnapshotCreationTime_,
        snapshotName = pSnapshotName_,
        status = pStatus_,
        subnetIds = Prelude.mempty,
        vpcSecurityGroupIds = Prelude.mempty
      }

-- | The name of the Elastic DocumentDB cluster administrator.
clusterSnapshot_adminUserName :: Lens.Lens' ClusterSnapshot Prelude.Text
clusterSnapshot_adminUserName = Lens.lens (\ClusterSnapshot' {adminUserName} -> adminUserName) (\s@ClusterSnapshot' {} a -> s {adminUserName = a} :: ClusterSnapshot)

-- | The arn of the Elastic DocumentDB cluster.
clusterSnapshot_clusterArn :: Lens.Lens' ClusterSnapshot Prelude.Text
clusterSnapshot_clusterArn = Lens.lens (\ClusterSnapshot' {clusterArn} -> clusterArn) (\s@ClusterSnapshot' {} a -> s {clusterArn = a} :: ClusterSnapshot)

-- | The time when the Elastic DocumentDB cluster was created in Universal
-- Coordinated Time (UTC).
clusterSnapshot_clusterCreationTime :: Lens.Lens' ClusterSnapshot Prelude.Text
clusterSnapshot_clusterCreationTime = Lens.lens (\ClusterSnapshot' {clusterCreationTime} -> clusterCreationTime) (\s@ClusterSnapshot' {} a -> s {clusterCreationTime = a} :: ClusterSnapshot)

-- | The KMS key identifier to use to encrypt the Elastic DocumentDB cluster.
clusterSnapshot_kmsKeyId :: Lens.Lens' ClusterSnapshot Prelude.Text
clusterSnapshot_kmsKeyId = Lens.lens (\ClusterSnapshot' {kmsKeyId} -> kmsKeyId) (\s@ClusterSnapshot' {} a -> s {kmsKeyId = a} :: ClusterSnapshot)

-- | The arn of the Elastic DocumentDB snapshot
clusterSnapshot_snapshotArn :: Lens.Lens' ClusterSnapshot Prelude.Text
clusterSnapshot_snapshotArn = Lens.lens (\ClusterSnapshot' {snapshotArn} -> snapshotArn) (\s@ClusterSnapshot' {} a -> s {snapshotArn = a} :: ClusterSnapshot)

-- | The time when the Elastic DocumentDB snapshot was created in Universal
-- Coordinated Time (UTC).
clusterSnapshot_snapshotCreationTime :: Lens.Lens' ClusterSnapshot Prelude.Text
clusterSnapshot_snapshotCreationTime = Lens.lens (\ClusterSnapshot' {snapshotCreationTime} -> snapshotCreationTime) (\s@ClusterSnapshot' {} a -> s {snapshotCreationTime = a} :: ClusterSnapshot)

-- | The name of the Elastic DocumentDB snapshot.
clusterSnapshot_snapshotName :: Lens.Lens' ClusterSnapshot Prelude.Text
clusterSnapshot_snapshotName = Lens.lens (\ClusterSnapshot' {snapshotName} -> snapshotName) (\s@ClusterSnapshot' {} a -> s {snapshotName = a} :: ClusterSnapshot)

-- | The status of the Elastic DocumentDB snapshot.
clusterSnapshot_status :: Lens.Lens' ClusterSnapshot Status
clusterSnapshot_status = Lens.lens (\ClusterSnapshot' {status} -> status) (\s@ClusterSnapshot' {} a -> s {status = a} :: ClusterSnapshot)

-- | A list of the IDs of subnets associated with the DB cluster snapshot.
clusterSnapshot_subnetIds :: Lens.Lens' ClusterSnapshot [Prelude.Text]
clusterSnapshot_subnetIds = Lens.lens (\ClusterSnapshot' {subnetIds} -> subnetIds) (\s@ClusterSnapshot' {} a -> s {subnetIds = a} :: ClusterSnapshot) Prelude.. Lens.coerced

-- | A list of the IDs of the VPC security groups associated with the cluster
-- snapshot.
clusterSnapshot_vpcSecurityGroupIds :: Lens.Lens' ClusterSnapshot [Prelude.Text]
clusterSnapshot_vpcSecurityGroupIds = Lens.lens (\ClusterSnapshot' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@ClusterSnapshot' {} a -> s {vpcSecurityGroupIds = a} :: ClusterSnapshot) Prelude.. Lens.coerced

instance Data.FromJSON ClusterSnapshot where
  parseJSON =
    Data.withObject
      "ClusterSnapshot"
      ( \x ->
          ClusterSnapshot'
            Prelude.<$> (x Data..: "adminUserName")
            Prelude.<*> (x Data..: "clusterArn")
            Prelude.<*> (x Data..: "clusterCreationTime")
            Prelude.<*> (x Data..: "kmsKeyId")
            Prelude.<*> (x Data..: "snapshotArn")
            Prelude.<*> (x Data..: "snapshotCreationTime")
            Prelude.<*> (x Data..: "snapshotName")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..:? "subnetIds" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "vpcSecurityGroupIds"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ClusterSnapshot where
  hashWithSalt _salt ClusterSnapshot' {..} =
    _salt `Prelude.hashWithSalt` adminUserName
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` clusterCreationTime
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` snapshotArn
      `Prelude.hashWithSalt` snapshotCreationTime
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcSecurityGroupIds

instance Prelude.NFData ClusterSnapshot where
  rnf ClusterSnapshot' {..} =
    Prelude.rnf adminUserName
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterCreationTime
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf snapshotArn
      `Prelude.seq` Prelude.rnf snapshotCreationTime
      `Prelude.seq` Prelude.rnf snapshotName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
