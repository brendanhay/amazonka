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
-- Module      : Amazonka.DocDbElastic.Types.ClusterSnapshotInList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocDbElastic.Types.ClusterSnapshotInList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | A list of Elastic DocumentDB snapshots.
--
-- /See:/ 'newClusterSnapshotInList' smart constructor.
data ClusterSnapshotInList = ClusterSnapshotInList'
  { -- | The arn of the Elastic DocumentDB cluster.
    clusterArn :: Prelude.Text,
    -- | The arn of the Elastic DocumentDB snapshot
    snapshotArn :: Prelude.Text,
    -- | The time when the Elastic DocumentDB snapshot was created in Universal
    -- Coordinated Time (UTC).
    snapshotCreationTime :: Prelude.Text,
    -- | The name of the Elastic DocumentDB snapshot.
    snapshotName :: Prelude.Text,
    -- | The status of the Elastic DocumentDB snapshot.
    status :: Status
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterSnapshotInList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'clusterSnapshotInList_clusterArn' - The arn of the Elastic DocumentDB cluster.
--
-- 'snapshotArn', 'clusterSnapshotInList_snapshotArn' - The arn of the Elastic DocumentDB snapshot
--
-- 'snapshotCreationTime', 'clusterSnapshotInList_snapshotCreationTime' - The time when the Elastic DocumentDB snapshot was created in Universal
-- Coordinated Time (UTC).
--
-- 'snapshotName', 'clusterSnapshotInList_snapshotName' - The name of the Elastic DocumentDB snapshot.
--
-- 'status', 'clusterSnapshotInList_status' - The status of the Elastic DocumentDB snapshot.
newClusterSnapshotInList ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'snapshotArn'
  Prelude.Text ->
  -- | 'snapshotCreationTime'
  Prelude.Text ->
  -- | 'snapshotName'
  Prelude.Text ->
  -- | 'status'
  Status ->
  ClusterSnapshotInList
newClusterSnapshotInList
  pClusterArn_
  pSnapshotArn_
  pSnapshotCreationTime_
  pSnapshotName_
  pStatus_ =
    ClusterSnapshotInList'
      { clusterArn = pClusterArn_,
        snapshotArn = pSnapshotArn_,
        snapshotCreationTime = pSnapshotCreationTime_,
        snapshotName = pSnapshotName_,
        status = pStatus_
      }

-- | The arn of the Elastic DocumentDB cluster.
clusterSnapshotInList_clusterArn :: Lens.Lens' ClusterSnapshotInList Prelude.Text
clusterSnapshotInList_clusterArn = Lens.lens (\ClusterSnapshotInList' {clusterArn} -> clusterArn) (\s@ClusterSnapshotInList' {} a -> s {clusterArn = a} :: ClusterSnapshotInList)

-- | The arn of the Elastic DocumentDB snapshot
clusterSnapshotInList_snapshotArn :: Lens.Lens' ClusterSnapshotInList Prelude.Text
clusterSnapshotInList_snapshotArn = Lens.lens (\ClusterSnapshotInList' {snapshotArn} -> snapshotArn) (\s@ClusterSnapshotInList' {} a -> s {snapshotArn = a} :: ClusterSnapshotInList)

-- | The time when the Elastic DocumentDB snapshot was created in Universal
-- Coordinated Time (UTC).
clusterSnapshotInList_snapshotCreationTime :: Lens.Lens' ClusterSnapshotInList Prelude.Text
clusterSnapshotInList_snapshotCreationTime = Lens.lens (\ClusterSnapshotInList' {snapshotCreationTime} -> snapshotCreationTime) (\s@ClusterSnapshotInList' {} a -> s {snapshotCreationTime = a} :: ClusterSnapshotInList)

-- | The name of the Elastic DocumentDB snapshot.
clusterSnapshotInList_snapshotName :: Lens.Lens' ClusterSnapshotInList Prelude.Text
clusterSnapshotInList_snapshotName = Lens.lens (\ClusterSnapshotInList' {snapshotName} -> snapshotName) (\s@ClusterSnapshotInList' {} a -> s {snapshotName = a} :: ClusterSnapshotInList)

-- | The status of the Elastic DocumentDB snapshot.
clusterSnapshotInList_status :: Lens.Lens' ClusterSnapshotInList Status
clusterSnapshotInList_status = Lens.lens (\ClusterSnapshotInList' {status} -> status) (\s@ClusterSnapshotInList' {} a -> s {status = a} :: ClusterSnapshotInList)

instance Data.FromJSON ClusterSnapshotInList where
  parseJSON =
    Data.withObject
      "ClusterSnapshotInList"
      ( \x ->
          ClusterSnapshotInList'
            Prelude.<$> (x Data..: "clusterArn")
            Prelude.<*> (x Data..: "snapshotArn")
            Prelude.<*> (x Data..: "snapshotCreationTime")
            Prelude.<*> (x Data..: "snapshotName")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ClusterSnapshotInList where
  hashWithSalt _salt ClusterSnapshotInList' {..} =
    _salt
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` snapshotArn
      `Prelude.hashWithSalt` snapshotCreationTime
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` status

instance Prelude.NFData ClusterSnapshotInList where
  rnf ClusterSnapshotInList' {..} =
    Prelude.rnf clusterArn `Prelude.seq`
      Prelude.rnf snapshotArn `Prelude.seq`
        Prelude.rnf snapshotCreationTime `Prelude.seq`
          Prelude.rnf snapshotName `Prelude.seq`
            Prelude.rnf status
