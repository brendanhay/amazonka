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
-- Module      : Amazonka.Redshift.Types.ClusterSnapshotCopyStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ClusterSnapshotCopyStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Returns the destination region and retention period that are configured
-- for cross-region snapshot copy.
--
-- /See:/ 'newClusterSnapshotCopyStatus' smart constructor.
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus'
  { -- | The destination region that snapshots are automatically copied to when
    -- cross-region snapshot copy is enabled.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | The number of days that automated snapshots are retained in the
    -- destination region after they are copied from a source region. If the
    -- value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The number of days that automated snapshots are retained in the
    -- destination region after they are copied from a source region.
    retentionPeriod :: Prelude.Maybe Prelude.Integer,
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterSnapshotCopyStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationRegion', 'clusterSnapshotCopyStatus_destinationRegion' - The destination region that snapshots are automatically copied to when
-- cross-region snapshot copy is enabled.
--
-- 'manualSnapshotRetentionPeriod', 'clusterSnapshotCopyStatus_manualSnapshotRetentionPeriod' - The number of days that automated snapshots are retained in the
-- destination region after they are copied from a source region. If the
-- value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- 'retentionPeriod', 'clusterSnapshotCopyStatus_retentionPeriod' - The number of days that automated snapshots are retained in the
-- destination region after they are copied from a source region.
--
-- 'snapshotCopyGrantName', 'clusterSnapshotCopyStatus_snapshotCopyGrantName' - The name of the snapshot copy grant.
newClusterSnapshotCopyStatus ::
  ClusterSnapshotCopyStatus
newClusterSnapshotCopyStatus =
  ClusterSnapshotCopyStatus'
    { destinationRegion =
        Prelude.Nothing,
      manualSnapshotRetentionPeriod = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      snapshotCopyGrantName = Prelude.Nothing
    }

-- | The destination region that snapshots are automatically copied to when
-- cross-region snapshot copy is enabled.
clusterSnapshotCopyStatus_destinationRegion :: Lens.Lens' ClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Text)
clusterSnapshotCopyStatus_destinationRegion = Lens.lens (\ClusterSnapshotCopyStatus' {destinationRegion} -> destinationRegion) (\s@ClusterSnapshotCopyStatus' {} a -> s {destinationRegion = a} :: ClusterSnapshotCopyStatus)

-- | The number of days that automated snapshots are retained in the
-- destination region after they are copied from a source region. If the
-- value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
clusterSnapshotCopyStatus_manualSnapshotRetentionPeriod :: Lens.Lens' ClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Int)
clusterSnapshotCopyStatus_manualSnapshotRetentionPeriod = Lens.lens (\ClusterSnapshotCopyStatus' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@ClusterSnapshotCopyStatus' {} a -> s {manualSnapshotRetentionPeriod = a} :: ClusterSnapshotCopyStatus)

-- | The number of days that automated snapshots are retained in the
-- destination region after they are copied from a source region.
clusterSnapshotCopyStatus_retentionPeriod :: Lens.Lens' ClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Integer)
clusterSnapshotCopyStatus_retentionPeriod = Lens.lens (\ClusterSnapshotCopyStatus' {retentionPeriod} -> retentionPeriod) (\s@ClusterSnapshotCopyStatus' {} a -> s {retentionPeriod = a} :: ClusterSnapshotCopyStatus)

-- | The name of the snapshot copy grant.
clusterSnapshotCopyStatus_snapshotCopyGrantName :: Lens.Lens' ClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Text)
clusterSnapshotCopyStatus_snapshotCopyGrantName = Lens.lens (\ClusterSnapshotCopyStatus' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@ClusterSnapshotCopyStatus' {} a -> s {snapshotCopyGrantName = a} :: ClusterSnapshotCopyStatus)

instance Data.FromXML ClusterSnapshotCopyStatus where
  parseXML x =
    ClusterSnapshotCopyStatus'
      Prelude.<$> (x Data..@? "DestinationRegion")
      Prelude.<*> (x Data..@? "ManualSnapshotRetentionPeriod")
      Prelude.<*> (x Data..@? "RetentionPeriod")
      Prelude.<*> (x Data..@? "SnapshotCopyGrantName")

instance Prelude.Hashable ClusterSnapshotCopyStatus where
  hashWithSalt _salt ClusterSnapshotCopyStatus' {..} =
    _salt
      `Prelude.hashWithSalt` destinationRegion
      `Prelude.hashWithSalt` manualSnapshotRetentionPeriod
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` snapshotCopyGrantName

instance Prelude.NFData ClusterSnapshotCopyStatus where
  rnf ClusterSnapshotCopyStatus' {..} =
    Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf manualSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf snapshotCopyGrantName
