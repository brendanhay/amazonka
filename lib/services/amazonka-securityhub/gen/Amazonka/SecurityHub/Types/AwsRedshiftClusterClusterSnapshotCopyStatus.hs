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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSnapshotCopyStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSnapshotCopyStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | You can configure Amazon Redshift to copy snapshots for a cluster to
-- another Amazon Web Services Region. This parameter provides information
-- about a cross-Region snapshot copy.
--
-- /See:/ 'newAwsRedshiftClusterClusterSnapshotCopyStatus' smart constructor.
data AwsRedshiftClusterClusterSnapshotCopyStatus = AwsRedshiftClusterClusterSnapshotCopyStatus'
  { -- | The destination Region that snapshots are automatically copied to when
    -- cross-Region snapshot copy is enabled.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | The number of days that manual snapshots are retained in the destination
    -- Region after they are copied from a source Region.
    --
    -- If the value is @-1@, then the manual snapshot is retained indefinitely.
    --
    -- Valid values: Either @-1@ or an integer between 1 and 3,653
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The number of days to retain automated snapshots in the destination
    -- Region after they are copied from a source Region.
    retentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterClusterSnapshotCopyStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationRegion', 'awsRedshiftClusterClusterSnapshotCopyStatus_destinationRegion' - The destination Region that snapshots are automatically copied to when
-- cross-Region snapshot copy is enabled.
--
-- 'manualSnapshotRetentionPeriod', 'awsRedshiftClusterClusterSnapshotCopyStatus_manualSnapshotRetentionPeriod' - The number of days that manual snapshots are retained in the destination
-- Region after they are copied from a source Region.
--
-- If the value is @-1@, then the manual snapshot is retained indefinitely.
--
-- Valid values: Either @-1@ or an integer between 1 and 3,653
--
-- 'retentionPeriod', 'awsRedshiftClusterClusterSnapshotCopyStatus_retentionPeriod' - The number of days to retain automated snapshots in the destination
-- Region after they are copied from a source Region.
--
-- 'snapshotCopyGrantName', 'awsRedshiftClusterClusterSnapshotCopyStatus_snapshotCopyGrantName' - The name of the snapshot copy grant.
newAwsRedshiftClusterClusterSnapshotCopyStatus ::
  AwsRedshiftClusterClusterSnapshotCopyStatus
newAwsRedshiftClusterClusterSnapshotCopyStatus =
  AwsRedshiftClusterClusterSnapshotCopyStatus'
    { destinationRegion =
        Prelude.Nothing,
      manualSnapshotRetentionPeriod =
        Prelude.Nothing,
      retentionPeriod =
        Prelude.Nothing,
      snapshotCopyGrantName =
        Prelude.Nothing
    }

-- | The destination Region that snapshots are automatically copied to when
-- cross-Region snapshot copy is enabled.
awsRedshiftClusterClusterSnapshotCopyStatus_destinationRegion :: Lens.Lens' AwsRedshiftClusterClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterSnapshotCopyStatus_destinationRegion = Lens.lens (\AwsRedshiftClusterClusterSnapshotCopyStatus' {destinationRegion} -> destinationRegion) (\s@AwsRedshiftClusterClusterSnapshotCopyStatus' {} a -> s {destinationRegion = a} :: AwsRedshiftClusterClusterSnapshotCopyStatus)

-- | The number of days that manual snapshots are retained in the destination
-- Region after they are copied from a source Region.
--
-- If the value is @-1@, then the manual snapshot is retained indefinitely.
--
-- Valid values: Either @-1@ or an integer between 1 and 3,653
awsRedshiftClusterClusterSnapshotCopyStatus_manualSnapshotRetentionPeriod :: Lens.Lens' AwsRedshiftClusterClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Int)
awsRedshiftClusterClusterSnapshotCopyStatus_manualSnapshotRetentionPeriod = Lens.lens (\AwsRedshiftClusterClusterSnapshotCopyStatus' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@AwsRedshiftClusterClusterSnapshotCopyStatus' {} a -> s {manualSnapshotRetentionPeriod = a} :: AwsRedshiftClusterClusterSnapshotCopyStatus)

-- | The number of days to retain automated snapshots in the destination
-- Region after they are copied from a source Region.
awsRedshiftClusterClusterSnapshotCopyStatus_retentionPeriod :: Lens.Lens' AwsRedshiftClusterClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Int)
awsRedshiftClusterClusterSnapshotCopyStatus_retentionPeriod = Lens.lens (\AwsRedshiftClusterClusterSnapshotCopyStatus' {retentionPeriod} -> retentionPeriod) (\s@AwsRedshiftClusterClusterSnapshotCopyStatus' {} a -> s {retentionPeriod = a} :: AwsRedshiftClusterClusterSnapshotCopyStatus)

-- | The name of the snapshot copy grant.
awsRedshiftClusterClusterSnapshotCopyStatus_snapshotCopyGrantName :: Lens.Lens' AwsRedshiftClusterClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterSnapshotCopyStatus_snapshotCopyGrantName = Lens.lens (\AwsRedshiftClusterClusterSnapshotCopyStatus' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@AwsRedshiftClusterClusterSnapshotCopyStatus' {} a -> s {snapshotCopyGrantName = a} :: AwsRedshiftClusterClusterSnapshotCopyStatus)

instance
  Data.FromJSON
    AwsRedshiftClusterClusterSnapshotCopyStatus
  where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterClusterSnapshotCopyStatus"
      ( \x ->
          AwsRedshiftClusterClusterSnapshotCopyStatus'
            Prelude.<$> (x Data..:? "DestinationRegion")
            Prelude.<*> (x Data..:? "ManualSnapshotRetentionPeriod")
            Prelude.<*> (x Data..:? "RetentionPeriod")
            Prelude.<*> (x Data..:? "SnapshotCopyGrantName")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterClusterSnapshotCopyStatus
  where
  hashWithSalt
    _salt
    AwsRedshiftClusterClusterSnapshotCopyStatus' {..} =
      _salt
        `Prelude.hashWithSalt` destinationRegion
        `Prelude.hashWithSalt` manualSnapshotRetentionPeriod
        `Prelude.hashWithSalt` retentionPeriod
        `Prelude.hashWithSalt` snapshotCopyGrantName

instance
  Prelude.NFData
    AwsRedshiftClusterClusterSnapshotCopyStatus
  where
  rnf AwsRedshiftClusterClusterSnapshotCopyStatus' {..} =
    Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf manualSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf snapshotCopyGrantName

instance
  Data.ToJSON
    AwsRedshiftClusterClusterSnapshotCopyStatus
  where
  toJSON
    AwsRedshiftClusterClusterSnapshotCopyStatus' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DestinationRegion" Data..=)
                Prelude.<$> destinationRegion,
              ("ManualSnapshotRetentionPeriod" Data..=)
                Prelude.<$> manualSnapshotRetentionPeriod,
              ("RetentionPeriod" Data..=)
                Prelude.<$> retentionPeriod,
              ("SnapshotCopyGrantName" Data..=)
                Prelude.<$> snapshotCopyGrantName
            ]
        )
