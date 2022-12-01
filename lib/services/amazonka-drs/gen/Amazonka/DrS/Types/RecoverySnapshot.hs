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
-- Module      : Amazonka.DrS.Types.RecoverySnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoverySnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A snapshot of a Source Server used during recovery.
--
-- /See:/ 'newRecoverySnapshot' smart constructor.
data RecoverySnapshot = RecoverySnapshot'
  { -- | The actual timestamp that the snapshot was taken.
    timestamp :: Prelude.Maybe Prelude.Text,
    -- | A list of EBS snapshots.
    ebsSnapshots :: Prelude.Maybe [Prelude.Text],
    -- | The timestamp of when we expect the snapshot to be taken.
    expectedTimestamp :: Prelude.Text,
    -- | The ID of the Recovery Snapshot.
    snapshotID :: Prelude.Text,
    -- | The ID of the Source Server that the snapshot was taken for.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoverySnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'recoverySnapshot_timestamp' - The actual timestamp that the snapshot was taken.
--
-- 'ebsSnapshots', 'recoverySnapshot_ebsSnapshots' - A list of EBS snapshots.
--
-- 'expectedTimestamp', 'recoverySnapshot_expectedTimestamp' - The timestamp of when we expect the snapshot to be taken.
--
-- 'snapshotID', 'recoverySnapshot_snapshotID' - The ID of the Recovery Snapshot.
--
-- 'sourceServerID', 'recoverySnapshot_sourceServerID' - The ID of the Source Server that the snapshot was taken for.
newRecoverySnapshot ::
  -- | 'expectedTimestamp'
  Prelude.Text ->
  -- | 'snapshotID'
  Prelude.Text ->
  -- | 'sourceServerID'
  Prelude.Text ->
  RecoverySnapshot
newRecoverySnapshot
  pExpectedTimestamp_
  pSnapshotID_
  pSourceServerID_ =
    RecoverySnapshot'
      { timestamp = Prelude.Nothing,
        ebsSnapshots = Prelude.Nothing,
        expectedTimestamp = pExpectedTimestamp_,
        snapshotID = pSnapshotID_,
        sourceServerID = pSourceServerID_
      }

-- | The actual timestamp that the snapshot was taken.
recoverySnapshot_timestamp :: Lens.Lens' RecoverySnapshot (Prelude.Maybe Prelude.Text)
recoverySnapshot_timestamp = Lens.lens (\RecoverySnapshot' {timestamp} -> timestamp) (\s@RecoverySnapshot' {} a -> s {timestamp = a} :: RecoverySnapshot)

-- | A list of EBS snapshots.
recoverySnapshot_ebsSnapshots :: Lens.Lens' RecoverySnapshot (Prelude.Maybe [Prelude.Text])
recoverySnapshot_ebsSnapshots = Lens.lens (\RecoverySnapshot' {ebsSnapshots} -> ebsSnapshots) (\s@RecoverySnapshot' {} a -> s {ebsSnapshots = a} :: RecoverySnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of when we expect the snapshot to be taken.
recoverySnapshot_expectedTimestamp :: Lens.Lens' RecoverySnapshot Prelude.Text
recoverySnapshot_expectedTimestamp = Lens.lens (\RecoverySnapshot' {expectedTimestamp} -> expectedTimestamp) (\s@RecoverySnapshot' {} a -> s {expectedTimestamp = a} :: RecoverySnapshot)

-- | The ID of the Recovery Snapshot.
recoverySnapshot_snapshotID :: Lens.Lens' RecoverySnapshot Prelude.Text
recoverySnapshot_snapshotID = Lens.lens (\RecoverySnapshot' {snapshotID} -> snapshotID) (\s@RecoverySnapshot' {} a -> s {snapshotID = a} :: RecoverySnapshot)

-- | The ID of the Source Server that the snapshot was taken for.
recoverySnapshot_sourceServerID :: Lens.Lens' RecoverySnapshot Prelude.Text
recoverySnapshot_sourceServerID = Lens.lens (\RecoverySnapshot' {sourceServerID} -> sourceServerID) (\s@RecoverySnapshot' {} a -> s {sourceServerID = a} :: RecoverySnapshot)

instance Core.FromJSON RecoverySnapshot where
  parseJSON =
    Core.withObject
      "RecoverySnapshot"
      ( \x ->
          RecoverySnapshot'
            Prelude.<$> (x Core..:? "timestamp")
            Prelude.<*> (x Core..:? "ebsSnapshots" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "expectedTimestamp")
            Prelude.<*> (x Core..: "snapshotID")
            Prelude.<*> (x Core..: "sourceServerID")
      )

instance Prelude.Hashable RecoverySnapshot where
  hashWithSalt _salt RecoverySnapshot' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` ebsSnapshots
      `Prelude.hashWithSalt` expectedTimestamp
      `Prelude.hashWithSalt` snapshotID
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData RecoverySnapshot where
  rnf RecoverySnapshot' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf ebsSnapshots
      `Prelude.seq` Prelude.rnf expectedTimestamp
      `Prelude.seq` Prelude.rnf snapshotID
      `Prelude.seq` Prelude.rnf sourceServerID
