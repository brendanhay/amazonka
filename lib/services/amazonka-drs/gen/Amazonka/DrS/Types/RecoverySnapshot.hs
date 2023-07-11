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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoverySnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A snapshot of a Source Server used during recovery.
--
-- /See:/ 'newRecoverySnapshot' smart constructor.
data RecoverySnapshot = RecoverySnapshot'
  { -- | A list of EBS snapshots.
    ebsSnapshots :: Prelude.Maybe [Prelude.Text],
    -- | The actual timestamp that the snapshot was taken.
    timestamp :: Prelude.Maybe Prelude.Text,
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
-- 'ebsSnapshots', 'recoverySnapshot_ebsSnapshots' - A list of EBS snapshots.
--
-- 'timestamp', 'recoverySnapshot_timestamp' - The actual timestamp that the snapshot was taken.
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
      { ebsSnapshots = Prelude.Nothing,
        timestamp = Prelude.Nothing,
        expectedTimestamp = pExpectedTimestamp_,
        snapshotID = pSnapshotID_,
        sourceServerID = pSourceServerID_
      }

-- | A list of EBS snapshots.
recoverySnapshot_ebsSnapshots :: Lens.Lens' RecoverySnapshot (Prelude.Maybe [Prelude.Text])
recoverySnapshot_ebsSnapshots = Lens.lens (\RecoverySnapshot' {ebsSnapshots} -> ebsSnapshots) (\s@RecoverySnapshot' {} a -> s {ebsSnapshots = a} :: RecoverySnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The actual timestamp that the snapshot was taken.
recoverySnapshot_timestamp :: Lens.Lens' RecoverySnapshot (Prelude.Maybe Prelude.Text)
recoverySnapshot_timestamp = Lens.lens (\RecoverySnapshot' {timestamp} -> timestamp) (\s@RecoverySnapshot' {} a -> s {timestamp = a} :: RecoverySnapshot)

-- | The timestamp of when we expect the snapshot to be taken.
recoverySnapshot_expectedTimestamp :: Lens.Lens' RecoverySnapshot Prelude.Text
recoverySnapshot_expectedTimestamp = Lens.lens (\RecoverySnapshot' {expectedTimestamp} -> expectedTimestamp) (\s@RecoverySnapshot' {} a -> s {expectedTimestamp = a} :: RecoverySnapshot)

-- | The ID of the Recovery Snapshot.
recoverySnapshot_snapshotID :: Lens.Lens' RecoverySnapshot Prelude.Text
recoverySnapshot_snapshotID = Lens.lens (\RecoverySnapshot' {snapshotID} -> snapshotID) (\s@RecoverySnapshot' {} a -> s {snapshotID = a} :: RecoverySnapshot)

-- | The ID of the Source Server that the snapshot was taken for.
recoverySnapshot_sourceServerID :: Lens.Lens' RecoverySnapshot Prelude.Text
recoverySnapshot_sourceServerID = Lens.lens (\RecoverySnapshot' {sourceServerID} -> sourceServerID) (\s@RecoverySnapshot' {} a -> s {sourceServerID = a} :: RecoverySnapshot)

instance Data.FromJSON RecoverySnapshot where
  parseJSON =
    Data.withObject
      "RecoverySnapshot"
      ( \x ->
          RecoverySnapshot'
            Prelude.<$> (x Data..:? "ebsSnapshots" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "timestamp")
            Prelude.<*> (x Data..: "expectedTimestamp")
            Prelude.<*> (x Data..: "snapshotID")
            Prelude.<*> (x Data..: "sourceServerID")
      )

instance Prelude.Hashable RecoverySnapshot where
  hashWithSalt _salt RecoverySnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` ebsSnapshots
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` expectedTimestamp
      `Prelude.hashWithSalt` snapshotID
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData RecoverySnapshot where
  rnf RecoverySnapshot' {..} =
    Prelude.rnf ebsSnapshots
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf expectedTimestamp
      `Prelude.seq` Prelude.rnf snapshotID
      `Prelude.seq` Prelude.rnf sourceServerID
