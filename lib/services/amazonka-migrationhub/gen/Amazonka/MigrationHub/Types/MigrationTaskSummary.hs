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
-- Module      : Amazonka.MigrationHub.Types.MigrationTaskSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHub.Types.MigrationTaskSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHub.Types.MigrationStatus
import qualified Amazonka.Prelude as Prelude

-- | MigrationTaskSummary includes @MigrationTaskName@, @ProgressPercent@,
-- @ProgressUpdateStream@, @Status@, and @UpdateDateTime@ for each task.
--
-- /See:/ 'newMigrationTaskSummary' smart constructor.
data MigrationTaskSummary = MigrationTaskSummary'
  { -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Prelude.Maybe Prelude.Text,
    -- | Indication of the percentage completion of the task.
    progressPercent :: Prelude.Maybe Prelude.Natural,
    -- | An AWS resource used for access control. It should uniquely identify the
    -- migration tool as it is used for all updates made by the tool.
    progressUpdateStream :: Prelude.Maybe Prelude.Text,
    -- | Status of the task.
    status :: Prelude.Maybe MigrationStatus,
    -- | Detail information of what is being done within the overall status
    -- state.
    statusDetail :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the task was gathered.
    updateDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MigrationTaskSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'migrationTaskName', 'migrationTaskSummary_migrationTaskName' - Unique identifier that references the migration task. /Do not store
-- personal data in this field./
--
-- 'progressPercent', 'migrationTaskSummary_progressPercent' - Indication of the percentage completion of the task.
--
-- 'progressUpdateStream', 'migrationTaskSummary_progressUpdateStream' - An AWS resource used for access control. It should uniquely identify the
-- migration tool as it is used for all updates made by the tool.
--
-- 'status', 'migrationTaskSummary_status' - Status of the task.
--
-- 'statusDetail', 'migrationTaskSummary_statusDetail' - Detail information of what is being done within the overall status
-- state.
--
-- 'updateDateTime', 'migrationTaskSummary_updateDateTime' - The timestamp when the task was gathered.
newMigrationTaskSummary ::
  MigrationTaskSummary
newMigrationTaskSummary =
  MigrationTaskSummary'
    { migrationTaskName =
        Prelude.Nothing,
      progressPercent = Prelude.Nothing,
      progressUpdateStream = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetail = Prelude.Nothing,
      updateDateTime = Prelude.Nothing
    }

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
migrationTaskSummary_migrationTaskName :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe Prelude.Text)
migrationTaskSummary_migrationTaskName = Lens.lens (\MigrationTaskSummary' {migrationTaskName} -> migrationTaskName) (\s@MigrationTaskSummary' {} a -> s {migrationTaskName = a} :: MigrationTaskSummary)

-- | Indication of the percentage completion of the task.
migrationTaskSummary_progressPercent :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe Prelude.Natural)
migrationTaskSummary_progressPercent = Lens.lens (\MigrationTaskSummary' {progressPercent} -> progressPercent) (\s@MigrationTaskSummary' {} a -> s {progressPercent = a} :: MigrationTaskSummary)

-- | An AWS resource used for access control. It should uniquely identify the
-- migration tool as it is used for all updates made by the tool.
migrationTaskSummary_progressUpdateStream :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe Prelude.Text)
migrationTaskSummary_progressUpdateStream = Lens.lens (\MigrationTaskSummary' {progressUpdateStream} -> progressUpdateStream) (\s@MigrationTaskSummary' {} a -> s {progressUpdateStream = a} :: MigrationTaskSummary)

-- | Status of the task.
migrationTaskSummary_status :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe MigrationStatus)
migrationTaskSummary_status = Lens.lens (\MigrationTaskSummary' {status} -> status) (\s@MigrationTaskSummary' {} a -> s {status = a} :: MigrationTaskSummary)

-- | Detail information of what is being done within the overall status
-- state.
migrationTaskSummary_statusDetail :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe Prelude.Text)
migrationTaskSummary_statusDetail = Lens.lens (\MigrationTaskSummary' {statusDetail} -> statusDetail) (\s@MigrationTaskSummary' {} a -> s {statusDetail = a} :: MigrationTaskSummary)

-- | The timestamp when the task was gathered.
migrationTaskSummary_updateDateTime :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe Prelude.UTCTime)
migrationTaskSummary_updateDateTime = Lens.lens (\MigrationTaskSummary' {updateDateTime} -> updateDateTime) (\s@MigrationTaskSummary' {} a -> s {updateDateTime = a} :: MigrationTaskSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON MigrationTaskSummary where
  parseJSON =
    Data.withObject
      "MigrationTaskSummary"
      ( \x ->
          MigrationTaskSummary'
            Prelude.<$> (x Data..:? "MigrationTaskName")
            Prelude.<*> (x Data..:? "ProgressPercent")
            Prelude.<*> (x Data..:? "ProgressUpdateStream")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusDetail")
            Prelude.<*> (x Data..:? "UpdateDateTime")
      )

instance Prelude.Hashable MigrationTaskSummary where
  hashWithSalt _salt MigrationTaskSummary' {..} =
    _salt
      `Prelude.hashWithSalt` migrationTaskName
      `Prelude.hashWithSalt` progressPercent
      `Prelude.hashWithSalt` progressUpdateStream
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusDetail
      `Prelude.hashWithSalt` updateDateTime

instance Prelude.NFData MigrationTaskSummary where
  rnf MigrationTaskSummary' {..} =
    Prelude.rnf migrationTaskName
      `Prelude.seq` Prelude.rnf progressPercent
      `Prelude.seq` Prelude.rnf progressUpdateStream
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusDetail
      `Prelude.seq` Prelude.rnf updateDateTime
