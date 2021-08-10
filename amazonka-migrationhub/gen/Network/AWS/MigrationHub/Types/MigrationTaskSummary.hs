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
-- Module      : Network.AWS.MigrationHub.Types.MigrationTaskSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.MigrationTaskSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.MigrationStatus
import qualified Network.AWS.Prelude as Prelude

-- | MigrationTaskSummary includes @MigrationTaskName@, @ProgressPercent@,
-- @ProgressUpdateStream@, @Status@, and @UpdateDateTime@ for each task.
--
-- /See:/ 'newMigrationTaskSummary' smart constructor.
data MigrationTaskSummary = MigrationTaskSummary'
  { -- | Status of the task.
    status :: Prelude.Maybe MigrationStatus,
    -- | Indication of the percentage completion of the task.
    progressPercent :: Prelude.Maybe Prelude.Natural,
    -- | The timestamp when the task was gathered.
    updateDateTime :: Prelude.Maybe Core.POSIX,
    -- | Detail information of what is being done within the overall status
    -- state.
    statusDetail :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Prelude.Maybe Prelude.Text,
    -- | An AWS resource used for access control. It should uniquely identify the
    -- migration tool as it is used for all updates made by the tool.
    progressUpdateStream :: Prelude.Maybe Prelude.Text
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
-- 'status', 'migrationTaskSummary_status' - Status of the task.
--
-- 'progressPercent', 'migrationTaskSummary_progressPercent' - Indication of the percentage completion of the task.
--
-- 'updateDateTime', 'migrationTaskSummary_updateDateTime' - The timestamp when the task was gathered.
--
-- 'statusDetail', 'migrationTaskSummary_statusDetail' - Detail information of what is being done within the overall status
-- state.
--
-- 'migrationTaskName', 'migrationTaskSummary_migrationTaskName' - Unique identifier that references the migration task. /Do not store
-- personal data in this field./
--
-- 'progressUpdateStream', 'migrationTaskSummary_progressUpdateStream' - An AWS resource used for access control. It should uniquely identify the
-- migration tool as it is used for all updates made by the tool.
newMigrationTaskSummary ::
  MigrationTaskSummary
newMigrationTaskSummary =
  MigrationTaskSummary'
    { status = Prelude.Nothing,
      progressPercent = Prelude.Nothing,
      updateDateTime = Prelude.Nothing,
      statusDetail = Prelude.Nothing,
      migrationTaskName = Prelude.Nothing,
      progressUpdateStream = Prelude.Nothing
    }

-- | Status of the task.
migrationTaskSummary_status :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe MigrationStatus)
migrationTaskSummary_status = Lens.lens (\MigrationTaskSummary' {status} -> status) (\s@MigrationTaskSummary' {} a -> s {status = a} :: MigrationTaskSummary)

-- | Indication of the percentage completion of the task.
migrationTaskSummary_progressPercent :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe Prelude.Natural)
migrationTaskSummary_progressPercent = Lens.lens (\MigrationTaskSummary' {progressPercent} -> progressPercent) (\s@MigrationTaskSummary' {} a -> s {progressPercent = a} :: MigrationTaskSummary)

-- | The timestamp when the task was gathered.
migrationTaskSummary_updateDateTime :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe Prelude.UTCTime)
migrationTaskSummary_updateDateTime = Lens.lens (\MigrationTaskSummary' {updateDateTime} -> updateDateTime) (\s@MigrationTaskSummary' {} a -> s {updateDateTime = a} :: MigrationTaskSummary) Prelude.. Lens.mapping Core._Time

-- | Detail information of what is being done within the overall status
-- state.
migrationTaskSummary_statusDetail :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe Prelude.Text)
migrationTaskSummary_statusDetail = Lens.lens (\MigrationTaskSummary' {statusDetail} -> statusDetail) (\s@MigrationTaskSummary' {} a -> s {statusDetail = a} :: MigrationTaskSummary)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
migrationTaskSummary_migrationTaskName :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe Prelude.Text)
migrationTaskSummary_migrationTaskName = Lens.lens (\MigrationTaskSummary' {migrationTaskName} -> migrationTaskName) (\s@MigrationTaskSummary' {} a -> s {migrationTaskName = a} :: MigrationTaskSummary)

-- | An AWS resource used for access control. It should uniquely identify the
-- migration tool as it is used for all updates made by the tool.
migrationTaskSummary_progressUpdateStream :: Lens.Lens' MigrationTaskSummary (Prelude.Maybe Prelude.Text)
migrationTaskSummary_progressUpdateStream = Lens.lens (\MigrationTaskSummary' {progressUpdateStream} -> progressUpdateStream) (\s@MigrationTaskSummary' {} a -> s {progressUpdateStream = a} :: MigrationTaskSummary)

instance Core.FromJSON MigrationTaskSummary where
  parseJSON =
    Core.withObject
      "MigrationTaskSummary"
      ( \x ->
          MigrationTaskSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ProgressPercent")
            Prelude.<*> (x Core..:? "UpdateDateTime")
            Prelude.<*> (x Core..:? "StatusDetail")
            Prelude.<*> (x Core..:? "MigrationTaskName")
            Prelude.<*> (x Core..:? "ProgressUpdateStream")
      )

instance Prelude.Hashable MigrationTaskSummary

instance Prelude.NFData MigrationTaskSummary
