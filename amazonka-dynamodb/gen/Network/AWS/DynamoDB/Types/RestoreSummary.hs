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
-- Module      : Network.AWS.DynamoDB.Types.RestoreSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.RestoreSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details for the restore.
--
-- /See:/ 'newRestoreSummary' smart constructor.
data RestoreSummary = RestoreSummary'
  { -- | The Amazon Resource Name (ARN) of the backup from which the table was
    -- restored.
    sourceBackupArn :: Core.Maybe Core.Text,
    -- | The ARN of the source table of the backup that is being restored.
    sourceTableArn :: Core.Maybe Core.Text,
    -- | Point in time or source backup time.
    restoreDateTime :: Core.POSIX,
    -- | Indicates if a restore is in progress or not.
    restoreInProgress :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RestoreSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceBackupArn', 'restoreSummary_sourceBackupArn' - The Amazon Resource Name (ARN) of the backup from which the table was
-- restored.
--
-- 'sourceTableArn', 'restoreSummary_sourceTableArn' - The ARN of the source table of the backup that is being restored.
--
-- 'restoreDateTime', 'restoreSummary_restoreDateTime' - Point in time or source backup time.
--
-- 'restoreInProgress', 'restoreSummary_restoreInProgress' - Indicates if a restore is in progress or not.
newRestoreSummary ::
  -- | 'restoreDateTime'
  Core.UTCTime ->
  -- | 'restoreInProgress'
  Core.Bool ->
  RestoreSummary
newRestoreSummary
  pRestoreDateTime_
  pRestoreInProgress_ =
    RestoreSummary'
      { sourceBackupArn = Core.Nothing,
        sourceTableArn = Core.Nothing,
        restoreDateTime =
          Core._Time Lens.# pRestoreDateTime_,
        restoreInProgress = pRestoreInProgress_
      }

-- | The Amazon Resource Name (ARN) of the backup from which the table was
-- restored.
restoreSummary_sourceBackupArn :: Lens.Lens' RestoreSummary (Core.Maybe Core.Text)
restoreSummary_sourceBackupArn = Lens.lens (\RestoreSummary' {sourceBackupArn} -> sourceBackupArn) (\s@RestoreSummary' {} a -> s {sourceBackupArn = a} :: RestoreSummary)

-- | The ARN of the source table of the backup that is being restored.
restoreSummary_sourceTableArn :: Lens.Lens' RestoreSummary (Core.Maybe Core.Text)
restoreSummary_sourceTableArn = Lens.lens (\RestoreSummary' {sourceTableArn} -> sourceTableArn) (\s@RestoreSummary' {} a -> s {sourceTableArn = a} :: RestoreSummary)

-- | Point in time or source backup time.
restoreSummary_restoreDateTime :: Lens.Lens' RestoreSummary Core.UTCTime
restoreSummary_restoreDateTime = Lens.lens (\RestoreSummary' {restoreDateTime} -> restoreDateTime) (\s@RestoreSummary' {} a -> s {restoreDateTime = a} :: RestoreSummary) Core.. Core._Time

-- | Indicates if a restore is in progress or not.
restoreSummary_restoreInProgress :: Lens.Lens' RestoreSummary Core.Bool
restoreSummary_restoreInProgress = Lens.lens (\RestoreSummary' {restoreInProgress} -> restoreInProgress) (\s@RestoreSummary' {} a -> s {restoreInProgress = a} :: RestoreSummary)

instance Core.FromJSON RestoreSummary where
  parseJSON =
    Core.withObject
      "RestoreSummary"
      ( \x ->
          RestoreSummary'
            Core.<$> (x Core..:? "SourceBackupArn")
            Core.<*> (x Core..:? "SourceTableArn")
            Core.<*> (x Core..: "RestoreDateTime")
            Core.<*> (x Core..: "RestoreInProgress")
      )

instance Core.Hashable RestoreSummary

instance Core.NFData RestoreSummary
