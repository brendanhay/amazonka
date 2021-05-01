{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details for the restore.
--
-- /See:/ 'newRestoreSummary' smart constructor.
data RestoreSummary = RestoreSummary'
  { -- | The Amazon Resource Name (ARN) of the backup from which the table was
    -- restored.
    sourceBackupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the source table of the backup that is being restored.
    sourceTableArn :: Prelude.Maybe Prelude.Text,
    -- | Point in time or source backup time.
    restoreDateTime :: Prelude.POSIX,
    -- | Indicates if a restore is in progress or not.
    restoreInProgress :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.UTCTime ->
  -- | 'restoreInProgress'
  Prelude.Bool ->
  RestoreSummary
newRestoreSummary
  pRestoreDateTime_
  pRestoreInProgress_ =
    RestoreSummary'
      { sourceBackupArn = Prelude.Nothing,
        sourceTableArn = Prelude.Nothing,
        restoreDateTime =
          Prelude._Time Lens.# pRestoreDateTime_,
        restoreInProgress = pRestoreInProgress_
      }

-- | The Amazon Resource Name (ARN) of the backup from which the table was
-- restored.
restoreSummary_sourceBackupArn :: Lens.Lens' RestoreSummary (Prelude.Maybe Prelude.Text)
restoreSummary_sourceBackupArn = Lens.lens (\RestoreSummary' {sourceBackupArn} -> sourceBackupArn) (\s@RestoreSummary' {} a -> s {sourceBackupArn = a} :: RestoreSummary)

-- | The ARN of the source table of the backup that is being restored.
restoreSummary_sourceTableArn :: Lens.Lens' RestoreSummary (Prelude.Maybe Prelude.Text)
restoreSummary_sourceTableArn = Lens.lens (\RestoreSummary' {sourceTableArn} -> sourceTableArn) (\s@RestoreSummary' {} a -> s {sourceTableArn = a} :: RestoreSummary)

-- | Point in time or source backup time.
restoreSummary_restoreDateTime :: Lens.Lens' RestoreSummary Prelude.UTCTime
restoreSummary_restoreDateTime = Lens.lens (\RestoreSummary' {restoreDateTime} -> restoreDateTime) (\s@RestoreSummary' {} a -> s {restoreDateTime = a} :: RestoreSummary) Prelude.. Prelude._Time

-- | Indicates if a restore is in progress or not.
restoreSummary_restoreInProgress :: Lens.Lens' RestoreSummary Prelude.Bool
restoreSummary_restoreInProgress = Lens.lens (\RestoreSummary' {restoreInProgress} -> restoreInProgress) (\s@RestoreSummary' {} a -> s {restoreInProgress = a} :: RestoreSummary)

instance Prelude.FromJSON RestoreSummary where
  parseJSON =
    Prelude.withObject
      "RestoreSummary"
      ( \x ->
          RestoreSummary'
            Prelude.<$> (x Prelude..:? "SourceBackupArn")
            Prelude.<*> (x Prelude..:? "SourceTableArn")
            Prelude.<*> (x Prelude..: "RestoreDateTime")
            Prelude.<*> (x Prelude..: "RestoreInProgress")
      )

instance Prelude.Hashable RestoreSummary

instance Prelude.NFData RestoreSummary
