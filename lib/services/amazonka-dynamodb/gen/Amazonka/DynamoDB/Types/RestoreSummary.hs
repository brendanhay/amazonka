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
-- Module      : Amazonka.DynamoDB.Types.RestoreSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.RestoreSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

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
    restoreDateTime :: Data.POSIX,
    -- | Indicates if a restore is in progress or not.
    restoreInProgress :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
          Data._Time Lens.# pRestoreDateTime_,
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
restoreSummary_restoreDateTime = Lens.lens (\RestoreSummary' {restoreDateTime} -> restoreDateTime) (\s@RestoreSummary' {} a -> s {restoreDateTime = a} :: RestoreSummary) Prelude.. Data._Time

-- | Indicates if a restore is in progress or not.
restoreSummary_restoreInProgress :: Lens.Lens' RestoreSummary Prelude.Bool
restoreSummary_restoreInProgress = Lens.lens (\RestoreSummary' {restoreInProgress} -> restoreInProgress) (\s@RestoreSummary' {} a -> s {restoreInProgress = a} :: RestoreSummary)

instance Data.FromJSON RestoreSummary where
  parseJSON =
    Data.withObject
      "RestoreSummary"
      ( \x ->
          RestoreSummary'
            Prelude.<$> (x Data..:? "SourceBackupArn")
            Prelude.<*> (x Data..:? "SourceTableArn")
            Prelude.<*> (x Data..: "RestoreDateTime")
            Prelude.<*> (x Data..: "RestoreInProgress")
      )

instance Prelude.Hashable RestoreSummary where
  hashWithSalt _salt RestoreSummary' {..} =
    _salt
      `Prelude.hashWithSalt` sourceBackupArn
      `Prelude.hashWithSalt` sourceTableArn
      `Prelude.hashWithSalt` restoreDateTime
      `Prelude.hashWithSalt` restoreInProgress

instance Prelude.NFData RestoreSummary where
  rnf RestoreSummary' {..} =
    Prelude.rnf sourceBackupArn `Prelude.seq`
      Prelude.rnf sourceTableArn `Prelude.seq`
        Prelude.rnf restoreDateTime `Prelude.seq`
          Prelude.rnf restoreInProgress
