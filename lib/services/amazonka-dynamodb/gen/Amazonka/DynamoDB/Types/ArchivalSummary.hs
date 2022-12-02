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
-- Module      : Amazonka.DynamoDB.Types.ArchivalSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ArchivalSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Contains details of a table archival operation.
--
-- /See:/ 'newArchivalSummary' smart constructor.
data ArchivalSummary = ArchivalSummary'
  { -- | The reason DynamoDB archived the table. Currently, the only possible
    -- value is:
    --
    -- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due
    --     to the table\'s KMS key being inaccessible for more than seven days.
    --     An On-Demand backup was created at the archival time.
    archivalReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time when table archival was initiated by DynamoDB, in UNIX
    -- epoch time format.
    archivalDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the backup the table was archived to,
    -- when applicable in the archival reason. If you wish to restore this
    -- backup to the same table name, you will need to delete the original
    -- table.
    archivalBackupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchivalSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archivalReason', 'archivalSummary_archivalReason' - The reason DynamoDB archived the table. Currently, the only possible
-- value is:
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due
--     to the table\'s KMS key being inaccessible for more than seven days.
--     An On-Demand backup was created at the archival time.
--
-- 'archivalDateTime', 'archivalSummary_archivalDateTime' - The date and time when table archival was initiated by DynamoDB, in UNIX
-- epoch time format.
--
-- 'archivalBackupArn', 'archivalSummary_archivalBackupArn' - The Amazon Resource Name (ARN) of the backup the table was archived to,
-- when applicable in the archival reason. If you wish to restore this
-- backup to the same table name, you will need to delete the original
-- table.
newArchivalSummary ::
  ArchivalSummary
newArchivalSummary =
  ArchivalSummary'
    { archivalReason = Prelude.Nothing,
      archivalDateTime = Prelude.Nothing,
      archivalBackupArn = Prelude.Nothing
    }

-- | The reason DynamoDB archived the table. Currently, the only possible
-- value is:
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due
--     to the table\'s KMS key being inaccessible for more than seven days.
--     An On-Demand backup was created at the archival time.
archivalSummary_archivalReason :: Lens.Lens' ArchivalSummary (Prelude.Maybe Prelude.Text)
archivalSummary_archivalReason = Lens.lens (\ArchivalSummary' {archivalReason} -> archivalReason) (\s@ArchivalSummary' {} a -> s {archivalReason = a} :: ArchivalSummary)

-- | The date and time when table archival was initiated by DynamoDB, in UNIX
-- epoch time format.
archivalSummary_archivalDateTime :: Lens.Lens' ArchivalSummary (Prelude.Maybe Prelude.UTCTime)
archivalSummary_archivalDateTime = Lens.lens (\ArchivalSummary' {archivalDateTime} -> archivalDateTime) (\s@ArchivalSummary' {} a -> s {archivalDateTime = a} :: ArchivalSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the backup the table was archived to,
-- when applicable in the archival reason. If you wish to restore this
-- backup to the same table name, you will need to delete the original
-- table.
archivalSummary_archivalBackupArn :: Lens.Lens' ArchivalSummary (Prelude.Maybe Prelude.Text)
archivalSummary_archivalBackupArn = Lens.lens (\ArchivalSummary' {archivalBackupArn} -> archivalBackupArn) (\s@ArchivalSummary' {} a -> s {archivalBackupArn = a} :: ArchivalSummary)

instance Data.FromJSON ArchivalSummary where
  parseJSON =
    Data.withObject
      "ArchivalSummary"
      ( \x ->
          ArchivalSummary'
            Prelude.<$> (x Data..:? "ArchivalReason")
            Prelude.<*> (x Data..:? "ArchivalDateTime")
            Prelude.<*> (x Data..:? "ArchivalBackupArn")
      )

instance Prelude.Hashable ArchivalSummary where
  hashWithSalt _salt ArchivalSummary' {..} =
    _salt `Prelude.hashWithSalt` archivalReason
      `Prelude.hashWithSalt` archivalDateTime
      `Prelude.hashWithSalt` archivalBackupArn

instance Prelude.NFData ArchivalSummary where
  rnf ArchivalSummary' {..} =
    Prelude.rnf archivalReason
      `Prelude.seq` Prelude.rnf archivalDateTime
      `Prelude.seq` Prelude.rnf archivalBackupArn
