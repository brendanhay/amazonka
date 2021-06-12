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
-- Module      : Network.AWS.DynamoDB.Types.ArchivalSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ArchivalSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details of a table archival operation.
--
-- /See:/ 'newArchivalSummary' smart constructor.
data ArchivalSummary = ArchivalSummary'
  { -- | The Amazon Resource Name (ARN) of the backup the table was archived to,
    -- when applicable in the archival reason. If you wish to restore this
    -- backup to the same table name, you will need to delete the original
    -- table.
    archivalBackupArn :: Core.Maybe Core.Text,
    -- | The reason DynamoDB archived the table. Currently, the only possible
    -- value is:
    --
    -- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due
    --     to the table\'s AWS KMS key being inaccessible for more than seven
    --     days. An On-Demand backup was created at the archival time.
    archivalReason :: Core.Maybe Core.Text,
    -- | The date and time when table archival was initiated by DynamoDB, in UNIX
    -- epoch time format.
    archivalDateTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ArchivalSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archivalBackupArn', 'archivalSummary_archivalBackupArn' - The Amazon Resource Name (ARN) of the backup the table was archived to,
-- when applicable in the archival reason. If you wish to restore this
-- backup to the same table name, you will need to delete the original
-- table.
--
-- 'archivalReason', 'archivalSummary_archivalReason' - The reason DynamoDB archived the table. Currently, the only possible
-- value is:
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due
--     to the table\'s AWS KMS key being inaccessible for more than seven
--     days. An On-Demand backup was created at the archival time.
--
-- 'archivalDateTime', 'archivalSummary_archivalDateTime' - The date and time when table archival was initiated by DynamoDB, in UNIX
-- epoch time format.
newArchivalSummary ::
  ArchivalSummary
newArchivalSummary =
  ArchivalSummary'
    { archivalBackupArn = Core.Nothing,
      archivalReason = Core.Nothing,
      archivalDateTime = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the backup the table was archived to,
-- when applicable in the archival reason. If you wish to restore this
-- backup to the same table name, you will need to delete the original
-- table.
archivalSummary_archivalBackupArn :: Lens.Lens' ArchivalSummary (Core.Maybe Core.Text)
archivalSummary_archivalBackupArn = Lens.lens (\ArchivalSummary' {archivalBackupArn} -> archivalBackupArn) (\s@ArchivalSummary' {} a -> s {archivalBackupArn = a} :: ArchivalSummary)

-- | The reason DynamoDB archived the table. Currently, the only possible
-- value is:
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due
--     to the table\'s AWS KMS key being inaccessible for more than seven
--     days. An On-Demand backup was created at the archival time.
archivalSummary_archivalReason :: Lens.Lens' ArchivalSummary (Core.Maybe Core.Text)
archivalSummary_archivalReason = Lens.lens (\ArchivalSummary' {archivalReason} -> archivalReason) (\s@ArchivalSummary' {} a -> s {archivalReason = a} :: ArchivalSummary)

-- | The date and time when table archival was initiated by DynamoDB, in UNIX
-- epoch time format.
archivalSummary_archivalDateTime :: Lens.Lens' ArchivalSummary (Core.Maybe Core.UTCTime)
archivalSummary_archivalDateTime = Lens.lens (\ArchivalSummary' {archivalDateTime} -> archivalDateTime) (\s@ArchivalSummary' {} a -> s {archivalDateTime = a} :: ArchivalSummary) Core.. Lens.mapping Core._Time

instance Core.FromJSON ArchivalSummary where
  parseJSON =
    Core.withObject
      "ArchivalSummary"
      ( \x ->
          ArchivalSummary'
            Core.<$> (x Core..:? "ArchivalBackupArn")
            Core.<*> (x Core..:? "ArchivalReason")
            Core.<*> (x Core..:? "ArchivalDateTime")
      )

instance Core.Hashable ArchivalSummary

instance Core.NFData ArchivalSummary
