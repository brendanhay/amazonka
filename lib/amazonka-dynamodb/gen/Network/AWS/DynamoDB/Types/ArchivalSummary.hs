{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ArchivalSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ArchivalSummary
  ( ArchivalSummary (..),

    -- * Smart constructor
    mkArchivalSummary,

    -- * Lenses
    asArchivalBackupArn,
    asArchivalDateTime,
    asArchivalReason,
  )
where

import qualified Network.AWS.DynamoDB.Types.ArchivalReason as Types
import qualified Network.AWS.DynamoDB.Types.BackupArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details of a table archival operation.
--
-- /See:/ 'mkArchivalSummary' smart constructor.
data ArchivalSummary = ArchivalSummary'
  { -- | The Amazon Resource Name (ARN) of the backup the table was archived to, when applicable in the archival reason. If you wish to restore this backup to the same table name, you will need to delete the original table.
    archivalBackupArn :: Core.Maybe Types.BackupArn,
    -- | The date and time when table archival was initiated by DynamoDB, in UNIX epoch time format.
    archivalDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The reason DynamoDB archived the table. Currently, the only possible value is:
    --
    --
    --     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due to the table's AWS KMS key being inaccessible for more than seven days. An On-Demand backup was created at the archival time.
    archivalReason :: Core.Maybe Types.ArchivalReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ArchivalSummary' value with any optional fields omitted.
mkArchivalSummary ::
  ArchivalSummary
mkArchivalSummary =
  ArchivalSummary'
    { archivalBackupArn = Core.Nothing,
      archivalDateTime = Core.Nothing,
      archivalReason = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the backup the table was archived to, when applicable in the archival reason. If you wish to restore this backup to the same table name, you will need to delete the original table.
--
-- /Note:/ Consider using 'archivalBackupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asArchivalBackupArn :: Lens.Lens' ArchivalSummary (Core.Maybe Types.BackupArn)
asArchivalBackupArn = Lens.field @"archivalBackupArn"
{-# DEPRECATED asArchivalBackupArn "Use generic-lens or generic-optics with 'archivalBackupArn' instead." #-}

-- | The date and time when table archival was initiated by DynamoDB, in UNIX epoch time format.
--
-- /Note:/ Consider using 'archivalDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asArchivalDateTime :: Lens.Lens' ArchivalSummary (Core.Maybe Core.NominalDiffTime)
asArchivalDateTime = Lens.field @"archivalDateTime"
{-# DEPRECATED asArchivalDateTime "Use generic-lens or generic-optics with 'archivalDateTime' instead." #-}

-- | The reason DynamoDB archived the table. Currently, the only possible value is:
--
--
--     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due to the table's AWS KMS key being inaccessible for more than seven days. An On-Demand backup was created at the archival time.
--
--
--
-- /Note:/ Consider using 'archivalReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asArchivalReason :: Lens.Lens' ArchivalSummary (Core.Maybe Types.ArchivalReason)
asArchivalReason = Lens.field @"archivalReason"
{-# DEPRECATED asArchivalReason "Use generic-lens or generic-optics with 'archivalReason' instead." #-}

instance Core.FromJSON ArchivalSummary where
  parseJSON =
    Core.withObject "ArchivalSummary" Core.$
      \x ->
        ArchivalSummary'
          Core.<$> (x Core..:? "ArchivalBackupArn")
          Core.<*> (x Core..:? "ArchivalDateTime")
          Core.<*> (x Core..:? "ArchivalReason")
