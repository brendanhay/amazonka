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
    asArchivalReason,
    asArchivalDateTime,
    asArchivalBackupARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details of a table archival operation.
--
-- /See:/ 'mkArchivalSummary' smart constructor.
data ArchivalSummary = ArchivalSummary'
  { archivalReason ::
      Lude.Maybe Lude.Text,
    archivalDateTime :: Lude.Maybe Lude.Timestamp,
    archivalBackupARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArchivalSummary' with the minimum fields required to make a request.
--
-- * 'archivalBackupARN' - The Amazon Resource Name (ARN) of the backup the table was archived to, when applicable in the archival reason. If you wish to restore this backup to the same table name, you will need to delete the original table.
-- * 'archivalDateTime' - The date and time when table archival was initiated by DynamoDB, in UNIX epoch time format.
-- * 'archivalReason' - The reason DynamoDB archived the table. Currently, the only possible value is:
--
--
--     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due to the table's AWS KMS key being inaccessible for more than seven days. An On-Demand backup was created at the archival time.
mkArchivalSummary ::
  ArchivalSummary
mkArchivalSummary =
  ArchivalSummary'
    { archivalReason = Lude.Nothing,
      archivalDateTime = Lude.Nothing,
      archivalBackupARN = Lude.Nothing
    }

-- | The reason DynamoDB archived the table. Currently, the only possible value is:
--
--
--     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due to the table's AWS KMS key being inaccessible for more than seven days. An On-Demand backup was created at the archival time.
--
--
--
-- /Note:/ Consider using 'archivalReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asArchivalReason :: Lens.Lens' ArchivalSummary (Lude.Maybe Lude.Text)
asArchivalReason = Lens.lens (archivalReason :: ArchivalSummary -> Lude.Maybe Lude.Text) (\s a -> s {archivalReason = a} :: ArchivalSummary)
{-# DEPRECATED asArchivalReason "Use generic-lens or generic-optics with 'archivalReason' instead." #-}

-- | The date and time when table archival was initiated by DynamoDB, in UNIX epoch time format.
--
-- /Note:/ Consider using 'archivalDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asArchivalDateTime :: Lens.Lens' ArchivalSummary (Lude.Maybe Lude.Timestamp)
asArchivalDateTime = Lens.lens (archivalDateTime :: ArchivalSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {archivalDateTime = a} :: ArchivalSummary)
{-# DEPRECATED asArchivalDateTime "Use generic-lens or generic-optics with 'archivalDateTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the backup the table was archived to, when applicable in the archival reason. If you wish to restore this backup to the same table name, you will need to delete the original table.
--
-- /Note:/ Consider using 'archivalBackupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asArchivalBackupARN :: Lens.Lens' ArchivalSummary (Lude.Maybe Lude.Text)
asArchivalBackupARN = Lens.lens (archivalBackupARN :: ArchivalSummary -> Lude.Maybe Lude.Text) (\s a -> s {archivalBackupARN = a} :: ArchivalSummary)
{-# DEPRECATED asArchivalBackupARN "Use generic-lens or generic-optics with 'archivalBackupARN' instead." #-}

instance Lude.FromJSON ArchivalSummary where
  parseJSON =
    Lude.withObject
      "ArchivalSummary"
      ( \x ->
          ArchivalSummary'
            Lude.<$> (x Lude..:? "ArchivalReason")
            Lude.<*> (x Lude..:? "ArchivalDateTime")
            Lude.<*> (x Lude..:? "ArchivalBackupArn")
      )
