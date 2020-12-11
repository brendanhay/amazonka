-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.RestoreSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.RestoreSummary
  ( RestoreSummary (..),

    -- * Smart constructor
    mkRestoreSummary,

    -- * Lenses
    rsSourceTableARN,
    rsSourceBackupARN,
    rsRestoreDateTime,
    rsRestoreInProgress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details for the restore.
--
-- /See:/ 'mkRestoreSummary' smart constructor.
data RestoreSummary = RestoreSummary'
  { sourceTableARN ::
      Lude.Maybe Lude.Text,
    sourceBackupARN :: Lude.Maybe Lude.Text,
    restoreDateTime :: Lude.Timestamp,
    restoreInProgress :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreSummary' with the minimum fields required to make a request.
--
-- * 'restoreDateTime' - Point in time or source backup time.
-- * 'restoreInProgress' - Indicates if a restore is in progress or not.
-- * 'sourceBackupARN' - The Amazon Resource Name (ARN) of the backup from which the table was restored.
-- * 'sourceTableARN' - The ARN of the source table of the backup that is being restored.
mkRestoreSummary ::
  -- | 'restoreDateTime'
  Lude.Timestamp ->
  -- | 'restoreInProgress'
  Lude.Bool ->
  RestoreSummary
mkRestoreSummary pRestoreDateTime_ pRestoreInProgress_ =
  RestoreSummary'
    { sourceTableARN = Lude.Nothing,
      sourceBackupARN = Lude.Nothing,
      restoreDateTime = pRestoreDateTime_,
      restoreInProgress = pRestoreInProgress_
    }

-- | The ARN of the source table of the backup that is being restored.
--
-- /Note:/ Consider using 'sourceTableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSourceTableARN :: Lens.Lens' RestoreSummary (Lude.Maybe Lude.Text)
rsSourceTableARN = Lens.lens (sourceTableARN :: RestoreSummary -> Lude.Maybe Lude.Text) (\s a -> s {sourceTableARN = a} :: RestoreSummary)
{-# DEPRECATED rsSourceTableARN "Use generic-lens or generic-optics with 'sourceTableARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the backup from which the table was restored.
--
-- /Note:/ Consider using 'sourceBackupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSourceBackupARN :: Lens.Lens' RestoreSummary (Lude.Maybe Lude.Text)
rsSourceBackupARN = Lens.lens (sourceBackupARN :: RestoreSummary -> Lude.Maybe Lude.Text) (\s a -> s {sourceBackupARN = a} :: RestoreSummary)
{-# DEPRECATED rsSourceBackupARN "Use generic-lens or generic-optics with 'sourceBackupARN' instead." #-}

-- | Point in time or source backup time.
--
-- /Note:/ Consider using 'restoreDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRestoreDateTime :: Lens.Lens' RestoreSummary Lude.Timestamp
rsRestoreDateTime = Lens.lens (restoreDateTime :: RestoreSummary -> Lude.Timestamp) (\s a -> s {restoreDateTime = a} :: RestoreSummary)
{-# DEPRECATED rsRestoreDateTime "Use generic-lens or generic-optics with 'restoreDateTime' instead." #-}

-- | Indicates if a restore is in progress or not.
--
-- /Note:/ Consider using 'restoreInProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRestoreInProgress :: Lens.Lens' RestoreSummary Lude.Bool
rsRestoreInProgress = Lens.lens (restoreInProgress :: RestoreSummary -> Lude.Bool) (\s a -> s {restoreInProgress = a} :: RestoreSummary)
{-# DEPRECATED rsRestoreInProgress "Use generic-lens or generic-optics with 'restoreInProgress' instead." #-}

instance Lude.FromJSON RestoreSummary where
  parseJSON =
    Lude.withObject
      "RestoreSummary"
      ( \x ->
          RestoreSummary'
            Lude.<$> (x Lude..:? "SourceTableArn")
            Lude.<*> (x Lude..:? "SourceBackupArn")
            Lude.<*> (x Lude..: "RestoreDateTime")
            Lude.<*> (x Lude..: "RestoreInProgress")
      )
