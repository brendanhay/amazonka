{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.RestoreSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.RestoreSummary
  ( RestoreSummary (..)
  -- * Smart constructor
  , mkRestoreSummary
  -- * Lenses
  , rsRestoreDateTime
  , rsRestoreInProgress
  , rsSourceBackupArn
  , rsSourceTableArn
  ) where

import qualified Network.AWS.DynamoDB.Types.SourceBackupArn as Types
import qualified Network.AWS.DynamoDB.Types.SourceTableArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details for the restore.
--
-- /See:/ 'mkRestoreSummary' smart constructor.
data RestoreSummary = RestoreSummary'
  { restoreDateTime :: Core.NominalDiffTime
    -- ^ Point in time or source backup time.
  , restoreInProgress :: Core.Bool
    -- ^ Indicates if a restore is in progress or not.
  , sourceBackupArn :: Core.Maybe Types.SourceBackupArn
    -- ^ The Amazon Resource Name (ARN) of the backup from which the table was restored.
  , sourceTableArn :: Core.Maybe Types.SourceTableArn
    -- ^ The ARN of the source table of the backup that is being restored.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreSummary' value with any optional fields omitted.
mkRestoreSummary
    :: Core.NominalDiffTime -- ^ 'restoreDateTime'
    -> Core.Bool -- ^ 'restoreInProgress'
    -> RestoreSummary
mkRestoreSummary restoreDateTime restoreInProgress
  = RestoreSummary'{restoreDateTime, restoreInProgress,
                    sourceBackupArn = Core.Nothing, sourceTableArn = Core.Nothing}

-- | Point in time or source backup time.
--
-- /Note:/ Consider using 'restoreDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRestoreDateTime :: Lens.Lens' RestoreSummary Core.NominalDiffTime
rsRestoreDateTime = Lens.field @"restoreDateTime"
{-# INLINEABLE rsRestoreDateTime #-}
{-# DEPRECATED restoreDateTime "Use generic-lens or generic-optics with 'restoreDateTime' instead"  #-}

-- | Indicates if a restore is in progress or not.
--
-- /Note:/ Consider using 'restoreInProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRestoreInProgress :: Lens.Lens' RestoreSummary Core.Bool
rsRestoreInProgress = Lens.field @"restoreInProgress"
{-# INLINEABLE rsRestoreInProgress #-}
{-# DEPRECATED restoreInProgress "Use generic-lens or generic-optics with 'restoreInProgress' instead"  #-}

-- | The Amazon Resource Name (ARN) of the backup from which the table was restored.
--
-- /Note:/ Consider using 'sourceBackupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSourceBackupArn :: Lens.Lens' RestoreSummary (Core.Maybe Types.SourceBackupArn)
rsSourceBackupArn = Lens.field @"sourceBackupArn"
{-# INLINEABLE rsSourceBackupArn #-}
{-# DEPRECATED sourceBackupArn "Use generic-lens or generic-optics with 'sourceBackupArn' instead"  #-}

-- | The ARN of the source table of the backup that is being restored.
--
-- /Note:/ Consider using 'sourceTableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSourceTableArn :: Lens.Lens' RestoreSummary (Core.Maybe Types.SourceTableArn)
rsSourceTableArn = Lens.field @"sourceTableArn"
{-# INLINEABLE rsSourceTableArn #-}
{-# DEPRECATED sourceTableArn "Use generic-lens or generic-optics with 'sourceTableArn' instead"  #-}

instance Core.FromJSON RestoreSummary where
        parseJSON
          = Core.withObject "RestoreSummary" Core.$
              \ x ->
                RestoreSummary' Core.<$>
                  (x Core..: "RestoreDateTime") Core.<*>
                    x Core..: "RestoreInProgress"
                    Core.<*> x Core..:? "SourceBackupArn"
                    Core.<*> x Core..:? "SourceTableArn"
