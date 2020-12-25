{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreErrorItem
  ( EnableFastSnapshotRestoreErrorItem (..),

    -- * Smart constructor
    mkEnableFastSnapshotRestoreErrorItem,

    -- * Lenses
    efsreiFastSnapshotRestoreStateErrors,
    efsreiSnapshotId,
  )
where

import qualified Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the errors that occurred when enabling fast snapshot restores.
--
-- /See:/ 'mkEnableFastSnapshotRestoreErrorItem' smart constructor.
data EnableFastSnapshotRestoreErrorItem = EnableFastSnapshotRestoreErrorItem'
  { -- | The errors.
    fastSnapshotRestoreStateErrors :: Core.Maybe [Types.EnableFastSnapshotRestoreStateErrorItem],
    -- | The ID of the snapshot.
    snapshotId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableFastSnapshotRestoreErrorItem' value with any optional fields omitted.
mkEnableFastSnapshotRestoreErrorItem ::
  EnableFastSnapshotRestoreErrorItem
mkEnableFastSnapshotRestoreErrorItem =
  EnableFastSnapshotRestoreErrorItem'
    { fastSnapshotRestoreStateErrors =
        Core.Nothing,
      snapshotId = Core.Nothing
    }

-- | The errors.
--
-- /Note:/ Consider using 'fastSnapshotRestoreStateErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsreiFastSnapshotRestoreStateErrors :: Lens.Lens' EnableFastSnapshotRestoreErrorItem (Core.Maybe [Types.EnableFastSnapshotRestoreStateErrorItem])
efsreiFastSnapshotRestoreStateErrors = Lens.field @"fastSnapshotRestoreStateErrors"
{-# DEPRECATED efsreiFastSnapshotRestoreStateErrors "Use generic-lens or generic-optics with 'fastSnapshotRestoreStateErrors' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsreiSnapshotId :: Lens.Lens' EnableFastSnapshotRestoreErrorItem (Core.Maybe Types.String)
efsreiSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED efsreiSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Core.FromXML EnableFastSnapshotRestoreErrorItem where
  parseXML x =
    EnableFastSnapshotRestoreErrorItem'
      Core.<$> ( x Core..@? "fastSnapshotRestoreStateErrorSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "snapshotId")
