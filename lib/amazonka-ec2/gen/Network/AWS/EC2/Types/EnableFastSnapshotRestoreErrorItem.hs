{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreErrorItem
  ( EnableFastSnapshotRestoreErrorItem (..)
  -- * Smart constructor
  , mkEnableFastSnapshotRestoreErrorItem
  -- * Lenses
  , efsreiFastSnapshotRestoreStateErrors
  , efsreiSnapshotId
  ) where

import qualified Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the errors that occurred when enabling fast snapshot restores.
--
-- /See:/ 'mkEnableFastSnapshotRestoreErrorItem' smart constructor.
data EnableFastSnapshotRestoreErrorItem = EnableFastSnapshotRestoreErrorItem'
  { fastSnapshotRestoreStateErrors :: Core.Maybe [Types.EnableFastSnapshotRestoreStateErrorItem]
    -- ^ The errors.
  , snapshotId :: Core.Maybe Core.Text
    -- ^ The ID of the snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableFastSnapshotRestoreErrorItem' value with any optional fields omitted.
mkEnableFastSnapshotRestoreErrorItem
    :: EnableFastSnapshotRestoreErrorItem
mkEnableFastSnapshotRestoreErrorItem
  = EnableFastSnapshotRestoreErrorItem'{fastSnapshotRestoreStateErrors
                                          = Core.Nothing,
                                        snapshotId = Core.Nothing}

-- | The errors.
--
-- /Note:/ Consider using 'fastSnapshotRestoreStateErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsreiFastSnapshotRestoreStateErrors :: Lens.Lens' EnableFastSnapshotRestoreErrorItem (Core.Maybe [Types.EnableFastSnapshotRestoreStateErrorItem])
efsreiFastSnapshotRestoreStateErrors = Lens.field @"fastSnapshotRestoreStateErrors"
{-# INLINEABLE efsreiFastSnapshotRestoreStateErrors #-}
{-# DEPRECATED fastSnapshotRestoreStateErrors "Use generic-lens or generic-optics with 'fastSnapshotRestoreStateErrors' instead"  #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsreiSnapshotId :: Lens.Lens' EnableFastSnapshotRestoreErrorItem (Core.Maybe Core.Text)
efsreiSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE efsreiSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

instance Core.FromXML EnableFastSnapshotRestoreErrorItem where
        parseXML x
          = EnableFastSnapshotRestoreErrorItem' Core.<$>
              (x Core..@? "fastSnapshotRestoreStateErrorSet" Core..<@>
                 Core.parseXMLList "item")
                Core.<*> x Core..@? "snapshotId"
