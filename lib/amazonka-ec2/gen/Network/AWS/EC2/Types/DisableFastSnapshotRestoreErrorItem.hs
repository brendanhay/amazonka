{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreErrorItem
  ( DisableFastSnapshotRestoreErrorItem (..)
  -- * Smart constructor
  , mkDisableFastSnapshotRestoreErrorItem
  -- * Lenses
  , dfsreiFastSnapshotRestoreStateErrors
  , dfsreiSnapshotId
  ) where

import qualified Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the errors that occurred when disabling fast snapshot restores.
--
-- /See:/ 'mkDisableFastSnapshotRestoreErrorItem' smart constructor.
data DisableFastSnapshotRestoreErrorItem = DisableFastSnapshotRestoreErrorItem'
  { fastSnapshotRestoreStateErrors :: Core.Maybe [Types.DisableFastSnapshotRestoreStateErrorItem]
    -- ^ The errors.
  , snapshotId :: Core.Maybe Core.Text
    -- ^ The ID of the snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableFastSnapshotRestoreErrorItem' value with any optional fields omitted.
mkDisableFastSnapshotRestoreErrorItem
    :: DisableFastSnapshotRestoreErrorItem
mkDisableFastSnapshotRestoreErrorItem
  = DisableFastSnapshotRestoreErrorItem'{fastSnapshotRestoreStateErrors
                                           = Core.Nothing,
                                         snapshotId = Core.Nothing}

-- | The errors.
--
-- /Note:/ Consider using 'fastSnapshotRestoreStateErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsreiFastSnapshotRestoreStateErrors :: Lens.Lens' DisableFastSnapshotRestoreErrorItem (Core.Maybe [Types.DisableFastSnapshotRestoreStateErrorItem])
dfsreiFastSnapshotRestoreStateErrors = Lens.field @"fastSnapshotRestoreStateErrors"
{-# INLINEABLE dfsreiFastSnapshotRestoreStateErrors #-}
{-# DEPRECATED fastSnapshotRestoreStateErrors "Use generic-lens or generic-optics with 'fastSnapshotRestoreStateErrors' instead"  #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsreiSnapshotId :: Lens.Lens' DisableFastSnapshotRestoreErrorItem (Core.Maybe Core.Text)
dfsreiSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE dfsreiSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

instance Core.FromXML DisableFastSnapshotRestoreErrorItem where
        parseXML x
          = DisableFastSnapshotRestoreErrorItem' Core.<$>
              (x Core..@? "fastSnapshotRestoreStateErrorSet" Core..<@>
                 Core.parseXMLList "item")
                Core.<*> x Core..@? "snapshotId"
