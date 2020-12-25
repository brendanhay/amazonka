{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
  ( DisableFastSnapshotRestoreStateErrorItem (..),

    -- * Smart constructor
    mkDisableFastSnapshotRestoreStateErrorItem,

    -- * Lenses
    dfsrseiAvailabilityZone,
    dfsrseiError,
  )
where

import qualified Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateError as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an error that occurred when disabling fast snapshot restores.
--
-- /See:/ 'mkDisableFastSnapshotRestoreStateErrorItem' smart constructor.
data DisableFastSnapshotRestoreStateErrorItem = DisableFastSnapshotRestoreStateErrorItem'
  { -- | The Availability Zone.
    availabilityZone :: Core.Maybe Types.String,
    -- | The error.
    error :: Core.Maybe Types.DisableFastSnapshotRestoreStateError
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableFastSnapshotRestoreStateErrorItem' value with any optional fields omitted.
mkDisableFastSnapshotRestoreStateErrorItem ::
  DisableFastSnapshotRestoreStateErrorItem
mkDisableFastSnapshotRestoreStateErrorItem =
  DisableFastSnapshotRestoreStateErrorItem'
    { availabilityZone =
        Core.Nothing,
      error = Core.Nothing
    }

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrseiAvailabilityZone :: Lens.Lens' DisableFastSnapshotRestoreStateErrorItem (Core.Maybe Types.String)
dfsrseiAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED dfsrseiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrseiError :: Lens.Lens' DisableFastSnapshotRestoreStateErrorItem (Core.Maybe Types.DisableFastSnapshotRestoreStateError)
dfsrseiError = Lens.field @"error"
{-# DEPRECATED dfsrseiError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Core.FromXML DisableFastSnapshotRestoreStateErrorItem where
  parseXML x =
    DisableFastSnapshotRestoreStateErrorItem'
      Core.<$> (x Core..@? "availabilityZone") Core.<*> (x Core..@? "error")
