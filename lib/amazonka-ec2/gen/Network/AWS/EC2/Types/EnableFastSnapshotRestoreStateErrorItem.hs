{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
  ( EnableFastSnapshotRestoreStateErrorItem (..),

    -- * Smart constructor
    mkEnableFastSnapshotRestoreStateErrorItem,

    -- * Lenses
    efsrseiAvailabilityZone,
    efsrseiError,
  )
where

import qualified Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an error that occurred when enabling fast snapshot restores.
--
-- /See:/ 'mkEnableFastSnapshotRestoreStateErrorItem' smart constructor.
data EnableFastSnapshotRestoreStateErrorItem = EnableFastSnapshotRestoreStateErrorItem'
  { -- | The Availability Zone.
    availabilityZone :: Core.Maybe Types.String,
    -- | The error.
    error :: Core.Maybe Types.EnableFastSnapshotRestoreStateError
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableFastSnapshotRestoreStateErrorItem' value with any optional fields omitted.
mkEnableFastSnapshotRestoreStateErrorItem ::
  EnableFastSnapshotRestoreStateErrorItem
mkEnableFastSnapshotRestoreStateErrorItem =
  EnableFastSnapshotRestoreStateErrorItem'
    { availabilityZone =
        Core.Nothing,
      error = Core.Nothing
    }

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrseiAvailabilityZone :: Lens.Lens' EnableFastSnapshotRestoreStateErrorItem (Core.Maybe Types.String)
efsrseiAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED efsrseiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrseiError :: Lens.Lens' EnableFastSnapshotRestoreStateErrorItem (Core.Maybe Types.EnableFastSnapshotRestoreStateError)
efsrseiError = Lens.field @"error"
{-# DEPRECATED efsrseiError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Core.FromXML EnableFastSnapshotRestoreStateErrorItem where
  parseXML x =
    EnableFastSnapshotRestoreStateErrorItem'
      Core.<$> (x Core..@? "availabilityZone") Core.<*> (x Core..@? "error")
