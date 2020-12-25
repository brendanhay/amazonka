{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AutoSnapshotDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AutoSnapshotDetails
  ( AutoSnapshotDetails (..),

    -- * Smart constructor
    mkAutoSnapshotDetails,

    -- * Lenses
    asdCreatedAt,
    asdDate,
    asdFromAttachedDisks,
    asdStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.AttachedDisk as Types
import qualified Network.AWS.Lightsail.Types.AutoSnapshotStatus as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an automatic snapshot.
--
-- /See:/ 'mkAutoSnapshotDetails' smart constructor.
data AutoSnapshotDetails = AutoSnapshotDetails'
  { -- | The timestamp when the automatic snapshot was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The date of the automatic snapshot in @YYYY-MM-DD@ format.
    date :: Core.Maybe Types.String,
    -- | An array of objects that describe the block storage disks attached to the instance when the automatic snapshot was created.
    fromAttachedDisks :: Core.Maybe [Types.AttachedDisk],
    -- | The status of the automatic snapshot.
    status :: Core.Maybe Types.AutoSnapshotStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AutoSnapshotDetails' value with any optional fields omitted.
mkAutoSnapshotDetails ::
  AutoSnapshotDetails
mkAutoSnapshotDetails =
  AutoSnapshotDetails'
    { createdAt = Core.Nothing,
      date = Core.Nothing,
      fromAttachedDisks = Core.Nothing,
      status = Core.Nothing
    }

-- | The timestamp when the automatic snapshot was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdCreatedAt :: Lens.Lens' AutoSnapshotDetails (Core.Maybe Core.NominalDiffTime)
asdCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED asdCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The date of the automatic snapshot in @YYYY-MM-DD@ format.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdDate :: Lens.Lens' AutoSnapshotDetails (Core.Maybe Types.String)
asdDate = Lens.field @"date"
{-# DEPRECATED asdDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | An array of objects that describe the block storage disks attached to the instance when the automatic snapshot was created.
--
-- /Note:/ Consider using 'fromAttachedDisks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdFromAttachedDisks :: Lens.Lens' AutoSnapshotDetails (Core.Maybe [Types.AttachedDisk])
asdFromAttachedDisks = Lens.field @"fromAttachedDisks"
{-# DEPRECATED asdFromAttachedDisks "Use generic-lens or generic-optics with 'fromAttachedDisks' instead." #-}

-- | The status of the automatic snapshot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdStatus :: Lens.Lens' AutoSnapshotDetails (Core.Maybe Types.AutoSnapshotStatus)
asdStatus = Lens.field @"status"
{-# DEPRECATED asdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON AutoSnapshotDetails where
  parseJSON =
    Core.withObject "AutoSnapshotDetails" Core.$
      \x ->
        AutoSnapshotDetails'
          Core.<$> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "date")
          Core.<*> (x Core..:? "fromAttachedDisks")
          Core.<*> (x Core..:? "status")
