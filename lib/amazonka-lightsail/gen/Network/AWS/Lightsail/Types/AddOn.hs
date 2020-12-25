{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AddOn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AddOn
  ( AddOn (..),

    -- * Smart constructor
    mkAddOn,

    -- * Lenses
    aoName,
    aoNextSnapshotTimeOfDay,
    aoSnapshotTimeOfDay,
    aoStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Lightsail.Types.TimeOfDay as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an add-on that is enabled for an Amazon Lightsail resource.
--
-- /See:/ 'mkAddOn' smart constructor.
data AddOn = AddOn'
  { -- | The name of the add-on.
    name :: Core.Maybe Types.String,
    -- | The next daily time an automatic snapshot will be created.
    --
    -- The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC).
    -- The snapshot is automatically created between the time shown and up to 45 minutes after.
    nextSnapshotTimeOfDay :: Core.Maybe Types.TimeOfDay,
    -- | The daily time when an automatic snapshot is created.
    --
    -- The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC).
    -- The snapshot is automatically created between the time shown and up to 45 minutes after.
    snapshotTimeOfDay :: Core.Maybe Types.TimeOfDay,
    -- | The status of the add-on.
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddOn' value with any optional fields omitted.
mkAddOn ::
  AddOn
mkAddOn =
  AddOn'
    { name = Core.Nothing,
      nextSnapshotTimeOfDay = Core.Nothing,
      snapshotTimeOfDay = Core.Nothing,
      status = Core.Nothing
    }

-- | The name of the add-on.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoName :: Lens.Lens' AddOn (Core.Maybe Types.String)
aoName = Lens.field @"name"
{-# DEPRECATED aoName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The next daily time an automatic snapshot will be created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC).
-- The snapshot is automatically created between the time shown and up to 45 minutes after.
--
-- /Note:/ Consider using 'nextSnapshotTimeOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoNextSnapshotTimeOfDay :: Lens.Lens' AddOn (Core.Maybe Types.TimeOfDay)
aoNextSnapshotTimeOfDay = Lens.field @"nextSnapshotTimeOfDay"
{-# DEPRECATED aoNextSnapshotTimeOfDay "Use generic-lens or generic-optics with 'nextSnapshotTimeOfDay' instead." #-}

-- | The daily time when an automatic snapshot is created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC).
-- The snapshot is automatically created between the time shown and up to 45 minutes after.
--
-- /Note:/ Consider using 'snapshotTimeOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoSnapshotTimeOfDay :: Lens.Lens' AddOn (Core.Maybe Types.TimeOfDay)
aoSnapshotTimeOfDay = Lens.field @"snapshotTimeOfDay"
{-# DEPRECATED aoSnapshotTimeOfDay "Use generic-lens or generic-optics with 'snapshotTimeOfDay' instead." #-}

-- | The status of the add-on.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoStatus :: Lens.Lens' AddOn (Core.Maybe Types.String)
aoStatus = Lens.field @"status"
{-# DEPRECATED aoStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON AddOn where
  parseJSON =
    Core.withObject "AddOn" Core.$
      \x ->
        AddOn'
          Core.<$> (x Core..:? "name")
          Core.<*> (x Core..:? "nextSnapshotTimeOfDay")
          Core.<*> (x Core..:? "snapshotTimeOfDay")
          Core.<*> (x Core..:? "status")
