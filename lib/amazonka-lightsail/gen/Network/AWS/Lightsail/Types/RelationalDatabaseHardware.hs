{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseHardware
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseHardware
  ( RelationalDatabaseHardware (..),

    -- * Smart constructor
    mkRelationalDatabaseHardware,

    -- * Lenses
    rdhCpuCount,
    rdhDiskSizeInGb,
    rdhRamSizeInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the hardware of a database.
--
-- /See:/ 'mkRelationalDatabaseHardware' smart constructor.
data RelationalDatabaseHardware = RelationalDatabaseHardware'
  { -- | The number of vCPUs for the database.
    cpuCount :: Core.Maybe Core.Int,
    -- | The size of the disk for the database.
    diskSizeInGb :: Core.Maybe Core.Int,
    -- | The amount of RAM in GB for the database.
    ramSizeInGb :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RelationalDatabaseHardware' value with any optional fields omitted.
mkRelationalDatabaseHardware ::
  RelationalDatabaseHardware
mkRelationalDatabaseHardware =
  RelationalDatabaseHardware'
    { cpuCount = Core.Nothing,
      diskSizeInGb = Core.Nothing,
      ramSizeInGb = Core.Nothing
    }

-- | The number of vCPUs for the database.
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdhCpuCount :: Lens.Lens' RelationalDatabaseHardware (Core.Maybe Core.Int)
rdhCpuCount = Lens.field @"cpuCount"
{-# DEPRECATED rdhCpuCount "Use generic-lens or generic-optics with 'cpuCount' instead." #-}

-- | The size of the disk for the database.
--
-- /Note:/ Consider using 'diskSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdhDiskSizeInGb :: Lens.Lens' RelationalDatabaseHardware (Core.Maybe Core.Int)
rdhDiskSizeInGb = Lens.field @"diskSizeInGb"
{-# DEPRECATED rdhDiskSizeInGb "Use generic-lens or generic-optics with 'diskSizeInGb' instead." #-}

-- | The amount of RAM in GB for the database.
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdhRamSizeInGb :: Lens.Lens' RelationalDatabaseHardware (Core.Maybe Core.Double)
rdhRamSizeInGb = Lens.field @"ramSizeInGb"
{-# DEPRECATED rdhRamSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead." #-}

instance Core.FromJSON RelationalDatabaseHardware where
  parseJSON =
    Core.withObject "RelationalDatabaseHardware" Core.$
      \x ->
        RelationalDatabaseHardware'
          Core.<$> (x Core..:? "cpuCount")
          Core.<*> (x Core..:? "diskSizeInGb")
          Core.<*> (x Core..:? "ramSizeInGb")
