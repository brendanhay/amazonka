{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseBundle
  ( RelationalDatabaseBundle (..),

    -- * Smart constructor
    mkRelationalDatabaseBundle,

    -- * Lenses
    rdbBundleId,
    rdbCpuCount,
    rdbDiskSizeInGb,
    rdbIsActive,
    rdbIsEncrypted,
    rdbName,
    rdbPrice,
    rdbRamSizeInGb,
    rdbTransferPerMonthInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a database bundle. A bundle describes the performance specifications of the database.
--
-- /See:/ 'mkRelationalDatabaseBundle' smart constructor.
data RelationalDatabaseBundle = RelationalDatabaseBundle'
  { -- | The ID for the database bundle.
    bundleId :: Core.Maybe Types.String,
    -- | The number of virtual CPUs (vCPUs) for the database bundle.
    cpuCount :: Core.Maybe Core.Int,
    -- | The size of the disk for the database bundle.
    diskSizeInGb :: Core.Maybe Core.Int,
    -- | A Boolean value indicating whether the database bundle is active.
    isActive :: Core.Maybe Core.Bool,
    -- | A Boolean value indicating whether the database bundle is encrypted.
    isEncrypted :: Core.Maybe Core.Bool,
    -- | The name for the database bundle.
    name :: Core.Maybe Types.String,
    -- | The cost of the database bundle in US currency.
    price :: Core.Maybe Core.Double,
    -- | The amount of RAM in GB (for example, @2.0@ ) for the database bundle.
    ramSizeInGb :: Core.Maybe Core.Double,
    -- | The data transfer rate per month in GB for the database bundle.
    transferPerMonthInGb :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RelationalDatabaseBundle' value with any optional fields omitted.
mkRelationalDatabaseBundle ::
  RelationalDatabaseBundle
mkRelationalDatabaseBundle =
  RelationalDatabaseBundle'
    { bundleId = Core.Nothing,
      cpuCount = Core.Nothing,
      diskSizeInGb = Core.Nothing,
      isActive = Core.Nothing,
      isEncrypted = Core.Nothing,
      name = Core.Nothing,
      price = Core.Nothing,
      ramSizeInGb = Core.Nothing,
      transferPerMonthInGb = Core.Nothing
    }

-- | The ID for the database bundle.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbBundleId :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Types.String)
rdbBundleId = Lens.field @"bundleId"
{-# DEPRECATED rdbBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The number of virtual CPUs (vCPUs) for the database bundle.
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbCpuCount :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Int)
rdbCpuCount = Lens.field @"cpuCount"
{-# DEPRECATED rdbCpuCount "Use generic-lens or generic-optics with 'cpuCount' instead." #-}

-- | The size of the disk for the database bundle.
--
-- /Note:/ Consider using 'diskSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbDiskSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Int)
rdbDiskSizeInGb = Lens.field @"diskSizeInGb"
{-# DEPRECATED rdbDiskSizeInGb "Use generic-lens or generic-optics with 'diskSizeInGb' instead." #-}

-- | A Boolean value indicating whether the database bundle is active.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbIsActive :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Bool)
rdbIsActive = Lens.field @"isActive"
{-# DEPRECATED rdbIsActive "Use generic-lens or generic-optics with 'isActive' instead." #-}

-- | A Boolean value indicating whether the database bundle is encrypted.
--
-- /Note:/ Consider using 'isEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbIsEncrypted :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Bool)
rdbIsEncrypted = Lens.field @"isEncrypted"
{-# DEPRECATED rdbIsEncrypted "Use generic-lens or generic-optics with 'isEncrypted' instead." #-}

-- | The name for the database bundle.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbName :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Types.String)
rdbName = Lens.field @"name"
{-# DEPRECATED rdbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The cost of the database bundle in US currency.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbPrice :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Double)
rdbPrice = Lens.field @"price"
{-# DEPRECATED rdbPrice "Use generic-lens or generic-optics with 'price' instead." #-}

-- | The amount of RAM in GB (for example, @2.0@ ) for the database bundle.
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbRamSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Double)
rdbRamSizeInGb = Lens.field @"ramSizeInGb"
{-# DEPRECATED rdbRamSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead." #-}

-- | The data transfer rate per month in GB for the database bundle.
--
-- /Note:/ Consider using 'transferPerMonthInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbTransferPerMonthInGb :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Int)
rdbTransferPerMonthInGb = Lens.field @"transferPerMonthInGb"
{-# DEPRECATED rdbTransferPerMonthInGb "Use generic-lens or generic-optics with 'transferPerMonthInGb' instead." #-}

instance Core.FromJSON RelationalDatabaseBundle where
  parseJSON =
    Core.withObject "RelationalDatabaseBundle" Core.$
      \x ->
        RelationalDatabaseBundle'
          Core.<$> (x Core..:? "bundleId")
          Core.<*> (x Core..:? "cpuCount")
          Core.<*> (x Core..:? "diskSizeInGb")
          Core.<*> (x Core..:? "isActive")
          Core.<*> (x Core..:? "isEncrypted")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "price")
          Core.<*> (x Core..:? "ramSizeInGb")
          Core.<*> (x Core..:? "transferPerMonthInGb")
