{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.RelationalDatabaseBundle
  ( RelationalDatabaseBundle (..)
  -- * Smart constructor
  , mkRelationalDatabaseBundle
  -- * Lenses
  , rdbBundleId
  , rdbCpuCount
  , rdbDiskSizeInGb
  , rdbIsActive
  , rdbIsEncrypted
  , rdbName
  , rdbPrice
  , rdbRamSizeInGb
  , rdbTransferPerMonthInGb
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a database bundle. A bundle describes the performance specifications of the database.
--
-- /See:/ 'mkRelationalDatabaseBundle' smart constructor.
data RelationalDatabaseBundle = RelationalDatabaseBundle'
  { bundleId :: Core.Maybe Core.Text
    -- ^ The ID for the database bundle.
  , cpuCount :: Core.Maybe Core.Int
    -- ^ The number of virtual CPUs (vCPUs) for the database bundle.
  , diskSizeInGb :: Core.Maybe Core.Int
    -- ^ The size of the disk for the database bundle.
  , isActive :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether the database bundle is active.
  , isEncrypted :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether the database bundle is encrypted.
  , name :: Core.Maybe Core.Text
    -- ^ The name for the database bundle.
  , price :: Core.Maybe Core.Double
    -- ^ The cost of the database bundle in US currency.
  , ramSizeInGb :: Core.Maybe Core.Double
    -- ^ The amount of RAM in GB (for example, @2.0@ ) for the database bundle.
  , transferPerMonthInGb :: Core.Maybe Core.Int
    -- ^ The data transfer rate per month in GB for the database bundle.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RelationalDatabaseBundle' value with any optional fields omitted.
mkRelationalDatabaseBundle
    :: RelationalDatabaseBundle
mkRelationalDatabaseBundle
  = RelationalDatabaseBundle'{bundleId = Core.Nothing,
                              cpuCount = Core.Nothing, diskSizeInGb = Core.Nothing,
                              isActive = Core.Nothing, isEncrypted = Core.Nothing,
                              name = Core.Nothing, price = Core.Nothing,
                              ramSizeInGb = Core.Nothing, transferPerMonthInGb = Core.Nothing}

-- | The ID for the database bundle.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbBundleId :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Text)
rdbBundleId = Lens.field @"bundleId"
{-# INLINEABLE rdbBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | The number of virtual CPUs (vCPUs) for the database bundle.
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbCpuCount :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Int)
rdbCpuCount = Lens.field @"cpuCount"
{-# INLINEABLE rdbCpuCount #-}
{-# DEPRECATED cpuCount "Use generic-lens or generic-optics with 'cpuCount' instead"  #-}

-- | The size of the disk for the database bundle.
--
-- /Note:/ Consider using 'diskSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbDiskSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Int)
rdbDiskSizeInGb = Lens.field @"diskSizeInGb"
{-# INLINEABLE rdbDiskSizeInGb #-}
{-# DEPRECATED diskSizeInGb "Use generic-lens or generic-optics with 'diskSizeInGb' instead"  #-}

-- | A Boolean value indicating whether the database bundle is active.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbIsActive :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Bool)
rdbIsActive = Lens.field @"isActive"
{-# INLINEABLE rdbIsActive #-}
{-# DEPRECATED isActive "Use generic-lens or generic-optics with 'isActive' instead"  #-}

-- | A Boolean value indicating whether the database bundle is encrypted.
--
-- /Note:/ Consider using 'isEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbIsEncrypted :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Bool)
rdbIsEncrypted = Lens.field @"isEncrypted"
{-# INLINEABLE rdbIsEncrypted #-}
{-# DEPRECATED isEncrypted "Use generic-lens or generic-optics with 'isEncrypted' instead"  #-}

-- | The name for the database bundle.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbName :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Text)
rdbName = Lens.field @"name"
{-# INLINEABLE rdbName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The cost of the database bundle in US currency.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbPrice :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Double)
rdbPrice = Lens.field @"price"
{-# INLINEABLE rdbPrice #-}
{-# DEPRECATED price "Use generic-lens or generic-optics with 'price' instead"  #-}

-- | The amount of RAM in GB (for example, @2.0@ ) for the database bundle.
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbRamSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Double)
rdbRamSizeInGb = Lens.field @"ramSizeInGb"
{-# INLINEABLE rdbRamSizeInGb #-}
{-# DEPRECATED ramSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead"  #-}

-- | The data transfer rate per month in GB for the database bundle.
--
-- /Note:/ Consider using 'transferPerMonthInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbTransferPerMonthInGb :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Int)
rdbTransferPerMonthInGb = Lens.field @"transferPerMonthInGb"
{-# INLINEABLE rdbTransferPerMonthInGb #-}
{-# DEPRECATED transferPerMonthInGb "Use generic-lens or generic-optics with 'transferPerMonthInGb' instead"  #-}

instance Core.FromJSON RelationalDatabaseBundle where
        parseJSON
          = Core.withObject "RelationalDatabaseBundle" Core.$
              \ x ->
                RelationalDatabaseBundle' Core.<$>
                  (x Core..:? "bundleId") Core.<*> x Core..:? "cpuCount" Core.<*>
                    x Core..:? "diskSizeInGb"
                    Core.<*> x Core..:? "isActive"
                    Core.<*> x Core..:? "isEncrypted"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "price"
                    Core.<*> x Core..:? "ramSizeInGb"
                    Core.<*> x Core..:? "transferPerMonthInGb"
