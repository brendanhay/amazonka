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
    rdbIsEncrypted,
    rdbCpuCount,
    rdbTransferPerMonthInGb,
    rdbBundleId,
    rdbName,
    rdbDiskSizeInGb,
    rdbPrice,
    rdbIsActive,
    rdbRamSizeInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a database bundle. A bundle describes the performance specifications of the database.
--
-- /See:/ 'mkRelationalDatabaseBundle' smart constructor.
data RelationalDatabaseBundle = RelationalDatabaseBundle'
  { isEncrypted ::
      Lude.Maybe Lude.Bool,
    cpuCount :: Lude.Maybe Lude.Int,
    transferPerMonthInGb ::
      Lude.Maybe Lude.Int,
    bundleId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    diskSizeInGb :: Lude.Maybe Lude.Int,
    price :: Lude.Maybe Lude.Double,
    isActive :: Lude.Maybe Lude.Bool,
    ramSizeInGb :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RelationalDatabaseBundle' with the minimum fields required to make a request.
--
-- * 'bundleId' - The ID for the database bundle.
-- * 'cpuCount' - The number of virtual CPUs (vCPUs) for the database bundle.
-- * 'diskSizeInGb' - The size of the disk for the database bundle.
-- * 'isActive' - A Boolean value indicating whether the database bundle is active.
-- * 'isEncrypted' - A Boolean value indicating whether the database bundle is encrypted.
-- * 'name' - The name for the database bundle.
-- * 'price' - The cost of the database bundle in US currency.
-- * 'ramSizeInGb' - The amount of RAM in GB (for example, @2.0@ ) for the database bundle.
-- * 'transferPerMonthInGb' - The data transfer rate per month in GB for the database bundle.
mkRelationalDatabaseBundle ::
  RelationalDatabaseBundle
mkRelationalDatabaseBundle =
  RelationalDatabaseBundle'
    { isEncrypted = Lude.Nothing,
      cpuCount = Lude.Nothing,
      transferPerMonthInGb = Lude.Nothing,
      bundleId = Lude.Nothing,
      name = Lude.Nothing,
      diskSizeInGb = Lude.Nothing,
      price = Lude.Nothing,
      isActive = Lude.Nothing,
      ramSizeInGb = Lude.Nothing
    }

-- | A Boolean value indicating whether the database bundle is encrypted.
--
-- /Note:/ Consider using 'isEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbIsEncrypted :: Lens.Lens' RelationalDatabaseBundle (Lude.Maybe Lude.Bool)
rdbIsEncrypted = Lens.lens (isEncrypted :: RelationalDatabaseBundle -> Lude.Maybe Lude.Bool) (\s a -> s {isEncrypted = a} :: RelationalDatabaseBundle)
{-# DEPRECATED rdbIsEncrypted "Use generic-lens or generic-optics with 'isEncrypted' instead." #-}

-- | The number of virtual CPUs (vCPUs) for the database bundle.
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbCpuCount :: Lens.Lens' RelationalDatabaseBundle (Lude.Maybe Lude.Int)
rdbCpuCount = Lens.lens (cpuCount :: RelationalDatabaseBundle -> Lude.Maybe Lude.Int) (\s a -> s {cpuCount = a} :: RelationalDatabaseBundle)
{-# DEPRECATED rdbCpuCount "Use generic-lens or generic-optics with 'cpuCount' instead." #-}

-- | The data transfer rate per month in GB for the database bundle.
--
-- /Note:/ Consider using 'transferPerMonthInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbTransferPerMonthInGb :: Lens.Lens' RelationalDatabaseBundle (Lude.Maybe Lude.Int)
rdbTransferPerMonthInGb = Lens.lens (transferPerMonthInGb :: RelationalDatabaseBundle -> Lude.Maybe Lude.Int) (\s a -> s {transferPerMonthInGb = a} :: RelationalDatabaseBundle)
{-# DEPRECATED rdbTransferPerMonthInGb "Use generic-lens or generic-optics with 'transferPerMonthInGb' instead." #-}

-- | The ID for the database bundle.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbBundleId :: Lens.Lens' RelationalDatabaseBundle (Lude.Maybe Lude.Text)
rdbBundleId = Lens.lens (bundleId :: RelationalDatabaseBundle -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: RelationalDatabaseBundle)
{-# DEPRECATED rdbBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The name for the database bundle.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbName :: Lens.Lens' RelationalDatabaseBundle (Lude.Maybe Lude.Text)
rdbName = Lens.lens (name :: RelationalDatabaseBundle -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RelationalDatabaseBundle)
{-# DEPRECATED rdbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The size of the disk for the database bundle.
--
-- /Note:/ Consider using 'diskSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbDiskSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Lude.Maybe Lude.Int)
rdbDiskSizeInGb = Lens.lens (diskSizeInGb :: RelationalDatabaseBundle -> Lude.Maybe Lude.Int) (\s a -> s {diskSizeInGb = a} :: RelationalDatabaseBundle)
{-# DEPRECATED rdbDiskSizeInGb "Use generic-lens or generic-optics with 'diskSizeInGb' instead." #-}

-- | The cost of the database bundle in US currency.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbPrice :: Lens.Lens' RelationalDatabaseBundle (Lude.Maybe Lude.Double)
rdbPrice = Lens.lens (price :: RelationalDatabaseBundle -> Lude.Maybe Lude.Double) (\s a -> s {price = a} :: RelationalDatabaseBundle)
{-# DEPRECATED rdbPrice "Use generic-lens or generic-optics with 'price' instead." #-}

-- | A Boolean value indicating whether the database bundle is active.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbIsActive :: Lens.Lens' RelationalDatabaseBundle (Lude.Maybe Lude.Bool)
rdbIsActive = Lens.lens (isActive :: RelationalDatabaseBundle -> Lude.Maybe Lude.Bool) (\s a -> s {isActive = a} :: RelationalDatabaseBundle)
{-# DEPRECATED rdbIsActive "Use generic-lens or generic-optics with 'isActive' instead." #-}

-- | The amount of RAM in GB (for example, @2.0@ ) for the database bundle.
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbRamSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Lude.Maybe Lude.Double)
rdbRamSizeInGb = Lens.lens (ramSizeInGb :: RelationalDatabaseBundle -> Lude.Maybe Lude.Double) (\s a -> s {ramSizeInGb = a} :: RelationalDatabaseBundle)
{-# DEPRECATED rdbRamSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead." #-}

instance Lude.FromJSON RelationalDatabaseBundle where
  parseJSON =
    Lude.withObject
      "RelationalDatabaseBundle"
      ( \x ->
          RelationalDatabaseBundle'
            Lude.<$> (x Lude..:? "isEncrypted")
            Lude.<*> (x Lude..:? "cpuCount")
            Lude.<*> (x Lude..:? "transferPerMonthInGb")
            Lude.<*> (x Lude..:? "bundleId")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "diskSizeInGb")
            Lude.<*> (x Lude..:? "price")
            Lude.<*> (x Lude..:? "isActive")
            Lude.<*> (x Lude..:? "ramSizeInGb")
      )
