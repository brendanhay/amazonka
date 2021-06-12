{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseBundle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseBundle where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a database bundle. A bundle describes the performance
-- specifications of the database.
--
-- /See:/ 'newRelationalDatabaseBundle' smart constructor.
data RelationalDatabaseBundle = RelationalDatabaseBundle'
  { -- | The amount of RAM in GB (for example, @2.0@) for the database bundle.
    ramSizeInGb :: Core.Maybe Core.Double,
    -- | The ID for the database bundle.
    bundleId :: Core.Maybe Core.Text,
    -- | A Boolean value indicating whether the database bundle is active.
    isActive :: Core.Maybe Core.Bool,
    -- | The name for the database bundle.
    name :: Core.Maybe Core.Text,
    -- | A Boolean value indicating whether the database bundle is encrypted.
    isEncrypted :: Core.Maybe Core.Bool,
    -- | The data transfer rate per month in GB for the database bundle.
    transferPerMonthInGb :: Core.Maybe Core.Int,
    -- | The number of virtual CPUs (vCPUs) for the database bundle.
    cpuCount :: Core.Maybe Core.Int,
    -- | The cost of the database bundle in US currency.
    price :: Core.Maybe Core.Double,
    -- | The size of the disk for the database bundle.
    diskSizeInGb :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RelationalDatabaseBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ramSizeInGb', 'relationalDatabaseBundle_ramSizeInGb' - The amount of RAM in GB (for example, @2.0@) for the database bundle.
--
-- 'bundleId', 'relationalDatabaseBundle_bundleId' - The ID for the database bundle.
--
-- 'isActive', 'relationalDatabaseBundle_isActive' - A Boolean value indicating whether the database bundle is active.
--
-- 'name', 'relationalDatabaseBundle_name' - The name for the database bundle.
--
-- 'isEncrypted', 'relationalDatabaseBundle_isEncrypted' - A Boolean value indicating whether the database bundle is encrypted.
--
-- 'transferPerMonthInGb', 'relationalDatabaseBundle_transferPerMonthInGb' - The data transfer rate per month in GB for the database bundle.
--
-- 'cpuCount', 'relationalDatabaseBundle_cpuCount' - The number of virtual CPUs (vCPUs) for the database bundle.
--
-- 'price', 'relationalDatabaseBundle_price' - The cost of the database bundle in US currency.
--
-- 'diskSizeInGb', 'relationalDatabaseBundle_diskSizeInGb' - The size of the disk for the database bundle.
newRelationalDatabaseBundle ::
  RelationalDatabaseBundle
newRelationalDatabaseBundle =
  RelationalDatabaseBundle'
    { ramSizeInGb =
        Core.Nothing,
      bundleId = Core.Nothing,
      isActive = Core.Nothing,
      name = Core.Nothing,
      isEncrypted = Core.Nothing,
      transferPerMonthInGb = Core.Nothing,
      cpuCount = Core.Nothing,
      price = Core.Nothing,
      diskSizeInGb = Core.Nothing
    }

-- | The amount of RAM in GB (for example, @2.0@) for the database bundle.
relationalDatabaseBundle_ramSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Double)
relationalDatabaseBundle_ramSizeInGb = Lens.lens (\RelationalDatabaseBundle' {ramSizeInGb} -> ramSizeInGb) (\s@RelationalDatabaseBundle' {} a -> s {ramSizeInGb = a} :: RelationalDatabaseBundle)

-- | The ID for the database bundle.
relationalDatabaseBundle_bundleId :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Text)
relationalDatabaseBundle_bundleId = Lens.lens (\RelationalDatabaseBundle' {bundleId} -> bundleId) (\s@RelationalDatabaseBundle' {} a -> s {bundleId = a} :: RelationalDatabaseBundle)

-- | A Boolean value indicating whether the database bundle is active.
relationalDatabaseBundle_isActive :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Bool)
relationalDatabaseBundle_isActive = Lens.lens (\RelationalDatabaseBundle' {isActive} -> isActive) (\s@RelationalDatabaseBundle' {} a -> s {isActive = a} :: RelationalDatabaseBundle)

-- | The name for the database bundle.
relationalDatabaseBundle_name :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Text)
relationalDatabaseBundle_name = Lens.lens (\RelationalDatabaseBundle' {name} -> name) (\s@RelationalDatabaseBundle' {} a -> s {name = a} :: RelationalDatabaseBundle)

-- | A Boolean value indicating whether the database bundle is encrypted.
relationalDatabaseBundle_isEncrypted :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Bool)
relationalDatabaseBundle_isEncrypted = Lens.lens (\RelationalDatabaseBundle' {isEncrypted} -> isEncrypted) (\s@RelationalDatabaseBundle' {} a -> s {isEncrypted = a} :: RelationalDatabaseBundle)

-- | The data transfer rate per month in GB for the database bundle.
relationalDatabaseBundle_transferPerMonthInGb :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Int)
relationalDatabaseBundle_transferPerMonthInGb = Lens.lens (\RelationalDatabaseBundle' {transferPerMonthInGb} -> transferPerMonthInGb) (\s@RelationalDatabaseBundle' {} a -> s {transferPerMonthInGb = a} :: RelationalDatabaseBundle)

-- | The number of virtual CPUs (vCPUs) for the database bundle.
relationalDatabaseBundle_cpuCount :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Int)
relationalDatabaseBundle_cpuCount = Lens.lens (\RelationalDatabaseBundle' {cpuCount} -> cpuCount) (\s@RelationalDatabaseBundle' {} a -> s {cpuCount = a} :: RelationalDatabaseBundle)

-- | The cost of the database bundle in US currency.
relationalDatabaseBundle_price :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Double)
relationalDatabaseBundle_price = Lens.lens (\RelationalDatabaseBundle' {price} -> price) (\s@RelationalDatabaseBundle' {} a -> s {price = a} :: RelationalDatabaseBundle)

-- | The size of the disk for the database bundle.
relationalDatabaseBundle_diskSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Core.Maybe Core.Int)
relationalDatabaseBundle_diskSizeInGb = Lens.lens (\RelationalDatabaseBundle' {diskSizeInGb} -> diskSizeInGb) (\s@RelationalDatabaseBundle' {} a -> s {diskSizeInGb = a} :: RelationalDatabaseBundle)

instance Core.FromJSON RelationalDatabaseBundle where
  parseJSON =
    Core.withObject
      "RelationalDatabaseBundle"
      ( \x ->
          RelationalDatabaseBundle'
            Core.<$> (x Core..:? "ramSizeInGb")
            Core.<*> (x Core..:? "bundleId")
            Core.<*> (x Core..:? "isActive")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "isEncrypted")
            Core.<*> (x Core..:? "transferPerMonthInGb")
            Core.<*> (x Core..:? "cpuCount")
            Core.<*> (x Core..:? "price")
            Core.<*> (x Core..:? "diskSizeInGb")
      )

instance Core.Hashable RelationalDatabaseBundle

instance Core.NFData RelationalDatabaseBundle
