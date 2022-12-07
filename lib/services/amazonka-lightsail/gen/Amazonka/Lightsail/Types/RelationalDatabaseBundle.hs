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
-- Module      : Amazonka.Lightsail.Types.RelationalDatabaseBundle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RelationalDatabaseBundle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a database bundle. A bundle describes the performance
-- specifications of the database.
--
-- /See:/ 'newRelationalDatabaseBundle' smart constructor.
data RelationalDatabaseBundle = RelationalDatabaseBundle'
  { -- | The number of virtual CPUs (vCPUs) for the database bundle.
    cpuCount :: Prelude.Maybe Prelude.Int,
    -- | A Boolean value indicating whether the database bundle is active.
    isActive :: Prelude.Maybe Prelude.Bool,
    -- | The name for the database bundle.
    name :: Prelude.Maybe Prelude.Text,
    -- | The data transfer rate per month in GB for the database bundle.
    transferPerMonthInGb :: Prelude.Maybe Prelude.Int,
    -- | The size of the disk for the database bundle.
    diskSizeInGb :: Prelude.Maybe Prelude.Int,
    -- | A Boolean value indicating whether the database bundle is encrypted.
    isEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The cost of the database bundle in US currency.
    price :: Prelude.Maybe Prelude.Double,
    -- | The ID for the database bundle.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The amount of RAM in GB (for example, @2.0@) for the database bundle.
    ramSizeInGb :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabaseBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuCount', 'relationalDatabaseBundle_cpuCount' - The number of virtual CPUs (vCPUs) for the database bundle.
--
-- 'isActive', 'relationalDatabaseBundle_isActive' - A Boolean value indicating whether the database bundle is active.
--
-- 'name', 'relationalDatabaseBundle_name' - The name for the database bundle.
--
-- 'transferPerMonthInGb', 'relationalDatabaseBundle_transferPerMonthInGb' - The data transfer rate per month in GB for the database bundle.
--
-- 'diskSizeInGb', 'relationalDatabaseBundle_diskSizeInGb' - The size of the disk for the database bundle.
--
-- 'isEncrypted', 'relationalDatabaseBundle_isEncrypted' - A Boolean value indicating whether the database bundle is encrypted.
--
-- 'price', 'relationalDatabaseBundle_price' - The cost of the database bundle in US currency.
--
-- 'bundleId', 'relationalDatabaseBundle_bundleId' - The ID for the database bundle.
--
-- 'ramSizeInGb', 'relationalDatabaseBundle_ramSizeInGb' - The amount of RAM in GB (for example, @2.0@) for the database bundle.
newRelationalDatabaseBundle ::
  RelationalDatabaseBundle
newRelationalDatabaseBundle =
  RelationalDatabaseBundle'
    { cpuCount =
        Prelude.Nothing,
      isActive = Prelude.Nothing,
      name = Prelude.Nothing,
      transferPerMonthInGb = Prelude.Nothing,
      diskSizeInGb = Prelude.Nothing,
      isEncrypted = Prelude.Nothing,
      price = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      ramSizeInGb = Prelude.Nothing
    }

-- | The number of virtual CPUs (vCPUs) for the database bundle.
relationalDatabaseBundle_cpuCount :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Int)
relationalDatabaseBundle_cpuCount = Lens.lens (\RelationalDatabaseBundle' {cpuCount} -> cpuCount) (\s@RelationalDatabaseBundle' {} a -> s {cpuCount = a} :: RelationalDatabaseBundle)

-- | A Boolean value indicating whether the database bundle is active.
relationalDatabaseBundle_isActive :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Bool)
relationalDatabaseBundle_isActive = Lens.lens (\RelationalDatabaseBundle' {isActive} -> isActive) (\s@RelationalDatabaseBundle' {} a -> s {isActive = a} :: RelationalDatabaseBundle)

-- | The name for the database bundle.
relationalDatabaseBundle_name :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Text)
relationalDatabaseBundle_name = Lens.lens (\RelationalDatabaseBundle' {name} -> name) (\s@RelationalDatabaseBundle' {} a -> s {name = a} :: RelationalDatabaseBundle)

-- | The data transfer rate per month in GB for the database bundle.
relationalDatabaseBundle_transferPerMonthInGb :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Int)
relationalDatabaseBundle_transferPerMonthInGb = Lens.lens (\RelationalDatabaseBundle' {transferPerMonthInGb} -> transferPerMonthInGb) (\s@RelationalDatabaseBundle' {} a -> s {transferPerMonthInGb = a} :: RelationalDatabaseBundle)

-- | The size of the disk for the database bundle.
relationalDatabaseBundle_diskSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Int)
relationalDatabaseBundle_diskSizeInGb = Lens.lens (\RelationalDatabaseBundle' {diskSizeInGb} -> diskSizeInGb) (\s@RelationalDatabaseBundle' {} a -> s {diskSizeInGb = a} :: RelationalDatabaseBundle)

-- | A Boolean value indicating whether the database bundle is encrypted.
relationalDatabaseBundle_isEncrypted :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Bool)
relationalDatabaseBundle_isEncrypted = Lens.lens (\RelationalDatabaseBundle' {isEncrypted} -> isEncrypted) (\s@RelationalDatabaseBundle' {} a -> s {isEncrypted = a} :: RelationalDatabaseBundle)

-- | The cost of the database bundle in US currency.
relationalDatabaseBundle_price :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Double)
relationalDatabaseBundle_price = Lens.lens (\RelationalDatabaseBundle' {price} -> price) (\s@RelationalDatabaseBundle' {} a -> s {price = a} :: RelationalDatabaseBundle)

-- | The ID for the database bundle.
relationalDatabaseBundle_bundleId :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Text)
relationalDatabaseBundle_bundleId = Lens.lens (\RelationalDatabaseBundle' {bundleId} -> bundleId) (\s@RelationalDatabaseBundle' {} a -> s {bundleId = a} :: RelationalDatabaseBundle)

-- | The amount of RAM in GB (for example, @2.0@) for the database bundle.
relationalDatabaseBundle_ramSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Double)
relationalDatabaseBundle_ramSizeInGb = Lens.lens (\RelationalDatabaseBundle' {ramSizeInGb} -> ramSizeInGb) (\s@RelationalDatabaseBundle' {} a -> s {ramSizeInGb = a} :: RelationalDatabaseBundle)

instance Data.FromJSON RelationalDatabaseBundle where
  parseJSON =
    Data.withObject
      "RelationalDatabaseBundle"
      ( \x ->
          RelationalDatabaseBundle'
            Prelude.<$> (x Data..:? "cpuCount")
            Prelude.<*> (x Data..:? "isActive")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "transferPerMonthInGb")
            Prelude.<*> (x Data..:? "diskSizeInGb")
            Prelude.<*> (x Data..:? "isEncrypted")
            Prelude.<*> (x Data..:? "price")
            Prelude.<*> (x Data..:? "bundleId")
            Prelude.<*> (x Data..:? "ramSizeInGb")
      )

instance Prelude.Hashable RelationalDatabaseBundle where
  hashWithSalt _salt RelationalDatabaseBundle' {..} =
    _salt `Prelude.hashWithSalt` cpuCount
      `Prelude.hashWithSalt` isActive
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` transferPerMonthInGb
      `Prelude.hashWithSalt` diskSizeInGb
      `Prelude.hashWithSalt` isEncrypted
      `Prelude.hashWithSalt` price
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` ramSizeInGb

instance Prelude.NFData RelationalDatabaseBundle where
  rnf RelationalDatabaseBundle' {..} =
    Prelude.rnf cpuCount
      `Prelude.seq` Prelude.rnf isActive
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf transferPerMonthInGb
      `Prelude.seq` Prelude.rnf diskSizeInGb
      `Prelude.seq` Prelude.rnf isEncrypted
      `Prelude.seq` Prelude.rnf price
      `Prelude.seq` Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf ramSizeInGb
