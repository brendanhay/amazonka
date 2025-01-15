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
-- Module      : Amazonka.Lightsail.Types.Bundle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Bundle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.InstancePlatform
import qualified Amazonka.Prelude as Prelude

-- | Describes a bundle, which is a set of specs describing your virtual
-- private server (or /instance/).
--
-- /See:/ 'newBundle' smart constructor.
data Bundle = Bundle'
  { -- | The bundle ID (e.g., @micro_1_0@).
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The number of vCPUs included in the bundle (e.g., @2@).
    cpuCount :: Prelude.Maybe Prelude.Int,
    -- | The size of the SSD (e.g., @30@).
    diskSizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The Amazon EC2 instance type (e.g., @t2.micro@).
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the bundle is active.
    isActive :: Prelude.Maybe Prelude.Bool,
    -- | A friendly name for the bundle (e.g., @Micro@).
    name :: Prelude.Maybe Prelude.Text,
    -- | A numeric value that represents the power of the bundle (e.g., @500@).
    -- You can use the bundle\'s power value in conjunction with a blueprint\'s
    -- minimum power value to determine whether the blueprint will run on the
    -- bundle. For example, you need a bundle with a power value of 500 or more
    -- to create an instance that uses a blueprint with a minimum power value
    -- of 500.
    power :: Prelude.Maybe Prelude.Int,
    -- | The price in US dollars (e.g., @5.0@) of the bundle.
    price :: Prelude.Maybe Prelude.Double,
    -- | The amount of RAM in GB (e.g., @2.0@).
    ramSizeInGb :: Prelude.Maybe Prelude.Double,
    -- | The operating system platform (Linux\/Unix-based or Windows
    -- Server-based) that the bundle supports. You can only launch a @WINDOWS@
    -- bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@
    -- blueprints require a @LINUX_UNIX@ bundle.
    supportedPlatforms :: Prelude.Maybe [InstancePlatform],
    -- | The data transfer rate per month in GB (e.g., @2000@).
    transferPerMonthInGb :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Bundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleId', 'bundle_bundleId' - The bundle ID (e.g., @micro_1_0@).
--
-- 'cpuCount', 'bundle_cpuCount' - The number of vCPUs included in the bundle (e.g., @2@).
--
-- 'diskSizeInGb', 'bundle_diskSizeInGb' - The size of the SSD (e.g., @30@).
--
-- 'instanceType', 'bundle_instanceType' - The Amazon EC2 instance type (e.g., @t2.micro@).
--
-- 'isActive', 'bundle_isActive' - A Boolean value indicating whether the bundle is active.
--
-- 'name', 'bundle_name' - A friendly name for the bundle (e.g., @Micro@).
--
-- 'power', 'bundle_power' - A numeric value that represents the power of the bundle (e.g., @500@).
-- You can use the bundle\'s power value in conjunction with a blueprint\'s
-- minimum power value to determine whether the blueprint will run on the
-- bundle. For example, you need a bundle with a power value of 500 or more
-- to create an instance that uses a blueprint with a minimum power value
-- of 500.
--
-- 'price', 'bundle_price' - The price in US dollars (e.g., @5.0@) of the bundle.
--
-- 'ramSizeInGb', 'bundle_ramSizeInGb' - The amount of RAM in GB (e.g., @2.0@).
--
-- 'supportedPlatforms', 'bundle_supportedPlatforms' - The operating system platform (Linux\/Unix-based or Windows
-- Server-based) that the bundle supports. You can only launch a @WINDOWS@
-- bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@
-- blueprints require a @LINUX_UNIX@ bundle.
--
-- 'transferPerMonthInGb', 'bundle_transferPerMonthInGb' - The data transfer rate per month in GB (e.g., @2000@).
newBundle ::
  Bundle
newBundle =
  Bundle'
    { bundleId = Prelude.Nothing,
      cpuCount = Prelude.Nothing,
      diskSizeInGb = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      isActive = Prelude.Nothing,
      name = Prelude.Nothing,
      power = Prelude.Nothing,
      price = Prelude.Nothing,
      ramSizeInGb = Prelude.Nothing,
      supportedPlatforms = Prelude.Nothing,
      transferPerMonthInGb = Prelude.Nothing
    }

-- | The bundle ID (e.g., @micro_1_0@).
bundle_bundleId :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Text)
bundle_bundleId = Lens.lens (\Bundle' {bundleId} -> bundleId) (\s@Bundle' {} a -> s {bundleId = a} :: Bundle)

-- | The number of vCPUs included in the bundle (e.g., @2@).
bundle_cpuCount :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Int)
bundle_cpuCount = Lens.lens (\Bundle' {cpuCount} -> cpuCount) (\s@Bundle' {} a -> s {cpuCount = a} :: Bundle)

-- | The size of the SSD (e.g., @30@).
bundle_diskSizeInGb :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Int)
bundle_diskSizeInGb = Lens.lens (\Bundle' {diskSizeInGb} -> diskSizeInGb) (\s@Bundle' {} a -> s {diskSizeInGb = a} :: Bundle)

-- | The Amazon EC2 instance type (e.g., @t2.micro@).
bundle_instanceType :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Text)
bundle_instanceType = Lens.lens (\Bundle' {instanceType} -> instanceType) (\s@Bundle' {} a -> s {instanceType = a} :: Bundle)

-- | A Boolean value indicating whether the bundle is active.
bundle_isActive :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Bool)
bundle_isActive = Lens.lens (\Bundle' {isActive} -> isActive) (\s@Bundle' {} a -> s {isActive = a} :: Bundle)

-- | A friendly name for the bundle (e.g., @Micro@).
bundle_name :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Text)
bundle_name = Lens.lens (\Bundle' {name} -> name) (\s@Bundle' {} a -> s {name = a} :: Bundle)

-- | A numeric value that represents the power of the bundle (e.g., @500@).
-- You can use the bundle\'s power value in conjunction with a blueprint\'s
-- minimum power value to determine whether the blueprint will run on the
-- bundle. For example, you need a bundle with a power value of 500 or more
-- to create an instance that uses a blueprint with a minimum power value
-- of 500.
bundle_power :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Int)
bundle_power = Lens.lens (\Bundle' {power} -> power) (\s@Bundle' {} a -> s {power = a} :: Bundle)

-- | The price in US dollars (e.g., @5.0@) of the bundle.
bundle_price :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Double)
bundle_price = Lens.lens (\Bundle' {price} -> price) (\s@Bundle' {} a -> s {price = a} :: Bundle)

-- | The amount of RAM in GB (e.g., @2.0@).
bundle_ramSizeInGb :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Double)
bundle_ramSizeInGb = Lens.lens (\Bundle' {ramSizeInGb} -> ramSizeInGb) (\s@Bundle' {} a -> s {ramSizeInGb = a} :: Bundle)

-- | The operating system platform (Linux\/Unix-based or Windows
-- Server-based) that the bundle supports. You can only launch a @WINDOWS@
-- bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@
-- blueprints require a @LINUX_UNIX@ bundle.
bundle_supportedPlatforms :: Lens.Lens' Bundle (Prelude.Maybe [InstancePlatform])
bundle_supportedPlatforms = Lens.lens (\Bundle' {supportedPlatforms} -> supportedPlatforms) (\s@Bundle' {} a -> s {supportedPlatforms = a} :: Bundle) Prelude.. Lens.mapping Lens.coerced

-- | The data transfer rate per month in GB (e.g., @2000@).
bundle_transferPerMonthInGb :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Int)
bundle_transferPerMonthInGb = Lens.lens (\Bundle' {transferPerMonthInGb} -> transferPerMonthInGb) (\s@Bundle' {} a -> s {transferPerMonthInGb = a} :: Bundle)

instance Data.FromJSON Bundle where
  parseJSON =
    Data.withObject
      "Bundle"
      ( \x ->
          Bundle'
            Prelude.<$> (x Data..:? "bundleId")
            Prelude.<*> (x Data..:? "cpuCount")
            Prelude.<*> (x Data..:? "diskSizeInGb")
            Prelude.<*> (x Data..:? "instanceType")
            Prelude.<*> (x Data..:? "isActive")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "power")
            Prelude.<*> (x Data..:? "price")
            Prelude.<*> (x Data..:? "ramSizeInGb")
            Prelude.<*> ( x
                            Data..:? "supportedPlatforms"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "transferPerMonthInGb")
      )

instance Prelude.Hashable Bundle where
  hashWithSalt _salt Bundle' {..} =
    _salt
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` cpuCount
      `Prelude.hashWithSalt` diskSizeInGb
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` isActive
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` power
      `Prelude.hashWithSalt` price
      `Prelude.hashWithSalt` ramSizeInGb
      `Prelude.hashWithSalt` supportedPlatforms
      `Prelude.hashWithSalt` transferPerMonthInGb

instance Prelude.NFData Bundle where
  rnf Bundle' {..} =
    Prelude.rnf bundleId `Prelude.seq`
      Prelude.rnf cpuCount `Prelude.seq`
        Prelude.rnf diskSizeInGb `Prelude.seq`
          Prelude.rnf instanceType `Prelude.seq`
            Prelude.rnf isActive `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf power `Prelude.seq`
                  Prelude.rnf price `Prelude.seq`
                    Prelude.rnf ramSizeInGb `Prelude.seq`
                      Prelude.rnf supportedPlatforms `Prelude.seq`
                        Prelude.rnf transferPerMonthInGb
