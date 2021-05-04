{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lightsail.Types.Bundle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Bundle where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.InstancePlatform
import qualified Network.AWS.Prelude as Prelude

-- | Describes a bundle, which is a set of specs describing your virtual
-- private server (or /instance/).
--
-- /See:/ 'newBundle' smart constructor.
data Bundle = Bundle'
  { -- | A numeric value that represents the power of the bundle (e.g., @500@).
    -- You can use the bundle\'s power value in conjunction with a blueprint\'s
    -- minimum power value to determine whether the blueprint will run on the
    -- bundle. For example, you need a bundle with a power value of 500 or more
    -- to create an instance that uses a blueprint with a minimum power value
    -- of 500.
    power :: Prelude.Maybe Prelude.Int,
    -- | The Amazon EC2 instance type (e.g., @t2.micro@).
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The amount of RAM in GB (e.g., @2.0@).
    ramSizeInGb :: Prelude.Maybe Prelude.Double,
    -- | The bundle ID (e.g., @micro_1_0@).
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the bundle is active.
    isActive :: Prelude.Maybe Prelude.Bool,
    -- | A friendly name for the bundle (e.g., @Micro@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The data transfer rate per month in GB (e.g., @2000@).
    transferPerMonthInGb :: Prelude.Maybe Prelude.Int,
    -- | The number of vCPUs included in the bundle (e.g., @2@).
    cpuCount :: Prelude.Maybe Prelude.Int,
    -- | The price in US dollars (e.g., @5.0@) of the bundle.
    price :: Prelude.Maybe Prelude.Double,
    -- | The operating system platform (Linux\/Unix-based or Windows
    -- Server-based) that the bundle supports. You can only launch a @WINDOWS@
    -- bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@
    -- blueprints require a @LINUX_UNIX@ bundle.
    supportedPlatforms :: Prelude.Maybe [InstancePlatform],
    -- | The size of the SSD (e.g., @30@).
    diskSizeInGb :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Bundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'power', 'bundle_power' - A numeric value that represents the power of the bundle (e.g., @500@).
-- You can use the bundle\'s power value in conjunction with a blueprint\'s
-- minimum power value to determine whether the blueprint will run on the
-- bundle. For example, you need a bundle with a power value of 500 or more
-- to create an instance that uses a blueprint with a minimum power value
-- of 500.
--
-- 'instanceType', 'bundle_instanceType' - The Amazon EC2 instance type (e.g., @t2.micro@).
--
-- 'ramSizeInGb', 'bundle_ramSizeInGb' - The amount of RAM in GB (e.g., @2.0@).
--
-- 'bundleId', 'bundle_bundleId' - The bundle ID (e.g., @micro_1_0@).
--
-- 'isActive', 'bundle_isActive' - A Boolean value indicating whether the bundle is active.
--
-- 'name', 'bundle_name' - A friendly name for the bundle (e.g., @Micro@).
--
-- 'transferPerMonthInGb', 'bundle_transferPerMonthInGb' - The data transfer rate per month in GB (e.g., @2000@).
--
-- 'cpuCount', 'bundle_cpuCount' - The number of vCPUs included in the bundle (e.g., @2@).
--
-- 'price', 'bundle_price' - The price in US dollars (e.g., @5.0@) of the bundle.
--
-- 'supportedPlatforms', 'bundle_supportedPlatforms' - The operating system platform (Linux\/Unix-based or Windows
-- Server-based) that the bundle supports. You can only launch a @WINDOWS@
-- bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@
-- blueprints require a @LINUX_UNIX@ bundle.
--
-- 'diskSizeInGb', 'bundle_diskSizeInGb' - The size of the SSD (e.g., @30@).
newBundle ::
  Bundle
newBundle =
  Bundle'
    { power = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      ramSizeInGb = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      isActive = Prelude.Nothing,
      name = Prelude.Nothing,
      transferPerMonthInGb = Prelude.Nothing,
      cpuCount = Prelude.Nothing,
      price = Prelude.Nothing,
      supportedPlatforms = Prelude.Nothing,
      diskSizeInGb = Prelude.Nothing
    }

-- | A numeric value that represents the power of the bundle (e.g., @500@).
-- You can use the bundle\'s power value in conjunction with a blueprint\'s
-- minimum power value to determine whether the blueprint will run on the
-- bundle. For example, you need a bundle with a power value of 500 or more
-- to create an instance that uses a blueprint with a minimum power value
-- of 500.
bundle_power :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Int)
bundle_power = Lens.lens (\Bundle' {power} -> power) (\s@Bundle' {} a -> s {power = a} :: Bundle)

-- | The Amazon EC2 instance type (e.g., @t2.micro@).
bundle_instanceType :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Text)
bundle_instanceType = Lens.lens (\Bundle' {instanceType} -> instanceType) (\s@Bundle' {} a -> s {instanceType = a} :: Bundle)

-- | The amount of RAM in GB (e.g., @2.0@).
bundle_ramSizeInGb :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Double)
bundle_ramSizeInGb = Lens.lens (\Bundle' {ramSizeInGb} -> ramSizeInGb) (\s@Bundle' {} a -> s {ramSizeInGb = a} :: Bundle)

-- | The bundle ID (e.g., @micro_1_0@).
bundle_bundleId :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Text)
bundle_bundleId = Lens.lens (\Bundle' {bundleId} -> bundleId) (\s@Bundle' {} a -> s {bundleId = a} :: Bundle)

-- | A Boolean value indicating whether the bundle is active.
bundle_isActive :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Bool)
bundle_isActive = Lens.lens (\Bundle' {isActive} -> isActive) (\s@Bundle' {} a -> s {isActive = a} :: Bundle)

-- | A friendly name for the bundle (e.g., @Micro@).
bundle_name :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Text)
bundle_name = Lens.lens (\Bundle' {name} -> name) (\s@Bundle' {} a -> s {name = a} :: Bundle)

-- | The data transfer rate per month in GB (e.g., @2000@).
bundle_transferPerMonthInGb :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Int)
bundle_transferPerMonthInGb = Lens.lens (\Bundle' {transferPerMonthInGb} -> transferPerMonthInGb) (\s@Bundle' {} a -> s {transferPerMonthInGb = a} :: Bundle)

-- | The number of vCPUs included in the bundle (e.g., @2@).
bundle_cpuCount :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Int)
bundle_cpuCount = Lens.lens (\Bundle' {cpuCount} -> cpuCount) (\s@Bundle' {} a -> s {cpuCount = a} :: Bundle)

-- | The price in US dollars (e.g., @5.0@) of the bundle.
bundle_price :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Double)
bundle_price = Lens.lens (\Bundle' {price} -> price) (\s@Bundle' {} a -> s {price = a} :: Bundle)

-- | The operating system platform (Linux\/Unix-based or Windows
-- Server-based) that the bundle supports. You can only launch a @WINDOWS@
-- bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@
-- blueprints require a @LINUX_UNIX@ bundle.
bundle_supportedPlatforms :: Lens.Lens' Bundle (Prelude.Maybe [InstancePlatform])
bundle_supportedPlatforms = Lens.lens (\Bundle' {supportedPlatforms} -> supportedPlatforms) (\s@Bundle' {} a -> s {supportedPlatforms = a} :: Bundle) Prelude.. Lens.mapping Prelude._Coerce

-- | The size of the SSD (e.g., @30@).
bundle_diskSizeInGb :: Lens.Lens' Bundle (Prelude.Maybe Prelude.Int)
bundle_diskSizeInGb = Lens.lens (\Bundle' {diskSizeInGb} -> diskSizeInGb) (\s@Bundle' {} a -> s {diskSizeInGb = a} :: Bundle)

instance Prelude.FromJSON Bundle where
  parseJSON =
    Prelude.withObject
      "Bundle"
      ( \x ->
          Bundle'
            Prelude.<$> (x Prelude..:? "power")
            Prelude.<*> (x Prelude..:? "instanceType")
            Prelude.<*> (x Prelude..:? "ramSizeInGb")
            Prelude.<*> (x Prelude..:? "bundleId")
            Prelude.<*> (x Prelude..:? "isActive")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "transferPerMonthInGb")
            Prelude.<*> (x Prelude..:? "cpuCount")
            Prelude.<*> (x Prelude..:? "price")
            Prelude.<*> ( x Prelude..:? "supportedPlatforms"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "diskSizeInGb")
      )

instance Prelude.Hashable Bundle

instance Prelude.NFData Bundle
