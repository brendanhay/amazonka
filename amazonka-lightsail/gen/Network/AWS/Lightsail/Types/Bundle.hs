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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.InstancePlatform

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
    power :: Core.Maybe Core.Int,
    -- | The Amazon EC2 instance type (e.g., @t2.micro@).
    instanceType :: Core.Maybe Core.Text,
    -- | The amount of RAM in GB (e.g., @2.0@).
    ramSizeInGb :: Core.Maybe Core.Double,
    -- | The bundle ID (e.g., @micro_1_0@).
    bundleId :: Core.Maybe Core.Text,
    -- | A Boolean value indicating whether the bundle is active.
    isActive :: Core.Maybe Core.Bool,
    -- | A friendly name for the bundle (e.g., @Micro@).
    name :: Core.Maybe Core.Text,
    -- | The data transfer rate per month in GB (e.g., @2000@).
    transferPerMonthInGb :: Core.Maybe Core.Int,
    -- | The number of vCPUs included in the bundle (e.g., @2@).
    cpuCount :: Core.Maybe Core.Int,
    -- | The price in US dollars (e.g., @5.0@) of the bundle.
    price :: Core.Maybe Core.Double,
    -- | The operating system platform (Linux\/Unix-based or Windows
    -- Server-based) that the bundle supports. You can only launch a @WINDOWS@
    -- bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@
    -- blueprints require a @LINUX_UNIX@ bundle.
    supportedPlatforms :: Core.Maybe [InstancePlatform],
    -- | The size of the SSD (e.g., @30@).
    diskSizeInGb :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { power = Core.Nothing,
      instanceType = Core.Nothing,
      ramSizeInGb = Core.Nothing,
      bundleId = Core.Nothing,
      isActive = Core.Nothing,
      name = Core.Nothing,
      transferPerMonthInGb = Core.Nothing,
      cpuCount = Core.Nothing,
      price = Core.Nothing,
      supportedPlatforms = Core.Nothing,
      diskSizeInGb = Core.Nothing
    }

-- | A numeric value that represents the power of the bundle (e.g., @500@).
-- You can use the bundle\'s power value in conjunction with a blueprint\'s
-- minimum power value to determine whether the blueprint will run on the
-- bundle. For example, you need a bundle with a power value of 500 or more
-- to create an instance that uses a blueprint with a minimum power value
-- of 500.
bundle_power :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bundle_power = Lens.lens (\Bundle' {power} -> power) (\s@Bundle' {} a -> s {power = a} :: Bundle)

-- | The Amazon EC2 instance type (e.g., @t2.micro@).
bundle_instanceType :: Lens.Lens' Bundle (Core.Maybe Core.Text)
bundle_instanceType = Lens.lens (\Bundle' {instanceType} -> instanceType) (\s@Bundle' {} a -> s {instanceType = a} :: Bundle)

-- | The amount of RAM in GB (e.g., @2.0@).
bundle_ramSizeInGb :: Lens.Lens' Bundle (Core.Maybe Core.Double)
bundle_ramSizeInGb = Lens.lens (\Bundle' {ramSizeInGb} -> ramSizeInGb) (\s@Bundle' {} a -> s {ramSizeInGb = a} :: Bundle)

-- | The bundle ID (e.g., @micro_1_0@).
bundle_bundleId :: Lens.Lens' Bundle (Core.Maybe Core.Text)
bundle_bundleId = Lens.lens (\Bundle' {bundleId} -> bundleId) (\s@Bundle' {} a -> s {bundleId = a} :: Bundle)

-- | A Boolean value indicating whether the bundle is active.
bundle_isActive :: Lens.Lens' Bundle (Core.Maybe Core.Bool)
bundle_isActive = Lens.lens (\Bundle' {isActive} -> isActive) (\s@Bundle' {} a -> s {isActive = a} :: Bundle)

-- | A friendly name for the bundle (e.g., @Micro@).
bundle_name :: Lens.Lens' Bundle (Core.Maybe Core.Text)
bundle_name = Lens.lens (\Bundle' {name} -> name) (\s@Bundle' {} a -> s {name = a} :: Bundle)

-- | The data transfer rate per month in GB (e.g., @2000@).
bundle_transferPerMonthInGb :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bundle_transferPerMonthInGb = Lens.lens (\Bundle' {transferPerMonthInGb} -> transferPerMonthInGb) (\s@Bundle' {} a -> s {transferPerMonthInGb = a} :: Bundle)

-- | The number of vCPUs included in the bundle (e.g., @2@).
bundle_cpuCount :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bundle_cpuCount = Lens.lens (\Bundle' {cpuCount} -> cpuCount) (\s@Bundle' {} a -> s {cpuCount = a} :: Bundle)

-- | The price in US dollars (e.g., @5.0@) of the bundle.
bundle_price :: Lens.Lens' Bundle (Core.Maybe Core.Double)
bundle_price = Lens.lens (\Bundle' {price} -> price) (\s@Bundle' {} a -> s {price = a} :: Bundle)

-- | The operating system platform (Linux\/Unix-based or Windows
-- Server-based) that the bundle supports. You can only launch a @WINDOWS@
-- bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@
-- blueprints require a @LINUX_UNIX@ bundle.
bundle_supportedPlatforms :: Lens.Lens' Bundle (Core.Maybe [InstancePlatform])
bundle_supportedPlatforms = Lens.lens (\Bundle' {supportedPlatforms} -> supportedPlatforms) (\s@Bundle' {} a -> s {supportedPlatforms = a} :: Bundle) Core.. Lens.mapping Lens._Coerce

-- | The size of the SSD (e.g., @30@).
bundle_diskSizeInGb :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bundle_diskSizeInGb = Lens.lens (\Bundle' {diskSizeInGb} -> diskSizeInGb) (\s@Bundle' {} a -> s {diskSizeInGb = a} :: Bundle)

instance Core.FromJSON Bundle where
  parseJSON =
    Core.withObject
      "Bundle"
      ( \x ->
          Bundle'
            Core.<$> (x Core..:? "power")
            Core.<*> (x Core..:? "instanceType")
            Core.<*> (x Core..:? "ramSizeInGb")
            Core.<*> (x Core..:? "bundleId")
            Core.<*> (x Core..:? "isActive")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "transferPerMonthInGb")
            Core.<*> (x Core..:? "cpuCount")
            Core.<*> (x Core..:? "price")
            Core.<*> ( x Core..:? "supportedPlatforms"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "diskSizeInGb")
      )

instance Core.Hashable Bundle

instance Core.NFData Bundle
