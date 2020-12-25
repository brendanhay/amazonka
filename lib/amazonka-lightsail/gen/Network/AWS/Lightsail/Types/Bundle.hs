{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Bundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Bundle
  ( Bundle (..),

    -- * Smart constructor
    mkBundle,

    -- * Lenses
    bfBundleId,
    bfCpuCount,
    bfDiskSizeInGb,
    bfInstanceType,
    bfIsActive,
    bfName,
    bfPower,
    bfPrice,
    bfRamSizeInGb,
    bfSupportedPlatforms,
    bfTransferPerMonthInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.BundleId as Types
import qualified Network.AWS.Lightsail.Types.InstancePlatform as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a bundle, which is a set of specs describing your virtual private server (or /instance/ ).
--
-- /See:/ 'mkBundle' smart constructor.
data Bundle = Bundle'
  { -- | The bundle ID (e.g., @micro_1_0@ ).
    bundleId :: Core.Maybe Types.BundleId,
    -- | The number of vCPUs included in the bundle (e.g., @2@ ).
    cpuCount :: Core.Maybe Core.Int,
    -- | The size of the SSD (e.g., @30@ ).
    diskSizeInGb :: Core.Maybe Core.Int,
    -- | The Amazon EC2 instance type (e.g., @t2.micro@ ).
    instanceType :: Core.Maybe Types.String,
    -- | A Boolean value indicating whether the bundle is active.
    isActive :: Core.Maybe Core.Bool,
    -- | A friendly name for the bundle (e.g., @Micro@ ).
    name :: Core.Maybe Types.String,
    -- | A numeric value that represents the power of the bundle (e.g., @500@ ). You can use the bundle's power value in conjunction with a blueprint's minimum power value to determine whether the blueprint will run on the bundle. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500.
    power :: Core.Maybe Core.Int,
    -- | The price in US dollars (e.g., @5.0@ ) of the bundle.
    price :: Core.Maybe Core.Double,
    -- | The amount of RAM in GB (e.g., @2.0@ ).
    ramSizeInGb :: Core.Maybe Core.Double,
    -- | The operating system platform (Linux/Unix-based or Windows Server-based) that the bundle supports. You can only launch a @WINDOWS@ bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@ blueprints require a @LINUX_UNIX@ bundle.
    supportedPlatforms :: Core.Maybe [Types.InstancePlatform],
    -- | The data transfer rate per month in GB (e.g., @2000@ ).
    transferPerMonthInGb :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Bundle' value with any optional fields omitted.
mkBundle ::
  Bundle
mkBundle =
  Bundle'
    { bundleId = Core.Nothing,
      cpuCount = Core.Nothing,
      diskSizeInGb = Core.Nothing,
      instanceType = Core.Nothing,
      isActive = Core.Nothing,
      name = Core.Nothing,
      power = Core.Nothing,
      price = Core.Nothing,
      ramSizeInGb = Core.Nothing,
      supportedPlatforms = Core.Nothing,
      transferPerMonthInGb = Core.Nothing
    }

-- | The bundle ID (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfBundleId :: Lens.Lens' Bundle (Core.Maybe Types.BundleId)
bfBundleId = Lens.field @"bundleId"
{-# DEPRECATED bfBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The number of vCPUs included in the bundle (e.g., @2@ ).
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfCpuCount :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bfCpuCount = Lens.field @"cpuCount"
{-# DEPRECATED bfCpuCount "Use generic-lens or generic-optics with 'cpuCount' instead." #-}

-- | The size of the SSD (e.g., @30@ ).
--
-- /Note:/ Consider using 'diskSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfDiskSizeInGb :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bfDiskSizeInGb = Lens.field @"diskSizeInGb"
{-# DEPRECATED bfDiskSizeInGb "Use generic-lens or generic-optics with 'diskSizeInGb' instead." #-}

-- | The Amazon EC2 instance type (e.g., @t2.micro@ ).
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfInstanceType :: Lens.Lens' Bundle (Core.Maybe Types.String)
bfInstanceType = Lens.field @"instanceType"
{-# DEPRECATED bfInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | A Boolean value indicating whether the bundle is active.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfIsActive :: Lens.Lens' Bundle (Core.Maybe Core.Bool)
bfIsActive = Lens.field @"isActive"
{-# DEPRECATED bfIsActive "Use generic-lens or generic-optics with 'isActive' instead." #-}

-- | A friendly name for the bundle (e.g., @Micro@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfName :: Lens.Lens' Bundle (Core.Maybe Types.String)
bfName = Lens.field @"name"
{-# DEPRECATED bfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A numeric value that represents the power of the bundle (e.g., @500@ ). You can use the bundle's power value in conjunction with a blueprint's minimum power value to determine whether the blueprint will run on the bundle. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500.
--
-- /Note:/ Consider using 'power' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfPower :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bfPower = Lens.field @"power"
{-# DEPRECATED bfPower "Use generic-lens or generic-optics with 'power' instead." #-}

-- | The price in US dollars (e.g., @5.0@ ) of the bundle.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfPrice :: Lens.Lens' Bundle (Core.Maybe Core.Double)
bfPrice = Lens.field @"price"
{-# DEPRECATED bfPrice "Use generic-lens or generic-optics with 'price' instead." #-}

-- | The amount of RAM in GB (e.g., @2.0@ ).
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfRamSizeInGb :: Lens.Lens' Bundle (Core.Maybe Core.Double)
bfRamSizeInGb = Lens.field @"ramSizeInGb"
{-# DEPRECATED bfRamSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead." #-}

-- | The operating system platform (Linux/Unix-based or Windows Server-based) that the bundle supports. You can only launch a @WINDOWS@ bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@ blueprints require a @LINUX_UNIX@ bundle.
--
-- /Note:/ Consider using 'supportedPlatforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfSupportedPlatforms :: Lens.Lens' Bundle (Core.Maybe [Types.InstancePlatform])
bfSupportedPlatforms = Lens.field @"supportedPlatforms"
{-# DEPRECATED bfSupportedPlatforms "Use generic-lens or generic-optics with 'supportedPlatforms' instead." #-}

-- | The data transfer rate per month in GB (e.g., @2000@ ).
--
-- /Note:/ Consider using 'transferPerMonthInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfTransferPerMonthInGb :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bfTransferPerMonthInGb = Lens.field @"transferPerMonthInGb"
{-# DEPRECATED bfTransferPerMonthInGb "Use generic-lens or generic-optics with 'transferPerMonthInGb' instead." #-}

instance Core.FromJSON Bundle where
  parseJSON =
    Core.withObject "Bundle" Core.$
      \x ->
        Bundle'
          Core.<$> (x Core..:? "bundleId")
          Core.<*> (x Core..:? "cpuCount")
          Core.<*> (x Core..:? "diskSizeInGb")
          Core.<*> (x Core..:? "instanceType")
          Core.<*> (x Core..:? "isActive")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "power")
          Core.<*> (x Core..:? "price")
          Core.<*> (x Core..:? "ramSizeInGb")
          Core.<*> (x Core..:? "supportedPlatforms")
          Core.<*> (x Core..:? "transferPerMonthInGb")
