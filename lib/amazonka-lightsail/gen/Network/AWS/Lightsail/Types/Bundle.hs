{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Bundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Bundle
  ( Bundle (..)
  -- * Smart constructor
  , mkBundle
  -- * Lenses
  , bfBundleId
  , bfCpuCount
  , bfDiskSizeInGb
  , bfInstanceType
  , bfIsActive
  , bfName
  , bfPower
  , bfPrice
  , bfRamSizeInGb
  , bfSupportedPlatforms
  , bfTransferPerMonthInGb
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.BundleId as Types
import qualified Network.AWS.Lightsail.Types.InstancePlatform as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a bundle, which is a set of specs describing your virtual private server (or /instance/ ).
--
-- /See:/ 'mkBundle' smart constructor.
data Bundle = Bundle'
  { bundleId :: Core.Maybe Types.BundleId
    -- ^ The bundle ID (e.g., @micro_1_0@ ).
  , cpuCount :: Core.Maybe Core.Int
    -- ^ The number of vCPUs included in the bundle (e.g., @2@ ).
  , diskSizeInGb :: Core.Maybe Core.Int
    -- ^ The size of the SSD (e.g., @30@ ).
  , instanceType :: Core.Maybe Core.Text
    -- ^ The Amazon EC2 instance type (e.g., @t2.micro@ ).
  , isActive :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether the bundle is active.
  , name :: Core.Maybe Core.Text
    -- ^ A friendly name for the bundle (e.g., @Micro@ ).
  , power :: Core.Maybe Core.Int
    -- ^ A numeric value that represents the power of the bundle (e.g., @500@ ). You can use the bundle's power value in conjunction with a blueprint's minimum power value to determine whether the blueprint will run on the bundle. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500.
  , price :: Core.Maybe Core.Double
    -- ^ The price in US dollars (e.g., @5.0@ ) of the bundle.
  , ramSizeInGb :: Core.Maybe Core.Double
    -- ^ The amount of RAM in GB (e.g., @2.0@ ).
  , supportedPlatforms :: Core.Maybe [Types.InstancePlatform]
    -- ^ The operating system platform (Linux/Unix-based or Windows Server-based) that the bundle supports. You can only launch a @WINDOWS@ bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@ blueprints require a @LINUX_UNIX@ bundle.
  , transferPerMonthInGb :: Core.Maybe Core.Int
    -- ^ The data transfer rate per month in GB (e.g., @2000@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Bundle' value with any optional fields omitted.
mkBundle
    :: Bundle
mkBundle
  = Bundle'{bundleId = Core.Nothing, cpuCount = Core.Nothing,
            diskSizeInGb = Core.Nothing, instanceType = Core.Nothing,
            isActive = Core.Nothing, name = Core.Nothing, power = Core.Nothing,
            price = Core.Nothing, ramSizeInGb = Core.Nothing,
            supportedPlatforms = Core.Nothing,
            transferPerMonthInGb = Core.Nothing}

-- | The bundle ID (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfBundleId :: Lens.Lens' Bundle (Core.Maybe Types.BundleId)
bfBundleId = Lens.field @"bundleId"
{-# INLINEABLE bfBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | The number of vCPUs included in the bundle (e.g., @2@ ).
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfCpuCount :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bfCpuCount = Lens.field @"cpuCount"
{-# INLINEABLE bfCpuCount #-}
{-# DEPRECATED cpuCount "Use generic-lens or generic-optics with 'cpuCount' instead"  #-}

-- | The size of the SSD (e.g., @30@ ).
--
-- /Note:/ Consider using 'diskSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfDiskSizeInGb :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bfDiskSizeInGb = Lens.field @"diskSizeInGb"
{-# INLINEABLE bfDiskSizeInGb #-}
{-# DEPRECATED diskSizeInGb "Use generic-lens or generic-optics with 'diskSizeInGb' instead"  #-}

-- | The Amazon EC2 instance type (e.g., @t2.micro@ ).
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfInstanceType :: Lens.Lens' Bundle (Core.Maybe Core.Text)
bfInstanceType = Lens.field @"instanceType"
{-# INLINEABLE bfInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | A Boolean value indicating whether the bundle is active.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfIsActive :: Lens.Lens' Bundle (Core.Maybe Core.Bool)
bfIsActive = Lens.field @"isActive"
{-# INLINEABLE bfIsActive #-}
{-# DEPRECATED isActive "Use generic-lens or generic-optics with 'isActive' instead"  #-}

-- | A friendly name for the bundle (e.g., @Micro@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfName :: Lens.Lens' Bundle (Core.Maybe Core.Text)
bfName = Lens.field @"name"
{-# INLINEABLE bfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A numeric value that represents the power of the bundle (e.g., @500@ ). You can use the bundle's power value in conjunction with a blueprint's minimum power value to determine whether the blueprint will run on the bundle. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500.
--
-- /Note:/ Consider using 'power' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfPower :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bfPower = Lens.field @"power"
{-# INLINEABLE bfPower #-}
{-# DEPRECATED power "Use generic-lens or generic-optics with 'power' instead"  #-}

-- | The price in US dollars (e.g., @5.0@ ) of the bundle.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfPrice :: Lens.Lens' Bundle (Core.Maybe Core.Double)
bfPrice = Lens.field @"price"
{-# INLINEABLE bfPrice #-}
{-# DEPRECATED price "Use generic-lens or generic-optics with 'price' instead"  #-}

-- | The amount of RAM in GB (e.g., @2.0@ ).
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfRamSizeInGb :: Lens.Lens' Bundle (Core.Maybe Core.Double)
bfRamSizeInGb = Lens.field @"ramSizeInGb"
{-# INLINEABLE bfRamSizeInGb #-}
{-# DEPRECATED ramSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead"  #-}

-- | The operating system platform (Linux/Unix-based or Windows Server-based) that the bundle supports. You can only launch a @WINDOWS@ bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@ blueprints require a @LINUX_UNIX@ bundle.
--
-- /Note:/ Consider using 'supportedPlatforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfSupportedPlatforms :: Lens.Lens' Bundle (Core.Maybe [Types.InstancePlatform])
bfSupportedPlatforms = Lens.field @"supportedPlatforms"
{-# INLINEABLE bfSupportedPlatforms #-}
{-# DEPRECATED supportedPlatforms "Use generic-lens or generic-optics with 'supportedPlatforms' instead"  #-}

-- | The data transfer rate per month in GB (e.g., @2000@ ).
--
-- /Note:/ Consider using 'transferPerMonthInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfTransferPerMonthInGb :: Lens.Lens' Bundle (Core.Maybe Core.Int)
bfTransferPerMonthInGb = Lens.field @"transferPerMonthInGb"
{-# INLINEABLE bfTransferPerMonthInGb #-}
{-# DEPRECATED transferPerMonthInGb "Use generic-lens or generic-optics with 'transferPerMonthInGb' instead"  #-}

instance Core.FromJSON Bundle where
        parseJSON
          = Core.withObject "Bundle" Core.$
              \ x ->
                Bundle' Core.<$>
                  (x Core..:? "bundleId") Core.<*> x Core..:? "cpuCount" Core.<*>
                    x Core..:? "diskSizeInGb"
                    Core.<*> x Core..:? "instanceType"
                    Core.<*> x Core..:? "isActive"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "power"
                    Core.<*> x Core..:? "price"
                    Core.<*> x Core..:? "ramSizeInGb"
                    Core.<*> x Core..:? "supportedPlatforms"
                    Core.<*> x Core..:? "transferPerMonthInGb"
