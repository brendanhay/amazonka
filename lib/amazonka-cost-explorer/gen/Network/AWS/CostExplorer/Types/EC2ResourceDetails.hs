{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2ResourceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.EC2ResourceDetails
  ( EC2ResourceDetails (..)
  -- * Smart constructor
  , mkEC2ResourceDetails
  -- * Lenses
  , ecrdHourlyOnDemandRate
  , ecrdInstanceType
  , ecrdMemory
  , ecrdNetworkPerformance
  , ecrdPlatform
  , ecrdRegion
  , ecrdSku
  , ecrdStorage
  , ecrdVcpu
  ) where

import qualified Network.AWS.CostExplorer.Types.HourlyOnDemandRate as Types
import qualified Network.AWS.CostExplorer.Types.InstanceType as Types
import qualified Network.AWS.CostExplorer.Types.Memory as Types
import qualified Network.AWS.CostExplorer.Types.NetworkPerformance as Types
import qualified Network.AWS.CostExplorer.Types.Platform as Types
import qualified Network.AWS.CostExplorer.Types.Region as Types
import qualified Network.AWS.CostExplorer.Types.Sku as Types
import qualified Network.AWS.CostExplorer.Types.Storage as Types
import qualified Network.AWS.CostExplorer.Types.Vcpu as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on the Amazon EC2 Resource.
--
-- /See:/ 'mkEC2ResourceDetails' smart constructor.
data EC2ResourceDetails = EC2ResourceDetails'
  { hourlyOnDemandRate :: Core.Maybe Types.HourlyOnDemandRate
    -- ^ Hourly public On-Demand rate for the instance type.
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The type of AWS instance.
  , memory :: Core.Maybe Types.Memory
    -- ^ Memory capacity of the AWS instance.
  , networkPerformance :: Core.Maybe Types.NetworkPerformance
    -- ^ Network performance capacity of the AWS instance.
  , platform :: Core.Maybe Types.Platform
    -- ^ The platform of the AWS instance. The platform is the specific combination of operating system, license model, and software on an instance.
  , region :: Core.Maybe Types.Region
    -- ^ The AWS Region of the instance.
  , sku :: Core.Maybe Types.Sku
    -- ^ The SKU of the product.
  , storage :: Core.Maybe Types.Storage
    -- ^ The disk storage of the AWS instance (not EBS storage).
  , vcpu :: Core.Maybe Types.Vcpu
    -- ^ Number of VCPU cores in the AWS instance type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EC2ResourceDetails' value with any optional fields omitted.
mkEC2ResourceDetails
    :: EC2ResourceDetails
mkEC2ResourceDetails
  = EC2ResourceDetails'{hourlyOnDemandRate = Core.Nothing,
                        instanceType = Core.Nothing, memory = Core.Nothing,
                        networkPerformance = Core.Nothing, platform = Core.Nothing,
                        region = Core.Nothing, sku = Core.Nothing, storage = Core.Nothing,
                        vcpu = Core.Nothing}

-- | Hourly public On-Demand rate for the instance type.
--
-- /Note:/ Consider using 'hourlyOnDemandRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrdHourlyOnDemandRate :: Lens.Lens' EC2ResourceDetails (Core.Maybe Types.HourlyOnDemandRate)
ecrdHourlyOnDemandRate = Lens.field @"hourlyOnDemandRate"
{-# INLINEABLE ecrdHourlyOnDemandRate #-}
{-# DEPRECATED hourlyOnDemandRate "Use generic-lens or generic-optics with 'hourlyOnDemandRate' instead"  #-}

-- | The type of AWS instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrdInstanceType :: Lens.Lens' EC2ResourceDetails (Core.Maybe Types.InstanceType)
ecrdInstanceType = Lens.field @"instanceType"
{-# INLINEABLE ecrdInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | Memory capacity of the AWS instance.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrdMemory :: Lens.Lens' EC2ResourceDetails (Core.Maybe Types.Memory)
ecrdMemory = Lens.field @"memory"
{-# INLINEABLE ecrdMemory #-}
{-# DEPRECATED memory "Use generic-lens or generic-optics with 'memory' instead"  #-}

-- | Network performance capacity of the AWS instance.
--
-- /Note:/ Consider using 'networkPerformance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrdNetworkPerformance :: Lens.Lens' EC2ResourceDetails (Core.Maybe Types.NetworkPerformance)
ecrdNetworkPerformance = Lens.field @"networkPerformance"
{-# INLINEABLE ecrdNetworkPerformance #-}
{-# DEPRECATED networkPerformance "Use generic-lens or generic-optics with 'networkPerformance' instead"  #-}

-- | The platform of the AWS instance. The platform is the specific combination of operating system, license model, and software on an instance.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrdPlatform :: Lens.Lens' EC2ResourceDetails (Core.Maybe Types.Platform)
ecrdPlatform = Lens.field @"platform"
{-# INLINEABLE ecrdPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The AWS Region of the instance.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrdRegion :: Lens.Lens' EC2ResourceDetails (Core.Maybe Types.Region)
ecrdRegion = Lens.field @"region"
{-# INLINEABLE ecrdRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The SKU of the product.
--
-- /Note:/ Consider using 'sku' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrdSku :: Lens.Lens' EC2ResourceDetails (Core.Maybe Types.Sku)
ecrdSku = Lens.field @"sku"
{-# INLINEABLE ecrdSku #-}
{-# DEPRECATED sku "Use generic-lens or generic-optics with 'sku' instead"  #-}

-- | The disk storage of the AWS instance (not EBS storage).
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrdStorage :: Lens.Lens' EC2ResourceDetails (Core.Maybe Types.Storage)
ecrdStorage = Lens.field @"storage"
{-# INLINEABLE ecrdStorage #-}
{-# DEPRECATED storage "Use generic-lens or generic-optics with 'storage' instead"  #-}

-- | Number of VCPU cores in the AWS instance type.
--
-- /Note:/ Consider using 'vcpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrdVcpu :: Lens.Lens' EC2ResourceDetails (Core.Maybe Types.Vcpu)
ecrdVcpu = Lens.field @"vcpu"
{-# INLINEABLE ecrdVcpu #-}
{-# DEPRECATED vcpu "Use generic-lens or generic-optics with 'vcpu' instead"  #-}

instance Core.FromJSON EC2ResourceDetails where
        parseJSON
          = Core.withObject "EC2ResourceDetails" Core.$
              \ x ->
                EC2ResourceDetails' Core.<$>
                  (x Core..:? "HourlyOnDemandRate") Core.<*>
                    x Core..:? "InstanceType"
                    Core.<*> x Core..:? "Memory"
                    Core.<*> x Core..:? "NetworkPerformance"
                    Core.<*> x Core..:? "Platform"
                    Core.<*> x Core..:? "Region"
                    Core.<*> x Core..:? "Sku"
                    Core.<*> x Core..:? "Storage"
                    Core.<*> x Core..:? "Vcpu"
