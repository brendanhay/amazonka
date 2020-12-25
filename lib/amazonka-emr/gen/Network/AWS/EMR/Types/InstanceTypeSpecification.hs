{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceTypeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTypeSpecification
  ( InstanceTypeSpecification (..),

    -- * Smart constructor
    mkInstanceTypeSpecification,

    -- * Lenses
    itsBidPrice,
    itsBidPriceAsPercentageOfOnDemandPrice,
    itsConfigurations,
    itsEbsBlockDevices,
    itsEbsOptimized,
    itsInstanceType,
    itsWeightedCapacity,
  )
where

import qualified Network.AWS.EMR.Types.BidPrice as Types
import qualified Network.AWS.EMR.Types.Configuration as Types
import qualified Network.AWS.EMR.Types.EbsBlockDevice as Types
import qualified Network.AWS.EMR.Types.InstanceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration specification for each instance type in an instance fleet.
--
-- /See:/ 'mkInstanceTypeSpecification' smart constructor.
data InstanceTypeSpecification = InstanceTypeSpecification'
  { -- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD.
    bidPrice :: Core.Maybe Types.BidPrice,
    -- | The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%).
    bidPriceAsPercentageOfOnDemandPrice :: Core.Maybe Core.Double,
    -- | A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR.
    configurations :: Core.Maybe [Types.Configuration],
    -- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
    ebsBlockDevices :: Core.Maybe [Types.EbsBlockDevice],
    -- | Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The EC2 instance type, for example @m3.xlarge@ .
    instanceType :: Core.Maybe Types.InstanceType,
    -- | The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . Capacity values represent performance characteristics such as vCPUs, memory, or I/O. If not specified, the default value is 1.
    weightedCapacity :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceTypeSpecification' value with any optional fields omitted.
mkInstanceTypeSpecification ::
  InstanceTypeSpecification
mkInstanceTypeSpecification =
  InstanceTypeSpecification'
    { bidPrice = Core.Nothing,
      bidPriceAsPercentageOfOnDemandPrice = Core.Nothing,
      configurations = Core.Nothing,
      ebsBlockDevices = Core.Nothing,
      ebsOptimized = Core.Nothing,
      instanceType = Core.Nothing,
      weightedCapacity = Core.Nothing
    }

-- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD.
--
-- /Note:/ Consider using 'bidPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsBidPrice :: Lens.Lens' InstanceTypeSpecification (Core.Maybe Types.BidPrice)
itsBidPrice = Lens.field @"bidPrice"
{-# DEPRECATED itsBidPrice "Use generic-lens or generic-optics with 'bidPrice' instead." #-}

-- | The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%).
--
-- /Note:/ Consider using 'bidPriceAsPercentageOfOnDemandPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsBidPriceAsPercentageOfOnDemandPrice :: Lens.Lens' InstanceTypeSpecification (Core.Maybe Core.Double)
itsBidPriceAsPercentageOfOnDemandPrice = Lens.field @"bidPriceAsPercentageOfOnDemandPrice"
{-# DEPRECATED itsBidPriceAsPercentageOfOnDemandPrice "Use generic-lens or generic-optics with 'bidPriceAsPercentageOfOnDemandPrice' instead." #-}

-- | A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsConfigurations :: Lens.Lens' InstanceTypeSpecification (Core.Maybe [Types.Configuration])
itsConfigurations = Lens.field @"configurations"
{-# DEPRECATED itsConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
--
-- /Note:/ Consider using 'ebsBlockDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsEbsBlockDevices :: Lens.Lens' InstanceTypeSpecification (Core.Maybe [Types.EbsBlockDevice])
itsEbsBlockDevices = Lens.field @"ebsBlockDevices"
{-# DEPRECATED itsEbsBlockDevices "Use generic-lens or generic-optics with 'ebsBlockDevices' instead." #-}

-- | Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsEbsOptimized :: Lens.Lens' InstanceTypeSpecification (Core.Maybe Core.Bool)
itsEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED itsEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The EC2 instance type, for example @m3.xlarge@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsInstanceType :: Lens.Lens' InstanceTypeSpecification (Core.Maybe Types.InstanceType)
itsInstanceType = Lens.field @"instanceType"
{-# DEPRECATED itsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . Capacity values represent performance characteristics such as vCPUs, memory, or I/O. If not specified, the default value is 1.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itsWeightedCapacity :: Lens.Lens' InstanceTypeSpecification (Core.Maybe Core.Natural)
itsWeightedCapacity = Lens.field @"weightedCapacity"
{-# DEPRECATED itsWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

instance Core.FromJSON InstanceTypeSpecification where
  parseJSON =
    Core.withObject "InstanceTypeSpecification" Core.$
      \x ->
        InstanceTypeSpecification'
          Core.<$> (x Core..:? "BidPrice")
          Core.<*> (x Core..:? "BidPriceAsPercentageOfOnDemandPrice")
          Core.<*> (x Core..:? "Configurations")
          Core.<*> (x Core..:? "EbsBlockDevices")
          Core.<*> (x Core..:? "EbsOptimized")
          Core.<*> (x Core..:? "InstanceType")
          Core.<*> (x Core..:? "WeightedCapacity")
