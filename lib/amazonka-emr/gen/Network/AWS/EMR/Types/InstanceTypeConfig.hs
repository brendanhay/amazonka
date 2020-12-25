{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceTypeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTypeConfig
  ( InstanceTypeConfig (..),

    -- * Smart constructor
    mkInstanceTypeConfig,

    -- * Lenses
    itcInstanceType,
    itcBidPrice,
    itcBidPriceAsPercentageOfOnDemandPrice,
    itcConfigurations,
    itcEbsConfiguration,
    itcWeightedCapacity,
  )
where

import qualified Network.AWS.EMR.Types.BidPrice as Types
import qualified Network.AWS.EMR.Types.Configuration as Types
import qualified Network.AWS.EMR.Types.EbsConfiguration as Types
import qualified Network.AWS.EMR.Types.InstanceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An instance type configuration for each instance type in an instance fleet, which determines the EC2 instances Amazon EMR attempts to provision to fulfill On-Demand and Spot target capacities. There can be a maximum of five instance type configurations in a fleet.
--
-- /See:/ 'mkInstanceTypeConfig' smart constructor.
data InstanceTypeConfig = InstanceTypeConfig'
  { -- | An EC2 instance type, such as @m3.xlarge@ .
    instanceType :: Types.InstanceType,
    -- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPrice :: Core.Maybe Types.BidPrice,
    -- | The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%). If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPriceAsPercentageOfOnDemandPrice :: Core.Maybe Core.Double,
    -- | A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software that run on the cluster.
    configurations :: Core.Maybe [Types.Configuration],
    -- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
    ebsConfiguration :: Core.Maybe Types.EbsConfiguration,
    -- | The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . This value is 1 for a master instance fleet, and must be 1 or greater for core and task instance fleets. Defaults to 1 if not specified.
    weightedCapacity :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceTypeConfig' value with any optional fields omitted.
mkInstanceTypeConfig ::
  -- | 'instanceType'
  Types.InstanceType ->
  InstanceTypeConfig
mkInstanceTypeConfig instanceType =
  InstanceTypeConfig'
    { instanceType,
      bidPrice = Core.Nothing,
      bidPriceAsPercentageOfOnDemandPrice = Core.Nothing,
      configurations = Core.Nothing,
      ebsConfiguration = Core.Nothing,
      weightedCapacity = Core.Nothing
    }

-- | An EC2 instance type, such as @m3.xlarge@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcInstanceType :: Lens.Lens' InstanceTypeConfig Types.InstanceType
itcInstanceType = Lens.field @"instanceType"
{-# DEPRECATED itcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- /Note:/ Consider using 'bidPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcBidPrice :: Lens.Lens' InstanceTypeConfig (Core.Maybe Types.BidPrice)
itcBidPrice = Lens.field @"bidPrice"
{-# DEPRECATED itcBidPrice "Use generic-lens or generic-optics with 'bidPrice' instead." #-}

-- | The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%). If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- /Note:/ Consider using 'bidPriceAsPercentageOfOnDemandPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcBidPriceAsPercentageOfOnDemandPrice :: Lens.Lens' InstanceTypeConfig (Core.Maybe Core.Double)
itcBidPriceAsPercentageOfOnDemandPrice = Lens.field @"bidPriceAsPercentageOfOnDemandPrice"
{-# DEPRECATED itcBidPriceAsPercentageOfOnDemandPrice "Use generic-lens or generic-optics with 'bidPriceAsPercentageOfOnDemandPrice' instead." #-}

-- | A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software that run on the cluster.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcConfigurations :: Lens.Lens' InstanceTypeConfig (Core.Maybe [Types.Configuration])
itcConfigurations = Lens.field @"configurations"
{-# DEPRECATED itcConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
--
-- /Note:/ Consider using 'ebsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcEbsConfiguration :: Lens.Lens' InstanceTypeConfig (Core.Maybe Types.EbsConfiguration)
itcEbsConfiguration = Lens.field @"ebsConfiguration"
{-# DEPRECATED itcEbsConfiguration "Use generic-lens or generic-optics with 'ebsConfiguration' instead." #-}

-- | The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . This value is 1 for a master instance fleet, and must be 1 or greater for core and task instance fleets. Defaults to 1 if not specified.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcWeightedCapacity :: Lens.Lens' InstanceTypeConfig (Core.Maybe Core.Natural)
itcWeightedCapacity = Lens.field @"weightedCapacity"
{-# DEPRECATED itcWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

instance Core.FromJSON InstanceTypeConfig where
  toJSON InstanceTypeConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceType" Core..= instanceType),
            ("BidPrice" Core..=) Core.<$> bidPrice,
            ("BidPriceAsPercentageOfOnDemandPrice" Core..=)
              Core.<$> bidPriceAsPercentageOfOnDemandPrice,
            ("Configurations" Core..=) Core.<$> configurations,
            ("EbsConfiguration" Core..=) Core.<$> ebsConfiguration,
            ("WeightedCapacity" Core..=) Core.<$> weightedCapacity
          ]
      )
