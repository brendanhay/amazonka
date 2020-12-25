{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupConfig
  ( InstanceGroupConfig (..),

    -- * Smart constructor
    mkInstanceGroupConfig,

    -- * Lenses
    igcInstanceRole,
    igcInstanceType,
    igcInstanceCount,
    igcAutoScalingPolicy,
    igcBidPrice,
    igcConfigurations,
    igcEbsConfiguration,
    igcMarket,
    igcName,
  )
where

import qualified Network.AWS.EMR.Types.AutoScalingPolicy as Types
import qualified Network.AWS.EMR.Types.BidPrice as Types
import qualified Network.AWS.EMR.Types.Configuration as Types
import qualified Network.AWS.EMR.Types.EbsConfiguration as Types
import qualified Network.AWS.EMR.Types.InstanceRoleType as Types
import qualified Network.AWS.EMR.Types.InstanceType as Types
import qualified Network.AWS.EMR.Types.MarketType as Types
import qualified Network.AWS.EMR.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration defining a new instance group.
--
-- /See:/ 'mkInstanceGroupConfig' smart constructor.
data InstanceGroupConfig = InstanceGroupConfig'
  { -- | The role of the instance group in the cluster.
    instanceRole :: Types.InstanceRoleType,
    -- | The EC2 instance type for all instances in the instance group.
    instanceType :: Types.InstanceType,
    -- | Target number of instances for the instance group.
    instanceCount :: Core.Int,
    -- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
    autoScalingPolicy :: Core.Maybe Types.AutoScalingPolicy,
    -- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPrice :: Core.Maybe Types.BidPrice,
    -- | The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
    configurations :: Core.Maybe [Types.Configuration],
    -- | EBS configurations that will be attached to each EC2 instance in the instance group.
    ebsConfiguration :: Core.Maybe Types.EbsConfiguration,
    -- | Market type of the EC2 instances used to create a cluster node.
    market :: Core.Maybe Types.MarketType,
    -- | Friendly name given to the instance group.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceGroupConfig' value with any optional fields omitted.
mkInstanceGroupConfig ::
  -- | 'instanceRole'
  Types.InstanceRoleType ->
  -- | 'instanceType'
  Types.InstanceType ->
  -- | 'instanceCount'
  Core.Int ->
  InstanceGroupConfig
mkInstanceGroupConfig instanceRole instanceType instanceCount =
  InstanceGroupConfig'
    { instanceRole,
      instanceType,
      instanceCount,
      autoScalingPolicy = Core.Nothing,
      bidPrice = Core.Nothing,
      configurations = Core.Nothing,
      ebsConfiguration = Core.Nothing,
      market = Core.Nothing,
      name = Core.Nothing
    }

-- | The role of the instance group in the cluster.
--
-- /Note:/ Consider using 'instanceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcInstanceRole :: Lens.Lens' InstanceGroupConfig Types.InstanceRoleType
igcInstanceRole = Lens.field @"instanceRole"
{-# DEPRECATED igcInstanceRole "Use generic-lens or generic-optics with 'instanceRole' instead." #-}

-- | The EC2 instance type for all instances in the instance group.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcInstanceType :: Lens.Lens' InstanceGroupConfig Types.InstanceType
igcInstanceType = Lens.field @"instanceType"
{-# DEPRECATED igcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Target number of instances for the instance group.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcInstanceCount :: Lens.Lens' InstanceGroupConfig Core.Int
igcInstanceCount = Lens.field @"instanceCount"
{-# DEPRECATED igcInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
-- /Note:/ Consider using 'autoScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcAutoScalingPolicy :: Lens.Lens' InstanceGroupConfig (Core.Maybe Types.AutoScalingPolicy)
igcAutoScalingPolicy = Lens.field @"autoScalingPolicy"
{-# DEPRECATED igcAutoScalingPolicy "Use generic-lens or generic-optics with 'autoScalingPolicy' instead." #-}

-- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- /Note:/ Consider using 'bidPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcBidPrice :: Lens.Lens' InstanceGroupConfig (Core.Maybe Types.BidPrice)
igcBidPrice = Lens.field @"bidPrice"
{-# DEPRECATED igcBidPrice "Use generic-lens or generic-optics with 'bidPrice' instead." #-}

-- | The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcConfigurations :: Lens.Lens' InstanceGroupConfig (Core.Maybe [Types.Configuration])
igcConfigurations = Lens.field @"configurations"
{-# DEPRECATED igcConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | EBS configurations that will be attached to each EC2 instance in the instance group.
--
-- /Note:/ Consider using 'ebsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcEbsConfiguration :: Lens.Lens' InstanceGroupConfig (Core.Maybe Types.EbsConfiguration)
igcEbsConfiguration = Lens.field @"ebsConfiguration"
{-# DEPRECATED igcEbsConfiguration "Use generic-lens or generic-optics with 'ebsConfiguration' instead." #-}

-- | Market type of the EC2 instances used to create a cluster node.
--
-- /Note:/ Consider using 'market' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcMarket :: Lens.Lens' InstanceGroupConfig (Core.Maybe Types.MarketType)
igcMarket = Lens.field @"market"
{-# DEPRECATED igcMarket "Use generic-lens or generic-optics with 'market' instead." #-}

-- | Friendly name given to the instance group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igcName :: Lens.Lens' InstanceGroupConfig (Core.Maybe Types.Name)
igcName = Lens.field @"name"
{-# DEPRECATED igcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON InstanceGroupConfig where
  toJSON InstanceGroupConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceRole" Core..= instanceRole),
            Core.Just ("InstanceType" Core..= instanceType),
            Core.Just ("InstanceCount" Core..= instanceCount),
            ("AutoScalingPolicy" Core..=) Core.<$> autoScalingPolicy,
            ("BidPrice" Core..=) Core.<$> bidPrice,
            ("Configurations" Core..=) Core.<$> configurations,
            ("EbsConfiguration" Core..=) Core.<$> ebsConfiguration,
            ("Market" Core..=) Core.<$> market,
            ("Name" Core..=) Core.<$> name
          ]
      )
