{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceGroup
  ( InstanceGroup (..)
  -- * Smart constructor
  , mkInstanceGroup
  -- * Lenses
  , igAutoScalingPolicy
  , igBidPrice
  , igConfigurations
  , igConfigurationsVersion
  , igEbsBlockDevices
  , igEbsOptimized
  , igId
  , igInstanceGroupType
  , igInstanceType
  , igLastSuccessfullyAppliedConfigurations
  , igLastSuccessfullyAppliedConfigurationsVersion
  , igMarket
  , igName
  , igRequestedInstanceCount
  , igRunningInstanceCount
  , igShrinkPolicy
  , igStatus
  ) where

import qualified Network.AWS.EMR.Types.AutoScalingPolicyDescription as Types
import qualified Network.AWS.EMR.Types.Configuration as Types
import qualified Network.AWS.EMR.Types.EbsBlockDevice as Types
import qualified Network.AWS.EMR.Types.InstanceGroupId as Types
import qualified Network.AWS.EMR.Types.InstanceGroupStatus as Types
import qualified Network.AWS.EMR.Types.InstanceGroupType as Types
import qualified Network.AWS.EMR.Types.InstanceType as Types
import qualified Network.AWS.EMR.Types.MarketType as Types
import qualified Network.AWS.EMR.Types.ShrinkPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This entity represents an instance group, which is a group of instances that have common purpose. For example, CORE instance group is used for HDFS.
--
-- /See:/ 'mkInstanceGroup' smart constructor.
data InstanceGroup = InstanceGroup'
  { autoScalingPolicy :: Core.Maybe Types.AutoScalingPolicyDescription
    -- ^ An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
  , bidPrice :: Core.Maybe Core.Text
    -- ^ The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
  , configurations :: Core.Maybe [Types.Configuration]
    -- ^ The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
  , configurationsVersion :: Core.Maybe Core.Integer
    -- ^ The version number of the requested configuration specification for this instance group.
  , ebsBlockDevices :: Core.Maybe [Types.EbsBlockDevice]
    -- ^ The EBS block devices that are mapped to this instance group.
  , ebsOptimized :: Core.Maybe Core.Bool
    -- ^ If the instance group is EBS-optimized. An Amazon EBS-optimized instance uses an optimized configuration stack and provides additional, dedicated capacity for Amazon EBS I/O.
  , id :: Core.Maybe Types.InstanceGroupId
    -- ^ The identifier of the instance group.
  , instanceGroupType :: Core.Maybe Types.InstanceGroupType
    -- ^ The type of the instance group. Valid values are MASTER, CORE or TASK.
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The EC2 instance type for all instances in the instance group.
  , lastSuccessfullyAppliedConfigurations :: Core.Maybe [Types.Configuration]
    -- ^ A list of configurations that were successfully applied for an instance group last time.
  , lastSuccessfullyAppliedConfigurationsVersion :: Core.Maybe Core.Integer
    -- ^ The version number of a configuration specification that was successfully applied for an instance group last time. 
  , market :: Core.Maybe Types.MarketType
    -- ^ The marketplace to provision instances for this group. Valid values are ON_DEMAND or SPOT.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the instance group.
  , requestedInstanceCount :: Core.Maybe Core.Int
    -- ^ The target number of instances for the instance group.
  , runningInstanceCount :: Core.Maybe Core.Int
    -- ^ The number of instances currently running in this instance group.
  , shrinkPolicy :: Core.Maybe Types.ShrinkPolicy
    -- ^ Policy for customizing shrink operations.
  , status :: Core.Maybe Types.InstanceGroupStatus
    -- ^ The current status of the instance group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceGroup' value with any optional fields omitted.
mkInstanceGroup
    :: InstanceGroup
mkInstanceGroup
  = InstanceGroup'{autoScalingPolicy = Core.Nothing,
                   bidPrice = Core.Nothing, configurations = Core.Nothing,
                   configurationsVersion = Core.Nothing,
                   ebsBlockDevices = Core.Nothing, ebsOptimized = Core.Nothing,
                   id = Core.Nothing, instanceGroupType = Core.Nothing,
                   instanceType = Core.Nothing,
                   lastSuccessfullyAppliedConfigurations = Core.Nothing,
                   lastSuccessfullyAppliedConfigurationsVersion = Core.Nothing,
                   market = Core.Nothing, name = Core.Nothing,
                   requestedInstanceCount = Core.Nothing,
                   runningInstanceCount = Core.Nothing, shrinkPolicy = Core.Nothing,
                   status = Core.Nothing}

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
--
-- /Note:/ Consider using 'autoScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igAutoScalingPolicy :: Lens.Lens' InstanceGroup (Core.Maybe Types.AutoScalingPolicyDescription)
igAutoScalingPolicy = Lens.field @"autoScalingPolicy"
{-# INLINEABLE igAutoScalingPolicy #-}
{-# DEPRECATED autoScalingPolicy "Use generic-lens or generic-optics with 'autoScalingPolicy' instead"  #-}

-- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- /Note:/ Consider using 'bidPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igBidPrice :: Lens.Lens' InstanceGroup (Core.Maybe Core.Text)
igBidPrice = Lens.field @"bidPrice"
{-# INLINEABLE igBidPrice #-}
{-# DEPRECATED bidPrice "Use generic-lens or generic-optics with 'bidPrice' instead"  #-}

-- | The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igConfigurations :: Lens.Lens' InstanceGroup (Core.Maybe [Types.Configuration])
igConfigurations = Lens.field @"configurations"
{-# INLINEABLE igConfigurations #-}
{-# DEPRECATED configurations "Use generic-lens or generic-optics with 'configurations' instead"  #-}

-- | The version number of the requested configuration specification for this instance group.
--
-- /Note:/ Consider using 'configurationsVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igConfigurationsVersion :: Lens.Lens' InstanceGroup (Core.Maybe Core.Integer)
igConfigurationsVersion = Lens.field @"configurationsVersion"
{-# INLINEABLE igConfigurationsVersion #-}
{-# DEPRECATED configurationsVersion "Use generic-lens or generic-optics with 'configurationsVersion' instead"  #-}

-- | The EBS block devices that are mapped to this instance group.
--
-- /Note:/ Consider using 'ebsBlockDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igEbsBlockDevices :: Lens.Lens' InstanceGroup (Core.Maybe [Types.EbsBlockDevice])
igEbsBlockDevices = Lens.field @"ebsBlockDevices"
{-# INLINEABLE igEbsBlockDevices #-}
{-# DEPRECATED ebsBlockDevices "Use generic-lens or generic-optics with 'ebsBlockDevices' instead"  #-}

-- | If the instance group is EBS-optimized. An Amazon EBS-optimized instance uses an optimized configuration stack and provides additional, dedicated capacity for Amazon EBS I/O.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igEbsOptimized :: Lens.Lens' InstanceGroup (Core.Maybe Core.Bool)
igEbsOptimized = Lens.field @"ebsOptimized"
{-# INLINEABLE igEbsOptimized #-}
{-# DEPRECATED ebsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead"  #-}

-- | The identifier of the instance group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igId :: Lens.Lens' InstanceGroup (Core.Maybe Types.InstanceGroupId)
igId = Lens.field @"id"
{-# INLINEABLE igId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
--
-- /Note:/ Consider using 'instanceGroupType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igInstanceGroupType :: Lens.Lens' InstanceGroup (Core.Maybe Types.InstanceGroupType)
igInstanceGroupType = Lens.field @"instanceGroupType"
{-# INLINEABLE igInstanceGroupType #-}
{-# DEPRECATED instanceGroupType "Use generic-lens or generic-optics with 'instanceGroupType' instead"  #-}

-- | The EC2 instance type for all instances in the instance group.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igInstanceType :: Lens.Lens' InstanceGroup (Core.Maybe Types.InstanceType)
igInstanceType = Lens.field @"instanceType"
{-# INLINEABLE igInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | A list of configurations that were successfully applied for an instance group last time.
--
-- /Note:/ Consider using 'lastSuccessfullyAppliedConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igLastSuccessfullyAppliedConfigurations :: Lens.Lens' InstanceGroup (Core.Maybe [Types.Configuration])
igLastSuccessfullyAppliedConfigurations = Lens.field @"lastSuccessfullyAppliedConfigurations"
{-# INLINEABLE igLastSuccessfullyAppliedConfigurations #-}
{-# DEPRECATED lastSuccessfullyAppliedConfigurations "Use generic-lens or generic-optics with 'lastSuccessfullyAppliedConfigurations' instead"  #-}

-- | The version number of a configuration specification that was successfully applied for an instance group last time. 
--
-- /Note:/ Consider using 'lastSuccessfullyAppliedConfigurationsVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igLastSuccessfullyAppliedConfigurationsVersion :: Lens.Lens' InstanceGroup (Core.Maybe Core.Integer)
igLastSuccessfullyAppliedConfigurationsVersion = Lens.field @"lastSuccessfullyAppliedConfigurationsVersion"
{-# INLINEABLE igLastSuccessfullyAppliedConfigurationsVersion #-}
{-# DEPRECATED lastSuccessfullyAppliedConfigurationsVersion "Use generic-lens or generic-optics with 'lastSuccessfullyAppliedConfigurationsVersion' instead"  #-}

-- | The marketplace to provision instances for this group. Valid values are ON_DEMAND or SPOT.
--
-- /Note:/ Consider using 'market' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igMarket :: Lens.Lens' InstanceGroup (Core.Maybe Types.MarketType)
igMarket = Lens.field @"market"
{-# INLINEABLE igMarket #-}
{-# DEPRECATED market "Use generic-lens or generic-optics with 'market' instead"  #-}

-- | The name of the instance group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igName :: Lens.Lens' InstanceGroup (Core.Maybe Core.Text)
igName = Lens.field @"name"
{-# INLINEABLE igName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The target number of instances for the instance group.
--
-- /Note:/ Consider using 'requestedInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igRequestedInstanceCount :: Lens.Lens' InstanceGroup (Core.Maybe Core.Int)
igRequestedInstanceCount = Lens.field @"requestedInstanceCount"
{-# INLINEABLE igRequestedInstanceCount #-}
{-# DEPRECATED requestedInstanceCount "Use generic-lens or generic-optics with 'requestedInstanceCount' instead"  #-}

-- | The number of instances currently running in this instance group.
--
-- /Note:/ Consider using 'runningInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igRunningInstanceCount :: Lens.Lens' InstanceGroup (Core.Maybe Core.Int)
igRunningInstanceCount = Lens.field @"runningInstanceCount"
{-# INLINEABLE igRunningInstanceCount #-}
{-# DEPRECATED runningInstanceCount "Use generic-lens or generic-optics with 'runningInstanceCount' instead"  #-}

-- | Policy for customizing shrink operations.
--
-- /Note:/ Consider using 'shrinkPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igShrinkPolicy :: Lens.Lens' InstanceGroup (Core.Maybe Types.ShrinkPolicy)
igShrinkPolicy = Lens.field @"shrinkPolicy"
{-# INLINEABLE igShrinkPolicy #-}
{-# DEPRECATED shrinkPolicy "Use generic-lens or generic-optics with 'shrinkPolicy' instead"  #-}

-- | The current status of the instance group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igStatus :: Lens.Lens' InstanceGroup (Core.Maybe Types.InstanceGroupStatus)
igStatus = Lens.field @"status"
{-# INLINEABLE igStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON InstanceGroup where
        parseJSON
          = Core.withObject "InstanceGroup" Core.$
              \ x ->
                InstanceGroup' Core.<$>
                  (x Core..:? "AutoScalingPolicy") Core.<*> x Core..:? "BidPrice"
                    Core.<*> x Core..:? "Configurations"
                    Core.<*> x Core..:? "ConfigurationsVersion"
                    Core.<*> x Core..:? "EbsBlockDevices"
                    Core.<*> x Core..:? "EbsOptimized"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "InstanceGroupType"
                    Core.<*> x Core..:? "InstanceType"
                    Core.<*> x Core..:? "LastSuccessfullyAppliedConfigurations"
                    Core.<*> x Core..:? "LastSuccessfullyAppliedConfigurationsVersion"
                    Core.<*> x Core..:? "Market"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "RequestedInstanceCount"
                    Core.<*> x Core..:? "RunningInstanceCount"
                    Core.<*> x Core..:? "ShrinkPolicy"
                    Core.<*> x Core..:? "Status"
