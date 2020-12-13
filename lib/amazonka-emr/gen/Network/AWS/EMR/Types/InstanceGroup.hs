{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroup
  ( InstanceGroup (..),

    -- * Smart constructor
    mkInstanceGroup,

    -- * Lenses
    igStatus,
    igLastSuccessfullyAppliedConfigurationsVersion,
    igBidPrice,
    igRequestedInstanceCount,
    igRunningInstanceCount,
    igLastSuccessfullyAppliedConfigurations,
    igConfigurations,
    igInstanceGroupType,
    igEBSBlockDevices,
    igInstanceType,
    igConfigurationsVersion,
    igEBSOptimized,
    igMarket,
    igName,
    igAutoScalingPolicy,
    igShrinkPolicy,
    igId,
  )
where

import Network.AWS.EMR.Types.AutoScalingPolicyDescription
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EBSBlockDevice
import Network.AWS.EMR.Types.InstanceGroupStatus
import Network.AWS.EMR.Types.InstanceGroupType
import Network.AWS.EMR.Types.MarketType
import Network.AWS.EMR.Types.ShrinkPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This entity represents an instance group, which is a group of instances that have common purpose. For example, CORE instance group is used for HDFS.
--
-- /See:/ 'mkInstanceGroup' smart constructor.
data InstanceGroup = InstanceGroup'
  { -- | The current status of the instance group.
    status :: Lude.Maybe InstanceGroupStatus,
    -- | The version number of a configuration specification that was successfully applied for an instance group last time.
    lastSuccessfullyAppliedConfigurationsVersion :: Lude.Maybe Lude.Integer,
    -- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPrice :: Lude.Maybe Lude.Text,
    -- | The target number of instances for the instance group.
    requestedInstanceCount :: Lude.Maybe Lude.Int,
    -- | The number of instances currently running in this instance group.
    runningInstanceCount :: Lude.Maybe Lude.Int,
    -- | A list of configurations that were successfully applied for an instance group last time.
    lastSuccessfullyAppliedConfigurations :: Lude.Maybe [Configuration],
    -- | The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
    configurations :: Lude.Maybe [Configuration],
    -- | The type of the instance group. Valid values are MASTER, CORE or TASK.
    instanceGroupType :: Lude.Maybe InstanceGroupType,
    -- | The EBS block devices that are mapped to this instance group.
    ebsBlockDevices :: Lude.Maybe [EBSBlockDevice],
    -- | The EC2 instance type for all instances in the instance group.
    instanceType :: Lude.Maybe Lude.Text,
    -- | The version number of the requested configuration specification for this instance group.
    configurationsVersion :: Lude.Maybe Lude.Integer,
    -- | If the instance group is EBS-optimized. An Amazon EBS-optimized instance uses an optimized configuration stack and provides additional, dedicated capacity for Amazon EBS I/O.
    ebsOptimized :: Lude.Maybe Lude.Bool,
    -- | The marketplace to provision instances for this group. Valid values are ON_DEMAND or SPOT.
    market :: Lude.Maybe MarketType,
    -- | The name of the instance group.
    name :: Lude.Maybe Lude.Text,
    -- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
    autoScalingPolicy :: Lude.Maybe AutoScalingPolicyDescription,
    -- | Policy for customizing shrink operations.
    shrinkPolicy :: Lude.Maybe ShrinkPolicy,
    -- | The identifier of the instance group.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceGroup' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the instance group.
-- * 'lastSuccessfullyAppliedConfigurationsVersion' - The version number of a configuration specification that was successfully applied for an instance group last time.
-- * 'bidPrice' - The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
-- * 'requestedInstanceCount' - The target number of instances for the instance group.
-- * 'runningInstanceCount' - The number of instances currently running in this instance group.
-- * 'lastSuccessfullyAppliedConfigurations' - A list of configurations that were successfully applied for an instance group last time.
-- * 'configurations' - The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
-- * 'instanceGroupType' - The type of the instance group. Valid values are MASTER, CORE or TASK.
-- * 'ebsBlockDevices' - The EBS block devices that are mapped to this instance group.
-- * 'instanceType' - The EC2 instance type for all instances in the instance group.
-- * 'configurationsVersion' - The version number of the requested configuration specification for this instance group.
-- * 'ebsOptimized' - If the instance group is EBS-optimized. An Amazon EBS-optimized instance uses an optimized configuration stack and provides additional, dedicated capacity for Amazon EBS I/O.
-- * 'market' - The marketplace to provision instances for this group. Valid values are ON_DEMAND or SPOT.
-- * 'name' - The name of the instance group.
-- * 'autoScalingPolicy' - An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
-- * 'shrinkPolicy' - Policy for customizing shrink operations.
-- * 'id' - The identifier of the instance group.
mkInstanceGroup ::
  InstanceGroup
mkInstanceGroup =
  InstanceGroup'
    { status = Lude.Nothing,
      lastSuccessfullyAppliedConfigurationsVersion = Lude.Nothing,
      bidPrice = Lude.Nothing,
      requestedInstanceCount = Lude.Nothing,
      runningInstanceCount = Lude.Nothing,
      lastSuccessfullyAppliedConfigurations = Lude.Nothing,
      configurations = Lude.Nothing,
      instanceGroupType = Lude.Nothing,
      ebsBlockDevices = Lude.Nothing,
      instanceType = Lude.Nothing,
      configurationsVersion = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      market = Lude.Nothing,
      name = Lude.Nothing,
      autoScalingPolicy = Lude.Nothing,
      shrinkPolicy = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The current status of the instance group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igStatus :: Lens.Lens' InstanceGroup (Lude.Maybe InstanceGroupStatus)
igStatus = Lens.lens (status :: InstanceGroup -> Lude.Maybe InstanceGroupStatus) (\s a -> s {status = a} :: InstanceGroup)
{-# DEPRECATED igStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The version number of a configuration specification that was successfully applied for an instance group last time.
--
-- /Note:/ Consider using 'lastSuccessfullyAppliedConfigurationsVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igLastSuccessfullyAppliedConfigurationsVersion :: Lens.Lens' InstanceGroup (Lude.Maybe Lude.Integer)
igLastSuccessfullyAppliedConfigurationsVersion = Lens.lens (lastSuccessfullyAppliedConfigurationsVersion :: InstanceGroup -> Lude.Maybe Lude.Integer) (\s a -> s {lastSuccessfullyAppliedConfigurationsVersion = a} :: InstanceGroup)
{-# DEPRECATED igLastSuccessfullyAppliedConfigurationsVersion "Use generic-lens or generic-optics with 'lastSuccessfullyAppliedConfigurationsVersion' instead." #-}

-- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- /Note:/ Consider using 'bidPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igBidPrice :: Lens.Lens' InstanceGroup (Lude.Maybe Lude.Text)
igBidPrice = Lens.lens (bidPrice :: InstanceGroup -> Lude.Maybe Lude.Text) (\s a -> s {bidPrice = a} :: InstanceGroup)
{-# DEPRECATED igBidPrice "Use generic-lens or generic-optics with 'bidPrice' instead." #-}

-- | The target number of instances for the instance group.
--
-- /Note:/ Consider using 'requestedInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igRequestedInstanceCount :: Lens.Lens' InstanceGroup (Lude.Maybe Lude.Int)
igRequestedInstanceCount = Lens.lens (requestedInstanceCount :: InstanceGroup -> Lude.Maybe Lude.Int) (\s a -> s {requestedInstanceCount = a} :: InstanceGroup)
{-# DEPRECATED igRequestedInstanceCount "Use generic-lens or generic-optics with 'requestedInstanceCount' instead." #-}

-- | The number of instances currently running in this instance group.
--
-- /Note:/ Consider using 'runningInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igRunningInstanceCount :: Lens.Lens' InstanceGroup (Lude.Maybe Lude.Int)
igRunningInstanceCount = Lens.lens (runningInstanceCount :: InstanceGroup -> Lude.Maybe Lude.Int) (\s a -> s {runningInstanceCount = a} :: InstanceGroup)
{-# DEPRECATED igRunningInstanceCount "Use generic-lens or generic-optics with 'runningInstanceCount' instead." #-}

-- | A list of configurations that were successfully applied for an instance group last time.
--
-- /Note:/ Consider using 'lastSuccessfullyAppliedConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igLastSuccessfullyAppliedConfigurations :: Lens.Lens' InstanceGroup (Lude.Maybe [Configuration])
igLastSuccessfullyAppliedConfigurations = Lens.lens (lastSuccessfullyAppliedConfigurations :: InstanceGroup -> Lude.Maybe [Configuration]) (\s a -> s {lastSuccessfullyAppliedConfigurations = a} :: InstanceGroup)
{-# DEPRECATED igLastSuccessfullyAppliedConfigurations "Use generic-lens or generic-optics with 'lastSuccessfullyAppliedConfigurations' instead." #-}

-- | The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igConfigurations :: Lens.Lens' InstanceGroup (Lude.Maybe [Configuration])
igConfigurations = Lens.lens (configurations :: InstanceGroup -> Lude.Maybe [Configuration]) (\s a -> s {configurations = a} :: InstanceGroup)
{-# DEPRECATED igConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
--
-- /Note:/ Consider using 'instanceGroupType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igInstanceGroupType :: Lens.Lens' InstanceGroup (Lude.Maybe InstanceGroupType)
igInstanceGroupType = Lens.lens (instanceGroupType :: InstanceGroup -> Lude.Maybe InstanceGroupType) (\s a -> s {instanceGroupType = a} :: InstanceGroup)
{-# DEPRECATED igInstanceGroupType "Use generic-lens or generic-optics with 'instanceGroupType' instead." #-}

-- | The EBS block devices that are mapped to this instance group.
--
-- /Note:/ Consider using 'ebsBlockDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igEBSBlockDevices :: Lens.Lens' InstanceGroup (Lude.Maybe [EBSBlockDevice])
igEBSBlockDevices = Lens.lens (ebsBlockDevices :: InstanceGroup -> Lude.Maybe [EBSBlockDevice]) (\s a -> s {ebsBlockDevices = a} :: InstanceGroup)
{-# DEPRECATED igEBSBlockDevices "Use generic-lens or generic-optics with 'ebsBlockDevices' instead." #-}

-- | The EC2 instance type for all instances in the instance group.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igInstanceType :: Lens.Lens' InstanceGroup (Lude.Maybe Lude.Text)
igInstanceType = Lens.lens (instanceType :: InstanceGroup -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: InstanceGroup)
{-# DEPRECATED igInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The version number of the requested configuration specification for this instance group.
--
-- /Note:/ Consider using 'configurationsVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igConfigurationsVersion :: Lens.Lens' InstanceGroup (Lude.Maybe Lude.Integer)
igConfigurationsVersion = Lens.lens (configurationsVersion :: InstanceGroup -> Lude.Maybe Lude.Integer) (\s a -> s {configurationsVersion = a} :: InstanceGroup)
{-# DEPRECATED igConfigurationsVersion "Use generic-lens or generic-optics with 'configurationsVersion' instead." #-}

-- | If the instance group is EBS-optimized. An Amazon EBS-optimized instance uses an optimized configuration stack and provides additional, dedicated capacity for Amazon EBS I/O.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igEBSOptimized :: Lens.Lens' InstanceGroup (Lude.Maybe Lude.Bool)
igEBSOptimized = Lens.lens (ebsOptimized :: InstanceGroup -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: InstanceGroup)
{-# DEPRECATED igEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The marketplace to provision instances for this group. Valid values are ON_DEMAND or SPOT.
--
-- /Note:/ Consider using 'market' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igMarket :: Lens.Lens' InstanceGroup (Lude.Maybe MarketType)
igMarket = Lens.lens (market :: InstanceGroup -> Lude.Maybe MarketType) (\s a -> s {market = a} :: InstanceGroup)
{-# DEPRECATED igMarket "Use generic-lens or generic-optics with 'market' instead." #-}

-- | The name of the instance group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igName :: Lens.Lens' InstanceGroup (Lude.Maybe Lude.Text)
igName = Lens.lens (name :: InstanceGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InstanceGroup)
{-# DEPRECATED igName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
--
-- /Note:/ Consider using 'autoScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igAutoScalingPolicy :: Lens.Lens' InstanceGroup (Lude.Maybe AutoScalingPolicyDescription)
igAutoScalingPolicy = Lens.lens (autoScalingPolicy :: InstanceGroup -> Lude.Maybe AutoScalingPolicyDescription) (\s a -> s {autoScalingPolicy = a} :: InstanceGroup)
{-# DEPRECATED igAutoScalingPolicy "Use generic-lens or generic-optics with 'autoScalingPolicy' instead." #-}

-- | Policy for customizing shrink operations.
--
-- /Note:/ Consider using 'shrinkPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igShrinkPolicy :: Lens.Lens' InstanceGroup (Lude.Maybe ShrinkPolicy)
igShrinkPolicy = Lens.lens (shrinkPolicy :: InstanceGroup -> Lude.Maybe ShrinkPolicy) (\s a -> s {shrinkPolicy = a} :: InstanceGroup)
{-# DEPRECATED igShrinkPolicy "Use generic-lens or generic-optics with 'shrinkPolicy' instead." #-}

-- | The identifier of the instance group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igId :: Lens.Lens' InstanceGroup (Lude.Maybe Lude.Text)
igId = Lens.lens (id :: InstanceGroup -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: InstanceGroup)
{-# DEPRECATED igId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON InstanceGroup where
  parseJSON =
    Lude.withObject
      "InstanceGroup"
      ( \x ->
          InstanceGroup'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LastSuccessfullyAppliedConfigurationsVersion")
            Lude.<*> (x Lude..:? "BidPrice")
            Lude.<*> (x Lude..:? "RequestedInstanceCount")
            Lude.<*> (x Lude..:? "RunningInstanceCount")
            Lude.<*> ( x Lude..:? "LastSuccessfullyAppliedConfigurations"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "Configurations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InstanceGroupType")
            Lude.<*> (x Lude..:? "EbsBlockDevices" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "ConfigurationsVersion")
            Lude.<*> (x Lude..:? "EbsOptimized")
            Lude.<*> (x Lude..:? "Market")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "AutoScalingPolicy")
            Lude.<*> (x Lude..:? "ShrinkPolicy")
            Lude.<*> (x Lude..:? "Id")
      )
