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
-- Module      : Network.AWS.EMR.Types.InstanceGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.AutoScalingPolicyDescription
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EbsBlockDevice
import Network.AWS.EMR.Types.InstanceGroupStatus
import Network.AWS.EMR.Types.InstanceGroupType
import Network.AWS.EMR.Types.MarketType
import Network.AWS.EMR.Types.ShrinkPolicy
import qualified Network.AWS.Lens as Lens

-- | This entity represents an instance group, which is a group of instances
-- that have common purpose. For example, CORE instance group is used for
-- HDFS.
--
-- /See:/ 'newInstanceGroup' smart constructor.
data InstanceGroup = InstanceGroup'
  { -- | The version number of a configuration specification that was
    -- successfully applied for an instance group last time.
    lastSuccessfullyAppliedConfigurationsVersion :: Core.Maybe Core.Integer,
    -- | The current status of the instance group.
    status :: Core.Maybe InstanceGroupStatus,
    -- | The EC2 instance type for all instances in the instance group.
    instanceType :: Core.Maybe Core.Text,
    -- | If the instance group is EBS-optimized. An Amazon EBS-optimized instance
    -- uses an optimized configuration stack and provides additional, dedicated
    -- capacity for Amazon EBS I\/O.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The EBS block devices that are mapped to this instance group.
    ebsBlockDevices :: Core.Maybe [EbsBlockDevice],
    -- | The type of the instance group. Valid values are MASTER, CORE or TASK.
    instanceGroupType :: Core.Maybe InstanceGroupType,
    -- | Amazon EMR releases 4.x or later.
    --
    -- The list of configurations supplied for an EMR cluster instance group.
    -- You can specify a separate configuration for each instance group
    -- (master, core, and task).
    configurations :: Core.Maybe [Configuration],
    -- | Policy for customizing shrink operations.
    shrinkPolicy :: Core.Maybe ShrinkPolicy,
    -- | The identifier of the instance group.
    id :: Core.Maybe Core.Text,
    -- | A list of configurations that were successfully applied for an instance
    -- group last time.
    lastSuccessfullyAppliedConfigurations :: Core.Maybe [Configuration],
    -- | The target number of instances for the instance group.
    requestedInstanceCount :: Core.Maybe Core.Int,
    -- | An automatic scaling policy for a core instance group or task instance
    -- group in an Amazon EMR cluster. The automatic scaling policy defines how
    -- an instance group dynamically adds and terminates EC2 instances in
    -- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
    autoScalingPolicy :: Core.Maybe AutoScalingPolicyDescription,
    -- | The bid price for each EC2 Spot Instance type as defined by
    -- @InstanceType@. Expressed in USD. If neither @BidPrice@ nor
    -- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
    -- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPrice :: Core.Maybe Core.Text,
    -- | The name of the instance group.
    name :: Core.Maybe Core.Text,
    -- | The marketplace to provision instances for this group. Valid values are
    -- ON_DEMAND or SPOT.
    market :: Core.Maybe MarketType,
    -- | The version number of the requested configuration specification for this
    -- instance group.
    configurationsVersion :: Core.Maybe Core.Integer,
    -- | The number of instances currently running in this instance group.
    runningInstanceCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastSuccessfullyAppliedConfigurationsVersion', 'instanceGroup_lastSuccessfullyAppliedConfigurationsVersion' - The version number of a configuration specification that was
-- successfully applied for an instance group last time.
--
-- 'status', 'instanceGroup_status' - The current status of the instance group.
--
-- 'instanceType', 'instanceGroup_instanceType' - The EC2 instance type for all instances in the instance group.
--
-- 'ebsOptimized', 'instanceGroup_ebsOptimized' - If the instance group is EBS-optimized. An Amazon EBS-optimized instance
-- uses an optimized configuration stack and provides additional, dedicated
-- capacity for Amazon EBS I\/O.
--
-- 'ebsBlockDevices', 'instanceGroup_ebsBlockDevices' - The EBS block devices that are mapped to this instance group.
--
-- 'instanceGroupType', 'instanceGroup_instanceGroupType' - The type of the instance group. Valid values are MASTER, CORE or TASK.
--
-- 'configurations', 'instanceGroup_configurations' - Amazon EMR releases 4.x or later.
--
-- The list of configurations supplied for an EMR cluster instance group.
-- You can specify a separate configuration for each instance group
-- (master, core, and task).
--
-- 'shrinkPolicy', 'instanceGroup_shrinkPolicy' - Policy for customizing shrink operations.
--
-- 'id', 'instanceGroup_id' - The identifier of the instance group.
--
-- 'lastSuccessfullyAppliedConfigurations', 'instanceGroup_lastSuccessfullyAppliedConfigurations' - A list of configurations that were successfully applied for an instance
-- group last time.
--
-- 'requestedInstanceCount', 'instanceGroup_requestedInstanceCount' - The target number of instances for the instance group.
--
-- 'autoScalingPolicy', 'instanceGroup_autoScalingPolicy' - An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
--
-- 'bidPrice', 'instanceGroup_bidPrice' - The bid price for each EC2 Spot Instance type as defined by
-- @InstanceType@. Expressed in USD. If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- 'name', 'instanceGroup_name' - The name of the instance group.
--
-- 'market', 'instanceGroup_market' - The marketplace to provision instances for this group. Valid values are
-- ON_DEMAND or SPOT.
--
-- 'configurationsVersion', 'instanceGroup_configurationsVersion' - The version number of the requested configuration specification for this
-- instance group.
--
-- 'runningInstanceCount', 'instanceGroup_runningInstanceCount' - The number of instances currently running in this instance group.
newInstanceGroup ::
  InstanceGroup
newInstanceGroup =
  InstanceGroup'
    { lastSuccessfullyAppliedConfigurationsVersion =
        Core.Nothing,
      status = Core.Nothing,
      instanceType = Core.Nothing,
      ebsOptimized = Core.Nothing,
      ebsBlockDevices = Core.Nothing,
      instanceGroupType = Core.Nothing,
      configurations = Core.Nothing,
      shrinkPolicy = Core.Nothing,
      id = Core.Nothing,
      lastSuccessfullyAppliedConfigurations = Core.Nothing,
      requestedInstanceCount = Core.Nothing,
      autoScalingPolicy = Core.Nothing,
      bidPrice = Core.Nothing,
      name = Core.Nothing,
      market = Core.Nothing,
      configurationsVersion = Core.Nothing,
      runningInstanceCount = Core.Nothing
    }

-- | The version number of a configuration specification that was
-- successfully applied for an instance group last time.
instanceGroup_lastSuccessfullyAppliedConfigurationsVersion :: Lens.Lens' InstanceGroup (Core.Maybe Core.Integer)
instanceGroup_lastSuccessfullyAppliedConfigurationsVersion = Lens.lens (\InstanceGroup' {lastSuccessfullyAppliedConfigurationsVersion} -> lastSuccessfullyAppliedConfigurationsVersion) (\s@InstanceGroup' {} a -> s {lastSuccessfullyAppliedConfigurationsVersion = a} :: InstanceGroup)

-- | The current status of the instance group.
instanceGroup_status :: Lens.Lens' InstanceGroup (Core.Maybe InstanceGroupStatus)
instanceGroup_status = Lens.lens (\InstanceGroup' {status} -> status) (\s@InstanceGroup' {} a -> s {status = a} :: InstanceGroup)

-- | The EC2 instance type for all instances in the instance group.
instanceGroup_instanceType :: Lens.Lens' InstanceGroup (Core.Maybe Core.Text)
instanceGroup_instanceType = Lens.lens (\InstanceGroup' {instanceType} -> instanceType) (\s@InstanceGroup' {} a -> s {instanceType = a} :: InstanceGroup)

-- | If the instance group is EBS-optimized. An Amazon EBS-optimized instance
-- uses an optimized configuration stack and provides additional, dedicated
-- capacity for Amazon EBS I\/O.
instanceGroup_ebsOptimized :: Lens.Lens' InstanceGroup (Core.Maybe Core.Bool)
instanceGroup_ebsOptimized = Lens.lens (\InstanceGroup' {ebsOptimized} -> ebsOptimized) (\s@InstanceGroup' {} a -> s {ebsOptimized = a} :: InstanceGroup)

-- | The EBS block devices that are mapped to this instance group.
instanceGroup_ebsBlockDevices :: Lens.Lens' InstanceGroup (Core.Maybe [EbsBlockDevice])
instanceGroup_ebsBlockDevices = Lens.lens (\InstanceGroup' {ebsBlockDevices} -> ebsBlockDevices) (\s@InstanceGroup' {} a -> s {ebsBlockDevices = a} :: InstanceGroup) Core.. Lens.mapping Lens._Coerce

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
instanceGroup_instanceGroupType :: Lens.Lens' InstanceGroup (Core.Maybe InstanceGroupType)
instanceGroup_instanceGroupType = Lens.lens (\InstanceGroup' {instanceGroupType} -> instanceGroupType) (\s@InstanceGroup' {} a -> s {instanceGroupType = a} :: InstanceGroup)

-- | Amazon EMR releases 4.x or later.
--
-- The list of configurations supplied for an EMR cluster instance group.
-- You can specify a separate configuration for each instance group
-- (master, core, and task).
instanceGroup_configurations :: Lens.Lens' InstanceGroup (Core.Maybe [Configuration])
instanceGroup_configurations = Lens.lens (\InstanceGroup' {configurations} -> configurations) (\s@InstanceGroup' {} a -> s {configurations = a} :: InstanceGroup) Core.. Lens.mapping Lens._Coerce

-- | Policy for customizing shrink operations.
instanceGroup_shrinkPolicy :: Lens.Lens' InstanceGroup (Core.Maybe ShrinkPolicy)
instanceGroup_shrinkPolicy = Lens.lens (\InstanceGroup' {shrinkPolicy} -> shrinkPolicy) (\s@InstanceGroup' {} a -> s {shrinkPolicy = a} :: InstanceGroup)

-- | The identifier of the instance group.
instanceGroup_id :: Lens.Lens' InstanceGroup (Core.Maybe Core.Text)
instanceGroup_id = Lens.lens (\InstanceGroup' {id} -> id) (\s@InstanceGroup' {} a -> s {id = a} :: InstanceGroup)

-- | A list of configurations that were successfully applied for an instance
-- group last time.
instanceGroup_lastSuccessfullyAppliedConfigurations :: Lens.Lens' InstanceGroup (Core.Maybe [Configuration])
instanceGroup_lastSuccessfullyAppliedConfigurations = Lens.lens (\InstanceGroup' {lastSuccessfullyAppliedConfigurations} -> lastSuccessfullyAppliedConfigurations) (\s@InstanceGroup' {} a -> s {lastSuccessfullyAppliedConfigurations = a} :: InstanceGroup) Core.. Lens.mapping Lens._Coerce

-- | The target number of instances for the instance group.
instanceGroup_requestedInstanceCount :: Lens.Lens' InstanceGroup (Core.Maybe Core.Int)
instanceGroup_requestedInstanceCount = Lens.lens (\InstanceGroup' {requestedInstanceCount} -> requestedInstanceCount) (\s@InstanceGroup' {} a -> s {requestedInstanceCount = a} :: InstanceGroup)

-- | An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
instanceGroup_autoScalingPolicy :: Lens.Lens' InstanceGroup (Core.Maybe AutoScalingPolicyDescription)
instanceGroup_autoScalingPolicy = Lens.lens (\InstanceGroup' {autoScalingPolicy} -> autoScalingPolicy) (\s@InstanceGroup' {} a -> s {autoScalingPolicy = a} :: InstanceGroup)

-- | The bid price for each EC2 Spot Instance type as defined by
-- @InstanceType@. Expressed in USD. If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
instanceGroup_bidPrice :: Lens.Lens' InstanceGroup (Core.Maybe Core.Text)
instanceGroup_bidPrice = Lens.lens (\InstanceGroup' {bidPrice} -> bidPrice) (\s@InstanceGroup' {} a -> s {bidPrice = a} :: InstanceGroup)

-- | The name of the instance group.
instanceGroup_name :: Lens.Lens' InstanceGroup (Core.Maybe Core.Text)
instanceGroup_name = Lens.lens (\InstanceGroup' {name} -> name) (\s@InstanceGroup' {} a -> s {name = a} :: InstanceGroup)

-- | The marketplace to provision instances for this group. Valid values are
-- ON_DEMAND or SPOT.
instanceGroup_market :: Lens.Lens' InstanceGroup (Core.Maybe MarketType)
instanceGroup_market = Lens.lens (\InstanceGroup' {market} -> market) (\s@InstanceGroup' {} a -> s {market = a} :: InstanceGroup)

-- | The version number of the requested configuration specification for this
-- instance group.
instanceGroup_configurationsVersion :: Lens.Lens' InstanceGroup (Core.Maybe Core.Integer)
instanceGroup_configurationsVersion = Lens.lens (\InstanceGroup' {configurationsVersion} -> configurationsVersion) (\s@InstanceGroup' {} a -> s {configurationsVersion = a} :: InstanceGroup)

-- | The number of instances currently running in this instance group.
instanceGroup_runningInstanceCount :: Lens.Lens' InstanceGroup (Core.Maybe Core.Int)
instanceGroup_runningInstanceCount = Lens.lens (\InstanceGroup' {runningInstanceCount} -> runningInstanceCount) (\s@InstanceGroup' {} a -> s {runningInstanceCount = a} :: InstanceGroup)

instance Core.FromJSON InstanceGroup where
  parseJSON =
    Core.withObject
      "InstanceGroup"
      ( \x ->
          InstanceGroup'
            Core.<$> ( x
                         Core..:? "LastSuccessfullyAppliedConfigurationsVersion"
                     )
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "InstanceType")
            Core.<*> (x Core..:? "EbsOptimized")
            Core.<*> (x Core..:? "EbsBlockDevices" Core..!= Core.mempty)
            Core.<*> (x Core..:? "InstanceGroupType")
            Core.<*> (x Core..:? "Configurations" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ShrinkPolicy")
            Core.<*> (x Core..:? "Id")
            Core.<*> ( x Core..:? "LastSuccessfullyAppliedConfigurations"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "RequestedInstanceCount")
            Core.<*> (x Core..:? "AutoScalingPolicy")
            Core.<*> (x Core..:? "BidPrice")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Market")
            Core.<*> (x Core..:? "ConfigurationsVersion")
            Core.<*> (x Core..:? "RunningInstanceCount")
      )

instance Core.Hashable InstanceGroup

instance Core.NFData InstanceGroup
