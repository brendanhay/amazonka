{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EMR.Types.AutoScalingPolicyDescription
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EbsBlockDevice
import Network.AWS.EMR.Types.InstanceGroupStatus
import Network.AWS.EMR.Types.InstanceGroupType
import Network.AWS.EMR.Types.MarketType
import Network.AWS.EMR.Types.ShrinkPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This entity represents an instance group, which is a group of instances
-- that have common purpose. For example, CORE instance group is used for
-- HDFS.
--
-- /See:/ 'newInstanceGroup' smart constructor.
data InstanceGroup = InstanceGroup'
  { -- | The version number of a configuration specification that was
    -- successfully applied for an instance group last time.
    lastSuccessfullyAppliedConfigurationsVersion :: Prelude.Maybe Prelude.Integer,
    -- | The current status of the instance group.
    status :: Prelude.Maybe InstanceGroupStatus,
    -- | The EC2 instance type for all instances in the instance group.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | If the instance group is EBS-optimized. An Amazon EBS-optimized instance
    -- uses an optimized configuration stack and provides additional, dedicated
    -- capacity for Amazon EBS I\/O.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The EBS block devices that are mapped to this instance group.
    ebsBlockDevices :: Prelude.Maybe [EbsBlockDevice],
    -- | The type of the instance group. Valid values are MASTER, CORE or TASK.
    instanceGroupType :: Prelude.Maybe InstanceGroupType,
    -- | Amazon EMR releases 4.x or later.
    --
    -- The list of configurations supplied for an EMR cluster instance group.
    -- You can specify a separate configuration for each instance group
    -- (master, core, and task).
    configurations :: Prelude.Maybe [Configuration],
    -- | Policy for customizing shrink operations.
    shrinkPolicy :: Prelude.Maybe ShrinkPolicy,
    -- | The identifier of the instance group.
    id :: Prelude.Maybe Prelude.Text,
    -- | A list of configurations that were successfully applied for an instance
    -- group last time.
    lastSuccessfullyAppliedConfigurations :: Prelude.Maybe [Configuration],
    -- | The target number of instances for the instance group.
    requestedInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | An automatic scaling policy for a core instance group or task instance
    -- group in an Amazon EMR cluster. The automatic scaling policy defines how
    -- an instance group dynamically adds and terminates EC2 instances in
    -- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
    autoScalingPolicy :: Prelude.Maybe AutoScalingPolicyDescription,
    -- | The bid price for each EC2 Spot Instance type as defined by
    -- @InstanceType@. Expressed in USD. If neither @BidPrice@ nor
    -- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
    -- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPrice :: Prelude.Maybe Prelude.Text,
    -- | The name of the instance group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The marketplace to provision instances for this group. Valid values are
    -- ON_DEMAND or SPOT.
    market :: Prelude.Maybe MarketType,
    -- | The version number of the requested configuration specification for this
    -- instance group.
    configurationsVersion :: Prelude.Maybe Prelude.Integer,
    -- | The number of instances currently running in this instance group.
    runningInstanceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      status = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      ebsBlockDevices = Prelude.Nothing,
      instanceGroupType = Prelude.Nothing,
      configurations = Prelude.Nothing,
      shrinkPolicy = Prelude.Nothing,
      id = Prelude.Nothing,
      lastSuccessfullyAppliedConfigurations =
        Prelude.Nothing,
      requestedInstanceCount = Prelude.Nothing,
      autoScalingPolicy = Prelude.Nothing,
      bidPrice = Prelude.Nothing,
      name = Prelude.Nothing,
      market = Prelude.Nothing,
      configurationsVersion = Prelude.Nothing,
      runningInstanceCount = Prelude.Nothing
    }

-- | The version number of a configuration specification that was
-- successfully applied for an instance group last time.
instanceGroup_lastSuccessfullyAppliedConfigurationsVersion :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Integer)
instanceGroup_lastSuccessfullyAppliedConfigurationsVersion = Lens.lens (\InstanceGroup' {lastSuccessfullyAppliedConfigurationsVersion} -> lastSuccessfullyAppliedConfigurationsVersion) (\s@InstanceGroup' {} a -> s {lastSuccessfullyAppliedConfigurationsVersion = a} :: InstanceGroup)

-- | The current status of the instance group.
instanceGroup_status :: Lens.Lens' InstanceGroup (Prelude.Maybe InstanceGroupStatus)
instanceGroup_status = Lens.lens (\InstanceGroup' {status} -> status) (\s@InstanceGroup' {} a -> s {status = a} :: InstanceGroup)

-- | The EC2 instance type for all instances in the instance group.
instanceGroup_instanceType :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Text)
instanceGroup_instanceType = Lens.lens (\InstanceGroup' {instanceType} -> instanceType) (\s@InstanceGroup' {} a -> s {instanceType = a} :: InstanceGroup)

-- | If the instance group is EBS-optimized. An Amazon EBS-optimized instance
-- uses an optimized configuration stack and provides additional, dedicated
-- capacity for Amazon EBS I\/O.
instanceGroup_ebsOptimized :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Bool)
instanceGroup_ebsOptimized = Lens.lens (\InstanceGroup' {ebsOptimized} -> ebsOptimized) (\s@InstanceGroup' {} a -> s {ebsOptimized = a} :: InstanceGroup)

-- | The EBS block devices that are mapped to this instance group.
instanceGroup_ebsBlockDevices :: Lens.Lens' InstanceGroup (Prelude.Maybe [EbsBlockDevice])
instanceGroup_ebsBlockDevices = Lens.lens (\InstanceGroup' {ebsBlockDevices} -> ebsBlockDevices) (\s@InstanceGroup' {} a -> s {ebsBlockDevices = a} :: InstanceGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
instanceGroup_instanceGroupType :: Lens.Lens' InstanceGroup (Prelude.Maybe InstanceGroupType)
instanceGroup_instanceGroupType = Lens.lens (\InstanceGroup' {instanceGroupType} -> instanceGroupType) (\s@InstanceGroup' {} a -> s {instanceGroupType = a} :: InstanceGroup)

-- | Amazon EMR releases 4.x or later.
--
-- The list of configurations supplied for an EMR cluster instance group.
-- You can specify a separate configuration for each instance group
-- (master, core, and task).
instanceGroup_configurations :: Lens.Lens' InstanceGroup (Prelude.Maybe [Configuration])
instanceGroup_configurations = Lens.lens (\InstanceGroup' {configurations} -> configurations) (\s@InstanceGroup' {} a -> s {configurations = a} :: InstanceGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | Policy for customizing shrink operations.
instanceGroup_shrinkPolicy :: Lens.Lens' InstanceGroup (Prelude.Maybe ShrinkPolicy)
instanceGroup_shrinkPolicy = Lens.lens (\InstanceGroup' {shrinkPolicy} -> shrinkPolicy) (\s@InstanceGroup' {} a -> s {shrinkPolicy = a} :: InstanceGroup)

-- | The identifier of the instance group.
instanceGroup_id :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Text)
instanceGroup_id = Lens.lens (\InstanceGroup' {id} -> id) (\s@InstanceGroup' {} a -> s {id = a} :: InstanceGroup)

-- | A list of configurations that were successfully applied for an instance
-- group last time.
instanceGroup_lastSuccessfullyAppliedConfigurations :: Lens.Lens' InstanceGroup (Prelude.Maybe [Configuration])
instanceGroup_lastSuccessfullyAppliedConfigurations = Lens.lens (\InstanceGroup' {lastSuccessfullyAppliedConfigurations} -> lastSuccessfullyAppliedConfigurations) (\s@InstanceGroup' {} a -> s {lastSuccessfullyAppliedConfigurations = a} :: InstanceGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The target number of instances for the instance group.
instanceGroup_requestedInstanceCount :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Int)
instanceGroup_requestedInstanceCount = Lens.lens (\InstanceGroup' {requestedInstanceCount} -> requestedInstanceCount) (\s@InstanceGroup' {} a -> s {requestedInstanceCount = a} :: InstanceGroup)

-- | An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
instanceGroup_autoScalingPolicy :: Lens.Lens' InstanceGroup (Prelude.Maybe AutoScalingPolicyDescription)
instanceGroup_autoScalingPolicy = Lens.lens (\InstanceGroup' {autoScalingPolicy} -> autoScalingPolicy) (\s@InstanceGroup' {} a -> s {autoScalingPolicy = a} :: InstanceGroup)

-- | The bid price for each EC2 Spot Instance type as defined by
-- @InstanceType@. Expressed in USD. If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
instanceGroup_bidPrice :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Text)
instanceGroup_bidPrice = Lens.lens (\InstanceGroup' {bidPrice} -> bidPrice) (\s@InstanceGroup' {} a -> s {bidPrice = a} :: InstanceGroup)

-- | The name of the instance group.
instanceGroup_name :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Text)
instanceGroup_name = Lens.lens (\InstanceGroup' {name} -> name) (\s@InstanceGroup' {} a -> s {name = a} :: InstanceGroup)

-- | The marketplace to provision instances for this group. Valid values are
-- ON_DEMAND or SPOT.
instanceGroup_market :: Lens.Lens' InstanceGroup (Prelude.Maybe MarketType)
instanceGroup_market = Lens.lens (\InstanceGroup' {market} -> market) (\s@InstanceGroup' {} a -> s {market = a} :: InstanceGroup)

-- | The version number of the requested configuration specification for this
-- instance group.
instanceGroup_configurationsVersion :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Integer)
instanceGroup_configurationsVersion = Lens.lens (\InstanceGroup' {configurationsVersion} -> configurationsVersion) (\s@InstanceGroup' {} a -> s {configurationsVersion = a} :: InstanceGroup)

-- | The number of instances currently running in this instance group.
instanceGroup_runningInstanceCount :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Int)
instanceGroup_runningInstanceCount = Lens.lens (\InstanceGroup' {runningInstanceCount} -> runningInstanceCount) (\s@InstanceGroup' {} a -> s {runningInstanceCount = a} :: InstanceGroup)

instance Prelude.FromJSON InstanceGroup where
  parseJSON =
    Prelude.withObject
      "InstanceGroup"
      ( \x ->
          InstanceGroup'
            Prelude.<$> ( x
                            Prelude..:? "LastSuccessfullyAppliedConfigurationsVersion"
                        )
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "InstanceType")
            Prelude.<*> (x Prelude..:? "EbsOptimized")
            Prelude.<*> ( x Prelude..:? "EbsBlockDevices"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "InstanceGroupType")
            Prelude.<*> ( x Prelude..:? "Configurations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ShrinkPolicy")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> ( x
                            Prelude..:? "LastSuccessfullyAppliedConfigurations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "RequestedInstanceCount")
            Prelude.<*> (x Prelude..:? "AutoScalingPolicy")
            Prelude.<*> (x Prelude..:? "BidPrice")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Market")
            Prelude.<*> (x Prelude..:? "ConfigurationsVersion")
            Prelude.<*> (x Prelude..:? "RunningInstanceCount")
      )

instance Prelude.Hashable InstanceGroup

instance Prelude.NFData InstanceGroup
