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
-- Module      : Amazonka.EMR.Types.InstanceGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.AutoScalingPolicyDescription
import Amazonka.EMR.Types.Configuration
import Amazonka.EMR.Types.EbsBlockDevice
import Amazonka.EMR.Types.InstanceGroupStatus
import Amazonka.EMR.Types.InstanceGroupType
import Amazonka.EMR.Types.MarketType
import Amazonka.EMR.Types.ShrinkPolicy
import qualified Amazonka.Prelude as Prelude

-- | This entity represents an instance group, which is a group of instances
-- that have common purpose. For example, CORE instance group is used for
-- HDFS.
--
-- /See:/ 'newInstanceGroup' smart constructor.
data InstanceGroup = InstanceGroup'
  { -- | An automatic scaling policy for a core instance group or task instance
    -- group in an Amazon EMR cluster. The automatic scaling policy defines how
    -- an instance group dynamically adds and terminates EC2 instances in
    -- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
    autoScalingPolicy :: Prelude.Maybe AutoScalingPolicyDescription,
    -- | If specified, indicates that the instance group uses Spot Instances.
    -- This is the maximum price you are willing to pay for Spot Instances.
    -- Specify @OnDemandPrice@ to set the amount equal to the On-Demand price,
    -- or specify an amount in USD.
    bidPrice :: Prelude.Maybe Prelude.Text,
    -- | Amazon EMR releases 4.x or later.
    --
    -- The list of configurations supplied for an Amazon EMR cluster instance
    -- group. You can specify a separate configuration for each instance group
    -- (master, core, and task).
    configurations :: Prelude.Maybe [Configuration],
    -- | The version number of the requested configuration specification for this
    -- instance group.
    configurationsVersion :: Prelude.Maybe Prelude.Integer,
    -- | The custom AMI ID to use for the provisioned instance group.
    customAmiId :: Prelude.Maybe Prelude.Text,
    -- | The EBS block devices that are mapped to this instance group.
    ebsBlockDevices :: Prelude.Maybe [EbsBlockDevice],
    -- | If the instance group is EBS-optimized. An Amazon EBS-optimized instance
    -- uses an optimized configuration stack and provides additional, dedicated
    -- capacity for Amazon EBS I\/O.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the instance group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of the instance group. Valid values are MASTER, CORE or TASK.
    instanceGroupType :: Prelude.Maybe InstanceGroupType,
    -- | The EC2 instance type for all instances in the instance group.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | A list of configurations that were successfully applied for an instance
    -- group last time.
    lastSuccessfullyAppliedConfigurations :: Prelude.Maybe [Configuration],
    -- | The version number of a configuration specification that was
    -- successfully applied for an instance group last time.
    lastSuccessfullyAppliedConfigurationsVersion :: Prelude.Maybe Prelude.Integer,
    -- | The marketplace to provision instances for this group. Valid values are
    -- ON_DEMAND or SPOT.
    market :: Prelude.Maybe MarketType,
    -- | The name of the instance group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The target number of instances for the instance group.
    requestedInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The number of instances currently running in this instance group.
    runningInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | Policy for customizing shrink operations.
    shrinkPolicy :: Prelude.Maybe ShrinkPolicy,
    -- | The current status of the instance group.
    status :: Prelude.Maybe InstanceGroupStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingPolicy', 'instanceGroup_autoScalingPolicy' - An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
--
-- 'bidPrice', 'instanceGroup_bidPrice' - If specified, indicates that the instance group uses Spot Instances.
-- This is the maximum price you are willing to pay for Spot Instances.
-- Specify @OnDemandPrice@ to set the amount equal to the On-Demand price,
-- or specify an amount in USD.
--
-- 'configurations', 'instanceGroup_configurations' - Amazon EMR releases 4.x or later.
--
-- The list of configurations supplied for an Amazon EMR cluster instance
-- group. You can specify a separate configuration for each instance group
-- (master, core, and task).
--
-- 'configurationsVersion', 'instanceGroup_configurationsVersion' - The version number of the requested configuration specification for this
-- instance group.
--
-- 'customAmiId', 'instanceGroup_customAmiId' - The custom AMI ID to use for the provisioned instance group.
--
-- 'ebsBlockDevices', 'instanceGroup_ebsBlockDevices' - The EBS block devices that are mapped to this instance group.
--
-- 'ebsOptimized', 'instanceGroup_ebsOptimized' - If the instance group is EBS-optimized. An Amazon EBS-optimized instance
-- uses an optimized configuration stack and provides additional, dedicated
-- capacity for Amazon EBS I\/O.
--
-- 'id', 'instanceGroup_id' - The identifier of the instance group.
--
-- 'instanceGroupType', 'instanceGroup_instanceGroupType' - The type of the instance group. Valid values are MASTER, CORE or TASK.
--
-- 'instanceType', 'instanceGroup_instanceType' - The EC2 instance type for all instances in the instance group.
--
-- 'lastSuccessfullyAppliedConfigurations', 'instanceGroup_lastSuccessfullyAppliedConfigurations' - A list of configurations that were successfully applied for an instance
-- group last time.
--
-- 'lastSuccessfullyAppliedConfigurationsVersion', 'instanceGroup_lastSuccessfullyAppliedConfigurationsVersion' - The version number of a configuration specification that was
-- successfully applied for an instance group last time.
--
-- 'market', 'instanceGroup_market' - The marketplace to provision instances for this group. Valid values are
-- ON_DEMAND or SPOT.
--
-- 'name', 'instanceGroup_name' - The name of the instance group.
--
-- 'requestedInstanceCount', 'instanceGroup_requestedInstanceCount' - The target number of instances for the instance group.
--
-- 'runningInstanceCount', 'instanceGroup_runningInstanceCount' - The number of instances currently running in this instance group.
--
-- 'shrinkPolicy', 'instanceGroup_shrinkPolicy' - Policy for customizing shrink operations.
--
-- 'status', 'instanceGroup_status' - The current status of the instance group.
newInstanceGroup ::
  InstanceGroup
newInstanceGroup =
  InstanceGroup'
    { autoScalingPolicy = Prelude.Nothing,
      bidPrice = Prelude.Nothing,
      configurations = Prelude.Nothing,
      configurationsVersion = Prelude.Nothing,
      customAmiId = Prelude.Nothing,
      ebsBlockDevices = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      id = Prelude.Nothing,
      instanceGroupType = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      lastSuccessfullyAppliedConfigurations =
        Prelude.Nothing,
      lastSuccessfullyAppliedConfigurationsVersion =
        Prelude.Nothing,
      market = Prelude.Nothing,
      name = Prelude.Nothing,
      requestedInstanceCount = Prelude.Nothing,
      runningInstanceCount = Prelude.Nothing,
      shrinkPolicy = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
instanceGroup_autoScalingPolicy :: Lens.Lens' InstanceGroup (Prelude.Maybe AutoScalingPolicyDescription)
instanceGroup_autoScalingPolicy = Lens.lens (\InstanceGroup' {autoScalingPolicy} -> autoScalingPolicy) (\s@InstanceGroup' {} a -> s {autoScalingPolicy = a} :: InstanceGroup)

-- | If specified, indicates that the instance group uses Spot Instances.
-- This is the maximum price you are willing to pay for Spot Instances.
-- Specify @OnDemandPrice@ to set the amount equal to the On-Demand price,
-- or specify an amount in USD.
instanceGroup_bidPrice :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Text)
instanceGroup_bidPrice = Lens.lens (\InstanceGroup' {bidPrice} -> bidPrice) (\s@InstanceGroup' {} a -> s {bidPrice = a} :: InstanceGroup)

-- | Amazon EMR releases 4.x or later.
--
-- The list of configurations supplied for an Amazon EMR cluster instance
-- group. You can specify a separate configuration for each instance group
-- (master, core, and task).
instanceGroup_configurations :: Lens.Lens' InstanceGroup (Prelude.Maybe [Configuration])
instanceGroup_configurations = Lens.lens (\InstanceGroup' {configurations} -> configurations) (\s@InstanceGroup' {} a -> s {configurations = a} :: InstanceGroup) Prelude.. Lens.mapping Lens.coerced

-- | The version number of the requested configuration specification for this
-- instance group.
instanceGroup_configurationsVersion :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Integer)
instanceGroup_configurationsVersion = Lens.lens (\InstanceGroup' {configurationsVersion} -> configurationsVersion) (\s@InstanceGroup' {} a -> s {configurationsVersion = a} :: InstanceGroup)

-- | The custom AMI ID to use for the provisioned instance group.
instanceGroup_customAmiId :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Text)
instanceGroup_customAmiId = Lens.lens (\InstanceGroup' {customAmiId} -> customAmiId) (\s@InstanceGroup' {} a -> s {customAmiId = a} :: InstanceGroup)

-- | The EBS block devices that are mapped to this instance group.
instanceGroup_ebsBlockDevices :: Lens.Lens' InstanceGroup (Prelude.Maybe [EbsBlockDevice])
instanceGroup_ebsBlockDevices = Lens.lens (\InstanceGroup' {ebsBlockDevices} -> ebsBlockDevices) (\s@InstanceGroup' {} a -> s {ebsBlockDevices = a} :: InstanceGroup) Prelude.. Lens.mapping Lens.coerced

-- | If the instance group is EBS-optimized. An Amazon EBS-optimized instance
-- uses an optimized configuration stack and provides additional, dedicated
-- capacity for Amazon EBS I\/O.
instanceGroup_ebsOptimized :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Bool)
instanceGroup_ebsOptimized = Lens.lens (\InstanceGroup' {ebsOptimized} -> ebsOptimized) (\s@InstanceGroup' {} a -> s {ebsOptimized = a} :: InstanceGroup)

-- | The identifier of the instance group.
instanceGroup_id :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Text)
instanceGroup_id = Lens.lens (\InstanceGroup' {id} -> id) (\s@InstanceGroup' {} a -> s {id = a} :: InstanceGroup)

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
instanceGroup_instanceGroupType :: Lens.Lens' InstanceGroup (Prelude.Maybe InstanceGroupType)
instanceGroup_instanceGroupType = Lens.lens (\InstanceGroup' {instanceGroupType} -> instanceGroupType) (\s@InstanceGroup' {} a -> s {instanceGroupType = a} :: InstanceGroup)

-- | The EC2 instance type for all instances in the instance group.
instanceGroup_instanceType :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Text)
instanceGroup_instanceType = Lens.lens (\InstanceGroup' {instanceType} -> instanceType) (\s@InstanceGroup' {} a -> s {instanceType = a} :: InstanceGroup)

-- | A list of configurations that were successfully applied for an instance
-- group last time.
instanceGroup_lastSuccessfullyAppliedConfigurations :: Lens.Lens' InstanceGroup (Prelude.Maybe [Configuration])
instanceGroup_lastSuccessfullyAppliedConfigurations = Lens.lens (\InstanceGroup' {lastSuccessfullyAppliedConfigurations} -> lastSuccessfullyAppliedConfigurations) (\s@InstanceGroup' {} a -> s {lastSuccessfullyAppliedConfigurations = a} :: InstanceGroup) Prelude.. Lens.mapping Lens.coerced

-- | The version number of a configuration specification that was
-- successfully applied for an instance group last time.
instanceGroup_lastSuccessfullyAppliedConfigurationsVersion :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Integer)
instanceGroup_lastSuccessfullyAppliedConfigurationsVersion = Lens.lens (\InstanceGroup' {lastSuccessfullyAppliedConfigurationsVersion} -> lastSuccessfullyAppliedConfigurationsVersion) (\s@InstanceGroup' {} a -> s {lastSuccessfullyAppliedConfigurationsVersion = a} :: InstanceGroup)

-- | The marketplace to provision instances for this group. Valid values are
-- ON_DEMAND or SPOT.
instanceGroup_market :: Lens.Lens' InstanceGroup (Prelude.Maybe MarketType)
instanceGroup_market = Lens.lens (\InstanceGroup' {market} -> market) (\s@InstanceGroup' {} a -> s {market = a} :: InstanceGroup)

-- | The name of the instance group.
instanceGroup_name :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Text)
instanceGroup_name = Lens.lens (\InstanceGroup' {name} -> name) (\s@InstanceGroup' {} a -> s {name = a} :: InstanceGroup)

-- | The target number of instances for the instance group.
instanceGroup_requestedInstanceCount :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Int)
instanceGroup_requestedInstanceCount = Lens.lens (\InstanceGroup' {requestedInstanceCount} -> requestedInstanceCount) (\s@InstanceGroup' {} a -> s {requestedInstanceCount = a} :: InstanceGroup)

-- | The number of instances currently running in this instance group.
instanceGroup_runningInstanceCount :: Lens.Lens' InstanceGroup (Prelude.Maybe Prelude.Int)
instanceGroup_runningInstanceCount = Lens.lens (\InstanceGroup' {runningInstanceCount} -> runningInstanceCount) (\s@InstanceGroup' {} a -> s {runningInstanceCount = a} :: InstanceGroup)

-- | Policy for customizing shrink operations.
instanceGroup_shrinkPolicy :: Lens.Lens' InstanceGroup (Prelude.Maybe ShrinkPolicy)
instanceGroup_shrinkPolicy = Lens.lens (\InstanceGroup' {shrinkPolicy} -> shrinkPolicy) (\s@InstanceGroup' {} a -> s {shrinkPolicy = a} :: InstanceGroup)

-- | The current status of the instance group.
instanceGroup_status :: Lens.Lens' InstanceGroup (Prelude.Maybe InstanceGroupStatus)
instanceGroup_status = Lens.lens (\InstanceGroup' {status} -> status) (\s@InstanceGroup' {} a -> s {status = a} :: InstanceGroup)

instance Data.FromJSON InstanceGroup where
  parseJSON =
    Data.withObject
      "InstanceGroup"
      ( \x ->
          InstanceGroup'
            Prelude.<$> (x Data..:? "AutoScalingPolicy")
            Prelude.<*> (x Data..:? "BidPrice")
            Prelude.<*> (x Data..:? "Configurations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ConfigurationsVersion")
            Prelude.<*> (x Data..:? "CustomAmiId")
            Prelude.<*> ( x
                            Data..:? "EbsBlockDevices"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "EbsOptimized")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InstanceGroupType")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> ( x
                            Data..:? "LastSuccessfullyAppliedConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "LastSuccessfullyAppliedConfigurationsVersion"
                        )
            Prelude.<*> (x Data..:? "Market")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RequestedInstanceCount")
            Prelude.<*> (x Data..:? "RunningInstanceCount")
            Prelude.<*> (x Data..:? "ShrinkPolicy")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable InstanceGroup where
  hashWithSalt _salt InstanceGroup' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingPolicy
      `Prelude.hashWithSalt` bidPrice
      `Prelude.hashWithSalt` configurations
      `Prelude.hashWithSalt` configurationsVersion
      `Prelude.hashWithSalt` customAmiId
      `Prelude.hashWithSalt` ebsBlockDevices
      `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` instanceGroupType
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` lastSuccessfullyAppliedConfigurations
      `Prelude.hashWithSalt` lastSuccessfullyAppliedConfigurationsVersion
      `Prelude.hashWithSalt` market
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` requestedInstanceCount
      `Prelude.hashWithSalt` runningInstanceCount
      `Prelude.hashWithSalt` shrinkPolicy
      `Prelude.hashWithSalt` status

instance Prelude.NFData InstanceGroup where
  rnf InstanceGroup' {..} =
    Prelude.rnf autoScalingPolicy
      `Prelude.seq` Prelude.rnf bidPrice
      `Prelude.seq` Prelude.rnf configurations
      `Prelude.seq` Prelude.rnf configurationsVersion
      `Prelude.seq` Prelude.rnf customAmiId
      `Prelude.seq` Prelude.rnf ebsBlockDevices
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf instanceGroupType
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf
        lastSuccessfullyAppliedConfigurations
      `Prelude.seq` Prelude.rnf
        lastSuccessfullyAppliedConfigurationsVersion
      `Prelude.seq` Prelude.rnf market
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requestedInstanceCount
      `Prelude.seq` Prelude.rnf runningInstanceCount
      `Prelude.seq` Prelude.rnf shrinkPolicy
      `Prelude.seq` Prelude.rnf status
