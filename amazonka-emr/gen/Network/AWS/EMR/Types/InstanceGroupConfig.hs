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
-- Module      : Network.AWS.EMR.Types.InstanceGroupConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.AutoScalingPolicy
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EbsConfiguration
import Network.AWS.EMR.Types.InstanceRoleType
import Network.AWS.EMR.Types.MarketType
import qualified Network.AWS.Lens as Lens

-- | Configuration defining a new instance group.
--
-- /See:/ 'newInstanceGroupConfig' smart constructor.
data InstanceGroupConfig = InstanceGroupConfig'
  { -- | EBS configurations that will be attached to each EC2 instance in the
    -- instance group.
    ebsConfiguration :: Core.Maybe EbsConfiguration,
    -- | Amazon EMR releases 4.x or later.
    --
    -- The list of configurations supplied for an EMR cluster instance group.
    -- You can specify a separate configuration for each instance group
    -- (master, core, and task).
    configurations :: Core.Maybe [Configuration],
    -- | An automatic scaling policy for a core instance group or task instance
    -- group in an Amazon EMR cluster. The automatic scaling policy defines how
    -- an instance group dynamically adds and terminates EC2 instances in
    -- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
    autoScalingPolicy :: Core.Maybe AutoScalingPolicy,
    -- | The bid price for each EC2 Spot Instance as defined by @InstanceType@.
    -- Expressed in USD. If neither @BidPrice@ nor
    -- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
    -- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPrice :: Core.Maybe Core.Text,
    -- | Friendly name given to the instance group.
    name :: Core.Maybe Core.Text,
    -- | Market type of the EC2 instances used to create a cluster node.
    market :: Core.Maybe MarketType,
    -- | The role of the instance group in the cluster.
    instanceRole :: InstanceRoleType,
    -- | The EC2 instance type for all instances in the instance group.
    instanceType :: Core.Text,
    -- | Target number of instances for the instance group.
    instanceCount :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceGroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsConfiguration', 'instanceGroupConfig_ebsConfiguration' - EBS configurations that will be attached to each EC2 instance in the
-- instance group.
--
-- 'configurations', 'instanceGroupConfig_configurations' - Amazon EMR releases 4.x or later.
--
-- The list of configurations supplied for an EMR cluster instance group.
-- You can specify a separate configuration for each instance group
-- (master, core, and task).
--
-- 'autoScalingPolicy', 'instanceGroupConfig_autoScalingPolicy' - An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
--
-- 'bidPrice', 'instanceGroupConfig_bidPrice' - The bid price for each EC2 Spot Instance as defined by @InstanceType@.
-- Expressed in USD. If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- 'name', 'instanceGroupConfig_name' - Friendly name given to the instance group.
--
-- 'market', 'instanceGroupConfig_market' - Market type of the EC2 instances used to create a cluster node.
--
-- 'instanceRole', 'instanceGroupConfig_instanceRole' - The role of the instance group in the cluster.
--
-- 'instanceType', 'instanceGroupConfig_instanceType' - The EC2 instance type for all instances in the instance group.
--
-- 'instanceCount', 'instanceGroupConfig_instanceCount' - Target number of instances for the instance group.
newInstanceGroupConfig ::
  -- | 'instanceRole'
  InstanceRoleType ->
  -- | 'instanceType'
  Core.Text ->
  -- | 'instanceCount'
  Core.Int ->
  InstanceGroupConfig
newInstanceGroupConfig
  pInstanceRole_
  pInstanceType_
  pInstanceCount_ =
    InstanceGroupConfig'
      { ebsConfiguration =
          Core.Nothing,
        configurations = Core.Nothing,
        autoScalingPolicy = Core.Nothing,
        bidPrice = Core.Nothing,
        name = Core.Nothing,
        market = Core.Nothing,
        instanceRole = pInstanceRole_,
        instanceType = pInstanceType_,
        instanceCount = pInstanceCount_
      }

-- | EBS configurations that will be attached to each EC2 instance in the
-- instance group.
instanceGroupConfig_ebsConfiguration :: Lens.Lens' InstanceGroupConfig (Core.Maybe EbsConfiguration)
instanceGroupConfig_ebsConfiguration = Lens.lens (\InstanceGroupConfig' {ebsConfiguration} -> ebsConfiguration) (\s@InstanceGroupConfig' {} a -> s {ebsConfiguration = a} :: InstanceGroupConfig)

-- | Amazon EMR releases 4.x or later.
--
-- The list of configurations supplied for an EMR cluster instance group.
-- You can specify a separate configuration for each instance group
-- (master, core, and task).
instanceGroupConfig_configurations :: Lens.Lens' InstanceGroupConfig (Core.Maybe [Configuration])
instanceGroupConfig_configurations = Lens.lens (\InstanceGroupConfig' {configurations} -> configurations) (\s@InstanceGroupConfig' {} a -> s {configurations = a} :: InstanceGroupConfig) Core.. Lens.mapping Lens._Coerce

-- | An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
instanceGroupConfig_autoScalingPolicy :: Lens.Lens' InstanceGroupConfig (Core.Maybe AutoScalingPolicy)
instanceGroupConfig_autoScalingPolicy = Lens.lens (\InstanceGroupConfig' {autoScalingPolicy} -> autoScalingPolicy) (\s@InstanceGroupConfig' {} a -> s {autoScalingPolicy = a} :: InstanceGroupConfig)

-- | The bid price for each EC2 Spot Instance as defined by @InstanceType@.
-- Expressed in USD. If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
instanceGroupConfig_bidPrice :: Lens.Lens' InstanceGroupConfig (Core.Maybe Core.Text)
instanceGroupConfig_bidPrice = Lens.lens (\InstanceGroupConfig' {bidPrice} -> bidPrice) (\s@InstanceGroupConfig' {} a -> s {bidPrice = a} :: InstanceGroupConfig)

-- | Friendly name given to the instance group.
instanceGroupConfig_name :: Lens.Lens' InstanceGroupConfig (Core.Maybe Core.Text)
instanceGroupConfig_name = Lens.lens (\InstanceGroupConfig' {name} -> name) (\s@InstanceGroupConfig' {} a -> s {name = a} :: InstanceGroupConfig)

-- | Market type of the EC2 instances used to create a cluster node.
instanceGroupConfig_market :: Lens.Lens' InstanceGroupConfig (Core.Maybe MarketType)
instanceGroupConfig_market = Lens.lens (\InstanceGroupConfig' {market} -> market) (\s@InstanceGroupConfig' {} a -> s {market = a} :: InstanceGroupConfig)

-- | The role of the instance group in the cluster.
instanceGroupConfig_instanceRole :: Lens.Lens' InstanceGroupConfig InstanceRoleType
instanceGroupConfig_instanceRole = Lens.lens (\InstanceGroupConfig' {instanceRole} -> instanceRole) (\s@InstanceGroupConfig' {} a -> s {instanceRole = a} :: InstanceGroupConfig)

-- | The EC2 instance type for all instances in the instance group.
instanceGroupConfig_instanceType :: Lens.Lens' InstanceGroupConfig Core.Text
instanceGroupConfig_instanceType = Lens.lens (\InstanceGroupConfig' {instanceType} -> instanceType) (\s@InstanceGroupConfig' {} a -> s {instanceType = a} :: InstanceGroupConfig)

-- | Target number of instances for the instance group.
instanceGroupConfig_instanceCount :: Lens.Lens' InstanceGroupConfig Core.Int
instanceGroupConfig_instanceCount = Lens.lens (\InstanceGroupConfig' {instanceCount} -> instanceCount) (\s@InstanceGroupConfig' {} a -> s {instanceCount = a} :: InstanceGroupConfig)

instance Core.Hashable InstanceGroupConfig

instance Core.NFData InstanceGroupConfig

instance Core.ToJSON InstanceGroupConfig where
  toJSON InstanceGroupConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EbsConfiguration" Core..=)
              Core.<$> ebsConfiguration,
            ("Configurations" Core..=) Core.<$> configurations,
            ("AutoScalingPolicy" Core..=)
              Core.<$> autoScalingPolicy,
            ("BidPrice" Core..=) Core.<$> bidPrice,
            ("Name" Core..=) Core.<$> name,
            ("Market" Core..=) Core.<$> market,
            Core.Just ("InstanceRole" Core..= instanceRole),
            Core.Just ("InstanceType" Core..= instanceType),
            Core.Just ("InstanceCount" Core..= instanceCount)
          ]
      )
