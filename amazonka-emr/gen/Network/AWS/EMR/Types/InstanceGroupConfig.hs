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
-- Module      : Network.AWS.EMR.Types.InstanceGroupConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupConfig where

import Network.AWS.EMR.Types.AutoScalingPolicy
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EbsConfiguration
import Network.AWS.EMR.Types.InstanceRoleType
import Network.AWS.EMR.Types.MarketType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration defining a new instance group.
--
-- /See:/ 'newInstanceGroupConfig' smart constructor.
data InstanceGroupConfig = InstanceGroupConfig'
  { -- | EBS configurations that will be attached to each EC2 instance in the
    -- instance group.
    ebsConfiguration :: Prelude.Maybe EbsConfiguration,
    -- | Amazon EMR releases 4.x or later.
    --
    -- The list of configurations supplied for an EMR cluster instance group.
    -- You can specify a separate configuration for each instance group
    -- (master, core, and task).
    configurations :: Prelude.Maybe [Configuration],
    -- | An automatic scaling policy for a core instance group or task instance
    -- group in an Amazon EMR cluster. The automatic scaling policy defines how
    -- an instance group dynamically adds and terminates EC2 instances in
    -- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
    autoScalingPolicy :: Prelude.Maybe AutoScalingPolicy,
    -- | The bid price for each EC2 Spot Instance as defined by @InstanceType@.
    -- Expressed in USD. If neither @BidPrice@ nor
    -- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
    -- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPrice :: Prelude.Maybe Prelude.Text,
    -- | Friendly name given to the instance group.
    name :: Prelude.Maybe Prelude.Text,
    -- | Market type of the EC2 instances used to create a cluster node.
    market :: Prelude.Maybe MarketType,
    -- | The role of the instance group in the cluster.
    instanceRole :: InstanceRoleType,
    -- | The EC2 instance type for all instances in the instance group.
    instanceType :: Prelude.Text,
    -- | Target number of instances for the instance group.
    instanceCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'instanceCount'
  Prelude.Int ->
  InstanceGroupConfig
newInstanceGroupConfig
  pInstanceRole_
  pInstanceType_
  pInstanceCount_ =
    InstanceGroupConfig'
      { ebsConfiguration =
          Prelude.Nothing,
        configurations = Prelude.Nothing,
        autoScalingPolicy = Prelude.Nothing,
        bidPrice = Prelude.Nothing,
        name = Prelude.Nothing,
        market = Prelude.Nothing,
        instanceRole = pInstanceRole_,
        instanceType = pInstanceType_,
        instanceCount = pInstanceCount_
      }

-- | EBS configurations that will be attached to each EC2 instance in the
-- instance group.
instanceGroupConfig_ebsConfiguration :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe EbsConfiguration)
instanceGroupConfig_ebsConfiguration = Lens.lens (\InstanceGroupConfig' {ebsConfiguration} -> ebsConfiguration) (\s@InstanceGroupConfig' {} a -> s {ebsConfiguration = a} :: InstanceGroupConfig)

-- | Amazon EMR releases 4.x or later.
--
-- The list of configurations supplied for an EMR cluster instance group.
-- You can specify a separate configuration for each instance group
-- (master, core, and task).
instanceGroupConfig_configurations :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe [Configuration])
instanceGroupConfig_configurations = Lens.lens (\InstanceGroupConfig' {configurations} -> configurations) (\s@InstanceGroupConfig' {} a -> s {configurations = a} :: InstanceGroupConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
instanceGroupConfig_autoScalingPolicy :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe AutoScalingPolicy)
instanceGroupConfig_autoScalingPolicy = Lens.lens (\InstanceGroupConfig' {autoScalingPolicy} -> autoScalingPolicy) (\s@InstanceGroupConfig' {} a -> s {autoScalingPolicy = a} :: InstanceGroupConfig)

-- | The bid price for each EC2 Spot Instance as defined by @InstanceType@.
-- Expressed in USD. If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
instanceGroupConfig_bidPrice :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe Prelude.Text)
instanceGroupConfig_bidPrice = Lens.lens (\InstanceGroupConfig' {bidPrice} -> bidPrice) (\s@InstanceGroupConfig' {} a -> s {bidPrice = a} :: InstanceGroupConfig)

-- | Friendly name given to the instance group.
instanceGroupConfig_name :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe Prelude.Text)
instanceGroupConfig_name = Lens.lens (\InstanceGroupConfig' {name} -> name) (\s@InstanceGroupConfig' {} a -> s {name = a} :: InstanceGroupConfig)

-- | Market type of the EC2 instances used to create a cluster node.
instanceGroupConfig_market :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe MarketType)
instanceGroupConfig_market = Lens.lens (\InstanceGroupConfig' {market} -> market) (\s@InstanceGroupConfig' {} a -> s {market = a} :: InstanceGroupConfig)

-- | The role of the instance group in the cluster.
instanceGroupConfig_instanceRole :: Lens.Lens' InstanceGroupConfig InstanceRoleType
instanceGroupConfig_instanceRole = Lens.lens (\InstanceGroupConfig' {instanceRole} -> instanceRole) (\s@InstanceGroupConfig' {} a -> s {instanceRole = a} :: InstanceGroupConfig)

-- | The EC2 instance type for all instances in the instance group.
instanceGroupConfig_instanceType :: Lens.Lens' InstanceGroupConfig Prelude.Text
instanceGroupConfig_instanceType = Lens.lens (\InstanceGroupConfig' {instanceType} -> instanceType) (\s@InstanceGroupConfig' {} a -> s {instanceType = a} :: InstanceGroupConfig)

-- | Target number of instances for the instance group.
instanceGroupConfig_instanceCount :: Lens.Lens' InstanceGroupConfig Prelude.Int
instanceGroupConfig_instanceCount = Lens.lens (\InstanceGroupConfig' {instanceCount} -> instanceCount) (\s@InstanceGroupConfig' {} a -> s {instanceCount = a} :: InstanceGroupConfig)

instance Prelude.Hashable InstanceGroupConfig

instance Prelude.NFData InstanceGroupConfig

instance Prelude.ToJSON InstanceGroupConfig where
  toJSON InstanceGroupConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EbsConfiguration" Prelude..=)
              Prelude.<$> ebsConfiguration,
            ("Configurations" Prelude..=)
              Prelude.<$> configurations,
            ("AutoScalingPolicy" Prelude..=)
              Prelude.<$> autoScalingPolicy,
            ("BidPrice" Prelude..=) Prelude.<$> bidPrice,
            ("Name" Prelude..=) Prelude.<$> name,
            ("Market" Prelude..=) Prelude.<$> market,
            Prelude.Just
              ("InstanceRole" Prelude..= instanceRole),
            Prelude.Just
              ("InstanceType" Prelude..= instanceType),
            Prelude.Just
              ("InstanceCount" Prelude..= instanceCount)
          ]
      )
