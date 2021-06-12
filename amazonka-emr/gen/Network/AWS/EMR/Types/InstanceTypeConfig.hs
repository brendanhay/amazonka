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
-- Module      : Network.AWS.EMR.Types.InstanceTypeConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTypeConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EbsConfiguration
import qualified Network.AWS.Lens as Lens

-- | An instance type configuration for each instance type in an instance
-- fleet, which determines the EC2 instances Amazon EMR attempts to
-- provision to fulfill On-Demand and Spot target capacities. There can be
-- a maximum of five instance type configurations in a fleet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- /See:/ 'newInstanceTypeConfig' smart constructor.
data InstanceTypeConfig = InstanceTypeConfig'
  { -- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached
    -- to each instance as defined by @InstanceType@.
    ebsConfiguration :: Core.Maybe EbsConfiguration,
    -- | A configuration classification that applies when provisioning cluster
    -- instances, which can include configurations for applications and
    -- software that run on the cluster.
    configurations :: Core.Maybe [Configuration],
    -- | The bid price, as a percentage of On-Demand price, for each EC2 Spot
    -- Instance as defined by @InstanceType@. Expressed as a number (for
    -- example, 20 specifies 20%). If neither @BidPrice@ nor
    -- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
    -- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPriceAsPercentageOfOnDemandPrice :: Core.Maybe Core.Double,
    -- | The bid price for each EC2 Spot Instance type as defined by
    -- @InstanceType@. Expressed in USD. If neither @BidPrice@ nor
    -- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
    -- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPrice :: Core.Maybe Core.Text,
    -- | The number of units that a provisioned instance of this type provides
    -- toward fulfilling the target capacities defined in InstanceFleetConfig.
    -- This value is 1 for a master instance fleet, and must be 1 or greater
    -- for core and task instance fleets. Defaults to 1 if not specified.
    weightedCapacity :: Core.Maybe Core.Natural,
    -- | An EC2 instance type, such as @m3.xlarge@.
    instanceType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceTypeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsConfiguration', 'instanceTypeConfig_ebsConfiguration' - The configuration of Amazon Elastic Block Storage (Amazon EBS) attached
-- to each instance as defined by @InstanceType@.
--
-- 'configurations', 'instanceTypeConfig_configurations' - A configuration classification that applies when provisioning cluster
-- instances, which can include configurations for applications and
-- software that run on the cluster.
--
-- 'bidPriceAsPercentageOfOnDemandPrice', 'instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice' - The bid price, as a percentage of On-Demand price, for each EC2 Spot
-- Instance as defined by @InstanceType@. Expressed as a number (for
-- example, 20 specifies 20%). If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- 'bidPrice', 'instanceTypeConfig_bidPrice' - The bid price for each EC2 Spot Instance type as defined by
-- @InstanceType@. Expressed in USD. If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- 'weightedCapacity', 'instanceTypeConfig_weightedCapacity' - The number of units that a provisioned instance of this type provides
-- toward fulfilling the target capacities defined in InstanceFleetConfig.
-- This value is 1 for a master instance fleet, and must be 1 or greater
-- for core and task instance fleets. Defaults to 1 if not specified.
--
-- 'instanceType', 'instanceTypeConfig_instanceType' - An EC2 instance type, such as @m3.xlarge@.
newInstanceTypeConfig ::
  -- | 'instanceType'
  Core.Text ->
  InstanceTypeConfig
newInstanceTypeConfig pInstanceType_ =
  InstanceTypeConfig'
    { ebsConfiguration =
        Core.Nothing,
      configurations = Core.Nothing,
      bidPriceAsPercentageOfOnDemandPrice = Core.Nothing,
      bidPrice = Core.Nothing,
      weightedCapacity = Core.Nothing,
      instanceType = pInstanceType_
    }

-- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached
-- to each instance as defined by @InstanceType@.
instanceTypeConfig_ebsConfiguration :: Lens.Lens' InstanceTypeConfig (Core.Maybe EbsConfiguration)
instanceTypeConfig_ebsConfiguration = Lens.lens (\InstanceTypeConfig' {ebsConfiguration} -> ebsConfiguration) (\s@InstanceTypeConfig' {} a -> s {ebsConfiguration = a} :: InstanceTypeConfig)

-- | A configuration classification that applies when provisioning cluster
-- instances, which can include configurations for applications and
-- software that run on the cluster.
instanceTypeConfig_configurations :: Lens.Lens' InstanceTypeConfig (Core.Maybe [Configuration])
instanceTypeConfig_configurations = Lens.lens (\InstanceTypeConfig' {configurations} -> configurations) (\s@InstanceTypeConfig' {} a -> s {configurations = a} :: InstanceTypeConfig) Core.. Lens.mapping Lens._Coerce

-- | The bid price, as a percentage of On-Demand price, for each EC2 Spot
-- Instance as defined by @InstanceType@. Expressed as a number (for
-- example, 20 specifies 20%). If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice :: Lens.Lens' InstanceTypeConfig (Core.Maybe Core.Double)
instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice = Lens.lens (\InstanceTypeConfig' {bidPriceAsPercentageOfOnDemandPrice} -> bidPriceAsPercentageOfOnDemandPrice) (\s@InstanceTypeConfig' {} a -> s {bidPriceAsPercentageOfOnDemandPrice = a} :: InstanceTypeConfig)

-- | The bid price for each EC2 Spot Instance type as defined by
-- @InstanceType@. Expressed in USD. If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
instanceTypeConfig_bidPrice :: Lens.Lens' InstanceTypeConfig (Core.Maybe Core.Text)
instanceTypeConfig_bidPrice = Lens.lens (\InstanceTypeConfig' {bidPrice} -> bidPrice) (\s@InstanceTypeConfig' {} a -> s {bidPrice = a} :: InstanceTypeConfig)

-- | The number of units that a provisioned instance of this type provides
-- toward fulfilling the target capacities defined in InstanceFleetConfig.
-- This value is 1 for a master instance fleet, and must be 1 or greater
-- for core and task instance fleets. Defaults to 1 if not specified.
instanceTypeConfig_weightedCapacity :: Lens.Lens' InstanceTypeConfig (Core.Maybe Core.Natural)
instanceTypeConfig_weightedCapacity = Lens.lens (\InstanceTypeConfig' {weightedCapacity} -> weightedCapacity) (\s@InstanceTypeConfig' {} a -> s {weightedCapacity = a} :: InstanceTypeConfig)

-- | An EC2 instance type, such as @m3.xlarge@.
instanceTypeConfig_instanceType :: Lens.Lens' InstanceTypeConfig Core.Text
instanceTypeConfig_instanceType = Lens.lens (\InstanceTypeConfig' {instanceType} -> instanceType) (\s@InstanceTypeConfig' {} a -> s {instanceType = a} :: InstanceTypeConfig)

instance Core.Hashable InstanceTypeConfig

instance Core.NFData InstanceTypeConfig

instance Core.ToJSON InstanceTypeConfig where
  toJSON InstanceTypeConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EbsConfiguration" Core..=)
              Core.<$> ebsConfiguration,
            ("Configurations" Core..=) Core.<$> configurations,
            ("BidPriceAsPercentageOfOnDemandPrice" Core..=)
              Core.<$> bidPriceAsPercentageOfOnDemandPrice,
            ("BidPrice" Core..=) Core.<$> bidPrice,
            ("WeightedCapacity" Core..=)
              Core.<$> weightedCapacity,
            Core.Just ("InstanceType" Core..= instanceType)
          ]
      )
