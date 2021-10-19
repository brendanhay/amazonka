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
-- Module      : Network.AWS.EMR.Types.InstanceTypeSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTypeSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EbsBlockDevice
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configuration specification for each instance type in an instance
-- fleet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- /See:/ 'newInstanceTypeSpecification' smart constructor.
data InstanceTypeSpecification = InstanceTypeSpecification'
  { -- | The bid price for each EC2 Spot Instance type as defined by
    -- @InstanceType@. Expressed in USD.
    bidPrice :: Prelude.Maybe Prelude.Text,
    -- | The number of units that a provisioned instance of this type provides
    -- toward fulfilling the target capacities defined in InstanceFleetConfig.
    -- Capacity values represent performance characteristics such as vCPUs,
    -- memory, or I\/O. If not specified, the default value is 1.
    weightedCapacity :: Prelude.Maybe Prelude.Natural,
    -- | A configuration classification that applies when provisioning cluster
    -- instances, which can include configurations for applications and
    -- software bundled with Amazon EMR.
    configurations :: Prelude.Maybe [Configuration],
    -- | The custom AMI ID to use for the instance type.
    customAmiId :: Prelude.Maybe Prelude.Text,
    -- | The configuration of Amazon Elastic Block Store (Amazon EBS) attached to
    -- each instance as defined by @InstanceType@.
    ebsBlockDevices :: Prelude.Maybe [EbsBlockDevice],
    -- | The EC2 instance type, for example @m3.xlarge@.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The bid price, as a percentage of On-Demand price, for each EC2 Spot
    -- Instance as defined by @InstanceType@. Expressed as a number (for
    -- example, 20 specifies 20%).
    bidPriceAsPercentageOfOnDemandPrice :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceTypeSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bidPrice', 'instanceTypeSpecification_bidPrice' - The bid price for each EC2 Spot Instance type as defined by
-- @InstanceType@. Expressed in USD.
--
-- 'weightedCapacity', 'instanceTypeSpecification_weightedCapacity' - The number of units that a provisioned instance of this type provides
-- toward fulfilling the target capacities defined in InstanceFleetConfig.
-- Capacity values represent performance characteristics such as vCPUs,
-- memory, or I\/O. If not specified, the default value is 1.
--
-- 'configurations', 'instanceTypeSpecification_configurations' - A configuration classification that applies when provisioning cluster
-- instances, which can include configurations for applications and
-- software bundled with Amazon EMR.
--
-- 'customAmiId', 'instanceTypeSpecification_customAmiId' - The custom AMI ID to use for the instance type.
--
-- 'ebsBlockDevices', 'instanceTypeSpecification_ebsBlockDevices' - The configuration of Amazon Elastic Block Store (Amazon EBS) attached to
-- each instance as defined by @InstanceType@.
--
-- 'instanceType', 'instanceTypeSpecification_instanceType' - The EC2 instance type, for example @m3.xlarge@.
--
-- 'ebsOptimized', 'instanceTypeSpecification_ebsOptimized' - Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
--
-- 'bidPriceAsPercentageOfOnDemandPrice', 'instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice' - The bid price, as a percentage of On-Demand price, for each EC2 Spot
-- Instance as defined by @InstanceType@. Expressed as a number (for
-- example, 20 specifies 20%).
newInstanceTypeSpecification ::
  InstanceTypeSpecification
newInstanceTypeSpecification =
  InstanceTypeSpecification'
    { bidPrice =
        Prelude.Nothing,
      weightedCapacity = Prelude.Nothing,
      configurations = Prelude.Nothing,
      customAmiId = Prelude.Nothing,
      ebsBlockDevices = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      bidPriceAsPercentageOfOnDemandPrice =
        Prelude.Nothing
    }

-- | The bid price for each EC2 Spot Instance type as defined by
-- @InstanceType@. Expressed in USD.
instanceTypeSpecification_bidPrice :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe Prelude.Text)
instanceTypeSpecification_bidPrice = Lens.lens (\InstanceTypeSpecification' {bidPrice} -> bidPrice) (\s@InstanceTypeSpecification' {} a -> s {bidPrice = a} :: InstanceTypeSpecification)

-- | The number of units that a provisioned instance of this type provides
-- toward fulfilling the target capacities defined in InstanceFleetConfig.
-- Capacity values represent performance characteristics such as vCPUs,
-- memory, or I\/O. If not specified, the default value is 1.
instanceTypeSpecification_weightedCapacity :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe Prelude.Natural)
instanceTypeSpecification_weightedCapacity = Lens.lens (\InstanceTypeSpecification' {weightedCapacity} -> weightedCapacity) (\s@InstanceTypeSpecification' {} a -> s {weightedCapacity = a} :: InstanceTypeSpecification)

-- | A configuration classification that applies when provisioning cluster
-- instances, which can include configurations for applications and
-- software bundled with Amazon EMR.
instanceTypeSpecification_configurations :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe [Configuration])
instanceTypeSpecification_configurations = Lens.lens (\InstanceTypeSpecification' {configurations} -> configurations) (\s@InstanceTypeSpecification' {} a -> s {configurations = a} :: InstanceTypeSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The custom AMI ID to use for the instance type.
instanceTypeSpecification_customAmiId :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe Prelude.Text)
instanceTypeSpecification_customAmiId = Lens.lens (\InstanceTypeSpecification' {customAmiId} -> customAmiId) (\s@InstanceTypeSpecification' {} a -> s {customAmiId = a} :: InstanceTypeSpecification)

-- | The configuration of Amazon Elastic Block Store (Amazon EBS) attached to
-- each instance as defined by @InstanceType@.
instanceTypeSpecification_ebsBlockDevices :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe [EbsBlockDevice])
instanceTypeSpecification_ebsBlockDevices = Lens.lens (\InstanceTypeSpecification' {ebsBlockDevices} -> ebsBlockDevices) (\s@InstanceTypeSpecification' {} a -> s {ebsBlockDevices = a} :: InstanceTypeSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The EC2 instance type, for example @m3.xlarge@.
instanceTypeSpecification_instanceType :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe Prelude.Text)
instanceTypeSpecification_instanceType = Lens.lens (\InstanceTypeSpecification' {instanceType} -> instanceType) (\s@InstanceTypeSpecification' {} a -> s {instanceType = a} :: InstanceTypeSpecification)

-- | Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
instanceTypeSpecification_ebsOptimized :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe Prelude.Bool)
instanceTypeSpecification_ebsOptimized = Lens.lens (\InstanceTypeSpecification' {ebsOptimized} -> ebsOptimized) (\s@InstanceTypeSpecification' {} a -> s {ebsOptimized = a} :: InstanceTypeSpecification)

-- | The bid price, as a percentage of On-Demand price, for each EC2 Spot
-- Instance as defined by @InstanceType@. Expressed as a number (for
-- example, 20 specifies 20%).
instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe Prelude.Double)
instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice = Lens.lens (\InstanceTypeSpecification' {bidPriceAsPercentageOfOnDemandPrice} -> bidPriceAsPercentageOfOnDemandPrice) (\s@InstanceTypeSpecification' {} a -> s {bidPriceAsPercentageOfOnDemandPrice = a} :: InstanceTypeSpecification)

instance Core.FromJSON InstanceTypeSpecification where
  parseJSON =
    Core.withObject
      "InstanceTypeSpecification"
      ( \x ->
          InstanceTypeSpecification'
            Prelude.<$> (x Core..:? "BidPrice")
            Prelude.<*> (x Core..:? "WeightedCapacity")
            Prelude.<*> (x Core..:? "Configurations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CustomAmiId")
            Prelude.<*> ( x Core..:? "EbsBlockDevices"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "EbsOptimized")
            Prelude.<*> (x Core..:? "BidPriceAsPercentageOfOnDemandPrice")
      )

instance Prelude.Hashable InstanceTypeSpecification

instance Prelude.NFData InstanceTypeSpecification
