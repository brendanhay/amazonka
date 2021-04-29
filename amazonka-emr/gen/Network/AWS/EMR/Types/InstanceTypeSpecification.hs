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
-- Module      : Network.AWS.EMR.Types.InstanceTypeSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTypeSpecification where

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
  { -- | The EC2 instance type, for example @m3.xlarge@.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached
    -- to each instance as defined by @InstanceType@.
    ebsBlockDevices :: Prelude.Maybe [EbsBlockDevice],
    -- | A configuration classification that applies when provisioning cluster
    -- instances, which can include configurations for applications and
    -- software bundled with Amazon EMR.
    configurations :: Prelude.Maybe [Configuration],
    -- | The bid price, as a percentage of On-Demand price, for each EC2 Spot
    -- Instance as defined by @InstanceType@. Expressed as a number (for
    -- example, 20 specifies 20%).
    bidPriceAsPercentageOfOnDemandPrice :: Prelude.Maybe Prelude.Double,
    -- | The bid price for each EC2 Spot Instance type as defined by
    -- @InstanceType@. Expressed in USD.
    bidPrice :: Prelude.Maybe Prelude.Text,
    -- | The number of units that a provisioned instance of this type provides
    -- toward fulfilling the target capacities defined in InstanceFleetConfig.
    -- Capacity values represent performance characteristics such as vCPUs,
    -- memory, or I\/O. If not specified, the default value is 1.
    weightedCapacity :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceTypeSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'instanceTypeSpecification_instanceType' - The EC2 instance type, for example @m3.xlarge@.
--
-- 'ebsOptimized', 'instanceTypeSpecification_ebsOptimized' - Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
--
-- 'ebsBlockDevices', 'instanceTypeSpecification_ebsBlockDevices' - The configuration of Amazon Elastic Block Storage (Amazon EBS) attached
-- to each instance as defined by @InstanceType@.
--
-- 'configurations', 'instanceTypeSpecification_configurations' - A configuration classification that applies when provisioning cluster
-- instances, which can include configurations for applications and
-- software bundled with Amazon EMR.
--
-- 'bidPriceAsPercentageOfOnDemandPrice', 'instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice' - The bid price, as a percentage of On-Demand price, for each EC2 Spot
-- Instance as defined by @InstanceType@. Expressed as a number (for
-- example, 20 specifies 20%).
--
-- 'bidPrice', 'instanceTypeSpecification_bidPrice' - The bid price for each EC2 Spot Instance type as defined by
-- @InstanceType@. Expressed in USD.
--
-- 'weightedCapacity', 'instanceTypeSpecification_weightedCapacity' - The number of units that a provisioned instance of this type provides
-- toward fulfilling the target capacities defined in InstanceFleetConfig.
-- Capacity values represent performance characteristics such as vCPUs,
-- memory, or I\/O. If not specified, the default value is 1.
newInstanceTypeSpecification ::
  InstanceTypeSpecification
newInstanceTypeSpecification =
  InstanceTypeSpecification'
    { instanceType =
        Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      ebsBlockDevices = Prelude.Nothing,
      configurations = Prelude.Nothing,
      bidPriceAsPercentageOfOnDemandPrice =
        Prelude.Nothing,
      bidPrice = Prelude.Nothing,
      weightedCapacity = Prelude.Nothing
    }

-- | The EC2 instance type, for example @m3.xlarge@.
instanceTypeSpecification_instanceType :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe Prelude.Text)
instanceTypeSpecification_instanceType = Lens.lens (\InstanceTypeSpecification' {instanceType} -> instanceType) (\s@InstanceTypeSpecification' {} a -> s {instanceType = a} :: InstanceTypeSpecification)

-- | Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
instanceTypeSpecification_ebsOptimized :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe Prelude.Bool)
instanceTypeSpecification_ebsOptimized = Lens.lens (\InstanceTypeSpecification' {ebsOptimized} -> ebsOptimized) (\s@InstanceTypeSpecification' {} a -> s {ebsOptimized = a} :: InstanceTypeSpecification)

-- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached
-- to each instance as defined by @InstanceType@.
instanceTypeSpecification_ebsBlockDevices :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe [EbsBlockDevice])
instanceTypeSpecification_ebsBlockDevices = Lens.lens (\InstanceTypeSpecification' {ebsBlockDevices} -> ebsBlockDevices) (\s@InstanceTypeSpecification' {} a -> s {ebsBlockDevices = a} :: InstanceTypeSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | A configuration classification that applies when provisioning cluster
-- instances, which can include configurations for applications and
-- software bundled with Amazon EMR.
instanceTypeSpecification_configurations :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe [Configuration])
instanceTypeSpecification_configurations = Lens.lens (\InstanceTypeSpecification' {configurations} -> configurations) (\s@InstanceTypeSpecification' {} a -> s {configurations = a} :: InstanceTypeSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The bid price, as a percentage of On-Demand price, for each EC2 Spot
-- Instance as defined by @InstanceType@. Expressed as a number (for
-- example, 20 specifies 20%).
instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice :: Lens.Lens' InstanceTypeSpecification (Prelude.Maybe Prelude.Double)
instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice = Lens.lens (\InstanceTypeSpecification' {bidPriceAsPercentageOfOnDemandPrice} -> bidPriceAsPercentageOfOnDemandPrice) (\s@InstanceTypeSpecification' {} a -> s {bidPriceAsPercentageOfOnDemandPrice = a} :: InstanceTypeSpecification)

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

instance Prelude.FromJSON InstanceTypeSpecification where
  parseJSON =
    Prelude.withObject
      "InstanceTypeSpecification"
      ( \x ->
          InstanceTypeSpecification'
            Prelude.<$> (x Prelude..:? "InstanceType")
            Prelude.<*> (x Prelude..:? "EbsOptimized")
            Prelude.<*> ( x Prelude..:? "EbsBlockDevices"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "Configurations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "BidPriceAsPercentageOfOnDemandPrice")
            Prelude.<*> (x Prelude..:? "BidPrice")
            Prelude.<*> (x Prelude..:? "WeightedCapacity")
      )

instance Prelude.Hashable InstanceTypeSpecification

instance Prelude.NFData InstanceTypeSpecification
