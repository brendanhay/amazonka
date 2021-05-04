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
-- Module      : Network.AWS.CostExplorer.Types.EC2ResourceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2ResourceDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on the Amazon EC2 Resource.
--
-- /See:/ 'newEC2ResourceDetails' smart constructor.
data EC2ResourceDetails = EC2ResourceDetails'
  { -- | The platform of the AWS instance. The platform is the specific
    -- combination of operating system, license model, and software on an
    -- instance.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The type of AWS instance.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Memory capacity of the AWS instance.
    memory :: Prelude.Maybe Prelude.Text,
    -- | Number of VCPU cores in the AWS instance type.
    vcpu :: Prelude.Maybe Prelude.Text,
    -- | Hourly public On-Demand rate for the instance type.
    hourlyOnDemandRate :: Prelude.Maybe Prelude.Text,
    -- | The disk storage of the AWS instance (not EBS storage).
    storage :: Prelude.Maybe Prelude.Text,
    -- | Network performance capacity of the AWS instance.
    networkPerformance :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region of the instance.
    region :: Prelude.Maybe Prelude.Text,
    -- | The SKU of the product.
    sku :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EC2ResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'eC2ResourceDetails_platform' - The platform of the AWS instance. The platform is the specific
-- combination of operating system, license model, and software on an
-- instance.
--
-- 'instanceType', 'eC2ResourceDetails_instanceType' - The type of AWS instance.
--
-- 'memory', 'eC2ResourceDetails_memory' - Memory capacity of the AWS instance.
--
-- 'vcpu', 'eC2ResourceDetails_vcpu' - Number of VCPU cores in the AWS instance type.
--
-- 'hourlyOnDemandRate', 'eC2ResourceDetails_hourlyOnDemandRate' - Hourly public On-Demand rate for the instance type.
--
-- 'storage', 'eC2ResourceDetails_storage' - The disk storage of the AWS instance (not EBS storage).
--
-- 'networkPerformance', 'eC2ResourceDetails_networkPerformance' - Network performance capacity of the AWS instance.
--
-- 'region', 'eC2ResourceDetails_region' - The AWS Region of the instance.
--
-- 'sku', 'eC2ResourceDetails_sku' - The SKU of the product.
newEC2ResourceDetails ::
  EC2ResourceDetails
newEC2ResourceDetails =
  EC2ResourceDetails'
    { platform = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      memory = Prelude.Nothing,
      vcpu = Prelude.Nothing,
      hourlyOnDemandRate = Prelude.Nothing,
      storage = Prelude.Nothing,
      networkPerformance = Prelude.Nothing,
      region = Prelude.Nothing,
      sku = Prelude.Nothing
    }

-- | The platform of the AWS instance. The platform is the specific
-- combination of operating system, license model, and software on an
-- instance.
eC2ResourceDetails_platform :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_platform = Lens.lens (\EC2ResourceDetails' {platform} -> platform) (\s@EC2ResourceDetails' {} a -> s {platform = a} :: EC2ResourceDetails)

-- | The type of AWS instance.
eC2ResourceDetails_instanceType :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_instanceType = Lens.lens (\EC2ResourceDetails' {instanceType} -> instanceType) (\s@EC2ResourceDetails' {} a -> s {instanceType = a} :: EC2ResourceDetails)

-- | Memory capacity of the AWS instance.
eC2ResourceDetails_memory :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_memory = Lens.lens (\EC2ResourceDetails' {memory} -> memory) (\s@EC2ResourceDetails' {} a -> s {memory = a} :: EC2ResourceDetails)

-- | Number of VCPU cores in the AWS instance type.
eC2ResourceDetails_vcpu :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_vcpu = Lens.lens (\EC2ResourceDetails' {vcpu} -> vcpu) (\s@EC2ResourceDetails' {} a -> s {vcpu = a} :: EC2ResourceDetails)

-- | Hourly public On-Demand rate for the instance type.
eC2ResourceDetails_hourlyOnDemandRate :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_hourlyOnDemandRate = Lens.lens (\EC2ResourceDetails' {hourlyOnDemandRate} -> hourlyOnDemandRate) (\s@EC2ResourceDetails' {} a -> s {hourlyOnDemandRate = a} :: EC2ResourceDetails)

-- | The disk storage of the AWS instance (not EBS storage).
eC2ResourceDetails_storage :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_storage = Lens.lens (\EC2ResourceDetails' {storage} -> storage) (\s@EC2ResourceDetails' {} a -> s {storage = a} :: EC2ResourceDetails)

-- | Network performance capacity of the AWS instance.
eC2ResourceDetails_networkPerformance :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_networkPerformance = Lens.lens (\EC2ResourceDetails' {networkPerformance} -> networkPerformance) (\s@EC2ResourceDetails' {} a -> s {networkPerformance = a} :: EC2ResourceDetails)

-- | The AWS Region of the instance.
eC2ResourceDetails_region :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_region = Lens.lens (\EC2ResourceDetails' {region} -> region) (\s@EC2ResourceDetails' {} a -> s {region = a} :: EC2ResourceDetails)

-- | The SKU of the product.
eC2ResourceDetails_sku :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_sku = Lens.lens (\EC2ResourceDetails' {sku} -> sku) (\s@EC2ResourceDetails' {} a -> s {sku = a} :: EC2ResourceDetails)

instance Prelude.FromJSON EC2ResourceDetails where
  parseJSON =
    Prelude.withObject
      "EC2ResourceDetails"
      ( \x ->
          EC2ResourceDetails'
            Prelude.<$> (x Prelude..:? "Platform")
            Prelude.<*> (x Prelude..:? "InstanceType")
            Prelude.<*> (x Prelude..:? "Memory")
            Prelude.<*> (x Prelude..:? "Vcpu")
            Prelude.<*> (x Prelude..:? "HourlyOnDemandRate")
            Prelude.<*> (x Prelude..:? "Storage")
            Prelude.<*> (x Prelude..:? "NetworkPerformance")
            Prelude.<*> (x Prelude..:? "Region")
            Prelude.<*> (x Prelude..:? "Sku")
      )

instance Prelude.Hashable EC2ResourceDetails

instance Prelude.NFData EC2ResourceDetails
