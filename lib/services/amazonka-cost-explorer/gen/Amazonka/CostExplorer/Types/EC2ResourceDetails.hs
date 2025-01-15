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
-- Module      : Amazonka.CostExplorer.Types.EC2ResourceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.EC2ResourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details on the Amazon EC2 Resource.
--
-- /See:/ 'newEC2ResourceDetails' smart constructor.
data EC2ResourceDetails = EC2ResourceDetails'
  { -- | The hourly public On-Demand rate for the instance type.
    hourlyOnDemandRate :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services instance.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The memory capacity of the Amazon Web Services instance.
    memory :: Prelude.Maybe Prelude.Text,
    -- | The network performance capacity of the Amazon Web Services instance.
    networkPerformance :: Prelude.Maybe Prelude.Text,
    -- | The platform of the Amazon Web Services instance. The platform is the
    -- specific combination of operating system, license model, and software on
    -- an instance.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region of the instance.
    region :: Prelude.Maybe Prelude.Text,
    -- | The SKU of the product.
    sku :: Prelude.Maybe Prelude.Text,
    -- | The disk storage of the Amazon Web Services instance. This doesn\'t
    -- include EBS storage.
    storage :: Prelude.Maybe Prelude.Text,
    -- | The number of VCPU cores in the Amazon Web Services instance type.
    vcpu :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2ResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hourlyOnDemandRate', 'eC2ResourceDetails_hourlyOnDemandRate' - The hourly public On-Demand rate for the instance type.
--
-- 'instanceType', 'eC2ResourceDetails_instanceType' - The type of Amazon Web Services instance.
--
-- 'memory', 'eC2ResourceDetails_memory' - The memory capacity of the Amazon Web Services instance.
--
-- 'networkPerformance', 'eC2ResourceDetails_networkPerformance' - The network performance capacity of the Amazon Web Services instance.
--
-- 'platform', 'eC2ResourceDetails_platform' - The platform of the Amazon Web Services instance. The platform is the
-- specific combination of operating system, license model, and software on
-- an instance.
--
-- 'region', 'eC2ResourceDetails_region' - The Amazon Web Services Region of the instance.
--
-- 'sku', 'eC2ResourceDetails_sku' - The SKU of the product.
--
-- 'storage', 'eC2ResourceDetails_storage' - The disk storage of the Amazon Web Services instance. This doesn\'t
-- include EBS storage.
--
-- 'vcpu', 'eC2ResourceDetails_vcpu' - The number of VCPU cores in the Amazon Web Services instance type.
newEC2ResourceDetails ::
  EC2ResourceDetails
newEC2ResourceDetails =
  EC2ResourceDetails'
    { hourlyOnDemandRate =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      memory = Prelude.Nothing,
      networkPerformance = Prelude.Nothing,
      platform = Prelude.Nothing,
      region = Prelude.Nothing,
      sku = Prelude.Nothing,
      storage = Prelude.Nothing,
      vcpu = Prelude.Nothing
    }

-- | The hourly public On-Demand rate for the instance type.
eC2ResourceDetails_hourlyOnDemandRate :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_hourlyOnDemandRate = Lens.lens (\EC2ResourceDetails' {hourlyOnDemandRate} -> hourlyOnDemandRate) (\s@EC2ResourceDetails' {} a -> s {hourlyOnDemandRate = a} :: EC2ResourceDetails)

-- | The type of Amazon Web Services instance.
eC2ResourceDetails_instanceType :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_instanceType = Lens.lens (\EC2ResourceDetails' {instanceType} -> instanceType) (\s@EC2ResourceDetails' {} a -> s {instanceType = a} :: EC2ResourceDetails)

-- | The memory capacity of the Amazon Web Services instance.
eC2ResourceDetails_memory :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_memory = Lens.lens (\EC2ResourceDetails' {memory} -> memory) (\s@EC2ResourceDetails' {} a -> s {memory = a} :: EC2ResourceDetails)

-- | The network performance capacity of the Amazon Web Services instance.
eC2ResourceDetails_networkPerformance :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_networkPerformance = Lens.lens (\EC2ResourceDetails' {networkPerformance} -> networkPerformance) (\s@EC2ResourceDetails' {} a -> s {networkPerformance = a} :: EC2ResourceDetails)

-- | The platform of the Amazon Web Services instance. The platform is the
-- specific combination of operating system, license model, and software on
-- an instance.
eC2ResourceDetails_platform :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_platform = Lens.lens (\EC2ResourceDetails' {platform} -> platform) (\s@EC2ResourceDetails' {} a -> s {platform = a} :: EC2ResourceDetails)

-- | The Amazon Web Services Region of the instance.
eC2ResourceDetails_region :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_region = Lens.lens (\EC2ResourceDetails' {region} -> region) (\s@EC2ResourceDetails' {} a -> s {region = a} :: EC2ResourceDetails)

-- | The SKU of the product.
eC2ResourceDetails_sku :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_sku = Lens.lens (\EC2ResourceDetails' {sku} -> sku) (\s@EC2ResourceDetails' {} a -> s {sku = a} :: EC2ResourceDetails)

-- | The disk storage of the Amazon Web Services instance. This doesn\'t
-- include EBS storage.
eC2ResourceDetails_storage :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_storage = Lens.lens (\EC2ResourceDetails' {storage} -> storage) (\s@EC2ResourceDetails' {} a -> s {storage = a} :: EC2ResourceDetails)

-- | The number of VCPU cores in the Amazon Web Services instance type.
eC2ResourceDetails_vcpu :: Lens.Lens' EC2ResourceDetails (Prelude.Maybe Prelude.Text)
eC2ResourceDetails_vcpu = Lens.lens (\EC2ResourceDetails' {vcpu} -> vcpu) (\s@EC2ResourceDetails' {} a -> s {vcpu = a} :: EC2ResourceDetails)

instance Data.FromJSON EC2ResourceDetails where
  parseJSON =
    Data.withObject
      "EC2ResourceDetails"
      ( \x ->
          EC2ResourceDetails'
            Prelude.<$> (x Data..:? "HourlyOnDemandRate")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "Memory")
            Prelude.<*> (x Data..:? "NetworkPerformance")
            Prelude.<*> (x Data..:? "Platform")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "Sku")
            Prelude.<*> (x Data..:? "Storage")
            Prelude.<*> (x Data..:? "Vcpu")
      )

instance Prelude.Hashable EC2ResourceDetails where
  hashWithSalt _salt EC2ResourceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` hourlyOnDemandRate
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` networkPerformance
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` sku
      `Prelude.hashWithSalt` storage
      `Prelude.hashWithSalt` vcpu

instance Prelude.NFData EC2ResourceDetails where
  rnf EC2ResourceDetails' {..} =
    Prelude.rnf hourlyOnDemandRate `Prelude.seq`
      Prelude.rnf instanceType `Prelude.seq`
        Prelude.rnf memory `Prelude.seq`
          Prelude.rnf networkPerformance `Prelude.seq`
            Prelude.rnf platform `Prelude.seq`
              Prelude.rnf region `Prelude.seq`
                Prelude.rnf sku `Prelude.seq`
                  Prelude.rnf storage `Prelude.seq`
                    Prelude.rnf vcpu
