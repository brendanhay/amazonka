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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails

-- | The attributes for the Amazon EC2 instance types.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataInstanceRequirementsDetails' smart constructor.
data AwsEc2LaunchTemplateDataInstanceRequirementsDetails = AwsEc2LaunchTemplateDataInstanceRequirementsDetails'
  { -- | The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
    -- Web Services Inferentia chips) on an instance.
    acceleratorCount :: Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails,
    -- | Indicates whether instance types must have accelerators by specific
    -- manufacturers.
    acceleratorManufacturers :: Prelude.Maybe [Prelude.Text],
    -- | The accelerators that must be on the instance type.
    acceleratorNames :: Prelude.Maybe [Prelude.Text],
    -- | The minimum and maximum amount of total accelerator memory, in MiB.
    acceleratorTotalMemoryMiB :: Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails,
    -- | The accelerator types that must be on the instance type.
    acceleratorTypes :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether bare metal instance types must be included, excluded,
    -- or required.
    bareMetal :: Prelude.Maybe Prelude.Text,
    -- | The minimum and maximum baseline bandwidth to Amazon EBS, in Mbps. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS optimized instances>
    -- in the /Amazon EC2 User Guide/.
    baselineEbsBandwidthMbps :: Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails,
    -- | Indicates whether burstable performance T instance types are included,
    -- excluded, or required. For more information,
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
    -- in the /Amazon EC2 User Guide/.
    burstablePerformance :: Prelude.Maybe Prelude.Text,
    -- | The CPU manufacturers to include.
    cpuManufacturers :: Prelude.Maybe [Prelude.Text],
    -- | The instance types to exclude.
    excludedInstanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether current or previous generation instance types are
    -- included.
    instanceGenerations :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether instance types with instance store volumes are
    -- included, excluded, or required. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html Amazon EC2 instance store>
    -- in the /Amazon EC2 User Guide/.
    localStorage :: Prelude.Maybe Prelude.Text,
    -- | The type of local storage that is required.
    localStorageTypes :: Prelude.Maybe [Prelude.Text],
    -- | The minimum and maximum amount of memory per vCPU, in GiB.
    memoryGiBPerVCpu :: Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails,
    -- | The minimum and maximum amount of memory, in MiB.
    memoryMiB :: Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails,
    -- | The minimum and maximum number of network interfaces.
    networkInterfaceCount :: Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails,
    -- | The price protection threshold for On-Demand Instances. This is the
    -- maximum you’ll pay for an On-Demand Instance, expressed as a percentage
    -- above the least expensive current generation M, C, or R instance type
    -- with your specified attributes. When Amazon EC2 selects instance types
    -- with your attributes, it excludes instance types priced above your
    -- threshold.
    --
    -- The parameter accepts an integer, which Amazon EC2 interprets as a
    -- percentage.
    --
    -- A high value, such as @999999@, turns off price protection.
    onDemandMaxPricePercentageOverLowestPrice :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether instance types must support hibernation for On-Demand
    -- Instances.
    requireHibernateSupport :: Prelude.Maybe Prelude.Bool,
    -- | The price protection threshold for Spot Instances. This is the maximum
    -- you’ll pay for a Spot Instance, expressed as a percentage above the
    -- least expensive current generation M, C, or R instance type with your
    -- specified attributes. When Amazon EC2 selects instance types with your
    -- attributes, it excludes instance types priced above your threshold.
    --
    -- The parameter accepts an integer, which Amazon EC2 interprets as a
    -- percentage.
    --
    -- A high value, such as @999999@, turns off price protection.
    spotMaxPricePercentageOverLowestPrice :: Prelude.Maybe Prelude.Int,
    -- | The minimum and maximum amount of total local storage, in GB.
    totalLocalStorageGB :: Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails,
    -- | The minimum and maximum number of vCPUs.
    vCpuCount :: Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataInstanceRequirementsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorCount', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorCount' - The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
-- Web Services Inferentia chips) on an instance.
--
-- 'acceleratorManufacturers', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorManufacturers' - Indicates whether instance types must have accelerators by specific
-- manufacturers.
--
-- 'acceleratorNames', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorNames' - The accelerators that must be on the instance type.
--
-- 'acceleratorTotalMemoryMiB', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorTotalMemoryMiB' - The minimum and maximum amount of total accelerator memory, in MiB.
--
-- 'acceleratorTypes', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorTypes' - The accelerator types that must be on the instance type.
--
-- 'bareMetal', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_bareMetal' - Indicates whether bare metal instance types must be included, excluded,
-- or required.
--
-- 'baselineEbsBandwidthMbps', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_baselineEbsBandwidthMbps' - The minimum and maximum baseline bandwidth to Amazon EBS, in Mbps. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS optimized instances>
-- in the /Amazon EC2 User Guide/.
--
-- 'burstablePerformance', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_burstablePerformance' - Indicates whether burstable performance T instance types are included,
-- excluded, or required. For more information,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
-- in the /Amazon EC2 User Guide/.
--
-- 'cpuManufacturers', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_cpuManufacturers' - The CPU manufacturers to include.
--
-- 'excludedInstanceTypes', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_excludedInstanceTypes' - The instance types to exclude.
--
-- 'instanceGenerations', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_instanceGenerations' - Indicates whether current or previous generation instance types are
-- included.
--
-- 'localStorage', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_localStorage' - Indicates whether instance types with instance store volumes are
-- included, excluded, or required. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html Amazon EC2 instance store>
-- in the /Amazon EC2 User Guide/.
--
-- 'localStorageTypes', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_localStorageTypes' - The type of local storage that is required.
--
-- 'memoryGiBPerVCpu', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_memoryGiBPerVCpu' - The minimum and maximum amount of memory per vCPU, in GiB.
--
-- 'memoryMiB', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_memoryMiB' - The minimum and maximum amount of memory, in MiB.
--
-- 'networkInterfaceCount', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_networkInterfaceCount' - The minimum and maximum number of network interfaces.
--
-- 'onDemandMaxPricePercentageOverLowestPrice', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_onDemandMaxPricePercentageOverLowestPrice' - The price protection threshold for On-Demand Instances. This is the
-- maximum you’ll pay for an On-Demand Instance, expressed as a percentage
-- above the least expensive current generation M, C, or R instance type
-- with your specified attributes. When Amazon EC2 selects instance types
-- with your attributes, it excludes instance types priced above your
-- threshold.
--
-- The parameter accepts an integer, which Amazon EC2 interprets as a
-- percentage.
--
-- A high value, such as @999999@, turns off price protection.
--
-- 'requireHibernateSupport', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_requireHibernateSupport' - Indicates whether instance types must support hibernation for On-Demand
-- Instances.
--
-- 'spotMaxPricePercentageOverLowestPrice', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_spotMaxPricePercentageOverLowestPrice' - The price protection threshold for Spot Instances. This is the maximum
-- you’ll pay for a Spot Instance, expressed as a percentage above the
-- least expensive current generation M, C, or R instance type with your
-- specified attributes. When Amazon EC2 selects instance types with your
-- attributes, it excludes instance types priced above your threshold.
--
-- The parameter accepts an integer, which Amazon EC2 interprets as a
-- percentage.
--
-- A high value, such as @999999@, turns off price protection.
--
-- 'totalLocalStorageGB', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_totalLocalStorageGB' - The minimum and maximum amount of total local storage, in GB.
--
-- 'vCpuCount', 'awsEc2LaunchTemplateDataInstanceRequirementsDetails_vCpuCount' - The minimum and maximum number of vCPUs.
newAwsEc2LaunchTemplateDataInstanceRequirementsDetails ::
  AwsEc2LaunchTemplateDataInstanceRequirementsDetails
newAwsEc2LaunchTemplateDataInstanceRequirementsDetails =
  AwsEc2LaunchTemplateDataInstanceRequirementsDetails'
    { acceleratorCount =
        Prelude.Nothing,
      acceleratorManufacturers =
        Prelude.Nothing,
      acceleratorNames =
        Prelude.Nothing,
      acceleratorTotalMemoryMiB =
        Prelude.Nothing,
      acceleratorTypes =
        Prelude.Nothing,
      bareMetal =
        Prelude.Nothing,
      baselineEbsBandwidthMbps =
        Prelude.Nothing,
      burstablePerformance =
        Prelude.Nothing,
      cpuManufacturers =
        Prelude.Nothing,
      excludedInstanceTypes =
        Prelude.Nothing,
      instanceGenerations =
        Prelude.Nothing,
      localStorage =
        Prelude.Nothing,
      localStorageTypes =
        Prelude.Nothing,
      memoryGiBPerVCpu =
        Prelude.Nothing,
      memoryMiB =
        Prelude.Nothing,
      networkInterfaceCount =
        Prelude.Nothing,
      onDemandMaxPricePercentageOverLowestPrice =
        Prelude.Nothing,
      requireHibernateSupport =
        Prelude.Nothing,
      spotMaxPricePercentageOverLowestPrice =
        Prelude.Nothing,
      totalLocalStorageGB =
        Prelude.Nothing,
      vCpuCount =
        Prelude.Nothing
    }

-- | The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
-- Web Services Inferentia chips) on an instance.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorCount :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorCount = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {acceleratorCount} -> acceleratorCount) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {acceleratorCount = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | Indicates whether instance types must have accelerators by specific
-- manufacturers.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorManufacturers :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe [Prelude.Text])
awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorManufacturers = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {acceleratorManufacturers} -> acceleratorManufacturers) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {acceleratorManufacturers = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The accelerators that must be on the instance type.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorNames :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe [Prelude.Text])
awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorNames = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {acceleratorNames} -> acceleratorNames) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {acceleratorNames = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum amount of total accelerator memory, in MiB.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorTotalMemoryMiB :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorTotalMemoryMiB = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {acceleratorTotalMemoryMiB} -> acceleratorTotalMemoryMiB) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {acceleratorTotalMemoryMiB = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | The accelerator types that must be on the instance type.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorTypes :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe [Prelude.Text])
awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorTypes = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {acceleratorTypes} -> acceleratorTypes) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {acceleratorTypes = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether bare metal instance types must be included, excluded,
-- or required.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_bareMetal :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_bareMetal = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {bareMetal} -> bareMetal) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {bareMetal = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | The minimum and maximum baseline bandwidth to Amazon EBS, in Mbps. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS optimized instances>
-- in the /Amazon EC2 User Guide/.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_baselineEbsBandwidthMbps :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_baselineEbsBandwidthMbps = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {baselineEbsBandwidthMbps} -> baselineEbsBandwidthMbps) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {baselineEbsBandwidthMbps = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | Indicates whether burstable performance T instance types are included,
-- excluded, or required. For more information,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
-- in the /Amazon EC2 User Guide/.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_burstablePerformance :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_burstablePerformance = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {burstablePerformance} -> burstablePerformance) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {burstablePerformance = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | The CPU manufacturers to include.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_cpuManufacturers :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe [Prelude.Text])
awsEc2LaunchTemplateDataInstanceRequirementsDetails_cpuManufacturers = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {cpuManufacturers} -> cpuManufacturers) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {cpuManufacturers = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The instance types to exclude.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_excludedInstanceTypes :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe [Prelude.Text])
awsEc2LaunchTemplateDataInstanceRequirementsDetails_excludedInstanceTypes = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {excludedInstanceTypes} -> excludedInstanceTypes) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {excludedInstanceTypes = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether current or previous generation instance types are
-- included.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_instanceGenerations :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe [Prelude.Text])
awsEc2LaunchTemplateDataInstanceRequirementsDetails_instanceGenerations = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {instanceGenerations} -> instanceGenerations) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {instanceGenerations = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether instance types with instance store volumes are
-- included, excluded, or required. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html Amazon EC2 instance store>
-- in the /Amazon EC2 User Guide/.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_localStorage :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_localStorage = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {localStorage} -> localStorage) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {localStorage = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | The type of local storage that is required.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_localStorageTypes :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe [Prelude.Text])
awsEc2LaunchTemplateDataInstanceRequirementsDetails_localStorageTypes = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {localStorageTypes} -> localStorageTypes) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {localStorageTypes = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum amount of memory per vCPU, in GiB.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_memoryGiBPerVCpu :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_memoryGiBPerVCpu = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {memoryGiBPerVCpu} -> memoryGiBPerVCpu) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {memoryGiBPerVCpu = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | The minimum and maximum amount of memory, in MiB.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_memoryMiB :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_memoryMiB = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {memoryMiB} -> memoryMiB) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {memoryMiB = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | The minimum and maximum number of network interfaces.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_networkInterfaceCount :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_networkInterfaceCount = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {networkInterfaceCount} -> networkInterfaceCount) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {networkInterfaceCount = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | The price protection threshold for On-Demand Instances. This is the
-- maximum you’ll pay for an On-Demand Instance, expressed as a percentage
-- above the least expensive current generation M, C, or R instance type
-- with your specified attributes. When Amazon EC2 selects instance types
-- with your attributes, it excludes instance types priced above your
-- threshold.
--
-- The parameter accepts an integer, which Amazon EC2 interprets as a
-- percentage.
--
-- A high value, such as @999999@, turns off price protection.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_onDemandMaxPricePercentageOverLowestPrice :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_onDemandMaxPricePercentageOverLowestPrice = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {onDemandMaxPricePercentageOverLowestPrice} -> onDemandMaxPricePercentageOverLowestPrice) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {onDemandMaxPricePercentageOverLowestPrice = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | Indicates whether instance types must support hibernation for On-Demand
-- Instances.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_requireHibernateSupport :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_requireHibernateSupport = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {requireHibernateSupport} -> requireHibernateSupport) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {requireHibernateSupport = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | The price protection threshold for Spot Instances. This is the maximum
-- you’ll pay for a Spot Instance, expressed as a percentage above the
-- least expensive current generation M, C, or R instance type with your
-- specified attributes. When Amazon EC2 selects instance types with your
-- attributes, it excludes instance types priced above your threshold.
--
-- The parameter accepts an integer, which Amazon EC2 interprets as a
-- percentage.
--
-- A high value, such as @999999@, turns off price protection.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_spotMaxPricePercentageOverLowestPrice :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_spotMaxPricePercentageOverLowestPrice = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {spotMaxPricePercentageOverLowestPrice} -> spotMaxPricePercentageOverLowestPrice) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {spotMaxPricePercentageOverLowestPrice = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | The minimum and maximum amount of total local storage, in GB.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_totalLocalStorageGB :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_totalLocalStorageGB = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {totalLocalStorageGB} -> totalLocalStorageGB) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {totalLocalStorageGB = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

-- | The minimum and maximum number of vCPUs.
awsEc2LaunchTemplateDataInstanceRequirementsDetails_vCpuCount :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsDetails (Prelude.Maybe AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails)
awsEc2LaunchTemplateDataInstanceRequirementsDetails_vCpuCount = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {vCpuCount} -> vCpuCount) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {} a -> s {vCpuCount = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataInstanceRequirementsDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataInstanceRequirementsDetails'
            Prelude.<$> (x Data..:? "AcceleratorCount")
            Prelude.<*> ( x
                            Data..:? "AcceleratorManufacturers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "AcceleratorNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AcceleratorTotalMemoryMiB")
            Prelude.<*> ( x
                            Data..:? "AcceleratorTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "BareMetal")
            Prelude.<*> (x Data..:? "BaselineEbsBandwidthMbps")
            Prelude.<*> (x Data..:? "BurstablePerformance")
            Prelude.<*> ( x
                            Data..:? "CpuManufacturers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "ExcludedInstanceTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "InstanceGenerations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LocalStorage")
            Prelude.<*> ( x
                            Data..:? "LocalStorageTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MemoryGiBPerVCpu")
            Prelude.<*> (x Data..:? "MemoryMiB")
            Prelude.<*> (x Data..:? "NetworkInterfaceCount")
            Prelude.<*> ( x
                            Data..:? "OnDemandMaxPricePercentageOverLowestPrice"
                        )
            Prelude.<*> (x Data..:? "RequireHibernateSupport")
            Prelude.<*> (x Data..:? "SpotMaxPricePercentageOverLowestPrice")
            Prelude.<*> (x Data..:? "TotalLocalStorageGB")
            Prelude.<*> (x Data..:? "VCpuCount")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataInstanceRequirementsDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` acceleratorCount
        `Prelude.hashWithSalt` acceleratorManufacturers
        `Prelude.hashWithSalt` acceleratorNames
        `Prelude.hashWithSalt` acceleratorTotalMemoryMiB
        `Prelude.hashWithSalt` acceleratorTypes
        `Prelude.hashWithSalt` bareMetal
        `Prelude.hashWithSalt` baselineEbsBandwidthMbps
        `Prelude.hashWithSalt` burstablePerformance
        `Prelude.hashWithSalt` cpuManufacturers
        `Prelude.hashWithSalt` excludedInstanceTypes
        `Prelude.hashWithSalt` instanceGenerations
        `Prelude.hashWithSalt` localStorage
        `Prelude.hashWithSalt` localStorageTypes
        `Prelude.hashWithSalt` memoryGiBPerVCpu
        `Prelude.hashWithSalt` memoryMiB
        `Prelude.hashWithSalt` networkInterfaceCount
        `Prelude.hashWithSalt` onDemandMaxPricePercentageOverLowestPrice
        `Prelude.hashWithSalt` requireHibernateSupport
        `Prelude.hashWithSalt` spotMaxPricePercentageOverLowestPrice
        `Prelude.hashWithSalt` totalLocalStorageGB
        `Prelude.hashWithSalt` vCpuCount

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataInstanceRequirementsDetails
  where
  rnf
    AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {..} =
      Prelude.rnf acceleratorCount
        `Prelude.seq` Prelude.rnf acceleratorManufacturers
        `Prelude.seq` Prelude.rnf acceleratorNames
        `Prelude.seq` Prelude.rnf acceleratorTotalMemoryMiB
        `Prelude.seq` Prelude.rnf acceleratorTypes
        `Prelude.seq` Prelude.rnf bareMetal
        `Prelude.seq` Prelude.rnf baselineEbsBandwidthMbps
        `Prelude.seq` Prelude.rnf burstablePerformance
        `Prelude.seq` Prelude.rnf cpuManufacturers
        `Prelude.seq` Prelude.rnf excludedInstanceTypes
        `Prelude.seq` Prelude.rnf instanceGenerations
        `Prelude.seq` Prelude.rnf localStorage
        `Prelude.seq` Prelude.rnf localStorageTypes
        `Prelude.seq` Prelude.rnf memoryGiBPerVCpu
        `Prelude.seq` Prelude.rnf memoryMiB
        `Prelude.seq` Prelude.rnf networkInterfaceCount
        `Prelude.seq` Prelude.rnf
          onDemandMaxPricePercentageOverLowestPrice
        `Prelude.seq` Prelude.rnf
          requireHibernateSupport
        `Prelude.seq` Prelude.rnf
          spotMaxPricePercentageOverLowestPrice
        `Prelude.seq` Prelude.rnf
          totalLocalStorageGB
        `Prelude.seq` Prelude.rnf vCpuCount

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AcceleratorCount" Data..=)
                Prelude.<$> acceleratorCount,
              ("AcceleratorManufacturers" Data..=)
                Prelude.<$> acceleratorManufacturers,
              ("AcceleratorNames" Data..=)
                Prelude.<$> acceleratorNames,
              ("AcceleratorTotalMemoryMiB" Data..=)
                Prelude.<$> acceleratorTotalMemoryMiB,
              ("AcceleratorTypes" Data..=)
                Prelude.<$> acceleratorTypes,
              ("BareMetal" Data..=) Prelude.<$> bareMetal,
              ("BaselineEbsBandwidthMbps" Data..=)
                Prelude.<$> baselineEbsBandwidthMbps,
              ("BurstablePerformance" Data..=)
                Prelude.<$> burstablePerformance,
              ("CpuManufacturers" Data..=)
                Prelude.<$> cpuManufacturers,
              ("ExcludedInstanceTypes" Data..=)
                Prelude.<$> excludedInstanceTypes,
              ("InstanceGenerations" Data..=)
                Prelude.<$> instanceGenerations,
              ("LocalStorage" Data..=) Prelude.<$> localStorage,
              ("LocalStorageTypes" Data..=)
                Prelude.<$> localStorageTypes,
              ("MemoryGiBPerVCpu" Data..=)
                Prelude.<$> memoryGiBPerVCpu,
              ("MemoryMiB" Data..=) Prelude.<$> memoryMiB,
              ("NetworkInterfaceCount" Data..=)
                Prelude.<$> networkInterfaceCount,
              ("OnDemandMaxPricePercentageOverLowestPrice" Data..=)
                Prelude.<$> onDemandMaxPricePercentageOverLowestPrice,
              ("RequireHibernateSupport" Data..=)
                Prelude.<$> requireHibernateSupport,
              ("SpotMaxPricePercentageOverLowestPrice" Data..=)
                Prelude.<$> spotMaxPricePercentageOverLowestPrice,
              ("TotalLocalStorageGB" Data..=)
                Prelude.<$> totalLocalStorageGB,
              ("VCpuCount" Data..=) Prelude.<$> vCpuCount
            ]
        )
