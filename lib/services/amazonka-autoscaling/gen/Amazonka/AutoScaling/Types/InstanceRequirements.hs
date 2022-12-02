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
-- Module      : Amazonka.AutoScaling.Types.InstanceRequirements
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstanceRequirements where

import Amazonka.AutoScaling.Types.AcceleratorCountRequest
import Amazonka.AutoScaling.Types.AcceleratorManufacturer
import Amazonka.AutoScaling.Types.AcceleratorName
import Amazonka.AutoScaling.Types.AcceleratorTotalMemoryMiBRequest
import Amazonka.AutoScaling.Types.AcceleratorType
import Amazonka.AutoScaling.Types.BareMetal
import Amazonka.AutoScaling.Types.BaselineEbsBandwidthMbpsRequest
import Amazonka.AutoScaling.Types.BurstablePerformance
import Amazonka.AutoScaling.Types.CpuManufacturer
import Amazonka.AutoScaling.Types.InstanceGeneration
import Amazonka.AutoScaling.Types.LocalStorage
import Amazonka.AutoScaling.Types.LocalStorageType
import Amazonka.AutoScaling.Types.MemoryGiBPerVCpuRequest
import Amazonka.AutoScaling.Types.MemoryMiBRequest
import Amazonka.AutoScaling.Types.NetworkBandwidthGbpsRequest
import Amazonka.AutoScaling.Types.NetworkInterfaceCountRequest
import Amazonka.AutoScaling.Types.TotalLocalStorageGBRequest
import Amazonka.AutoScaling.Types.VCpuCountRequest
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The attributes for the instance types for a mixed instances policy.
-- Amazon EC2 Auto Scaling uses your specified requirements to identify
-- instance types. Then, it uses your On-Demand and Spot allocation
-- strategies to launch instances from these instance types.
--
-- When you specify multiple attributes, you get instance types that
-- satisfy all of the specified attributes. If you specify multiple values
-- for an attribute, you get instance types that satisfy any of the
-- specified values.
--
-- To limit the list of instance types from which Amazon EC2 Auto Scaling
-- can identify matching instance types, you can use one of the following
-- parameters, but not both in the same request:
--
-- -   @AllowedInstanceTypes@ - The instance types to include in the list.
--     All other instance types are ignored, even if they match your
--     specified attributes.
--
-- -   @ExcludedInstanceTypes@ - The instance types to exclude from the
--     list, even if they match your specified attributes.
--
-- You must specify @VCpuCount@ and @MemoryMiB@. All other attributes are
-- optional. Any unspecified optional attribute is set to its default.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-instance-type-requirements.html Creating an Auto Scaling group using attribute-based instance type selection>
-- in the /Amazon EC2 Auto Scaling User Guide/. For help determining which
-- instance types match your attributes before you apply them to your Auto
-- Scaling group, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-attribute-based-instance-type-selection.html#ec2fleet-get-instance-types-from-instance-requirements Preview instance types with specified attributes>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- /See:/ 'newInstanceRequirements' smart constructor.
data InstanceRequirements = InstanceRequirements'
  { -- | Indicates whether current or previous generation instance types are
    -- included.
    --
    -- -   For current generation instance types, specify @current@. The
    --     current generation includes EC2 instance types currently recommended
    --     for use. This typically includes the latest two to three generations
    --     in each instance family. For more information, see
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    --     in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- -   For previous generation instance types, specify @previous@.
    --
    -- Default: Any current or previous generation
    instanceGenerations :: Prelude.Maybe [InstanceGeneration],
    -- | The minimum and maximum baseline bandwidth performance for an instance
    -- type, in Mbps. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS–optimized instances>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- Default: No minimum or maximum limits
    baselineEbsBandwidthMbps :: Prelude.Maybe BaselineEbsBandwidthMbpsRequest,
    -- | Indicates whether bare metal instance types are included, excluded, or
    -- required.
    --
    -- Default: @excluded@
    bareMetal :: Prelude.Maybe BareMetal,
    -- | The price protection threshold for Spot Instances. This is the maximum
    -- you’ll pay for a Spot Instance, expressed as a percentage higher than
    -- the least expensive current generation M, C, or R instance type with
    -- your specified attributes. When Amazon EC2 Auto Scaling selects instance
    -- types with your attributes, we will exclude instance types whose price
    -- is higher than your threshold. The parameter accepts an integer, which
    -- Amazon EC2 Auto Scaling interprets as a percentage. To turn off price
    -- protection, specify a high value, such as @999999@.
    --
    -- If you set @DesiredCapacityType@ to @vcpu@ or @memory-mib@, the price
    -- protection threshold is applied based on the per vCPU or per memory
    -- price instead of the per instance price.
    --
    -- Default: @100@
    spotMaxPricePercentageOverLowestPrice :: Prelude.Maybe Prelude.Natural,
    -- | Lists the accelerator types that must be on an instance type.
    --
    -- -   For instance types with GPU accelerators, specify @gpu@.
    --
    -- -   For instance types with FPGA accelerators, specify @fpga@.
    --
    -- -   For instance types with inference accelerators, specify @inference@.
    --
    -- Default: Any accelerator type
    acceleratorTypes :: Prelude.Maybe [AcceleratorType],
    -- | The minimum and maximum total local storage size for an instance type,
    -- in GB.
    --
    -- Default: No minimum or maximum limits
    totalLocalStorageGB :: Prelude.Maybe TotalLocalStorageGBRequest,
    -- | Indicates the type of local storage that is required.
    --
    -- -   For instance types with hard disk drive (HDD) storage, specify
    --     @hdd@.
    --
    -- -   For instance types with solid state drive (SSD) storage, specify
    --     @ssd@.
    --
    -- Default: Any local storage type
    localStorageTypes :: Prelude.Maybe [LocalStorageType],
    -- | The price protection threshold for On-Demand Instances. This is the
    -- maximum you’ll pay for an On-Demand Instance, expressed as a percentage
    -- higher than the least expensive current generation M, C, or R instance
    -- type with your specified attributes. When Amazon EC2 Auto Scaling
    -- selects instance types with your attributes, we will exclude instance
    -- types whose price is higher than your threshold. The parameter accepts
    -- an integer, which Amazon EC2 Auto Scaling interprets as a percentage. To
    -- turn off price protection, specify a high value, such as @999999@.
    --
    -- If you set @DesiredCapacityType@ to @vcpu@ or @memory-mib@, the price
    -- protection threshold is applied based on the per vCPU or per memory
    -- price instead of the per instance price.
    --
    -- Default: @20@
    onDemandMaxPricePercentageOverLowestPrice :: Prelude.Maybe Prelude.Natural,
    -- | The instance types to apply your specified attributes against. All other
    -- instance types are ignored, even if they match your specified
    -- attributes.
    --
    -- You can use strings with one or more wild cards, represented by an
    -- asterisk (@*@), to allow an instance type, size, or generation. The
    -- following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@, @r*@, @*3*@.
    --
    -- For example, if you specify @c5*@, Amazon EC2 Auto Scaling will allow
    -- the entire C5 instance family, which includes all C5a and C5n instance
    -- types. If you specify @m5a.*@, Amazon EC2 Auto Scaling will allow all
    -- the M5a instance types, but not the M5n instance types.
    --
    -- If you specify @AllowedInstanceTypes@, you can\'t specify
    -- @ExcludedInstanceTypes@.
    --
    -- Default: All instance types
    allowedInstanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | Lists the accelerators that must be on an instance type.
    --
    -- -   For instance types with NVIDIA A100 GPUs, specify @a100@.
    --
    -- -   For instance types with NVIDIA V100 GPUs, specify @v100@.
    --
    -- -   For instance types with NVIDIA K80 GPUs, specify @k80@.
    --
    -- -   For instance types with NVIDIA T4 GPUs, specify @t4@.
    --
    -- -   For instance types with NVIDIA M60 GPUs, specify @m60@.
    --
    -- -   For instance types with AMD Radeon Pro V520 GPUs, specify
    --     @radeon-pro-v520@.
    --
    -- -   For instance types with Xilinx VU9P FPGAs, specify @vu9p@.
    --
    -- Default: Any accelerator
    acceleratorNames :: Prelude.Maybe [AcceleratorName],
    -- | The minimum and maximum amount of network bandwidth, in gigabits per
    -- second (Gbps).
    --
    -- Default: No minimum or maximum limits
    networkBandwidthGbps :: Prelude.Maybe NetworkBandwidthGbpsRequest,
    -- | Indicates whether instance types must have accelerators by specific
    -- manufacturers.
    --
    -- -   For instance types with NVIDIA devices, specify @nvidia@.
    --
    -- -   For instance types with AMD devices, specify @amd@.
    --
    -- -   For instance types with Amazon Web Services devices, specify
    --     @amazon-web-services@.
    --
    -- -   For instance types with Xilinx devices, specify @xilinx@.
    --
    -- Default: Any manufacturer
    acceleratorManufacturers :: Prelude.Maybe [AcceleratorManufacturer],
    -- | The instance types to exclude. You can use strings with one or more wild
    -- cards, represented by an asterisk (@*@), to exclude an instance family,
    -- type, size, or generation. The following are examples: @m5.8xlarge@,
    -- @c5*.*@, @m5a.*@, @r*@, @*3*@.
    --
    -- For example, if you specify @c5*@, you are excluding the entire C5
    -- instance family, which includes all C5a and C5n instance types. If you
    -- specify @m5a.*@, Amazon EC2 Auto Scaling will exclude all the M5a
    -- instance types, but not the M5n instance types.
    --
    -- If you specify @ExcludedInstanceTypes@, you can\'t specify
    -- @AllowedInstanceTypes@.
    --
    -- Default: No excluded instance types
    excludedInstanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The minimum and maximum number of network interfaces for an instance
    -- type.
    --
    -- Default: No minimum or maximum limits
    networkInterfaceCount :: Prelude.Maybe NetworkInterfaceCountRequest,
    -- | Indicates whether instance types must provide On-Demand Instance
    -- hibernation support.
    --
    -- Default: @false@
    requireHibernateSupport :: Prelude.Maybe Prelude.Bool,
    -- | The minimum and maximum total memory size for the accelerators on an
    -- instance type, in MiB.
    --
    -- Default: No minimum or maximum limits
    acceleratorTotalMemoryMiB :: Prelude.Maybe AcceleratorTotalMemoryMiBRequest,
    -- | The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
    -- Web Services Inferentia chips) for an instance type.
    --
    -- To exclude accelerator-enabled instance types, set @Max@ to @0@.
    --
    -- Default: No minimum or maximum limits
    acceleratorCount :: Prelude.Maybe AcceleratorCountRequest,
    -- | Indicates whether burstable performance instance types are included,
    -- excluded, or required. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- Default: @excluded@
    burstablePerformance :: Prelude.Maybe BurstablePerformance,
    -- | Lists which specific CPU manufacturers to include.
    --
    -- -   For instance types with Intel CPUs, specify @intel@.
    --
    -- -   For instance types with AMD CPUs, specify @amd@.
    --
    -- -   For instance types with Amazon Web Services CPUs, specify
    --     @amazon-web-services@.
    --
    -- Don\'t confuse the CPU hardware manufacturer with the CPU hardware
    -- architecture. Instances will be launched with a compatible CPU
    -- architecture based on the Amazon Machine Image (AMI) that you specify in
    -- your launch template.
    --
    -- Default: Any manufacturer
    cpuManufacturers :: Prelude.Maybe [CpuManufacturer],
    -- | The minimum and maximum amount of memory per vCPU for an instance type,
    -- in GiB.
    --
    -- Default: No minimum or maximum limits
    memoryGiBPerVCpu :: Prelude.Maybe MemoryGiBPerVCpuRequest,
    -- | Indicates whether instance types with instance store volumes are
    -- included, excluded, or required. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html Amazon EC2 instance store>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- Default: @included@
    localStorage :: Prelude.Maybe LocalStorage,
    -- | The minimum and maximum number of vCPUs for an instance type.
    vCpuCount :: VCpuCountRequest,
    -- | The minimum and maximum instance memory size for an instance type, in
    -- MiB.
    memoryMiB :: MemoryMiBRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceRequirements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceGenerations', 'instanceRequirements_instanceGenerations' - Indicates whether current or previous generation instance types are
-- included.
--
-- -   For current generation instance types, specify @current@. The
--     current generation includes EC2 instance types currently recommended
--     for use. This typically includes the latest two to three generations
--     in each instance family. For more information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
--     in the /Amazon EC2 User Guide for Linux Instances/.
--
-- -   For previous generation instance types, specify @previous@.
--
-- Default: Any current or previous generation
--
-- 'baselineEbsBandwidthMbps', 'instanceRequirements_baselineEbsBandwidthMbps' - The minimum and maximum baseline bandwidth performance for an instance
-- type, in Mbps. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS–optimized instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Default: No minimum or maximum limits
--
-- 'bareMetal', 'instanceRequirements_bareMetal' - Indicates whether bare metal instance types are included, excluded, or
-- required.
--
-- Default: @excluded@
--
-- 'spotMaxPricePercentageOverLowestPrice', 'instanceRequirements_spotMaxPricePercentageOverLowestPrice' - The price protection threshold for Spot Instances. This is the maximum
-- you’ll pay for a Spot Instance, expressed as a percentage higher than
-- the least expensive current generation M, C, or R instance type with
-- your specified attributes. When Amazon EC2 Auto Scaling selects instance
-- types with your attributes, we will exclude instance types whose price
-- is higher than your threshold. The parameter accepts an integer, which
-- Amazon EC2 Auto Scaling interprets as a percentage. To turn off price
-- protection, specify a high value, such as @999999@.
--
-- If you set @DesiredCapacityType@ to @vcpu@ or @memory-mib@, the price
-- protection threshold is applied based on the per vCPU or per memory
-- price instead of the per instance price.
--
-- Default: @100@
--
-- 'acceleratorTypes', 'instanceRequirements_acceleratorTypes' - Lists the accelerator types that must be on an instance type.
--
-- -   For instance types with GPU accelerators, specify @gpu@.
--
-- -   For instance types with FPGA accelerators, specify @fpga@.
--
-- -   For instance types with inference accelerators, specify @inference@.
--
-- Default: Any accelerator type
--
-- 'totalLocalStorageGB', 'instanceRequirements_totalLocalStorageGB' - The minimum and maximum total local storage size for an instance type,
-- in GB.
--
-- Default: No minimum or maximum limits
--
-- 'localStorageTypes', 'instanceRequirements_localStorageTypes' - Indicates the type of local storage that is required.
--
-- -   For instance types with hard disk drive (HDD) storage, specify
--     @hdd@.
--
-- -   For instance types with solid state drive (SSD) storage, specify
--     @ssd@.
--
-- Default: Any local storage type
--
-- 'onDemandMaxPricePercentageOverLowestPrice', 'instanceRequirements_onDemandMaxPricePercentageOverLowestPrice' - The price protection threshold for On-Demand Instances. This is the
-- maximum you’ll pay for an On-Demand Instance, expressed as a percentage
-- higher than the least expensive current generation M, C, or R instance
-- type with your specified attributes. When Amazon EC2 Auto Scaling
-- selects instance types with your attributes, we will exclude instance
-- types whose price is higher than your threshold. The parameter accepts
-- an integer, which Amazon EC2 Auto Scaling interprets as a percentage. To
-- turn off price protection, specify a high value, such as @999999@.
--
-- If you set @DesiredCapacityType@ to @vcpu@ or @memory-mib@, the price
-- protection threshold is applied based on the per vCPU or per memory
-- price instead of the per instance price.
--
-- Default: @20@
--
-- 'allowedInstanceTypes', 'instanceRequirements_allowedInstanceTypes' - The instance types to apply your specified attributes against. All other
-- instance types are ignored, even if they match your specified
-- attributes.
--
-- You can use strings with one or more wild cards, represented by an
-- asterisk (@*@), to allow an instance type, size, or generation. The
-- following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@, @r*@, @*3*@.
--
-- For example, if you specify @c5*@, Amazon EC2 Auto Scaling will allow
-- the entire C5 instance family, which includes all C5a and C5n instance
-- types. If you specify @m5a.*@, Amazon EC2 Auto Scaling will allow all
-- the M5a instance types, but not the M5n instance types.
--
-- If you specify @AllowedInstanceTypes@, you can\'t specify
-- @ExcludedInstanceTypes@.
--
-- Default: All instance types
--
-- 'acceleratorNames', 'instanceRequirements_acceleratorNames' - Lists the accelerators that must be on an instance type.
--
-- -   For instance types with NVIDIA A100 GPUs, specify @a100@.
--
-- -   For instance types with NVIDIA V100 GPUs, specify @v100@.
--
-- -   For instance types with NVIDIA K80 GPUs, specify @k80@.
--
-- -   For instance types with NVIDIA T4 GPUs, specify @t4@.
--
-- -   For instance types with NVIDIA M60 GPUs, specify @m60@.
--
-- -   For instance types with AMD Radeon Pro V520 GPUs, specify
--     @radeon-pro-v520@.
--
-- -   For instance types with Xilinx VU9P FPGAs, specify @vu9p@.
--
-- Default: Any accelerator
--
-- 'networkBandwidthGbps', 'instanceRequirements_networkBandwidthGbps' - The minimum and maximum amount of network bandwidth, in gigabits per
-- second (Gbps).
--
-- Default: No minimum or maximum limits
--
-- 'acceleratorManufacturers', 'instanceRequirements_acceleratorManufacturers' - Indicates whether instance types must have accelerators by specific
-- manufacturers.
--
-- -   For instance types with NVIDIA devices, specify @nvidia@.
--
-- -   For instance types with AMD devices, specify @amd@.
--
-- -   For instance types with Amazon Web Services devices, specify
--     @amazon-web-services@.
--
-- -   For instance types with Xilinx devices, specify @xilinx@.
--
-- Default: Any manufacturer
--
-- 'excludedInstanceTypes', 'instanceRequirements_excludedInstanceTypes' - The instance types to exclude. You can use strings with one or more wild
-- cards, represented by an asterisk (@*@), to exclude an instance family,
-- type, size, or generation. The following are examples: @m5.8xlarge@,
-- @c5*.*@, @m5a.*@, @r*@, @*3*@.
--
-- For example, if you specify @c5*@, you are excluding the entire C5
-- instance family, which includes all C5a and C5n instance types. If you
-- specify @m5a.*@, Amazon EC2 Auto Scaling will exclude all the M5a
-- instance types, but not the M5n instance types.
--
-- If you specify @ExcludedInstanceTypes@, you can\'t specify
-- @AllowedInstanceTypes@.
--
-- Default: No excluded instance types
--
-- 'networkInterfaceCount', 'instanceRequirements_networkInterfaceCount' - The minimum and maximum number of network interfaces for an instance
-- type.
--
-- Default: No minimum or maximum limits
--
-- 'requireHibernateSupport', 'instanceRequirements_requireHibernateSupport' - Indicates whether instance types must provide On-Demand Instance
-- hibernation support.
--
-- Default: @false@
--
-- 'acceleratorTotalMemoryMiB', 'instanceRequirements_acceleratorTotalMemoryMiB' - The minimum and maximum total memory size for the accelerators on an
-- instance type, in MiB.
--
-- Default: No minimum or maximum limits
--
-- 'acceleratorCount', 'instanceRequirements_acceleratorCount' - The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
-- Web Services Inferentia chips) for an instance type.
--
-- To exclude accelerator-enabled instance types, set @Max@ to @0@.
--
-- Default: No minimum or maximum limits
--
-- 'burstablePerformance', 'instanceRequirements_burstablePerformance' - Indicates whether burstable performance instance types are included,
-- excluded, or required. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Default: @excluded@
--
-- 'cpuManufacturers', 'instanceRequirements_cpuManufacturers' - Lists which specific CPU manufacturers to include.
--
-- -   For instance types with Intel CPUs, specify @intel@.
--
-- -   For instance types with AMD CPUs, specify @amd@.
--
-- -   For instance types with Amazon Web Services CPUs, specify
--     @amazon-web-services@.
--
-- Don\'t confuse the CPU hardware manufacturer with the CPU hardware
-- architecture. Instances will be launched with a compatible CPU
-- architecture based on the Amazon Machine Image (AMI) that you specify in
-- your launch template.
--
-- Default: Any manufacturer
--
-- 'memoryGiBPerVCpu', 'instanceRequirements_memoryGiBPerVCpu' - The minimum and maximum amount of memory per vCPU for an instance type,
-- in GiB.
--
-- Default: No minimum or maximum limits
--
-- 'localStorage', 'instanceRequirements_localStorage' - Indicates whether instance types with instance store volumes are
-- included, excluded, or required. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html Amazon EC2 instance store>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Default: @included@
--
-- 'vCpuCount', 'instanceRequirements_vCpuCount' - The minimum and maximum number of vCPUs for an instance type.
--
-- 'memoryMiB', 'instanceRequirements_memoryMiB' - The minimum and maximum instance memory size for an instance type, in
-- MiB.
newInstanceRequirements ::
  -- | 'vCpuCount'
  VCpuCountRequest ->
  -- | 'memoryMiB'
  MemoryMiBRequest ->
  InstanceRequirements
newInstanceRequirements pVCpuCount_ pMemoryMiB_ =
  InstanceRequirements'
    { instanceGenerations =
        Prelude.Nothing,
      baselineEbsBandwidthMbps = Prelude.Nothing,
      bareMetal = Prelude.Nothing,
      spotMaxPricePercentageOverLowestPrice =
        Prelude.Nothing,
      acceleratorTypes = Prelude.Nothing,
      totalLocalStorageGB = Prelude.Nothing,
      localStorageTypes = Prelude.Nothing,
      onDemandMaxPricePercentageOverLowestPrice =
        Prelude.Nothing,
      allowedInstanceTypes = Prelude.Nothing,
      acceleratorNames = Prelude.Nothing,
      networkBandwidthGbps = Prelude.Nothing,
      acceleratorManufacturers = Prelude.Nothing,
      excludedInstanceTypes = Prelude.Nothing,
      networkInterfaceCount = Prelude.Nothing,
      requireHibernateSupport = Prelude.Nothing,
      acceleratorTotalMemoryMiB = Prelude.Nothing,
      acceleratorCount = Prelude.Nothing,
      burstablePerformance = Prelude.Nothing,
      cpuManufacturers = Prelude.Nothing,
      memoryGiBPerVCpu = Prelude.Nothing,
      localStorage = Prelude.Nothing,
      vCpuCount = pVCpuCount_,
      memoryMiB = pMemoryMiB_
    }

-- | Indicates whether current or previous generation instance types are
-- included.
--
-- -   For current generation instance types, specify @current@. The
--     current generation includes EC2 instance types currently recommended
--     for use. This typically includes the latest two to three generations
--     in each instance family. For more information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
--     in the /Amazon EC2 User Guide for Linux Instances/.
--
-- -   For previous generation instance types, specify @previous@.
--
-- Default: Any current or previous generation
instanceRequirements_instanceGenerations :: Lens.Lens' InstanceRequirements (Prelude.Maybe [InstanceGeneration])
instanceRequirements_instanceGenerations = Lens.lens (\InstanceRequirements' {instanceGenerations} -> instanceGenerations) (\s@InstanceRequirements' {} a -> s {instanceGenerations = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum baseline bandwidth performance for an instance
-- type, in Mbps. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS–optimized instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Default: No minimum or maximum limits
instanceRequirements_baselineEbsBandwidthMbps :: Lens.Lens' InstanceRequirements (Prelude.Maybe BaselineEbsBandwidthMbpsRequest)
instanceRequirements_baselineEbsBandwidthMbps = Lens.lens (\InstanceRequirements' {baselineEbsBandwidthMbps} -> baselineEbsBandwidthMbps) (\s@InstanceRequirements' {} a -> s {baselineEbsBandwidthMbps = a} :: InstanceRequirements)

-- | Indicates whether bare metal instance types are included, excluded, or
-- required.
--
-- Default: @excluded@
instanceRequirements_bareMetal :: Lens.Lens' InstanceRequirements (Prelude.Maybe BareMetal)
instanceRequirements_bareMetal = Lens.lens (\InstanceRequirements' {bareMetal} -> bareMetal) (\s@InstanceRequirements' {} a -> s {bareMetal = a} :: InstanceRequirements)

-- | The price protection threshold for Spot Instances. This is the maximum
-- you’ll pay for a Spot Instance, expressed as a percentage higher than
-- the least expensive current generation M, C, or R instance type with
-- your specified attributes. When Amazon EC2 Auto Scaling selects instance
-- types with your attributes, we will exclude instance types whose price
-- is higher than your threshold. The parameter accepts an integer, which
-- Amazon EC2 Auto Scaling interprets as a percentage. To turn off price
-- protection, specify a high value, such as @999999@.
--
-- If you set @DesiredCapacityType@ to @vcpu@ or @memory-mib@, the price
-- protection threshold is applied based on the per vCPU or per memory
-- price instead of the per instance price.
--
-- Default: @100@
instanceRequirements_spotMaxPricePercentageOverLowestPrice :: Lens.Lens' InstanceRequirements (Prelude.Maybe Prelude.Natural)
instanceRequirements_spotMaxPricePercentageOverLowestPrice = Lens.lens (\InstanceRequirements' {spotMaxPricePercentageOverLowestPrice} -> spotMaxPricePercentageOverLowestPrice) (\s@InstanceRequirements' {} a -> s {spotMaxPricePercentageOverLowestPrice = a} :: InstanceRequirements)

-- | Lists the accelerator types that must be on an instance type.
--
-- -   For instance types with GPU accelerators, specify @gpu@.
--
-- -   For instance types with FPGA accelerators, specify @fpga@.
--
-- -   For instance types with inference accelerators, specify @inference@.
--
-- Default: Any accelerator type
instanceRequirements_acceleratorTypes :: Lens.Lens' InstanceRequirements (Prelude.Maybe [AcceleratorType])
instanceRequirements_acceleratorTypes = Lens.lens (\InstanceRequirements' {acceleratorTypes} -> acceleratorTypes) (\s@InstanceRequirements' {} a -> s {acceleratorTypes = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum total local storage size for an instance type,
-- in GB.
--
-- Default: No minimum or maximum limits
instanceRequirements_totalLocalStorageGB :: Lens.Lens' InstanceRequirements (Prelude.Maybe TotalLocalStorageGBRequest)
instanceRequirements_totalLocalStorageGB = Lens.lens (\InstanceRequirements' {totalLocalStorageGB} -> totalLocalStorageGB) (\s@InstanceRequirements' {} a -> s {totalLocalStorageGB = a} :: InstanceRequirements)

-- | Indicates the type of local storage that is required.
--
-- -   For instance types with hard disk drive (HDD) storage, specify
--     @hdd@.
--
-- -   For instance types with solid state drive (SSD) storage, specify
--     @ssd@.
--
-- Default: Any local storage type
instanceRequirements_localStorageTypes :: Lens.Lens' InstanceRequirements (Prelude.Maybe [LocalStorageType])
instanceRequirements_localStorageTypes = Lens.lens (\InstanceRequirements' {localStorageTypes} -> localStorageTypes) (\s@InstanceRequirements' {} a -> s {localStorageTypes = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The price protection threshold for On-Demand Instances. This is the
-- maximum you’ll pay for an On-Demand Instance, expressed as a percentage
-- higher than the least expensive current generation M, C, or R instance
-- type with your specified attributes. When Amazon EC2 Auto Scaling
-- selects instance types with your attributes, we will exclude instance
-- types whose price is higher than your threshold. The parameter accepts
-- an integer, which Amazon EC2 Auto Scaling interprets as a percentage. To
-- turn off price protection, specify a high value, such as @999999@.
--
-- If you set @DesiredCapacityType@ to @vcpu@ or @memory-mib@, the price
-- protection threshold is applied based on the per vCPU or per memory
-- price instead of the per instance price.
--
-- Default: @20@
instanceRequirements_onDemandMaxPricePercentageOverLowestPrice :: Lens.Lens' InstanceRequirements (Prelude.Maybe Prelude.Natural)
instanceRequirements_onDemandMaxPricePercentageOverLowestPrice = Lens.lens (\InstanceRequirements' {onDemandMaxPricePercentageOverLowestPrice} -> onDemandMaxPricePercentageOverLowestPrice) (\s@InstanceRequirements' {} a -> s {onDemandMaxPricePercentageOverLowestPrice = a} :: InstanceRequirements)

-- | The instance types to apply your specified attributes against. All other
-- instance types are ignored, even if they match your specified
-- attributes.
--
-- You can use strings with one or more wild cards, represented by an
-- asterisk (@*@), to allow an instance type, size, or generation. The
-- following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@, @r*@, @*3*@.
--
-- For example, if you specify @c5*@, Amazon EC2 Auto Scaling will allow
-- the entire C5 instance family, which includes all C5a and C5n instance
-- types. If you specify @m5a.*@, Amazon EC2 Auto Scaling will allow all
-- the M5a instance types, but not the M5n instance types.
--
-- If you specify @AllowedInstanceTypes@, you can\'t specify
-- @ExcludedInstanceTypes@.
--
-- Default: All instance types
instanceRequirements_allowedInstanceTypes :: Lens.Lens' InstanceRequirements (Prelude.Maybe [Prelude.Text])
instanceRequirements_allowedInstanceTypes = Lens.lens (\InstanceRequirements' {allowedInstanceTypes} -> allowedInstanceTypes) (\s@InstanceRequirements' {} a -> s {allowedInstanceTypes = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | Lists the accelerators that must be on an instance type.
--
-- -   For instance types with NVIDIA A100 GPUs, specify @a100@.
--
-- -   For instance types with NVIDIA V100 GPUs, specify @v100@.
--
-- -   For instance types with NVIDIA K80 GPUs, specify @k80@.
--
-- -   For instance types with NVIDIA T4 GPUs, specify @t4@.
--
-- -   For instance types with NVIDIA M60 GPUs, specify @m60@.
--
-- -   For instance types with AMD Radeon Pro V520 GPUs, specify
--     @radeon-pro-v520@.
--
-- -   For instance types with Xilinx VU9P FPGAs, specify @vu9p@.
--
-- Default: Any accelerator
instanceRequirements_acceleratorNames :: Lens.Lens' InstanceRequirements (Prelude.Maybe [AcceleratorName])
instanceRequirements_acceleratorNames = Lens.lens (\InstanceRequirements' {acceleratorNames} -> acceleratorNames) (\s@InstanceRequirements' {} a -> s {acceleratorNames = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum amount of network bandwidth, in gigabits per
-- second (Gbps).
--
-- Default: No minimum or maximum limits
instanceRequirements_networkBandwidthGbps :: Lens.Lens' InstanceRequirements (Prelude.Maybe NetworkBandwidthGbpsRequest)
instanceRequirements_networkBandwidthGbps = Lens.lens (\InstanceRequirements' {networkBandwidthGbps} -> networkBandwidthGbps) (\s@InstanceRequirements' {} a -> s {networkBandwidthGbps = a} :: InstanceRequirements)

-- | Indicates whether instance types must have accelerators by specific
-- manufacturers.
--
-- -   For instance types with NVIDIA devices, specify @nvidia@.
--
-- -   For instance types with AMD devices, specify @amd@.
--
-- -   For instance types with Amazon Web Services devices, specify
--     @amazon-web-services@.
--
-- -   For instance types with Xilinx devices, specify @xilinx@.
--
-- Default: Any manufacturer
instanceRequirements_acceleratorManufacturers :: Lens.Lens' InstanceRequirements (Prelude.Maybe [AcceleratorManufacturer])
instanceRequirements_acceleratorManufacturers = Lens.lens (\InstanceRequirements' {acceleratorManufacturers} -> acceleratorManufacturers) (\s@InstanceRequirements' {} a -> s {acceleratorManufacturers = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The instance types to exclude. You can use strings with one or more wild
-- cards, represented by an asterisk (@*@), to exclude an instance family,
-- type, size, or generation. The following are examples: @m5.8xlarge@,
-- @c5*.*@, @m5a.*@, @r*@, @*3*@.
--
-- For example, if you specify @c5*@, you are excluding the entire C5
-- instance family, which includes all C5a and C5n instance types. If you
-- specify @m5a.*@, Amazon EC2 Auto Scaling will exclude all the M5a
-- instance types, but not the M5n instance types.
--
-- If you specify @ExcludedInstanceTypes@, you can\'t specify
-- @AllowedInstanceTypes@.
--
-- Default: No excluded instance types
instanceRequirements_excludedInstanceTypes :: Lens.Lens' InstanceRequirements (Prelude.Maybe [Prelude.Text])
instanceRequirements_excludedInstanceTypes = Lens.lens (\InstanceRequirements' {excludedInstanceTypes} -> excludedInstanceTypes) (\s@InstanceRequirements' {} a -> s {excludedInstanceTypes = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum number of network interfaces for an instance
-- type.
--
-- Default: No minimum or maximum limits
instanceRequirements_networkInterfaceCount :: Lens.Lens' InstanceRequirements (Prelude.Maybe NetworkInterfaceCountRequest)
instanceRequirements_networkInterfaceCount = Lens.lens (\InstanceRequirements' {networkInterfaceCount} -> networkInterfaceCount) (\s@InstanceRequirements' {} a -> s {networkInterfaceCount = a} :: InstanceRequirements)

-- | Indicates whether instance types must provide On-Demand Instance
-- hibernation support.
--
-- Default: @false@
instanceRequirements_requireHibernateSupport :: Lens.Lens' InstanceRequirements (Prelude.Maybe Prelude.Bool)
instanceRequirements_requireHibernateSupport = Lens.lens (\InstanceRequirements' {requireHibernateSupport} -> requireHibernateSupport) (\s@InstanceRequirements' {} a -> s {requireHibernateSupport = a} :: InstanceRequirements)

-- | The minimum and maximum total memory size for the accelerators on an
-- instance type, in MiB.
--
-- Default: No minimum or maximum limits
instanceRequirements_acceleratorTotalMemoryMiB :: Lens.Lens' InstanceRequirements (Prelude.Maybe AcceleratorTotalMemoryMiBRequest)
instanceRequirements_acceleratorTotalMemoryMiB = Lens.lens (\InstanceRequirements' {acceleratorTotalMemoryMiB} -> acceleratorTotalMemoryMiB) (\s@InstanceRequirements' {} a -> s {acceleratorTotalMemoryMiB = a} :: InstanceRequirements)

-- | The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
-- Web Services Inferentia chips) for an instance type.
--
-- To exclude accelerator-enabled instance types, set @Max@ to @0@.
--
-- Default: No minimum or maximum limits
instanceRequirements_acceleratorCount :: Lens.Lens' InstanceRequirements (Prelude.Maybe AcceleratorCountRequest)
instanceRequirements_acceleratorCount = Lens.lens (\InstanceRequirements' {acceleratorCount} -> acceleratorCount) (\s@InstanceRequirements' {} a -> s {acceleratorCount = a} :: InstanceRequirements)

-- | Indicates whether burstable performance instance types are included,
-- excluded, or required. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Default: @excluded@
instanceRequirements_burstablePerformance :: Lens.Lens' InstanceRequirements (Prelude.Maybe BurstablePerformance)
instanceRequirements_burstablePerformance = Lens.lens (\InstanceRequirements' {burstablePerformance} -> burstablePerformance) (\s@InstanceRequirements' {} a -> s {burstablePerformance = a} :: InstanceRequirements)

-- | Lists which specific CPU manufacturers to include.
--
-- -   For instance types with Intel CPUs, specify @intel@.
--
-- -   For instance types with AMD CPUs, specify @amd@.
--
-- -   For instance types with Amazon Web Services CPUs, specify
--     @amazon-web-services@.
--
-- Don\'t confuse the CPU hardware manufacturer with the CPU hardware
-- architecture. Instances will be launched with a compatible CPU
-- architecture based on the Amazon Machine Image (AMI) that you specify in
-- your launch template.
--
-- Default: Any manufacturer
instanceRequirements_cpuManufacturers :: Lens.Lens' InstanceRequirements (Prelude.Maybe [CpuManufacturer])
instanceRequirements_cpuManufacturers = Lens.lens (\InstanceRequirements' {cpuManufacturers} -> cpuManufacturers) (\s@InstanceRequirements' {} a -> s {cpuManufacturers = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum amount of memory per vCPU for an instance type,
-- in GiB.
--
-- Default: No minimum or maximum limits
instanceRequirements_memoryGiBPerVCpu :: Lens.Lens' InstanceRequirements (Prelude.Maybe MemoryGiBPerVCpuRequest)
instanceRequirements_memoryGiBPerVCpu = Lens.lens (\InstanceRequirements' {memoryGiBPerVCpu} -> memoryGiBPerVCpu) (\s@InstanceRequirements' {} a -> s {memoryGiBPerVCpu = a} :: InstanceRequirements)

-- | Indicates whether instance types with instance store volumes are
-- included, excluded, or required. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html Amazon EC2 instance store>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- Default: @included@
instanceRequirements_localStorage :: Lens.Lens' InstanceRequirements (Prelude.Maybe LocalStorage)
instanceRequirements_localStorage = Lens.lens (\InstanceRequirements' {localStorage} -> localStorage) (\s@InstanceRequirements' {} a -> s {localStorage = a} :: InstanceRequirements)

-- | The minimum and maximum number of vCPUs for an instance type.
instanceRequirements_vCpuCount :: Lens.Lens' InstanceRequirements VCpuCountRequest
instanceRequirements_vCpuCount = Lens.lens (\InstanceRequirements' {vCpuCount} -> vCpuCount) (\s@InstanceRequirements' {} a -> s {vCpuCount = a} :: InstanceRequirements)

-- | The minimum and maximum instance memory size for an instance type, in
-- MiB.
instanceRequirements_memoryMiB :: Lens.Lens' InstanceRequirements MemoryMiBRequest
instanceRequirements_memoryMiB = Lens.lens (\InstanceRequirements' {memoryMiB} -> memoryMiB) (\s@InstanceRequirements' {} a -> s {memoryMiB = a} :: InstanceRequirements)

instance Data.FromXML InstanceRequirements where
  parseXML x =
    InstanceRequirements'
      Prelude.<$> ( x Data..@? "InstanceGenerations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "BaselineEbsBandwidthMbps")
      Prelude.<*> (x Data..@? "BareMetal")
      Prelude.<*> (x Data..@? "SpotMaxPricePercentageOverLowestPrice")
      Prelude.<*> ( x Data..@? "AcceleratorTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "TotalLocalStorageGB")
      Prelude.<*> ( x Data..@? "LocalStorageTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "OnDemandMaxPricePercentageOverLowestPrice"
                  )
      Prelude.<*> ( x Data..@? "AllowedInstanceTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x Data..@? "AcceleratorNames"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "NetworkBandwidthGbps")
      Prelude.<*> ( x Data..@? "AcceleratorManufacturers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x Data..@? "ExcludedInstanceTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "NetworkInterfaceCount")
      Prelude.<*> (x Data..@? "RequireHibernateSupport")
      Prelude.<*> (x Data..@? "AcceleratorTotalMemoryMiB")
      Prelude.<*> (x Data..@? "AcceleratorCount")
      Prelude.<*> (x Data..@? "BurstablePerformance")
      Prelude.<*> ( x Data..@? "CpuManufacturers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "MemoryGiBPerVCpu")
      Prelude.<*> (x Data..@? "LocalStorage")
      Prelude.<*> (x Data..@ "VCpuCount")
      Prelude.<*> (x Data..@ "MemoryMiB")

instance Prelude.Hashable InstanceRequirements where
  hashWithSalt _salt InstanceRequirements' {..} =
    _salt `Prelude.hashWithSalt` instanceGenerations
      `Prelude.hashWithSalt` baselineEbsBandwidthMbps
      `Prelude.hashWithSalt` bareMetal
      `Prelude.hashWithSalt` spotMaxPricePercentageOverLowestPrice
      `Prelude.hashWithSalt` acceleratorTypes
      `Prelude.hashWithSalt` totalLocalStorageGB
      `Prelude.hashWithSalt` localStorageTypes
      `Prelude.hashWithSalt` onDemandMaxPricePercentageOverLowestPrice
      `Prelude.hashWithSalt` allowedInstanceTypes
      `Prelude.hashWithSalt` acceleratorNames
      `Prelude.hashWithSalt` networkBandwidthGbps
      `Prelude.hashWithSalt` acceleratorManufacturers
      `Prelude.hashWithSalt` excludedInstanceTypes
      `Prelude.hashWithSalt` networkInterfaceCount
      `Prelude.hashWithSalt` requireHibernateSupport
      `Prelude.hashWithSalt` acceleratorTotalMemoryMiB
      `Prelude.hashWithSalt` acceleratorCount
      `Prelude.hashWithSalt` burstablePerformance
      `Prelude.hashWithSalt` cpuManufacturers
      `Prelude.hashWithSalt` memoryGiBPerVCpu
      `Prelude.hashWithSalt` localStorage
      `Prelude.hashWithSalt` vCpuCount
      `Prelude.hashWithSalt` memoryMiB

instance Prelude.NFData InstanceRequirements where
  rnf InstanceRequirements' {..} =
    Prelude.rnf instanceGenerations
      `Prelude.seq` Prelude.rnf baselineEbsBandwidthMbps
      `Prelude.seq` Prelude.rnf bareMetal
      `Prelude.seq` Prelude.rnf spotMaxPricePercentageOverLowestPrice
      `Prelude.seq` Prelude.rnf acceleratorTypes
      `Prelude.seq` Prelude.rnf totalLocalStorageGB
      `Prelude.seq` Prelude.rnf localStorageTypes
      `Prelude.seq` Prelude.rnf onDemandMaxPricePercentageOverLowestPrice
      `Prelude.seq` Prelude.rnf allowedInstanceTypes
      `Prelude.seq` Prelude.rnf acceleratorNames
      `Prelude.seq` Prelude.rnf networkBandwidthGbps
      `Prelude.seq` Prelude.rnf acceleratorManufacturers
      `Prelude.seq` Prelude.rnf excludedInstanceTypes
      `Prelude.seq` Prelude.rnf networkInterfaceCount
      `Prelude.seq` Prelude.rnf requireHibernateSupport
      `Prelude.seq` Prelude.rnf acceleratorTotalMemoryMiB
      `Prelude.seq` Prelude.rnf acceleratorCount
      `Prelude.seq` Prelude.rnf burstablePerformance
      `Prelude.seq` Prelude.rnf cpuManufacturers
      `Prelude.seq` Prelude.rnf memoryGiBPerVCpu
      `Prelude.seq` Prelude.rnf localStorage
      `Prelude.seq` Prelude.rnf vCpuCount
      `Prelude.seq` Prelude.rnf memoryMiB

instance Data.ToQuery InstanceRequirements where
  toQuery InstanceRequirements' {..} =
    Prelude.mconcat
      [ "InstanceGenerations"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> instanceGenerations
            ),
        "BaselineEbsBandwidthMbps"
          Data.=: baselineEbsBandwidthMbps,
        "BareMetal" Data.=: bareMetal,
        "SpotMaxPricePercentageOverLowestPrice"
          Data.=: spotMaxPricePercentageOverLowestPrice,
        "AcceleratorTypes"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> acceleratorTypes
            ),
        "TotalLocalStorageGB" Data.=: totalLocalStorageGB,
        "LocalStorageTypes"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> localStorageTypes
            ),
        "OnDemandMaxPricePercentageOverLowestPrice"
          Data.=: onDemandMaxPricePercentageOverLowestPrice,
        "AllowedInstanceTypes"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> allowedInstanceTypes
            ),
        "AcceleratorNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> acceleratorNames
            ),
        "NetworkBandwidthGbps" Data.=: networkBandwidthGbps,
        "AcceleratorManufacturers"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> acceleratorManufacturers
            ),
        "ExcludedInstanceTypes"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> excludedInstanceTypes
            ),
        "NetworkInterfaceCount"
          Data.=: networkInterfaceCount,
        "RequireHibernateSupport"
          Data.=: requireHibernateSupport,
        "AcceleratorTotalMemoryMiB"
          Data.=: acceleratorTotalMemoryMiB,
        "AcceleratorCount" Data.=: acceleratorCount,
        "BurstablePerformance" Data.=: burstablePerformance,
        "CpuManufacturers"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> cpuManufacturers
            ),
        "MemoryGiBPerVCpu" Data.=: memoryGiBPerVCpu,
        "LocalStorage" Data.=: localStorage,
        "VCpuCount" Data.=: vCpuCount,
        "MemoryMiB" Data.=: memoryMiB
      ]
