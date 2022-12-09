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
-- Module      : Amazonka.EC2.Types.InstanceRequirements
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceRequirements where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AcceleratorCount
import Amazonka.EC2.Types.AcceleratorManufacturer
import Amazonka.EC2.Types.AcceleratorName
import Amazonka.EC2.Types.AcceleratorTotalMemoryMiB
import Amazonka.EC2.Types.AcceleratorType
import Amazonka.EC2.Types.BareMetal
import Amazonka.EC2.Types.BaselineEbsBandwidthMbps
import Amazonka.EC2.Types.BurstablePerformance
import Amazonka.EC2.Types.CpuManufacturer
import Amazonka.EC2.Types.InstanceGeneration
import Amazonka.EC2.Types.LocalStorage
import Amazonka.EC2.Types.LocalStorageType
import Amazonka.EC2.Types.MemoryGiBPerVCpu
import Amazonka.EC2.Types.MemoryMiB
import Amazonka.EC2.Types.NetworkBandwidthGbps
import Amazonka.EC2.Types.NetworkInterfaceCount
import Amazonka.EC2.Types.TotalLocalStorageGB
import Amazonka.EC2.Types.VCpuCountRange
import qualified Amazonka.Prelude as Prelude

-- | The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with these
-- attributes.
--
-- When you specify multiple attributes, you get instance types that
-- satisfy all of the specified attributes. If you specify multiple values
-- for an attribute, you get instance types that satisfy any of the
-- specified values.
--
-- To limit the list of instance types from which Amazon EC2 can identify
-- matching instance types, you can use one of the following parameters,
-- but not both in the same request:
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
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-attribute-based-instance-type-selection.html Attribute-based instance type selection for EC2 Fleet>,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-attribute-based-instance-type-selection.html Attribute-based instance type selection for Spot Fleet>,
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-placement-score.html Spot placement score>
-- in the /Amazon EC2 User Guide/.
--
-- /See:/ 'newInstanceRequirements' smart constructor.
data InstanceRequirements = InstanceRequirements'
  { -- | The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
    -- Web Services Inferentia chips) on an instance.
    --
    -- To exclude accelerator-enabled instance types, set @Max@ to @0@.
    --
    -- Default: No minimum or maximum limits
    acceleratorCount :: Prelude.Maybe AcceleratorCount,
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
    -- | The accelerators that must be on the instance type.
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
    -- -   For instance types with Amazon Web Services Inferentia chips,
    --     specify @inferentia@.
    --
    -- -   For instance types with NVIDIA GRID K520 GPUs, specify @k520@.
    --
    -- Default: Any accelerator
    acceleratorNames :: Prelude.Maybe [AcceleratorName],
    -- | The minimum and maximum amount of total accelerator memory, in MiB.
    --
    -- Default: No minimum or maximum limits
    acceleratorTotalMemoryMiB :: Prelude.Maybe AcceleratorTotalMemoryMiB,
    -- | The accelerator types that must be on the instance type.
    --
    -- -   For instance types with GPU accelerators, specify @gpu@.
    --
    -- -   For instance types with FPGA accelerators, specify @fpga@.
    --
    -- -   For instance types with inference accelerators, specify @inference@.
    --
    -- Default: Any accelerator type
    acceleratorTypes :: Prelude.Maybe [AcceleratorType],
    -- | The instance types to apply your specified attributes against. All other
    -- instance types are ignored, even if they match your specified
    -- attributes.
    --
    -- You can use strings with one or more wild cards, represented by an
    -- asterisk (@*@), to allow an instance type, size, or generation. The
    -- following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@, @r*@, @*3*@.
    --
    -- For example, if you specify @c5*@,Amazon EC2 will allow the entire C5
    -- instance family, which includes all C5a and C5n instance types. If you
    -- specify @m5a.*@, Amazon EC2 will allow all the M5a instance types, but
    -- not the M5n instance types.
    --
    -- If you specify @AllowedInstanceTypes@, you can\'t specify
    -- @ExcludedInstanceTypes@.
    --
    -- Default: All instance types
    allowedInstanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether bare metal instance types must be included, excluded,
    -- or required.
    --
    -- -   To include bare metal instance types, specify @included@.
    --
    -- -   To require only bare metal instance types, specify @required@.
    --
    -- -   To exclude bare metal instance types, specify @excluded@.
    --
    -- Default: @excluded@
    bareMetal :: Prelude.Maybe BareMetal,
    -- | The minimum and maximum baseline bandwidth to Amazon EBS, in Mbps. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS–optimized instances>
    -- in the /Amazon EC2 User Guide/.
    --
    -- Default: No minimum or maximum limits
    baselineEbsBandwidthMbps :: Prelude.Maybe BaselineEbsBandwidthMbps,
    -- | Indicates whether burstable performance T instance types are included,
    -- excluded, or required. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>.
    --
    -- -   To include burstable performance instance types, specify @included@.
    --
    -- -   To require only burstable performance instance types, specify
    --     @required@.
    --
    -- -   To exclude burstable performance instance types, specify @excluded@.
    --
    -- Default: @excluded@
    burstablePerformance :: Prelude.Maybe BurstablePerformance,
    -- | The CPU manufacturers to include.
    --
    -- -   For instance types with Intel CPUs, specify @intel@.
    --
    -- -   For instance types with AMD CPUs, specify @amd@.
    --
    -- -   For instance types with Amazon Web Services CPUs, specify
    --     @amazon-web-services@.
    --
    -- Don\'t confuse the CPU manufacturer with the CPU architecture. Instances
    -- will be launched with a compatible CPU architecture based on the Amazon
    -- Machine Image (AMI) that you specify in your launch template.
    --
    -- Default: Any manufacturer
    cpuManufacturers :: Prelude.Maybe [CpuManufacturer],
    -- | The instance types to exclude.
    --
    -- You can use strings with one or more wild cards, represented by an
    -- asterisk (@*@), to exclude an instance type, size, or generation. The
    -- following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@, @r*@, @*3*@.
    --
    -- For example, if you specify @c5*@,Amazon EC2 will exclude the entire C5
    -- instance family, which includes all C5a and C5n instance types. If you
    -- specify @m5a.*@, Amazon EC2 will exclude all the M5a instance types, but
    -- not the M5n instance types.
    --
    -- If you specify @ExcludedInstanceTypes@, you can\'t specify
    -- @AllowedInstanceTypes@.
    --
    -- Default: No excluded instance types
    excludedInstanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether current or previous generation instance types are
    -- included. The current generation instance types are recommended for use.
    -- Current generation instance types are typically the latest two to three
    -- generations in each instance family. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/.
    --
    -- For current generation instance types, specify @current@.
    --
    -- For previous generation instance types, specify @previous@.
    --
    -- Default: Current and previous generation instance types
    instanceGenerations :: Prelude.Maybe [InstanceGeneration],
    -- | Indicates whether instance types with instance store volumes are
    -- included, excluded, or required. For more information,
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html Amazon EC2 instance store>
    -- in the /Amazon EC2 User Guide/.
    --
    -- -   To include instance types with instance store volumes, specify
    --     @included@.
    --
    -- -   To require only instance types with instance store volumes, specify
    --     @required@.
    --
    -- -   To exclude instance types with instance store volumes, specify
    --     @excluded@.
    --
    -- Default: @included@
    localStorage :: Prelude.Maybe LocalStorage,
    -- | The type of local storage that is required.
    --
    -- -   For instance types with hard disk drive (HDD) storage, specify
    --     @hdd@.
    --
    -- -   For instance types with solid state drive (SSD) storage, specify
    --     @ssd@.
    --
    -- Default: @hdd@ and @ssd@
    localStorageTypes :: Prelude.Maybe [LocalStorageType],
    -- | The minimum and maximum amount of memory per vCPU, in GiB.
    --
    -- Default: No minimum or maximum limits
    memoryGiBPerVCpu :: Prelude.Maybe MemoryGiBPerVCpu,
    -- | The minimum and maximum amount of memory, in MiB.
    memoryMiB :: Prelude.Maybe MemoryMiB,
    -- | The minimum and maximum amount of network bandwidth, in gigabits per
    -- second (Gbps).
    --
    -- Default: No minimum or maximum limits
    networkBandwidthGbps :: Prelude.Maybe NetworkBandwidthGbps,
    -- | The minimum and maximum number of network interfaces.
    --
    -- Default: No minimum or maximum limits
    networkInterfaceCount :: Prelude.Maybe NetworkInterfaceCount,
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
    -- To turn off price protection, specify a high value, such as @999999@.
    --
    -- This parameter is not supported for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>
    -- and
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceTypesFromInstanceRequirements.html GetInstanceTypesFromInstanceRequirements>.
    --
    -- If you set @TargetCapacityUnitType@ to @vcpu@ or @memory-mib@, the price
    -- protection threshold is applied based on the per-vCPU or per-memory
    -- price instead of the per-instance price.
    --
    -- Default: @20@
    onDemandMaxPricePercentageOverLowestPrice :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether instance types must support hibernation for On-Demand
    -- Instances.
    --
    -- This parameter is not supported for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>.
    --
    -- Default: @false@
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
    -- To turn off price protection, specify a high value, such as @999999@.
    --
    -- This parameter is not supported for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>
    -- and
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceTypesFromInstanceRequirements.html GetInstanceTypesFromInstanceRequirements>.
    --
    -- If you set @TargetCapacityUnitType@ to @vcpu@ or @memory-mib@, the price
    -- protection threshold is applied based on the per-vCPU or per-memory
    -- price instead of the per-instance price.
    --
    -- Default: @100@
    spotMaxPricePercentageOverLowestPrice :: Prelude.Maybe Prelude.Int,
    -- | The minimum and maximum amount of total local storage, in GB.
    --
    -- Default: No minimum or maximum limits
    totalLocalStorageGB :: Prelude.Maybe TotalLocalStorageGB,
    -- | The minimum and maximum number of vCPUs.
    vCpuCount :: Prelude.Maybe VCpuCountRange
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
-- 'acceleratorCount', 'instanceRequirements_acceleratorCount' - The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
-- Web Services Inferentia chips) on an instance.
--
-- To exclude accelerator-enabled instance types, set @Max@ to @0@.
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
-- 'acceleratorNames', 'instanceRequirements_acceleratorNames' - The accelerators that must be on the instance type.
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
-- -   For instance types with Amazon Web Services Inferentia chips,
--     specify @inferentia@.
--
-- -   For instance types with NVIDIA GRID K520 GPUs, specify @k520@.
--
-- Default: Any accelerator
--
-- 'acceleratorTotalMemoryMiB', 'instanceRequirements_acceleratorTotalMemoryMiB' - The minimum and maximum amount of total accelerator memory, in MiB.
--
-- Default: No minimum or maximum limits
--
-- 'acceleratorTypes', 'instanceRequirements_acceleratorTypes' - The accelerator types that must be on the instance type.
--
-- -   For instance types with GPU accelerators, specify @gpu@.
--
-- -   For instance types with FPGA accelerators, specify @fpga@.
--
-- -   For instance types with inference accelerators, specify @inference@.
--
-- Default: Any accelerator type
--
-- 'allowedInstanceTypes', 'instanceRequirements_allowedInstanceTypes' - The instance types to apply your specified attributes against. All other
-- instance types are ignored, even if they match your specified
-- attributes.
--
-- You can use strings with one or more wild cards, represented by an
-- asterisk (@*@), to allow an instance type, size, or generation. The
-- following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@, @r*@, @*3*@.
--
-- For example, if you specify @c5*@,Amazon EC2 will allow the entire C5
-- instance family, which includes all C5a and C5n instance types. If you
-- specify @m5a.*@, Amazon EC2 will allow all the M5a instance types, but
-- not the M5n instance types.
--
-- If you specify @AllowedInstanceTypes@, you can\'t specify
-- @ExcludedInstanceTypes@.
--
-- Default: All instance types
--
-- 'bareMetal', 'instanceRequirements_bareMetal' - Indicates whether bare metal instance types must be included, excluded,
-- or required.
--
-- -   To include bare metal instance types, specify @included@.
--
-- -   To require only bare metal instance types, specify @required@.
--
-- -   To exclude bare metal instance types, specify @excluded@.
--
-- Default: @excluded@
--
-- 'baselineEbsBandwidthMbps', 'instanceRequirements_baselineEbsBandwidthMbps' - The minimum and maximum baseline bandwidth to Amazon EBS, in Mbps. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS–optimized instances>
-- in the /Amazon EC2 User Guide/.
--
-- Default: No minimum or maximum limits
--
-- 'burstablePerformance', 'instanceRequirements_burstablePerformance' - Indicates whether burstable performance T instance types are included,
-- excluded, or required. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>.
--
-- -   To include burstable performance instance types, specify @included@.
--
-- -   To require only burstable performance instance types, specify
--     @required@.
--
-- -   To exclude burstable performance instance types, specify @excluded@.
--
-- Default: @excluded@
--
-- 'cpuManufacturers', 'instanceRequirements_cpuManufacturers' - The CPU manufacturers to include.
--
-- -   For instance types with Intel CPUs, specify @intel@.
--
-- -   For instance types with AMD CPUs, specify @amd@.
--
-- -   For instance types with Amazon Web Services CPUs, specify
--     @amazon-web-services@.
--
-- Don\'t confuse the CPU manufacturer with the CPU architecture. Instances
-- will be launched with a compatible CPU architecture based on the Amazon
-- Machine Image (AMI) that you specify in your launch template.
--
-- Default: Any manufacturer
--
-- 'excludedInstanceTypes', 'instanceRequirements_excludedInstanceTypes' - The instance types to exclude.
--
-- You can use strings with one or more wild cards, represented by an
-- asterisk (@*@), to exclude an instance type, size, or generation. The
-- following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@, @r*@, @*3*@.
--
-- For example, if you specify @c5*@,Amazon EC2 will exclude the entire C5
-- instance family, which includes all C5a and C5n instance types. If you
-- specify @m5a.*@, Amazon EC2 will exclude all the M5a instance types, but
-- not the M5n instance types.
--
-- If you specify @ExcludedInstanceTypes@, you can\'t specify
-- @AllowedInstanceTypes@.
--
-- Default: No excluded instance types
--
-- 'instanceGenerations', 'instanceRequirements_instanceGenerations' - Indicates whether current or previous generation instance types are
-- included. The current generation instance types are recommended for use.
-- Current generation instance types are typically the latest two to three
-- generations in each instance family. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
--
-- For current generation instance types, specify @current@.
--
-- For previous generation instance types, specify @previous@.
--
-- Default: Current and previous generation instance types
--
-- 'localStorage', 'instanceRequirements_localStorage' - Indicates whether instance types with instance store volumes are
-- included, excluded, or required. For more information,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html Amazon EC2 instance store>
-- in the /Amazon EC2 User Guide/.
--
-- -   To include instance types with instance store volumes, specify
--     @included@.
--
-- -   To require only instance types with instance store volumes, specify
--     @required@.
--
-- -   To exclude instance types with instance store volumes, specify
--     @excluded@.
--
-- Default: @included@
--
-- 'localStorageTypes', 'instanceRequirements_localStorageTypes' - The type of local storage that is required.
--
-- -   For instance types with hard disk drive (HDD) storage, specify
--     @hdd@.
--
-- -   For instance types with solid state drive (SSD) storage, specify
--     @ssd@.
--
-- Default: @hdd@ and @ssd@
--
-- 'memoryGiBPerVCpu', 'instanceRequirements_memoryGiBPerVCpu' - The minimum and maximum amount of memory per vCPU, in GiB.
--
-- Default: No minimum or maximum limits
--
-- 'memoryMiB', 'instanceRequirements_memoryMiB' - The minimum and maximum amount of memory, in MiB.
--
-- 'networkBandwidthGbps', 'instanceRequirements_networkBandwidthGbps' - The minimum and maximum amount of network bandwidth, in gigabits per
-- second (Gbps).
--
-- Default: No minimum or maximum limits
--
-- 'networkInterfaceCount', 'instanceRequirements_networkInterfaceCount' - The minimum and maximum number of network interfaces.
--
-- Default: No minimum or maximum limits
--
-- 'onDemandMaxPricePercentageOverLowestPrice', 'instanceRequirements_onDemandMaxPricePercentageOverLowestPrice' - The price protection threshold for On-Demand Instances. This is the
-- maximum you’ll pay for an On-Demand Instance, expressed as a percentage
-- above the least expensive current generation M, C, or R instance type
-- with your specified attributes. When Amazon EC2 selects instance types
-- with your attributes, it excludes instance types priced above your
-- threshold.
--
-- The parameter accepts an integer, which Amazon EC2 interprets as a
-- percentage.
--
-- To turn off price protection, specify a high value, such as @999999@.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceTypesFromInstanceRequirements.html GetInstanceTypesFromInstanceRequirements>.
--
-- If you set @TargetCapacityUnitType@ to @vcpu@ or @memory-mib@, the price
-- protection threshold is applied based on the per-vCPU or per-memory
-- price instead of the per-instance price.
--
-- Default: @20@
--
-- 'requireHibernateSupport', 'instanceRequirements_requireHibernateSupport' - Indicates whether instance types must support hibernation for On-Demand
-- Instances.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>.
--
-- Default: @false@
--
-- 'spotMaxPricePercentageOverLowestPrice', 'instanceRequirements_spotMaxPricePercentageOverLowestPrice' - The price protection threshold for Spot Instances. This is the maximum
-- you’ll pay for a Spot Instance, expressed as a percentage above the
-- least expensive current generation M, C, or R instance type with your
-- specified attributes. When Amazon EC2 selects instance types with your
-- attributes, it excludes instance types priced above your threshold.
--
-- The parameter accepts an integer, which Amazon EC2 interprets as a
-- percentage.
--
-- To turn off price protection, specify a high value, such as @999999@.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceTypesFromInstanceRequirements.html GetInstanceTypesFromInstanceRequirements>.
--
-- If you set @TargetCapacityUnitType@ to @vcpu@ or @memory-mib@, the price
-- protection threshold is applied based on the per-vCPU or per-memory
-- price instead of the per-instance price.
--
-- Default: @100@
--
-- 'totalLocalStorageGB', 'instanceRequirements_totalLocalStorageGB' - The minimum and maximum amount of total local storage, in GB.
--
-- Default: No minimum or maximum limits
--
-- 'vCpuCount', 'instanceRequirements_vCpuCount' - The minimum and maximum number of vCPUs.
newInstanceRequirements ::
  InstanceRequirements
newInstanceRequirements =
  InstanceRequirements'
    { acceleratorCount =
        Prelude.Nothing,
      acceleratorManufacturers = Prelude.Nothing,
      acceleratorNames = Prelude.Nothing,
      acceleratorTotalMemoryMiB = Prelude.Nothing,
      acceleratorTypes = Prelude.Nothing,
      allowedInstanceTypes = Prelude.Nothing,
      bareMetal = Prelude.Nothing,
      baselineEbsBandwidthMbps = Prelude.Nothing,
      burstablePerformance = Prelude.Nothing,
      cpuManufacturers = Prelude.Nothing,
      excludedInstanceTypes = Prelude.Nothing,
      instanceGenerations = Prelude.Nothing,
      localStorage = Prelude.Nothing,
      localStorageTypes = Prelude.Nothing,
      memoryGiBPerVCpu = Prelude.Nothing,
      memoryMiB = Prelude.Nothing,
      networkBandwidthGbps = Prelude.Nothing,
      networkInterfaceCount = Prelude.Nothing,
      onDemandMaxPricePercentageOverLowestPrice =
        Prelude.Nothing,
      requireHibernateSupport = Prelude.Nothing,
      spotMaxPricePercentageOverLowestPrice =
        Prelude.Nothing,
      totalLocalStorageGB = Prelude.Nothing,
      vCpuCount = Prelude.Nothing
    }

-- | The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
-- Web Services Inferentia chips) on an instance.
--
-- To exclude accelerator-enabled instance types, set @Max@ to @0@.
--
-- Default: No minimum or maximum limits
instanceRequirements_acceleratorCount :: Lens.Lens' InstanceRequirements (Prelude.Maybe AcceleratorCount)
instanceRequirements_acceleratorCount = Lens.lens (\InstanceRequirements' {acceleratorCount} -> acceleratorCount) (\s@InstanceRequirements' {} a -> s {acceleratorCount = a} :: InstanceRequirements)

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

-- | The accelerators that must be on the instance type.
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
-- -   For instance types with Amazon Web Services Inferentia chips,
--     specify @inferentia@.
--
-- -   For instance types with NVIDIA GRID K520 GPUs, specify @k520@.
--
-- Default: Any accelerator
instanceRequirements_acceleratorNames :: Lens.Lens' InstanceRequirements (Prelude.Maybe [AcceleratorName])
instanceRequirements_acceleratorNames = Lens.lens (\InstanceRequirements' {acceleratorNames} -> acceleratorNames) (\s@InstanceRequirements' {} a -> s {acceleratorNames = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum amount of total accelerator memory, in MiB.
--
-- Default: No minimum or maximum limits
instanceRequirements_acceleratorTotalMemoryMiB :: Lens.Lens' InstanceRequirements (Prelude.Maybe AcceleratorTotalMemoryMiB)
instanceRequirements_acceleratorTotalMemoryMiB = Lens.lens (\InstanceRequirements' {acceleratorTotalMemoryMiB} -> acceleratorTotalMemoryMiB) (\s@InstanceRequirements' {} a -> s {acceleratorTotalMemoryMiB = a} :: InstanceRequirements)

-- | The accelerator types that must be on the instance type.
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

-- | The instance types to apply your specified attributes against. All other
-- instance types are ignored, even if they match your specified
-- attributes.
--
-- You can use strings with one or more wild cards, represented by an
-- asterisk (@*@), to allow an instance type, size, or generation. The
-- following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@, @r*@, @*3*@.
--
-- For example, if you specify @c5*@,Amazon EC2 will allow the entire C5
-- instance family, which includes all C5a and C5n instance types. If you
-- specify @m5a.*@, Amazon EC2 will allow all the M5a instance types, but
-- not the M5n instance types.
--
-- If you specify @AllowedInstanceTypes@, you can\'t specify
-- @ExcludedInstanceTypes@.
--
-- Default: All instance types
instanceRequirements_allowedInstanceTypes :: Lens.Lens' InstanceRequirements (Prelude.Maybe [Prelude.Text])
instanceRequirements_allowedInstanceTypes = Lens.lens (\InstanceRequirements' {allowedInstanceTypes} -> allowedInstanceTypes) (\s@InstanceRequirements' {} a -> s {allowedInstanceTypes = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether bare metal instance types must be included, excluded,
-- or required.
--
-- -   To include bare metal instance types, specify @included@.
--
-- -   To require only bare metal instance types, specify @required@.
--
-- -   To exclude bare metal instance types, specify @excluded@.
--
-- Default: @excluded@
instanceRequirements_bareMetal :: Lens.Lens' InstanceRequirements (Prelude.Maybe BareMetal)
instanceRequirements_bareMetal = Lens.lens (\InstanceRequirements' {bareMetal} -> bareMetal) (\s@InstanceRequirements' {} a -> s {bareMetal = a} :: InstanceRequirements)

-- | The minimum and maximum baseline bandwidth to Amazon EBS, in Mbps. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS–optimized instances>
-- in the /Amazon EC2 User Guide/.
--
-- Default: No minimum or maximum limits
instanceRequirements_baselineEbsBandwidthMbps :: Lens.Lens' InstanceRequirements (Prelude.Maybe BaselineEbsBandwidthMbps)
instanceRequirements_baselineEbsBandwidthMbps = Lens.lens (\InstanceRequirements' {baselineEbsBandwidthMbps} -> baselineEbsBandwidthMbps) (\s@InstanceRequirements' {} a -> s {baselineEbsBandwidthMbps = a} :: InstanceRequirements)

-- | Indicates whether burstable performance T instance types are included,
-- excluded, or required. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances>.
--
-- -   To include burstable performance instance types, specify @included@.
--
-- -   To require only burstable performance instance types, specify
--     @required@.
--
-- -   To exclude burstable performance instance types, specify @excluded@.
--
-- Default: @excluded@
instanceRequirements_burstablePerformance :: Lens.Lens' InstanceRequirements (Prelude.Maybe BurstablePerformance)
instanceRequirements_burstablePerformance = Lens.lens (\InstanceRequirements' {burstablePerformance} -> burstablePerformance) (\s@InstanceRequirements' {} a -> s {burstablePerformance = a} :: InstanceRequirements)

-- | The CPU manufacturers to include.
--
-- -   For instance types with Intel CPUs, specify @intel@.
--
-- -   For instance types with AMD CPUs, specify @amd@.
--
-- -   For instance types with Amazon Web Services CPUs, specify
--     @amazon-web-services@.
--
-- Don\'t confuse the CPU manufacturer with the CPU architecture. Instances
-- will be launched with a compatible CPU architecture based on the Amazon
-- Machine Image (AMI) that you specify in your launch template.
--
-- Default: Any manufacturer
instanceRequirements_cpuManufacturers :: Lens.Lens' InstanceRequirements (Prelude.Maybe [CpuManufacturer])
instanceRequirements_cpuManufacturers = Lens.lens (\InstanceRequirements' {cpuManufacturers} -> cpuManufacturers) (\s@InstanceRequirements' {} a -> s {cpuManufacturers = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The instance types to exclude.
--
-- You can use strings with one or more wild cards, represented by an
-- asterisk (@*@), to exclude an instance type, size, or generation. The
-- following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@, @r*@, @*3*@.
--
-- For example, if you specify @c5*@,Amazon EC2 will exclude the entire C5
-- instance family, which includes all C5a and C5n instance types. If you
-- specify @m5a.*@, Amazon EC2 will exclude all the M5a instance types, but
-- not the M5n instance types.
--
-- If you specify @ExcludedInstanceTypes@, you can\'t specify
-- @AllowedInstanceTypes@.
--
-- Default: No excluded instance types
instanceRequirements_excludedInstanceTypes :: Lens.Lens' InstanceRequirements (Prelude.Maybe [Prelude.Text])
instanceRequirements_excludedInstanceTypes = Lens.lens (\InstanceRequirements' {excludedInstanceTypes} -> excludedInstanceTypes) (\s@InstanceRequirements' {} a -> s {excludedInstanceTypes = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether current or previous generation instance types are
-- included. The current generation instance types are recommended for use.
-- Current generation instance types are typically the latest two to three
-- generations in each instance family. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
--
-- For current generation instance types, specify @current@.
--
-- For previous generation instance types, specify @previous@.
--
-- Default: Current and previous generation instance types
instanceRequirements_instanceGenerations :: Lens.Lens' InstanceRequirements (Prelude.Maybe [InstanceGeneration])
instanceRequirements_instanceGenerations = Lens.lens (\InstanceRequirements' {instanceGenerations} -> instanceGenerations) (\s@InstanceRequirements' {} a -> s {instanceGenerations = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether instance types with instance store volumes are
-- included, excluded, or required. For more information,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html Amazon EC2 instance store>
-- in the /Amazon EC2 User Guide/.
--
-- -   To include instance types with instance store volumes, specify
--     @included@.
--
-- -   To require only instance types with instance store volumes, specify
--     @required@.
--
-- -   To exclude instance types with instance store volumes, specify
--     @excluded@.
--
-- Default: @included@
instanceRequirements_localStorage :: Lens.Lens' InstanceRequirements (Prelude.Maybe LocalStorage)
instanceRequirements_localStorage = Lens.lens (\InstanceRequirements' {localStorage} -> localStorage) (\s@InstanceRequirements' {} a -> s {localStorage = a} :: InstanceRequirements)

-- | The type of local storage that is required.
--
-- -   For instance types with hard disk drive (HDD) storage, specify
--     @hdd@.
--
-- -   For instance types with solid state drive (SSD) storage, specify
--     @ssd@.
--
-- Default: @hdd@ and @ssd@
instanceRequirements_localStorageTypes :: Lens.Lens' InstanceRequirements (Prelude.Maybe [LocalStorageType])
instanceRequirements_localStorageTypes = Lens.lens (\InstanceRequirements' {localStorageTypes} -> localStorageTypes) (\s@InstanceRequirements' {} a -> s {localStorageTypes = a} :: InstanceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum amount of memory per vCPU, in GiB.
--
-- Default: No minimum or maximum limits
instanceRequirements_memoryGiBPerVCpu :: Lens.Lens' InstanceRequirements (Prelude.Maybe MemoryGiBPerVCpu)
instanceRequirements_memoryGiBPerVCpu = Lens.lens (\InstanceRequirements' {memoryGiBPerVCpu} -> memoryGiBPerVCpu) (\s@InstanceRequirements' {} a -> s {memoryGiBPerVCpu = a} :: InstanceRequirements)

-- | The minimum and maximum amount of memory, in MiB.
instanceRequirements_memoryMiB :: Lens.Lens' InstanceRequirements (Prelude.Maybe MemoryMiB)
instanceRequirements_memoryMiB = Lens.lens (\InstanceRequirements' {memoryMiB} -> memoryMiB) (\s@InstanceRequirements' {} a -> s {memoryMiB = a} :: InstanceRequirements)

-- | The minimum and maximum amount of network bandwidth, in gigabits per
-- second (Gbps).
--
-- Default: No minimum or maximum limits
instanceRequirements_networkBandwidthGbps :: Lens.Lens' InstanceRequirements (Prelude.Maybe NetworkBandwidthGbps)
instanceRequirements_networkBandwidthGbps = Lens.lens (\InstanceRequirements' {networkBandwidthGbps} -> networkBandwidthGbps) (\s@InstanceRequirements' {} a -> s {networkBandwidthGbps = a} :: InstanceRequirements)

-- | The minimum and maximum number of network interfaces.
--
-- Default: No minimum or maximum limits
instanceRequirements_networkInterfaceCount :: Lens.Lens' InstanceRequirements (Prelude.Maybe NetworkInterfaceCount)
instanceRequirements_networkInterfaceCount = Lens.lens (\InstanceRequirements' {networkInterfaceCount} -> networkInterfaceCount) (\s@InstanceRequirements' {} a -> s {networkInterfaceCount = a} :: InstanceRequirements)

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
-- To turn off price protection, specify a high value, such as @999999@.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceTypesFromInstanceRequirements.html GetInstanceTypesFromInstanceRequirements>.
--
-- If you set @TargetCapacityUnitType@ to @vcpu@ or @memory-mib@, the price
-- protection threshold is applied based on the per-vCPU or per-memory
-- price instead of the per-instance price.
--
-- Default: @20@
instanceRequirements_onDemandMaxPricePercentageOverLowestPrice :: Lens.Lens' InstanceRequirements (Prelude.Maybe Prelude.Int)
instanceRequirements_onDemandMaxPricePercentageOverLowestPrice = Lens.lens (\InstanceRequirements' {onDemandMaxPricePercentageOverLowestPrice} -> onDemandMaxPricePercentageOverLowestPrice) (\s@InstanceRequirements' {} a -> s {onDemandMaxPricePercentageOverLowestPrice = a} :: InstanceRequirements)

-- | Indicates whether instance types must support hibernation for On-Demand
-- Instances.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>.
--
-- Default: @false@
instanceRequirements_requireHibernateSupport :: Lens.Lens' InstanceRequirements (Prelude.Maybe Prelude.Bool)
instanceRequirements_requireHibernateSupport = Lens.lens (\InstanceRequirements' {requireHibernateSupport} -> requireHibernateSupport) (\s@InstanceRequirements' {} a -> s {requireHibernateSupport = a} :: InstanceRequirements)

-- | The price protection threshold for Spot Instances. This is the maximum
-- you’ll pay for a Spot Instance, expressed as a percentage above the
-- least expensive current generation M, C, or R instance type with your
-- specified attributes. When Amazon EC2 selects instance types with your
-- attributes, it excludes instance types priced above your threshold.
--
-- The parameter accepts an integer, which Amazon EC2 interprets as a
-- percentage.
--
-- To turn off price protection, specify a high value, such as @999999@.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceTypesFromInstanceRequirements.html GetInstanceTypesFromInstanceRequirements>.
--
-- If you set @TargetCapacityUnitType@ to @vcpu@ or @memory-mib@, the price
-- protection threshold is applied based on the per-vCPU or per-memory
-- price instead of the per-instance price.
--
-- Default: @100@
instanceRequirements_spotMaxPricePercentageOverLowestPrice :: Lens.Lens' InstanceRequirements (Prelude.Maybe Prelude.Int)
instanceRequirements_spotMaxPricePercentageOverLowestPrice = Lens.lens (\InstanceRequirements' {spotMaxPricePercentageOverLowestPrice} -> spotMaxPricePercentageOverLowestPrice) (\s@InstanceRequirements' {} a -> s {spotMaxPricePercentageOverLowestPrice = a} :: InstanceRequirements)

-- | The minimum and maximum amount of total local storage, in GB.
--
-- Default: No minimum or maximum limits
instanceRequirements_totalLocalStorageGB :: Lens.Lens' InstanceRequirements (Prelude.Maybe TotalLocalStorageGB)
instanceRequirements_totalLocalStorageGB = Lens.lens (\InstanceRequirements' {totalLocalStorageGB} -> totalLocalStorageGB) (\s@InstanceRequirements' {} a -> s {totalLocalStorageGB = a} :: InstanceRequirements)

-- | The minimum and maximum number of vCPUs.
instanceRequirements_vCpuCount :: Lens.Lens' InstanceRequirements (Prelude.Maybe VCpuCountRange)
instanceRequirements_vCpuCount = Lens.lens (\InstanceRequirements' {vCpuCount} -> vCpuCount) (\s@InstanceRequirements' {} a -> s {vCpuCount = a} :: InstanceRequirements)

instance Data.FromXML InstanceRequirements where
  parseXML x =
    InstanceRequirements'
      Prelude.<$> (x Data..@? "acceleratorCount")
      Prelude.<*> ( x Data..@? "acceleratorManufacturerSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "acceleratorNameSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "acceleratorTotalMemoryMiB")
      Prelude.<*> ( x Data..@? "acceleratorTypeSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "allowedInstanceTypeSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "bareMetal")
      Prelude.<*> (x Data..@? "baselineEbsBandwidthMbps")
      Prelude.<*> (x Data..@? "burstablePerformance")
      Prelude.<*> ( x Data..@? "cpuManufacturerSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "excludedInstanceTypeSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "instanceGenerationSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "localStorage")
      Prelude.<*> ( x Data..@? "localStorageTypeSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "memoryGiBPerVCpu")
      Prelude.<*> (x Data..@? "memoryMiB")
      Prelude.<*> (x Data..@? "networkBandwidthGbps")
      Prelude.<*> (x Data..@? "networkInterfaceCount")
      Prelude.<*> ( x
                      Data..@? "onDemandMaxPricePercentageOverLowestPrice"
                  )
      Prelude.<*> (x Data..@? "requireHibernateSupport")
      Prelude.<*> (x Data..@? "spotMaxPricePercentageOverLowestPrice")
      Prelude.<*> (x Data..@? "totalLocalStorageGB")
      Prelude.<*> (x Data..@? "vCpuCount")

instance Prelude.Hashable InstanceRequirements where
  hashWithSalt _salt InstanceRequirements' {..} =
    _salt `Prelude.hashWithSalt` acceleratorCount
      `Prelude.hashWithSalt` acceleratorManufacturers
      `Prelude.hashWithSalt` acceleratorNames
      `Prelude.hashWithSalt` acceleratorTotalMemoryMiB
      `Prelude.hashWithSalt` acceleratorTypes
      `Prelude.hashWithSalt` allowedInstanceTypes
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
      `Prelude.hashWithSalt` networkBandwidthGbps
      `Prelude.hashWithSalt` networkInterfaceCount
      `Prelude.hashWithSalt` onDemandMaxPricePercentageOverLowestPrice
      `Prelude.hashWithSalt` requireHibernateSupport
      `Prelude.hashWithSalt` spotMaxPricePercentageOverLowestPrice
      `Prelude.hashWithSalt` totalLocalStorageGB
      `Prelude.hashWithSalt` vCpuCount

instance Prelude.NFData InstanceRequirements where
  rnf InstanceRequirements' {..} =
    Prelude.rnf acceleratorCount
      `Prelude.seq` Prelude.rnf acceleratorManufacturers
      `Prelude.seq` Prelude.rnf acceleratorNames
      `Prelude.seq` Prelude.rnf acceleratorTotalMemoryMiB
      `Prelude.seq` Prelude.rnf acceleratorTypes
      `Prelude.seq` Prelude.rnf allowedInstanceTypes
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
      `Prelude.seq` Prelude.rnf networkBandwidthGbps
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

instance Data.ToQuery InstanceRequirements where
  toQuery InstanceRequirements' {..} =
    Prelude.mconcat
      [ "AcceleratorCount" Data.=: acceleratorCount,
        Data.toQuery
          ( Data.toQueryList "AcceleratorManufacturerSet"
              Prelude.<$> acceleratorManufacturers
          ),
        Data.toQuery
          ( Data.toQueryList "AcceleratorNameSet"
              Prelude.<$> acceleratorNames
          ),
        "AcceleratorTotalMemoryMiB"
          Data.=: acceleratorTotalMemoryMiB,
        Data.toQuery
          ( Data.toQueryList "AcceleratorTypeSet"
              Prelude.<$> acceleratorTypes
          ),
        Data.toQuery
          ( Data.toQueryList "AllowedInstanceTypeSet"
              Prelude.<$> allowedInstanceTypes
          ),
        "BareMetal" Data.=: bareMetal,
        "BaselineEbsBandwidthMbps"
          Data.=: baselineEbsBandwidthMbps,
        "BurstablePerformance" Data.=: burstablePerformance,
        Data.toQuery
          ( Data.toQueryList "CpuManufacturerSet"
              Prelude.<$> cpuManufacturers
          ),
        Data.toQuery
          ( Data.toQueryList "ExcludedInstanceTypeSet"
              Prelude.<$> excludedInstanceTypes
          ),
        Data.toQuery
          ( Data.toQueryList "InstanceGenerationSet"
              Prelude.<$> instanceGenerations
          ),
        "LocalStorage" Data.=: localStorage,
        Data.toQuery
          ( Data.toQueryList "LocalStorageTypeSet"
              Prelude.<$> localStorageTypes
          ),
        "MemoryGiBPerVCpu" Data.=: memoryGiBPerVCpu,
        "MemoryMiB" Data.=: memoryMiB,
        "NetworkBandwidthGbps" Data.=: networkBandwidthGbps,
        "NetworkInterfaceCount"
          Data.=: networkInterfaceCount,
        "OnDemandMaxPricePercentageOverLowestPrice"
          Data.=: onDemandMaxPricePercentageOverLowestPrice,
        "RequireHibernateSupport"
          Data.=: requireHibernateSupport,
        "SpotMaxPricePercentageOverLowestPrice"
          Data.=: spotMaxPricePercentageOverLowestPrice,
        "TotalLocalStorageGB" Data.=: totalLocalStorageGB,
        "VCpuCount" Data.=: vCpuCount
      ]
