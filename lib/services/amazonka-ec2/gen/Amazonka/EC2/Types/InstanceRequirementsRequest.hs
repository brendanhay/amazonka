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
-- Module      : Amazonka.EC2.Types.InstanceRequirementsRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceRequirementsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AcceleratorCountRequest
import Amazonka.EC2.Types.AcceleratorManufacturer
import Amazonka.EC2.Types.AcceleratorName
import Amazonka.EC2.Types.AcceleratorTotalMemoryMiBRequest
import Amazonka.EC2.Types.AcceleratorType
import Amazonka.EC2.Types.BareMetal
import Amazonka.EC2.Types.BaselineEbsBandwidthMbpsRequest
import Amazonka.EC2.Types.BurstablePerformance
import Amazonka.EC2.Types.CpuManufacturer
import Amazonka.EC2.Types.InstanceGeneration
import Amazonka.EC2.Types.LocalStorage
import Amazonka.EC2.Types.LocalStorageType
import Amazonka.EC2.Types.MemoryGiBPerVCpuRequest
import Amazonka.EC2.Types.MemoryMiBRequest
import Amazonka.EC2.Types.NetworkBandwidthGbpsRequest
import Amazonka.EC2.Types.NetworkInterfaceCountRequest
import Amazonka.EC2.Types.TotalLocalStorageGBRequest
import Amazonka.EC2.Types.VCpuCountRangeRequest
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
-- /See:/ 'newInstanceRequirementsRequest' smart constructor.
data InstanceRequirementsRequest = InstanceRequirementsRequest'
  { -- | Indicates whether current or previous generation instance types are
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
    -- | The minimum and maximum baseline bandwidth to Amazon EBS, in Mbps. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS–optimized instances>
    -- in the /Amazon EC2 User Guide/.
    --
    -- Default: No minimum or maximum limits
    baselineEbsBandwidthMbps :: Prelude.Maybe BaselineEbsBandwidthMbpsRequest,
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
    -- | The price protection threshold for Spot Instance. This is the maximum
    -- you’ll pay for an Spot Instance, expressed as a percentage above the
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
    -- | The accelerator types that must be on the instance type.
    --
    -- -   To include instance types with GPU hardware, specify @gpu@.
    --
    -- -   To include instance types with FPGA hardware, specify @fpga@.
    --
    -- -   To include instance types with inference hardware, specify
    --     @inference@.
    --
    -- Default: Any accelerator type
    acceleratorTypes :: Prelude.Maybe [AcceleratorType],
    -- | The minimum and maximum amount of total local storage, in GB.
    --
    -- Default: No minimum or maximum limits
    totalLocalStorageGB :: Prelude.Maybe TotalLocalStorageGBRequest,
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
    -- -   For instance types with Xilinx VU9P FPGAs, specify @ vu9p@.
    --
    -- -   For instance types with Amazon Web Services Inferentia chips,
    --     specify @inferentia@.
    --
    -- -   For instance types with NVIDIA GRID K520 GPUs, specify @k520@.
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
    -- | The instance types to exclude.
    --
    -- You can use strings with one or more wild cards, represented by an
    -- asterisk (@*@), to exclude an instance family, type, size, or
    -- generation. The following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@,
    -- @r*@, @*3*@.
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
    -- | The minimum and maximum number of network interfaces.
    --
    -- Default: No minimum or maximum limits
    networkInterfaceCount :: Prelude.Maybe NetworkInterfaceCountRequest,
    -- | Indicates whether instance types must support hibernation for On-Demand
    -- Instances.
    --
    -- This parameter is not supported for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>.
    --
    -- Default: @false@
    requireHibernateSupport :: Prelude.Maybe Prelude.Bool,
    -- | The minimum and maximum amount of total accelerator memory, in MiB.
    --
    -- Default: No minimum or maximum limits
    acceleratorTotalMemoryMiB :: Prelude.Maybe AcceleratorTotalMemoryMiBRequest,
    -- | The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
    -- Web Services Inferentia chips) on an instance.
    --
    -- To exclude accelerator-enabled instance types, set @Max@ to @0@.
    --
    -- Default: No minimum or maximum limits
    acceleratorCount :: Prelude.Maybe AcceleratorCountRequest,
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
    -- | The minimum and maximum amount of memory per vCPU, in GiB.
    --
    -- Default: No minimum or maximum limits
    memoryGiBPerVCpu :: Prelude.Maybe MemoryGiBPerVCpuRequest,
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
    -- | The minimum and maximum number of vCPUs.
    vCpuCount :: VCpuCountRangeRequest,
    -- | The minimum and maximum amount of memory, in MiB.
    memoryMiB :: MemoryMiBRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceRequirementsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceGenerations', 'instanceRequirementsRequest_instanceGenerations' - Indicates whether current or previous generation instance types are
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
-- 'baselineEbsBandwidthMbps', 'instanceRequirementsRequest_baselineEbsBandwidthMbps' - The minimum and maximum baseline bandwidth to Amazon EBS, in Mbps. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS–optimized instances>
-- in the /Amazon EC2 User Guide/.
--
-- Default: No minimum or maximum limits
--
-- 'bareMetal', 'instanceRequirementsRequest_bareMetal' - Indicates whether bare metal instance types must be included, excluded,
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
-- 'spotMaxPricePercentageOverLowestPrice', 'instanceRequirementsRequest_spotMaxPricePercentageOverLowestPrice' - The price protection threshold for Spot Instance. This is the maximum
-- you’ll pay for an Spot Instance, expressed as a percentage above the
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
-- 'acceleratorTypes', 'instanceRequirementsRequest_acceleratorTypes' - The accelerator types that must be on the instance type.
--
-- -   To include instance types with GPU hardware, specify @gpu@.
--
-- -   To include instance types with FPGA hardware, specify @fpga@.
--
-- -   To include instance types with inference hardware, specify
--     @inference@.
--
-- Default: Any accelerator type
--
-- 'totalLocalStorageGB', 'instanceRequirementsRequest_totalLocalStorageGB' - The minimum and maximum amount of total local storage, in GB.
--
-- Default: No minimum or maximum limits
--
-- 'localStorageTypes', 'instanceRequirementsRequest_localStorageTypes' - The type of local storage that is required.
--
-- -   For instance types with hard disk drive (HDD) storage, specify
--     @hdd@.
--
-- -   For instance types with solid state drive (SSD) storage, specify
--     @ssd@.
--
-- Default: @hdd@ and @ssd@
--
-- 'onDemandMaxPricePercentageOverLowestPrice', 'instanceRequirementsRequest_onDemandMaxPricePercentageOverLowestPrice' - The price protection threshold for On-Demand Instances. This is the
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
-- 'allowedInstanceTypes', 'instanceRequirementsRequest_allowedInstanceTypes' - The instance types to apply your specified attributes against. All other
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
-- 'acceleratorNames', 'instanceRequirementsRequest_acceleratorNames' - The accelerators that must be on the instance type.
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
-- -   For instance types with Xilinx VU9P FPGAs, specify @ vu9p@.
--
-- -   For instance types with Amazon Web Services Inferentia chips,
--     specify @inferentia@.
--
-- -   For instance types with NVIDIA GRID K520 GPUs, specify @k520@.
--
-- Default: Any accelerator
--
-- 'networkBandwidthGbps', 'instanceRequirementsRequest_networkBandwidthGbps' - The minimum and maximum amount of network bandwidth, in gigabits per
-- second (Gbps).
--
-- Default: No minimum or maximum limits
--
-- 'acceleratorManufacturers', 'instanceRequirementsRequest_acceleratorManufacturers' - Indicates whether instance types must have accelerators by specific
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
-- 'excludedInstanceTypes', 'instanceRequirementsRequest_excludedInstanceTypes' - The instance types to exclude.
--
-- You can use strings with one or more wild cards, represented by an
-- asterisk (@*@), to exclude an instance family, type, size, or
-- generation. The following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@,
-- @r*@, @*3*@.
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
-- 'networkInterfaceCount', 'instanceRequirementsRequest_networkInterfaceCount' - The minimum and maximum number of network interfaces.
--
-- Default: No minimum or maximum limits
--
-- 'requireHibernateSupport', 'instanceRequirementsRequest_requireHibernateSupport' - Indicates whether instance types must support hibernation for On-Demand
-- Instances.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>.
--
-- Default: @false@
--
-- 'acceleratorTotalMemoryMiB', 'instanceRequirementsRequest_acceleratorTotalMemoryMiB' - The minimum and maximum amount of total accelerator memory, in MiB.
--
-- Default: No minimum or maximum limits
--
-- 'acceleratorCount', 'instanceRequirementsRequest_acceleratorCount' - The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
-- Web Services Inferentia chips) on an instance.
--
-- To exclude accelerator-enabled instance types, set @Max@ to @0@.
--
-- Default: No minimum or maximum limits
--
-- 'burstablePerformance', 'instanceRequirementsRequest_burstablePerformance' - Indicates whether burstable performance T instance types are included,
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
-- 'cpuManufacturers', 'instanceRequirementsRequest_cpuManufacturers' - The CPU manufacturers to include.
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
-- 'memoryGiBPerVCpu', 'instanceRequirementsRequest_memoryGiBPerVCpu' - The minimum and maximum amount of memory per vCPU, in GiB.
--
-- Default: No minimum or maximum limits
--
-- 'localStorage', 'instanceRequirementsRequest_localStorage' - Indicates whether instance types with instance store volumes are
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
-- 'vCpuCount', 'instanceRequirementsRequest_vCpuCount' - The minimum and maximum number of vCPUs.
--
-- 'memoryMiB', 'instanceRequirementsRequest_memoryMiB' - The minimum and maximum amount of memory, in MiB.
newInstanceRequirementsRequest ::
  -- | 'vCpuCount'
  VCpuCountRangeRequest ->
  -- | 'memoryMiB'
  MemoryMiBRequest ->
  InstanceRequirementsRequest
newInstanceRequirementsRequest
  pVCpuCount_
  pMemoryMiB_ =
    InstanceRequirementsRequest'
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
instanceRequirementsRequest_instanceGenerations :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe [InstanceGeneration])
instanceRequirementsRequest_instanceGenerations = Lens.lens (\InstanceRequirementsRequest' {instanceGenerations} -> instanceGenerations) (\s@InstanceRequirementsRequest' {} a -> s {instanceGenerations = a} :: InstanceRequirementsRequest) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum baseline bandwidth to Amazon EBS, in Mbps. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-optimized.html Amazon EBS–optimized instances>
-- in the /Amazon EC2 User Guide/.
--
-- Default: No minimum or maximum limits
instanceRequirementsRequest_baselineEbsBandwidthMbps :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe BaselineEbsBandwidthMbpsRequest)
instanceRequirementsRequest_baselineEbsBandwidthMbps = Lens.lens (\InstanceRequirementsRequest' {baselineEbsBandwidthMbps} -> baselineEbsBandwidthMbps) (\s@InstanceRequirementsRequest' {} a -> s {baselineEbsBandwidthMbps = a} :: InstanceRequirementsRequest)

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
instanceRequirementsRequest_bareMetal :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe BareMetal)
instanceRequirementsRequest_bareMetal = Lens.lens (\InstanceRequirementsRequest' {bareMetal} -> bareMetal) (\s@InstanceRequirementsRequest' {} a -> s {bareMetal = a} :: InstanceRequirementsRequest)

-- | The price protection threshold for Spot Instance. This is the maximum
-- you’ll pay for an Spot Instance, expressed as a percentage above the
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
instanceRequirementsRequest_spotMaxPricePercentageOverLowestPrice :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe Prelude.Int)
instanceRequirementsRequest_spotMaxPricePercentageOverLowestPrice = Lens.lens (\InstanceRequirementsRequest' {spotMaxPricePercentageOverLowestPrice} -> spotMaxPricePercentageOverLowestPrice) (\s@InstanceRequirementsRequest' {} a -> s {spotMaxPricePercentageOverLowestPrice = a} :: InstanceRequirementsRequest)

-- | The accelerator types that must be on the instance type.
--
-- -   To include instance types with GPU hardware, specify @gpu@.
--
-- -   To include instance types with FPGA hardware, specify @fpga@.
--
-- -   To include instance types with inference hardware, specify
--     @inference@.
--
-- Default: Any accelerator type
instanceRequirementsRequest_acceleratorTypes :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe [AcceleratorType])
instanceRequirementsRequest_acceleratorTypes = Lens.lens (\InstanceRequirementsRequest' {acceleratorTypes} -> acceleratorTypes) (\s@InstanceRequirementsRequest' {} a -> s {acceleratorTypes = a} :: InstanceRequirementsRequest) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum amount of total local storage, in GB.
--
-- Default: No minimum or maximum limits
instanceRequirementsRequest_totalLocalStorageGB :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe TotalLocalStorageGBRequest)
instanceRequirementsRequest_totalLocalStorageGB = Lens.lens (\InstanceRequirementsRequest' {totalLocalStorageGB} -> totalLocalStorageGB) (\s@InstanceRequirementsRequest' {} a -> s {totalLocalStorageGB = a} :: InstanceRequirementsRequest)

-- | The type of local storage that is required.
--
-- -   For instance types with hard disk drive (HDD) storage, specify
--     @hdd@.
--
-- -   For instance types with solid state drive (SSD) storage, specify
--     @ssd@.
--
-- Default: @hdd@ and @ssd@
instanceRequirementsRequest_localStorageTypes :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe [LocalStorageType])
instanceRequirementsRequest_localStorageTypes = Lens.lens (\InstanceRequirementsRequest' {localStorageTypes} -> localStorageTypes) (\s@InstanceRequirementsRequest' {} a -> s {localStorageTypes = a} :: InstanceRequirementsRequest) Prelude.. Lens.mapping Lens.coerced

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
instanceRequirementsRequest_onDemandMaxPricePercentageOverLowestPrice :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe Prelude.Int)
instanceRequirementsRequest_onDemandMaxPricePercentageOverLowestPrice = Lens.lens (\InstanceRequirementsRequest' {onDemandMaxPricePercentageOverLowestPrice} -> onDemandMaxPricePercentageOverLowestPrice) (\s@InstanceRequirementsRequest' {} a -> s {onDemandMaxPricePercentageOverLowestPrice = a} :: InstanceRequirementsRequest)

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
instanceRequirementsRequest_allowedInstanceTypes :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe [Prelude.Text])
instanceRequirementsRequest_allowedInstanceTypes = Lens.lens (\InstanceRequirementsRequest' {allowedInstanceTypes} -> allowedInstanceTypes) (\s@InstanceRequirementsRequest' {} a -> s {allowedInstanceTypes = a} :: InstanceRequirementsRequest) Prelude.. Lens.mapping Lens.coerced

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
-- -   For instance types with Xilinx VU9P FPGAs, specify @ vu9p@.
--
-- -   For instance types with Amazon Web Services Inferentia chips,
--     specify @inferentia@.
--
-- -   For instance types with NVIDIA GRID K520 GPUs, specify @k520@.
--
-- Default: Any accelerator
instanceRequirementsRequest_acceleratorNames :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe [AcceleratorName])
instanceRequirementsRequest_acceleratorNames = Lens.lens (\InstanceRequirementsRequest' {acceleratorNames} -> acceleratorNames) (\s@InstanceRequirementsRequest' {} a -> s {acceleratorNames = a} :: InstanceRequirementsRequest) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum amount of network bandwidth, in gigabits per
-- second (Gbps).
--
-- Default: No minimum or maximum limits
instanceRequirementsRequest_networkBandwidthGbps :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe NetworkBandwidthGbpsRequest)
instanceRequirementsRequest_networkBandwidthGbps = Lens.lens (\InstanceRequirementsRequest' {networkBandwidthGbps} -> networkBandwidthGbps) (\s@InstanceRequirementsRequest' {} a -> s {networkBandwidthGbps = a} :: InstanceRequirementsRequest)

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
instanceRequirementsRequest_acceleratorManufacturers :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe [AcceleratorManufacturer])
instanceRequirementsRequest_acceleratorManufacturers = Lens.lens (\InstanceRequirementsRequest' {acceleratorManufacturers} -> acceleratorManufacturers) (\s@InstanceRequirementsRequest' {} a -> s {acceleratorManufacturers = a} :: InstanceRequirementsRequest) Prelude.. Lens.mapping Lens.coerced

-- | The instance types to exclude.
--
-- You can use strings with one or more wild cards, represented by an
-- asterisk (@*@), to exclude an instance family, type, size, or
-- generation. The following are examples: @m5.8xlarge@, @c5*.*@, @m5a.*@,
-- @r*@, @*3*@.
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
instanceRequirementsRequest_excludedInstanceTypes :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe [Prelude.Text])
instanceRequirementsRequest_excludedInstanceTypes = Lens.lens (\InstanceRequirementsRequest' {excludedInstanceTypes} -> excludedInstanceTypes) (\s@InstanceRequirementsRequest' {} a -> s {excludedInstanceTypes = a} :: InstanceRequirementsRequest) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum number of network interfaces.
--
-- Default: No minimum or maximum limits
instanceRequirementsRequest_networkInterfaceCount :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe NetworkInterfaceCountRequest)
instanceRequirementsRequest_networkInterfaceCount = Lens.lens (\InstanceRequirementsRequest' {networkInterfaceCount} -> networkInterfaceCount) (\s@InstanceRequirementsRequest' {} a -> s {networkInterfaceCount = a} :: InstanceRequirementsRequest)

-- | Indicates whether instance types must support hibernation for On-Demand
-- Instances.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetSpotPlacementScores.html GetSpotPlacementScores>.
--
-- Default: @false@
instanceRequirementsRequest_requireHibernateSupport :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe Prelude.Bool)
instanceRequirementsRequest_requireHibernateSupport = Lens.lens (\InstanceRequirementsRequest' {requireHibernateSupport} -> requireHibernateSupport) (\s@InstanceRequirementsRequest' {} a -> s {requireHibernateSupport = a} :: InstanceRequirementsRequest)

-- | The minimum and maximum amount of total accelerator memory, in MiB.
--
-- Default: No minimum or maximum limits
instanceRequirementsRequest_acceleratorTotalMemoryMiB :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe AcceleratorTotalMemoryMiBRequest)
instanceRequirementsRequest_acceleratorTotalMemoryMiB = Lens.lens (\InstanceRequirementsRequest' {acceleratorTotalMemoryMiB} -> acceleratorTotalMemoryMiB) (\s@InstanceRequirementsRequest' {} a -> s {acceleratorTotalMemoryMiB = a} :: InstanceRequirementsRequest)

-- | The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
-- Web Services Inferentia chips) on an instance.
--
-- To exclude accelerator-enabled instance types, set @Max@ to @0@.
--
-- Default: No minimum or maximum limits
instanceRequirementsRequest_acceleratorCount :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe AcceleratorCountRequest)
instanceRequirementsRequest_acceleratorCount = Lens.lens (\InstanceRequirementsRequest' {acceleratorCount} -> acceleratorCount) (\s@InstanceRequirementsRequest' {} a -> s {acceleratorCount = a} :: InstanceRequirementsRequest)

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
instanceRequirementsRequest_burstablePerformance :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe BurstablePerformance)
instanceRequirementsRequest_burstablePerformance = Lens.lens (\InstanceRequirementsRequest' {burstablePerformance} -> burstablePerformance) (\s@InstanceRequirementsRequest' {} a -> s {burstablePerformance = a} :: InstanceRequirementsRequest)

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
instanceRequirementsRequest_cpuManufacturers :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe [CpuManufacturer])
instanceRequirementsRequest_cpuManufacturers = Lens.lens (\InstanceRequirementsRequest' {cpuManufacturers} -> cpuManufacturers) (\s@InstanceRequirementsRequest' {} a -> s {cpuManufacturers = a} :: InstanceRequirementsRequest) Prelude.. Lens.mapping Lens.coerced

-- | The minimum and maximum amount of memory per vCPU, in GiB.
--
-- Default: No minimum or maximum limits
instanceRequirementsRequest_memoryGiBPerVCpu :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe MemoryGiBPerVCpuRequest)
instanceRequirementsRequest_memoryGiBPerVCpu = Lens.lens (\InstanceRequirementsRequest' {memoryGiBPerVCpu} -> memoryGiBPerVCpu) (\s@InstanceRequirementsRequest' {} a -> s {memoryGiBPerVCpu = a} :: InstanceRequirementsRequest)

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
instanceRequirementsRequest_localStorage :: Lens.Lens' InstanceRequirementsRequest (Prelude.Maybe LocalStorage)
instanceRequirementsRequest_localStorage = Lens.lens (\InstanceRequirementsRequest' {localStorage} -> localStorage) (\s@InstanceRequirementsRequest' {} a -> s {localStorage = a} :: InstanceRequirementsRequest)

-- | The minimum and maximum number of vCPUs.
instanceRequirementsRequest_vCpuCount :: Lens.Lens' InstanceRequirementsRequest VCpuCountRangeRequest
instanceRequirementsRequest_vCpuCount = Lens.lens (\InstanceRequirementsRequest' {vCpuCount} -> vCpuCount) (\s@InstanceRequirementsRequest' {} a -> s {vCpuCount = a} :: InstanceRequirementsRequest)

-- | The minimum and maximum amount of memory, in MiB.
instanceRequirementsRequest_memoryMiB :: Lens.Lens' InstanceRequirementsRequest MemoryMiBRequest
instanceRequirementsRequest_memoryMiB = Lens.lens (\InstanceRequirementsRequest' {memoryMiB} -> memoryMiB) (\s@InstanceRequirementsRequest' {} a -> s {memoryMiB = a} :: InstanceRequirementsRequest)

instance Prelude.Hashable InstanceRequirementsRequest where
  hashWithSalt _salt InstanceRequirementsRequest' {..} =
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

instance Prelude.NFData InstanceRequirementsRequest where
  rnf InstanceRequirementsRequest' {..} =
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

instance Data.ToQuery InstanceRequirementsRequest where
  toQuery InstanceRequirementsRequest' {..} =
    Prelude.mconcat
      [ Data.toQuery
          ( Data.toQueryList "InstanceGeneration"
              Prelude.<$> instanceGenerations
          ),
        "BaselineEbsBandwidthMbps"
          Data.=: baselineEbsBandwidthMbps,
        "BareMetal" Data.=: bareMetal,
        "SpotMaxPricePercentageOverLowestPrice"
          Data.=: spotMaxPricePercentageOverLowestPrice,
        Data.toQuery
          ( Data.toQueryList "AcceleratorType"
              Prelude.<$> acceleratorTypes
          ),
        "TotalLocalStorageGB" Data.=: totalLocalStorageGB,
        Data.toQuery
          ( Data.toQueryList "LocalStorageType"
              Prelude.<$> localStorageTypes
          ),
        "OnDemandMaxPricePercentageOverLowestPrice"
          Data.=: onDemandMaxPricePercentageOverLowestPrice,
        Data.toQuery
          ( Data.toQueryList "AllowedInstanceType"
              Prelude.<$> allowedInstanceTypes
          ),
        Data.toQuery
          ( Data.toQueryList "AcceleratorName"
              Prelude.<$> acceleratorNames
          ),
        "NetworkBandwidthGbps" Data.=: networkBandwidthGbps,
        Data.toQuery
          ( Data.toQueryList "AcceleratorManufacturer"
              Prelude.<$> acceleratorManufacturers
          ),
        Data.toQuery
          ( Data.toQueryList "ExcludedInstanceType"
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
        Data.toQuery
          ( Data.toQueryList "CpuManufacturer"
              Prelude.<$> cpuManufacturers
          ),
        "MemoryGiBPerVCpu" Data.=: memoryGiBPerVCpu,
        "LocalStorage" Data.=: localStorage,
        "VCpuCount" Data.=: vCpuCount,
        "MemoryMiB" Data.=: memoryMiB
      ]
