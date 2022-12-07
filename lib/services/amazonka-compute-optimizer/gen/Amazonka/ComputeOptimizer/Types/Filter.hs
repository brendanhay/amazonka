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
-- Module      : Amazonka.ComputeOptimizer.Types.Filter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.Filter where

import Amazonka.ComputeOptimizer.Types.FilterName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a filter that returns a more specific list of recommendations.
-- Use this filter with the GetAutoScalingGroupRecommendations and
-- GetEC2InstanceRecommendations actions.
--
-- You can use @EBSFilter@ with the GetEBSVolumeRecommendations action,
-- @LambdaFunctionRecommendationFilter@ with the
-- GetLambdaFunctionRecommendations action, and @JobFilter@ with the
-- DescribeRecommendationExportJobs action.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The name of the filter.
    --
    -- Specify @Finding@ to return recommendations with a specific finding
    -- classification (for example, @Underprovisioned@).
    --
    -- Specify @RecommendationSourceType@ to return recommendations of a
    -- specific resource type (for example, @Ec2Instance@).
    --
    -- Specify @FindingReasonCodes@ to return recommendations with a specific
    -- finding reason code (for example, @CPUUnderprovisioned@).
    name :: Prelude.Maybe FilterName,
    -- | The value of the filter.
    --
    -- The valid values for this parameter are as follows, depending on what
    -- you specify for the @name@ parameter and the resource type that you wish
    -- to filter results for:
    --
    -- -   Specify @Optimized@ or @NotOptimized@ if you specify the @name@
    --     parameter as @Finding@ and you want to filter results for Auto
    --     Scaling groups.
    --
    -- -   Specify @Underprovisioned@, @Overprovisioned@, or @Optimized@ if you
    --     specify the @name@ parameter as @Finding@ and you want to filter
    --     results for EC2 instances.
    --
    -- -   Specify @Ec2Instance@ or @AutoScalingGroup@ if you specify the
    --     @name@ parameter as @RecommendationSourceType@.
    --
    -- -   Specify one of the following options if you specify the @name@
    --     parameter as @FindingReasonCodes@:
    --
    --     -   __@CPUOverprovisioned@__ — The instance’s CPU configuration can
    --         be sized down while still meeting the performance requirements
    --         of your workload.
    --
    --     -   __@CPUUnderprovisioned@__ — The instance’s CPU configuration
    --         doesn\'t meet the performance requirements of your workload and
    --         there is an alternative instance type that provides better CPU
    --         performance.
    --
    --     -   __@MemoryOverprovisioned@__ — The instance’s memory
    --         configuration can be sized down while still meeting the
    --         performance requirements of your workload.
    --
    --     -   __@MemoryUnderprovisioned@__ — The instance’s memory
    --         configuration doesn\'t meet the performance requirements of your
    --         workload and there is an alternative instance type that provides
    --         better memory performance.
    --
    --     -   __@EBSThroughputOverprovisioned@__ — The instance’s EBS
    --         throughput configuration can be sized down while still meeting
    --         the performance requirements of your workload.
    --
    --     -   __@EBSThroughputUnderprovisioned@__ — The instance’s EBS
    --         throughput configuration doesn\'t meet the performance
    --         requirements of your workload and there is an alternative
    --         instance type that provides better EBS throughput performance.
    --
    --     -   __@EBSIOPSOverprovisioned@__ — The instance’s EBS IOPS
    --         configuration can be sized down while still meeting the
    --         performance requirements of your workload.
    --
    --     -   __@EBSIOPSUnderprovisioned@__ — The instance’s EBS IOPS
    --         configuration doesn\'t meet the performance requirements of your
    --         workload and there is an alternative instance type that provides
    --         better EBS IOPS performance.
    --
    --     -   __@NetworkBandwidthOverprovisioned@__ — The instance’s network
    --         bandwidth configuration can be sized down while still meeting
    --         the performance requirements of your workload.
    --
    --     -   __@NetworkBandwidthUnderprovisioned@__ — The instance’s network
    --         bandwidth configuration doesn\'t meet the performance
    --         requirements of your workload and there is an alternative
    --         instance type that provides better network bandwidth
    --         performance. This finding reason happens when the @NetworkIn@ or
    --         @NetworkOut@ performance of an instance is impacted.
    --
    --     -   __@NetworkPPSOverprovisioned@__ — The instance’s network PPS
    --         (packets per second) configuration can be sized down while still
    --         meeting the performance requirements of your workload.
    --
    --     -   __@NetworkPPSUnderprovisioned@__ — The instance’s network PPS
    --         (packets per second) configuration doesn\'t meet the performance
    --         requirements of your workload and there is an alternative
    --         instance type that provides better network PPS performance.
    --
    --     -   __@DiskIOPSOverprovisioned@__ — The instance’s disk IOPS
    --         configuration can be sized down while still meeting the
    --         performance requirements of your workload.
    --
    --     -   __@DiskIOPSUnderprovisioned@__ — The instance’s disk IOPS
    --         configuration doesn\'t meet the performance requirements of your
    --         workload and there is an alternative instance type that provides
    --         better disk IOPS performance.
    --
    --     -   __@DiskThroughputOverprovisioned@__ — The instance’s disk
    --         throughput configuration can be sized down while still meeting
    --         the performance requirements of your workload.
    --
    --     -   __@DiskThroughputUnderprovisioned@__ — The instance’s disk
    --         throughput configuration doesn\'t meet the performance
    --         requirements of your workload and there is an alternative
    --         instance type that provides better disk throughput performance.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'filter_name' - The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification (for example, @Underprovisioned@).
--
-- Specify @RecommendationSourceType@ to return recommendations of a
-- specific resource type (for example, @Ec2Instance@).
--
-- Specify @FindingReasonCodes@ to return recommendations with a specific
-- finding reason code (for example, @CPUUnderprovisioned@).
--
-- 'values', 'filter_values' - The value of the filter.
--
-- The valid values for this parameter are as follows, depending on what
-- you specify for the @name@ parameter and the resource type that you wish
-- to filter results for:
--
-- -   Specify @Optimized@ or @NotOptimized@ if you specify the @name@
--     parameter as @Finding@ and you want to filter results for Auto
--     Scaling groups.
--
-- -   Specify @Underprovisioned@, @Overprovisioned@, or @Optimized@ if you
--     specify the @name@ parameter as @Finding@ and you want to filter
--     results for EC2 instances.
--
-- -   Specify @Ec2Instance@ or @AutoScalingGroup@ if you specify the
--     @name@ parameter as @RecommendationSourceType@.
--
-- -   Specify one of the following options if you specify the @name@
--     parameter as @FindingReasonCodes@:
--
--     -   __@CPUOverprovisioned@__ — The instance’s CPU configuration can
--         be sized down while still meeting the performance requirements
--         of your workload.
--
--     -   __@CPUUnderprovisioned@__ — The instance’s CPU configuration
--         doesn\'t meet the performance requirements of your workload and
--         there is an alternative instance type that provides better CPU
--         performance.
--
--     -   __@MemoryOverprovisioned@__ — The instance’s memory
--         configuration can be sized down while still meeting the
--         performance requirements of your workload.
--
--     -   __@MemoryUnderprovisioned@__ — The instance’s memory
--         configuration doesn\'t meet the performance requirements of your
--         workload and there is an alternative instance type that provides
--         better memory performance.
--
--     -   __@EBSThroughputOverprovisioned@__ — The instance’s EBS
--         throughput configuration can be sized down while still meeting
--         the performance requirements of your workload.
--
--     -   __@EBSThroughputUnderprovisioned@__ — The instance’s EBS
--         throughput configuration doesn\'t meet the performance
--         requirements of your workload and there is an alternative
--         instance type that provides better EBS throughput performance.
--
--     -   __@EBSIOPSOverprovisioned@__ — The instance’s EBS IOPS
--         configuration can be sized down while still meeting the
--         performance requirements of your workload.
--
--     -   __@EBSIOPSUnderprovisioned@__ — The instance’s EBS IOPS
--         configuration doesn\'t meet the performance requirements of your
--         workload and there is an alternative instance type that provides
--         better EBS IOPS performance.
--
--     -   __@NetworkBandwidthOverprovisioned@__ — The instance’s network
--         bandwidth configuration can be sized down while still meeting
--         the performance requirements of your workload.
--
--     -   __@NetworkBandwidthUnderprovisioned@__ — The instance’s network
--         bandwidth configuration doesn\'t meet the performance
--         requirements of your workload and there is an alternative
--         instance type that provides better network bandwidth
--         performance. This finding reason happens when the @NetworkIn@ or
--         @NetworkOut@ performance of an instance is impacted.
--
--     -   __@NetworkPPSOverprovisioned@__ — The instance’s network PPS
--         (packets per second) configuration can be sized down while still
--         meeting the performance requirements of your workload.
--
--     -   __@NetworkPPSUnderprovisioned@__ — The instance’s network PPS
--         (packets per second) configuration doesn\'t meet the performance
--         requirements of your workload and there is an alternative
--         instance type that provides better network PPS performance.
--
--     -   __@DiskIOPSOverprovisioned@__ — The instance’s disk IOPS
--         configuration can be sized down while still meeting the
--         performance requirements of your workload.
--
--     -   __@DiskIOPSUnderprovisioned@__ — The instance’s disk IOPS
--         configuration doesn\'t meet the performance requirements of your
--         workload and there is an alternative instance type that provides
--         better disk IOPS performance.
--
--     -   __@DiskThroughputOverprovisioned@__ — The instance’s disk
--         throughput configuration can be sized down while still meeting
--         the performance requirements of your workload.
--
--     -   __@DiskThroughputUnderprovisioned@__ — The instance’s disk
--         throughput configuration doesn\'t meet the performance
--         requirements of your workload and there is an alternative
--         instance type that provides better disk throughput performance.
newFilter ::
  Filter
newFilter =
  Filter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification (for example, @Underprovisioned@).
--
-- Specify @RecommendationSourceType@ to return recommendations of a
-- specific resource type (for example, @Ec2Instance@).
--
-- Specify @FindingReasonCodes@ to return recommendations with a specific
-- finding reason code (for example, @CPUUnderprovisioned@).
filter_name :: Lens.Lens' Filter (Prelude.Maybe FilterName)
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | The value of the filter.
--
-- The valid values for this parameter are as follows, depending on what
-- you specify for the @name@ parameter and the resource type that you wish
-- to filter results for:
--
-- -   Specify @Optimized@ or @NotOptimized@ if you specify the @name@
--     parameter as @Finding@ and you want to filter results for Auto
--     Scaling groups.
--
-- -   Specify @Underprovisioned@, @Overprovisioned@, or @Optimized@ if you
--     specify the @name@ parameter as @Finding@ and you want to filter
--     results for EC2 instances.
--
-- -   Specify @Ec2Instance@ or @AutoScalingGroup@ if you specify the
--     @name@ parameter as @RecommendationSourceType@.
--
-- -   Specify one of the following options if you specify the @name@
--     parameter as @FindingReasonCodes@:
--
--     -   __@CPUOverprovisioned@__ — The instance’s CPU configuration can
--         be sized down while still meeting the performance requirements
--         of your workload.
--
--     -   __@CPUUnderprovisioned@__ — The instance’s CPU configuration
--         doesn\'t meet the performance requirements of your workload and
--         there is an alternative instance type that provides better CPU
--         performance.
--
--     -   __@MemoryOverprovisioned@__ — The instance’s memory
--         configuration can be sized down while still meeting the
--         performance requirements of your workload.
--
--     -   __@MemoryUnderprovisioned@__ — The instance’s memory
--         configuration doesn\'t meet the performance requirements of your
--         workload and there is an alternative instance type that provides
--         better memory performance.
--
--     -   __@EBSThroughputOverprovisioned@__ — The instance’s EBS
--         throughput configuration can be sized down while still meeting
--         the performance requirements of your workload.
--
--     -   __@EBSThroughputUnderprovisioned@__ — The instance’s EBS
--         throughput configuration doesn\'t meet the performance
--         requirements of your workload and there is an alternative
--         instance type that provides better EBS throughput performance.
--
--     -   __@EBSIOPSOverprovisioned@__ — The instance’s EBS IOPS
--         configuration can be sized down while still meeting the
--         performance requirements of your workload.
--
--     -   __@EBSIOPSUnderprovisioned@__ — The instance’s EBS IOPS
--         configuration doesn\'t meet the performance requirements of your
--         workload and there is an alternative instance type that provides
--         better EBS IOPS performance.
--
--     -   __@NetworkBandwidthOverprovisioned@__ — The instance’s network
--         bandwidth configuration can be sized down while still meeting
--         the performance requirements of your workload.
--
--     -   __@NetworkBandwidthUnderprovisioned@__ — The instance’s network
--         bandwidth configuration doesn\'t meet the performance
--         requirements of your workload and there is an alternative
--         instance type that provides better network bandwidth
--         performance. This finding reason happens when the @NetworkIn@ or
--         @NetworkOut@ performance of an instance is impacted.
--
--     -   __@NetworkPPSOverprovisioned@__ — The instance’s network PPS
--         (packets per second) configuration can be sized down while still
--         meeting the performance requirements of your workload.
--
--     -   __@NetworkPPSUnderprovisioned@__ — The instance’s network PPS
--         (packets per second) configuration doesn\'t meet the performance
--         requirements of your workload and there is an alternative
--         instance type that provides better network PPS performance.
--
--     -   __@DiskIOPSOverprovisioned@__ — The instance’s disk IOPS
--         configuration can be sized down while still meeting the
--         performance requirements of your workload.
--
--     -   __@DiskIOPSUnderprovisioned@__ — The instance’s disk IOPS
--         configuration doesn\'t meet the performance requirements of your
--         workload and there is an alternative instance type that provides
--         better disk IOPS performance.
--
--     -   __@DiskThroughputOverprovisioned@__ — The instance’s disk
--         throughput configuration can be sized down while still meeting
--         the performance requirements of your workload.
--
--     -   __@DiskThroughputUnderprovisioned@__ — The instance’s disk
--         throughput configuration doesn\'t meet the performance
--         requirements of your workload and there is an alternative
--         instance type that provides better disk throughput performance.
filter_values :: Lens.Lens' Filter (Prelude.Maybe [Prelude.Text])
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )
