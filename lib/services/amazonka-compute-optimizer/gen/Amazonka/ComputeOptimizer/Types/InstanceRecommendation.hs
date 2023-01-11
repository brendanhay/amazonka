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
-- Module      : Amazonka.ComputeOptimizer.Types.InstanceRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.InstanceRecommendation where

import Amazonka.ComputeOptimizer.Types.CurrentPerformanceRisk
import Amazonka.ComputeOptimizer.Types.EffectiveRecommendationPreferences
import Amazonka.ComputeOptimizer.Types.Finding
import Amazonka.ComputeOptimizer.Types.InferredWorkloadType
import Amazonka.ComputeOptimizer.Types.InstanceRecommendationFindingReasonCode
import Amazonka.ComputeOptimizer.Types.InstanceRecommendationOption
import Amazonka.ComputeOptimizer.Types.RecommendationSource
import Amazonka.ComputeOptimizer.Types.UtilizationMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon EC2 instance recommendation.
--
-- /See:/ 'newInstanceRecommendation' smart constructor.
data InstanceRecommendation = InstanceRecommendation'
  { -- | The Amazon Web Services account ID of the instance.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The instance type of the current instance.
    currentInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The risk of the current instance not meeting the performance needs of
    -- its workloads. The higher the risk, the more likely the current instance
    -- cannot meet the performance requirements of its workload.
    currentPerformanceRisk :: Prelude.Maybe CurrentPerformanceRisk,
    -- | An object that describes the effective recommendation preferences for
    -- the instance.
    effectiveRecommendationPreferences :: Prelude.Maybe EffectiveRecommendationPreferences,
    -- | The finding classification of the instance.
    --
    -- Findings for instances include:
    --
    -- -   __@Underprovisioned@__ —An instance is considered under-provisioned
    --     when at least one specification of your instance, such as CPU,
    --     memory, or network, does not meet the performance requirements of
    --     your workload. Under-provisioned instances may lead to poor
    --     application performance.
    --
    -- -   __@Overprovisioned@__ —An instance is considered over-provisioned
    --     when at least one specification of your instance, such as CPU,
    --     memory, or network, can be sized down while still meeting the
    --     performance requirements of your workload, and no specification is
    --     under-provisioned. Over-provisioned instances may lead to
    --     unnecessary infrastructure cost.
    --
    -- -   __@Optimized@__ —An instance is considered optimized when all
    --     specifications of your instance, such as CPU, memory, and network,
    --     meet the performance requirements of your workload and is not over
    --     provisioned. For optimized resources, Compute Optimizer might
    --     recommend a new generation instance type.
    finding :: Prelude.Maybe Finding,
    -- | The reason for the finding classification of the instance.
    --
    -- Finding reason codes for instances include:
    --
    -- -   __@CPUOverprovisioned@__ — The instance’s CPU configuration can be
    --     sized down while still meeting the performance requirements of your
    --     workload. This is identified by analyzing the @CPUUtilization@
    --     metric of the current instance during the look-back period.
    --
    -- -   __@CPUUnderprovisioned@__ — The instance’s CPU configuration
    --     doesn\'t meet the performance requirements of your workload and
    --     there is an alternative instance type that provides better CPU
    --     performance. This is identified by analyzing the @CPUUtilization@
    --     metric of the current instance during the look-back period.
    --
    -- -   __@MemoryOverprovisioned@__ — The instance’s memory configuration
    --     can be sized down while still meeting the performance requirements
    --     of your workload. This is identified by analyzing the memory
    --     utilization metric of the current instance during the look-back
    --     period.
    --
    -- -   __@MemoryUnderprovisioned@__ — The instance’s memory configuration
    --     doesn\'t meet the performance requirements of your workload and
    --     there is an alternative instance type that provides better memory
    --     performance. This is identified by analyzing the memory utilization
    --     metric of the current instance during the look-back period.
    --
    --     Memory utilization is analyzed only for resources that have the
    --     unified CloudWatch agent installed on them. For more information,
    --     see
    --     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling memory utilization with the Amazon CloudWatch Agent>
    --     in the /Compute Optimizer User Guide/. On Linux instances, Compute
    --     Optimizer analyses the @mem_used_percent@ metric in the @CWAgent@
    --     namespace, or the legacy @MemoryUtilization@ metric in the
    --     @System\/Linux@ namespace. On Windows instances, Compute Optimizer
    --     analyses the @Memory % Committed Bytes In Use@ metric in the
    --     @CWAgent@ namespace.
    --
    -- -   __@EBSThroughputOverprovisioned@__ — The instance’s EBS throughput
    --     configuration can be sized down while still meeting the performance
    --     requirements of your workload. This is identified by analyzing the
    --     @VolumeReadOps@ and @VolumeWriteOps@ metrics of EBS volumes attached
    --     to the current instance during the look-back period.
    --
    -- -   __@EBSThroughputUnderprovisioned@__ — The instance’s EBS throughput
    --     configuration doesn\'t meet the performance requirements of your
    --     workload and there is an alternative instance type that provides
    --     better EBS throughput performance. This is identified by analyzing
    --     the @VolumeReadOps@ and @VolumeWriteOps@ metrics of EBS volumes
    --     attached to the current instance during the look-back period.
    --
    -- -   __@EBSIOPSOverprovisioned@__ — The instance’s EBS IOPS configuration
    --     can be sized down while still meeting the performance requirements
    --     of your workload. This is identified by analyzing the
    --     @VolumeReadBytes@ and @VolumeWriteBytes@ metric of EBS volumes
    --     attached to the current instance during the look-back period.
    --
    -- -   __@EBSIOPSUnderprovisioned@__ — The instance’s EBS IOPS
    --     configuration doesn\'t meet the performance requirements of your
    --     workload and there is an alternative instance type that provides
    --     better EBS IOPS performance. This is identified by analyzing the
    --     @VolumeReadBytes@ and @VolumeWriteBytes@ metric of EBS volumes
    --     attached to the current instance during the look-back period.
    --
    -- -   __@NetworkBandwidthOverprovisioned@__ — The instance’s network
    --     bandwidth configuration can be sized down while still meeting the
    --     performance requirements of your workload. This is identified by
    --     analyzing the @NetworkIn@ and @NetworkOut@ metrics of the current
    --     instance during the look-back period.
    --
    -- -   __@NetworkBandwidthUnderprovisioned@__ — The instance’s network
    --     bandwidth configuration doesn\'t meet the performance requirements
    --     of your workload and there is an alternative instance type that
    --     provides better network bandwidth performance. This is identified by
    --     analyzing the @NetworkIn@ and @NetworkOut@ metrics of the current
    --     instance during the look-back period. This finding reason happens
    --     when the @NetworkIn@ or @NetworkOut@ performance of an instance is
    --     impacted.
    --
    -- -   __@NetworkPPSOverprovisioned@__ — The instance’s network PPS
    --     (packets per second) configuration can be sized down while still
    --     meeting the performance requirements of your workload. This is
    --     identified by analyzing the @NetworkPacketsIn@ and
    --     @NetworkPacketsIn@ metrics of the current instance during the
    --     look-back period.
    --
    -- -   __@NetworkPPSUnderprovisioned@__ — The instance’s network PPS
    --     (packets per second) configuration doesn\'t meet the performance
    --     requirements of your workload and there is an alternative instance
    --     type that provides better network PPS performance. This is
    --     identified by analyzing the @NetworkPacketsIn@ and
    --     @NetworkPacketsIn@ metrics of the current instance during the
    --     look-back period.
    --
    -- -   __@DiskIOPSOverprovisioned@__ — The instance’s disk IOPS
    --     configuration can be sized down while still meeting the performance
    --     requirements of your workload. This is identified by analyzing the
    --     @DiskReadOps@ and @DiskWriteOps@ metrics of the current instance
    --     during the look-back period.
    --
    -- -   __@DiskIOPSUnderprovisioned@__ — The instance’s disk IOPS
    --     configuration doesn\'t meet the performance requirements of your
    --     workload and there is an alternative instance type that provides
    --     better disk IOPS performance. This is identified by analyzing the
    --     @DiskReadOps@ and @DiskWriteOps@ metrics of the current instance
    --     during the look-back period.
    --
    -- -   __@DiskThroughputOverprovisioned@__ — The instance’s disk throughput
    --     configuration can be sized down while still meeting the performance
    --     requirements of your workload. This is identified by analyzing the
    --     @DiskReadBytes@ and @DiskWriteBytes@ metrics of the current instance
    --     during the look-back period.
    --
    -- -   __@DiskThroughputUnderprovisioned@__ — The instance’s disk
    --     throughput configuration doesn\'t meet the performance requirements
    --     of your workload and there is an alternative instance type that
    --     provides better disk throughput performance. This is identified by
    --     analyzing the @DiskReadBytes@ and @DiskWriteBytes@ metrics of the
    --     current instance during the look-back period.
    --
    -- For more information about instance metrics, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/viewing_metrics_with_cloudwatch.html List the available CloudWatch metrics for your instances>
    -- in the /Amazon Elastic Compute Cloud User Guide/. For more information
    -- about EBS volume metrics, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using_cloudwatch_ebs.html Amazon CloudWatch metrics for Amazon EBS>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    findingReasonCodes :: Prelude.Maybe [InstanceRecommendationFindingReasonCode],
    -- | The applications that might be running on the instance as inferred by
    -- Compute Optimizer.
    --
    -- Compute Optimizer can infer if one of the following applications might
    -- be running on the instance:
    --
    -- -   @AmazonEmr@ - Infers that Amazon EMR might be running on the
    --     instance.
    --
    -- -   @ApacheCassandra@ - Infers that Apache Cassandra might be running on
    --     the instance.
    --
    -- -   @ApacheHadoop@ - Infers that Apache Hadoop might be running on the
    --     instance.
    --
    -- -   @Memcached@ - Infers that Memcached might be running on the
    --     instance.
    --
    -- -   @NGINX@ - Infers that NGINX might be running on the instance.
    --
    -- -   @PostgreSql@ - Infers that PostgreSQL might be running on the
    --     instance.
    --
    -- -   @Redis@ - Infers that Redis might be running on the instance.
    inferredWorkloadTypes :: Prelude.Maybe [InferredWorkloadType],
    -- | The Amazon Resource Name (ARN) of the current instance.
    instanceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the current instance.
    instanceName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the instance recommendation was last generated.
    lastRefreshTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The number of days for which utilization metrics were analyzed for the
    -- instance.
    lookBackPeriodInDays :: Prelude.Maybe Prelude.Double,
    -- | An array of objects that describe the recommendation options for the
    -- instance.
    recommendationOptions :: Prelude.Maybe [InstanceRecommendationOption],
    -- | An array of objects that describe the source resource of the
    -- recommendation.
    recommendationSources :: Prelude.Maybe [RecommendationSource],
    -- | An array of objects that describe the utilization metrics of the
    -- instance.
    utilizationMetrics :: Prelude.Maybe [UtilizationMetric]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'instanceRecommendation_accountId' - The Amazon Web Services account ID of the instance.
--
-- 'currentInstanceType', 'instanceRecommendation_currentInstanceType' - The instance type of the current instance.
--
-- 'currentPerformanceRisk', 'instanceRecommendation_currentPerformanceRisk' - The risk of the current instance not meeting the performance needs of
-- its workloads. The higher the risk, the more likely the current instance
-- cannot meet the performance requirements of its workload.
--
-- 'effectiveRecommendationPreferences', 'instanceRecommendation_effectiveRecommendationPreferences' - An object that describes the effective recommendation preferences for
-- the instance.
--
-- 'finding', 'instanceRecommendation_finding' - The finding classification of the instance.
--
-- Findings for instances include:
--
-- -   __@Underprovisioned@__ —An instance is considered under-provisioned
--     when at least one specification of your instance, such as CPU,
--     memory, or network, does not meet the performance requirements of
--     your workload. Under-provisioned instances may lead to poor
--     application performance.
--
-- -   __@Overprovisioned@__ —An instance is considered over-provisioned
--     when at least one specification of your instance, such as CPU,
--     memory, or network, can be sized down while still meeting the
--     performance requirements of your workload, and no specification is
--     under-provisioned. Over-provisioned instances may lead to
--     unnecessary infrastructure cost.
--
-- -   __@Optimized@__ —An instance is considered optimized when all
--     specifications of your instance, such as CPU, memory, and network,
--     meet the performance requirements of your workload and is not over
--     provisioned. For optimized resources, Compute Optimizer might
--     recommend a new generation instance type.
--
-- 'findingReasonCodes', 'instanceRecommendation_findingReasonCodes' - The reason for the finding classification of the instance.
--
-- Finding reason codes for instances include:
--
-- -   __@CPUOverprovisioned@__ — The instance’s CPU configuration can be
--     sized down while still meeting the performance requirements of your
--     workload. This is identified by analyzing the @CPUUtilization@
--     metric of the current instance during the look-back period.
--
-- -   __@CPUUnderprovisioned@__ — The instance’s CPU configuration
--     doesn\'t meet the performance requirements of your workload and
--     there is an alternative instance type that provides better CPU
--     performance. This is identified by analyzing the @CPUUtilization@
--     metric of the current instance during the look-back period.
--
-- -   __@MemoryOverprovisioned@__ — The instance’s memory configuration
--     can be sized down while still meeting the performance requirements
--     of your workload. This is identified by analyzing the memory
--     utilization metric of the current instance during the look-back
--     period.
--
-- -   __@MemoryUnderprovisioned@__ — The instance’s memory configuration
--     doesn\'t meet the performance requirements of your workload and
--     there is an alternative instance type that provides better memory
--     performance. This is identified by analyzing the memory utilization
--     metric of the current instance during the look-back period.
--
--     Memory utilization is analyzed only for resources that have the
--     unified CloudWatch agent installed on them. For more information,
--     see
--     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling memory utilization with the Amazon CloudWatch Agent>
--     in the /Compute Optimizer User Guide/. On Linux instances, Compute
--     Optimizer analyses the @mem_used_percent@ metric in the @CWAgent@
--     namespace, or the legacy @MemoryUtilization@ metric in the
--     @System\/Linux@ namespace. On Windows instances, Compute Optimizer
--     analyses the @Memory % Committed Bytes In Use@ metric in the
--     @CWAgent@ namespace.
--
-- -   __@EBSThroughputOverprovisioned@__ — The instance’s EBS throughput
--     configuration can be sized down while still meeting the performance
--     requirements of your workload. This is identified by analyzing the
--     @VolumeReadOps@ and @VolumeWriteOps@ metrics of EBS volumes attached
--     to the current instance during the look-back period.
--
-- -   __@EBSThroughputUnderprovisioned@__ — The instance’s EBS throughput
--     configuration doesn\'t meet the performance requirements of your
--     workload and there is an alternative instance type that provides
--     better EBS throughput performance. This is identified by analyzing
--     the @VolumeReadOps@ and @VolumeWriteOps@ metrics of EBS volumes
--     attached to the current instance during the look-back period.
--
-- -   __@EBSIOPSOverprovisioned@__ — The instance’s EBS IOPS configuration
--     can be sized down while still meeting the performance requirements
--     of your workload. This is identified by analyzing the
--     @VolumeReadBytes@ and @VolumeWriteBytes@ metric of EBS volumes
--     attached to the current instance during the look-back period.
--
-- -   __@EBSIOPSUnderprovisioned@__ — The instance’s EBS IOPS
--     configuration doesn\'t meet the performance requirements of your
--     workload and there is an alternative instance type that provides
--     better EBS IOPS performance. This is identified by analyzing the
--     @VolumeReadBytes@ and @VolumeWriteBytes@ metric of EBS volumes
--     attached to the current instance during the look-back period.
--
-- -   __@NetworkBandwidthOverprovisioned@__ — The instance’s network
--     bandwidth configuration can be sized down while still meeting the
--     performance requirements of your workload. This is identified by
--     analyzing the @NetworkIn@ and @NetworkOut@ metrics of the current
--     instance during the look-back period.
--
-- -   __@NetworkBandwidthUnderprovisioned@__ — The instance’s network
--     bandwidth configuration doesn\'t meet the performance requirements
--     of your workload and there is an alternative instance type that
--     provides better network bandwidth performance. This is identified by
--     analyzing the @NetworkIn@ and @NetworkOut@ metrics of the current
--     instance during the look-back period. This finding reason happens
--     when the @NetworkIn@ or @NetworkOut@ performance of an instance is
--     impacted.
--
-- -   __@NetworkPPSOverprovisioned@__ — The instance’s network PPS
--     (packets per second) configuration can be sized down while still
--     meeting the performance requirements of your workload. This is
--     identified by analyzing the @NetworkPacketsIn@ and
--     @NetworkPacketsIn@ metrics of the current instance during the
--     look-back period.
--
-- -   __@NetworkPPSUnderprovisioned@__ — The instance’s network PPS
--     (packets per second) configuration doesn\'t meet the performance
--     requirements of your workload and there is an alternative instance
--     type that provides better network PPS performance. This is
--     identified by analyzing the @NetworkPacketsIn@ and
--     @NetworkPacketsIn@ metrics of the current instance during the
--     look-back period.
--
-- -   __@DiskIOPSOverprovisioned@__ — The instance’s disk IOPS
--     configuration can be sized down while still meeting the performance
--     requirements of your workload. This is identified by analyzing the
--     @DiskReadOps@ and @DiskWriteOps@ metrics of the current instance
--     during the look-back period.
--
-- -   __@DiskIOPSUnderprovisioned@__ — The instance’s disk IOPS
--     configuration doesn\'t meet the performance requirements of your
--     workload and there is an alternative instance type that provides
--     better disk IOPS performance. This is identified by analyzing the
--     @DiskReadOps@ and @DiskWriteOps@ metrics of the current instance
--     during the look-back period.
--
-- -   __@DiskThroughputOverprovisioned@__ — The instance’s disk throughput
--     configuration can be sized down while still meeting the performance
--     requirements of your workload. This is identified by analyzing the
--     @DiskReadBytes@ and @DiskWriteBytes@ metrics of the current instance
--     during the look-back period.
--
-- -   __@DiskThroughputUnderprovisioned@__ — The instance’s disk
--     throughput configuration doesn\'t meet the performance requirements
--     of your workload and there is an alternative instance type that
--     provides better disk throughput performance. This is identified by
--     analyzing the @DiskReadBytes@ and @DiskWriteBytes@ metrics of the
--     current instance during the look-back period.
--
-- For more information about instance metrics, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/viewing_metrics_with_cloudwatch.html List the available CloudWatch metrics for your instances>
-- in the /Amazon Elastic Compute Cloud User Guide/. For more information
-- about EBS volume metrics, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using_cloudwatch_ebs.html Amazon CloudWatch metrics for Amazon EBS>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'inferredWorkloadTypes', 'instanceRecommendation_inferredWorkloadTypes' - The applications that might be running on the instance as inferred by
-- Compute Optimizer.
--
-- Compute Optimizer can infer if one of the following applications might
-- be running on the instance:
--
-- -   @AmazonEmr@ - Infers that Amazon EMR might be running on the
--     instance.
--
-- -   @ApacheCassandra@ - Infers that Apache Cassandra might be running on
--     the instance.
--
-- -   @ApacheHadoop@ - Infers that Apache Hadoop might be running on the
--     instance.
--
-- -   @Memcached@ - Infers that Memcached might be running on the
--     instance.
--
-- -   @NGINX@ - Infers that NGINX might be running on the instance.
--
-- -   @PostgreSql@ - Infers that PostgreSQL might be running on the
--     instance.
--
-- -   @Redis@ - Infers that Redis might be running on the instance.
--
-- 'instanceArn', 'instanceRecommendation_instanceArn' - The Amazon Resource Name (ARN) of the current instance.
--
-- 'instanceName', 'instanceRecommendation_instanceName' - The name of the current instance.
--
-- 'lastRefreshTimestamp', 'instanceRecommendation_lastRefreshTimestamp' - The timestamp of when the instance recommendation was last generated.
--
-- 'lookBackPeriodInDays', 'instanceRecommendation_lookBackPeriodInDays' - The number of days for which utilization metrics were analyzed for the
-- instance.
--
-- 'recommendationOptions', 'instanceRecommendation_recommendationOptions' - An array of objects that describe the recommendation options for the
-- instance.
--
-- 'recommendationSources', 'instanceRecommendation_recommendationSources' - An array of objects that describe the source resource of the
-- recommendation.
--
-- 'utilizationMetrics', 'instanceRecommendation_utilizationMetrics' - An array of objects that describe the utilization metrics of the
-- instance.
newInstanceRecommendation ::
  InstanceRecommendation
newInstanceRecommendation =
  InstanceRecommendation'
    { accountId =
        Prelude.Nothing,
      currentInstanceType = Prelude.Nothing,
      currentPerformanceRisk = Prelude.Nothing,
      effectiveRecommendationPreferences =
        Prelude.Nothing,
      finding = Prelude.Nothing,
      findingReasonCodes = Prelude.Nothing,
      inferredWorkloadTypes = Prelude.Nothing,
      instanceArn = Prelude.Nothing,
      instanceName = Prelude.Nothing,
      lastRefreshTimestamp = Prelude.Nothing,
      lookBackPeriodInDays = Prelude.Nothing,
      recommendationOptions = Prelude.Nothing,
      recommendationSources = Prelude.Nothing,
      utilizationMetrics = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the instance.
instanceRecommendation_accountId :: Lens.Lens' InstanceRecommendation (Prelude.Maybe Prelude.Text)
instanceRecommendation_accountId = Lens.lens (\InstanceRecommendation' {accountId} -> accountId) (\s@InstanceRecommendation' {} a -> s {accountId = a} :: InstanceRecommendation)

-- | The instance type of the current instance.
instanceRecommendation_currentInstanceType :: Lens.Lens' InstanceRecommendation (Prelude.Maybe Prelude.Text)
instanceRecommendation_currentInstanceType = Lens.lens (\InstanceRecommendation' {currentInstanceType} -> currentInstanceType) (\s@InstanceRecommendation' {} a -> s {currentInstanceType = a} :: InstanceRecommendation)

-- | The risk of the current instance not meeting the performance needs of
-- its workloads. The higher the risk, the more likely the current instance
-- cannot meet the performance requirements of its workload.
instanceRecommendation_currentPerformanceRisk :: Lens.Lens' InstanceRecommendation (Prelude.Maybe CurrentPerformanceRisk)
instanceRecommendation_currentPerformanceRisk = Lens.lens (\InstanceRecommendation' {currentPerformanceRisk} -> currentPerformanceRisk) (\s@InstanceRecommendation' {} a -> s {currentPerformanceRisk = a} :: InstanceRecommendation)

-- | An object that describes the effective recommendation preferences for
-- the instance.
instanceRecommendation_effectiveRecommendationPreferences :: Lens.Lens' InstanceRecommendation (Prelude.Maybe EffectiveRecommendationPreferences)
instanceRecommendation_effectiveRecommendationPreferences = Lens.lens (\InstanceRecommendation' {effectiveRecommendationPreferences} -> effectiveRecommendationPreferences) (\s@InstanceRecommendation' {} a -> s {effectiveRecommendationPreferences = a} :: InstanceRecommendation)

-- | The finding classification of the instance.
--
-- Findings for instances include:
--
-- -   __@Underprovisioned@__ —An instance is considered under-provisioned
--     when at least one specification of your instance, such as CPU,
--     memory, or network, does not meet the performance requirements of
--     your workload. Under-provisioned instances may lead to poor
--     application performance.
--
-- -   __@Overprovisioned@__ —An instance is considered over-provisioned
--     when at least one specification of your instance, such as CPU,
--     memory, or network, can be sized down while still meeting the
--     performance requirements of your workload, and no specification is
--     under-provisioned. Over-provisioned instances may lead to
--     unnecessary infrastructure cost.
--
-- -   __@Optimized@__ —An instance is considered optimized when all
--     specifications of your instance, such as CPU, memory, and network,
--     meet the performance requirements of your workload and is not over
--     provisioned. For optimized resources, Compute Optimizer might
--     recommend a new generation instance type.
instanceRecommendation_finding :: Lens.Lens' InstanceRecommendation (Prelude.Maybe Finding)
instanceRecommendation_finding = Lens.lens (\InstanceRecommendation' {finding} -> finding) (\s@InstanceRecommendation' {} a -> s {finding = a} :: InstanceRecommendation)

-- | The reason for the finding classification of the instance.
--
-- Finding reason codes for instances include:
--
-- -   __@CPUOverprovisioned@__ — The instance’s CPU configuration can be
--     sized down while still meeting the performance requirements of your
--     workload. This is identified by analyzing the @CPUUtilization@
--     metric of the current instance during the look-back period.
--
-- -   __@CPUUnderprovisioned@__ — The instance’s CPU configuration
--     doesn\'t meet the performance requirements of your workload and
--     there is an alternative instance type that provides better CPU
--     performance. This is identified by analyzing the @CPUUtilization@
--     metric of the current instance during the look-back period.
--
-- -   __@MemoryOverprovisioned@__ — The instance’s memory configuration
--     can be sized down while still meeting the performance requirements
--     of your workload. This is identified by analyzing the memory
--     utilization metric of the current instance during the look-back
--     period.
--
-- -   __@MemoryUnderprovisioned@__ — The instance’s memory configuration
--     doesn\'t meet the performance requirements of your workload and
--     there is an alternative instance type that provides better memory
--     performance. This is identified by analyzing the memory utilization
--     metric of the current instance during the look-back period.
--
--     Memory utilization is analyzed only for resources that have the
--     unified CloudWatch agent installed on them. For more information,
--     see
--     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling memory utilization with the Amazon CloudWatch Agent>
--     in the /Compute Optimizer User Guide/. On Linux instances, Compute
--     Optimizer analyses the @mem_used_percent@ metric in the @CWAgent@
--     namespace, or the legacy @MemoryUtilization@ metric in the
--     @System\/Linux@ namespace. On Windows instances, Compute Optimizer
--     analyses the @Memory % Committed Bytes In Use@ metric in the
--     @CWAgent@ namespace.
--
-- -   __@EBSThroughputOverprovisioned@__ — The instance’s EBS throughput
--     configuration can be sized down while still meeting the performance
--     requirements of your workload. This is identified by analyzing the
--     @VolumeReadOps@ and @VolumeWriteOps@ metrics of EBS volumes attached
--     to the current instance during the look-back period.
--
-- -   __@EBSThroughputUnderprovisioned@__ — The instance’s EBS throughput
--     configuration doesn\'t meet the performance requirements of your
--     workload and there is an alternative instance type that provides
--     better EBS throughput performance. This is identified by analyzing
--     the @VolumeReadOps@ and @VolumeWriteOps@ metrics of EBS volumes
--     attached to the current instance during the look-back period.
--
-- -   __@EBSIOPSOverprovisioned@__ — The instance’s EBS IOPS configuration
--     can be sized down while still meeting the performance requirements
--     of your workload. This is identified by analyzing the
--     @VolumeReadBytes@ and @VolumeWriteBytes@ metric of EBS volumes
--     attached to the current instance during the look-back period.
--
-- -   __@EBSIOPSUnderprovisioned@__ — The instance’s EBS IOPS
--     configuration doesn\'t meet the performance requirements of your
--     workload and there is an alternative instance type that provides
--     better EBS IOPS performance. This is identified by analyzing the
--     @VolumeReadBytes@ and @VolumeWriteBytes@ metric of EBS volumes
--     attached to the current instance during the look-back period.
--
-- -   __@NetworkBandwidthOverprovisioned@__ — The instance’s network
--     bandwidth configuration can be sized down while still meeting the
--     performance requirements of your workload. This is identified by
--     analyzing the @NetworkIn@ and @NetworkOut@ metrics of the current
--     instance during the look-back period.
--
-- -   __@NetworkBandwidthUnderprovisioned@__ — The instance’s network
--     bandwidth configuration doesn\'t meet the performance requirements
--     of your workload and there is an alternative instance type that
--     provides better network bandwidth performance. This is identified by
--     analyzing the @NetworkIn@ and @NetworkOut@ metrics of the current
--     instance during the look-back period. This finding reason happens
--     when the @NetworkIn@ or @NetworkOut@ performance of an instance is
--     impacted.
--
-- -   __@NetworkPPSOverprovisioned@__ — The instance’s network PPS
--     (packets per second) configuration can be sized down while still
--     meeting the performance requirements of your workload. This is
--     identified by analyzing the @NetworkPacketsIn@ and
--     @NetworkPacketsIn@ metrics of the current instance during the
--     look-back period.
--
-- -   __@NetworkPPSUnderprovisioned@__ — The instance’s network PPS
--     (packets per second) configuration doesn\'t meet the performance
--     requirements of your workload and there is an alternative instance
--     type that provides better network PPS performance. This is
--     identified by analyzing the @NetworkPacketsIn@ and
--     @NetworkPacketsIn@ metrics of the current instance during the
--     look-back period.
--
-- -   __@DiskIOPSOverprovisioned@__ — The instance’s disk IOPS
--     configuration can be sized down while still meeting the performance
--     requirements of your workload. This is identified by analyzing the
--     @DiskReadOps@ and @DiskWriteOps@ metrics of the current instance
--     during the look-back period.
--
-- -   __@DiskIOPSUnderprovisioned@__ — The instance’s disk IOPS
--     configuration doesn\'t meet the performance requirements of your
--     workload and there is an alternative instance type that provides
--     better disk IOPS performance. This is identified by analyzing the
--     @DiskReadOps@ and @DiskWriteOps@ metrics of the current instance
--     during the look-back period.
--
-- -   __@DiskThroughputOverprovisioned@__ — The instance’s disk throughput
--     configuration can be sized down while still meeting the performance
--     requirements of your workload. This is identified by analyzing the
--     @DiskReadBytes@ and @DiskWriteBytes@ metrics of the current instance
--     during the look-back period.
--
-- -   __@DiskThroughputUnderprovisioned@__ — The instance’s disk
--     throughput configuration doesn\'t meet the performance requirements
--     of your workload and there is an alternative instance type that
--     provides better disk throughput performance. This is identified by
--     analyzing the @DiskReadBytes@ and @DiskWriteBytes@ metrics of the
--     current instance during the look-back period.
--
-- For more information about instance metrics, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/viewing_metrics_with_cloudwatch.html List the available CloudWatch metrics for your instances>
-- in the /Amazon Elastic Compute Cloud User Guide/. For more information
-- about EBS volume metrics, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using_cloudwatch_ebs.html Amazon CloudWatch metrics for Amazon EBS>
-- in the /Amazon Elastic Compute Cloud User Guide/.
instanceRecommendation_findingReasonCodes :: Lens.Lens' InstanceRecommendation (Prelude.Maybe [InstanceRecommendationFindingReasonCode])
instanceRecommendation_findingReasonCodes = Lens.lens (\InstanceRecommendation' {findingReasonCodes} -> findingReasonCodes) (\s@InstanceRecommendation' {} a -> s {findingReasonCodes = a} :: InstanceRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The applications that might be running on the instance as inferred by
-- Compute Optimizer.
--
-- Compute Optimizer can infer if one of the following applications might
-- be running on the instance:
--
-- -   @AmazonEmr@ - Infers that Amazon EMR might be running on the
--     instance.
--
-- -   @ApacheCassandra@ - Infers that Apache Cassandra might be running on
--     the instance.
--
-- -   @ApacheHadoop@ - Infers that Apache Hadoop might be running on the
--     instance.
--
-- -   @Memcached@ - Infers that Memcached might be running on the
--     instance.
--
-- -   @NGINX@ - Infers that NGINX might be running on the instance.
--
-- -   @PostgreSql@ - Infers that PostgreSQL might be running on the
--     instance.
--
-- -   @Redis@ - Infers that Redis might be running on the instance.
instanceRecommendation_inferredWorkloadTypes :: Lens.Lens' InstanceRecommendation (Prelude.Maybe [InferredWorkloadType])
instanceRecommendation_inferredWorkloadTypes = Lens.lens (\InstanceRecommendation' {inferredWorkloadTypes} -> inferredWorkloadTypes) (\s@InstanceRecommendation' {} a -> s {inferredWorkloadTypes = a} :: InstanceRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the current instance.
instanceRecommendation_instanceArn :: Lens.Lens' InstanceRecommendation (Prelude.Maybe Prelude.Text)
instanceRecommendation_instanceArn = Lens.lens (\InstanceRecommendation' {instanceArn} -> instanceArn) (\s@InstanceRecommendation' {} a -> s {instanceArn = a} :: InstanceRecommendation)

-- | The name of the current instance.
instanceRecommendation_instanceName :: Lens.Lens' InstanceRecommendation (Prelude.Maybe Prelude.Text)
instanceRecommendation_instanceName = Lens.lens (\InstanceRecommendation' {instanceName} -> instanceName) (\s@InstanceRecommendation' {} a -> s {instanceName = a} :: InstanceRecommendation)

-- | The timestamp of when the instance recommendation was last generated.
instanceRecommendation_lastRefreshTimestamp :: Lens.Lens' InstanceRecommendation (Prelude.Maybe Prelude.UTCTime)
instanceRecommendation_lastRefreshTimestamp = Lens.lens (\InstanceRecommendation' {lastRefreshTimestamp} -> lastRefreshTimestamp) (\s@InstanceRecommendation' {} a -> s {lastRefreshTimestamp = a} :: InstanceRecommendation) Prelude.. Lens.mapping Data._Time

-- | The number of days for which utilization metrics were analyzed for the
-- instance.
instanceRecommendation_lookBackPeriodInDays :: Lens.Lens' InstanceRecommendation (Prelude.Maybe Prelude.Double)
instanceRecommendation_lookBackPeriodInDays = Lens.lens (\InstanceRecommendation' {lookBackPeriodInDays} -> lookBackPeriodInDays) (\s@InstanceRecommendation' {} a -> s {lookBackPeriodInDays = a} :: InstanceRecommendation)

-- | An array of objects that describe the recommendation options for the
-- instance.
instanceRecommendation_recommendationOptions :: Lens.Lens' InstanceRecommendation (Prelude.Maybe [InstanceRecommendationOption])
instanceRecommendation_recommendationOptions = Lens.lens (\InstanceRecommendation' {recommendationOptions} -> recommendationOptions) (\s@InstanceRecommendation' {} a -> s {recommendationOptions = a} :: InstanceRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that describe the source resource of the
-- recommendation.
instanceRecommendation_recommendationSources :: Lens.Lens' InstanceRecommendation (Prelude.Maybe [RecommendationSource])
instanceRecommendation_recommendationSources = Lens.lens (\InstanceRecommendation' {recommendationSources} -> recommendationSources) (\s@InstanceRecommendation' {} a -> s {recommendationSources = a} :: InstanceRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that describe the utilization metrics of the
-- instance.
instanceRecommendation_utilizationMetrics :: Lens.Lens' InstanceRecommendation (Prelude.Maybe [UtilizationMetric])
instanceRecommendation_utilizationMetrics = Lens.lens (\InstanceRecommendation' {utilizationMetrics} -> utilizationMetrics) (\s@InstanceRecommendation' {} a -> s {utilizationMetrics = a} :: InstanceRecommendation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON InstanceRecommendation where
  parseJSON =
    Data.withObject
      "InstanceRecommendation"
      ( \x ->
          InstanceRecommendation'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "currentInstanceType")
            Prelude.<*> (x Data..:? "currentPerformanceRisk")
            Prelude.<*> (x Data..:? "effectiveRecommendationPreferences")
            Prelude.<*> (x Data..:? "finding")
            Prelude.<*> ( x Data..:? "findingReasonCodes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "inferredWorkloadTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "instanceArn")
            Prelude.<*> (x Data..:? "instanceName")
            Prelude.<*> (x Data..:? "lastRefreshTimestamp")
            Prelude.<*> (x Data..:? "lookBackPeriodInDays")
            Prelude.<*> ( x Data..:? "recommendationOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "recommendationSources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "utilizationMetrics"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable InstanceRecommendation where
  hashWithSalt _salt InstanceRecommendation' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` currentInstanceType
      `Prelude.hashWithSalt` currentPerformanceRisk
      `Prelude.hashWithSalt` effectiveRecommendationPreferences
      `Prelude.hashWithSalt` finding
      `Prelude.hashWithSalt` findingReasonCodes
      `Prelude.hashWithSalt` inferredWorkloadTypes
      `Prelude.hashWithSalt` instanceArn
      `Prelude.hashWithSalt` instanceName
      `Prelude.hashWithSalt` lastRefreshTimestamp
      `Prelude.hashWithSalt` lookBackPeriodInDays
      `Prelude.hashWithSalt` recommendationOptions
      `Prelude.hashWithSalt` recommendationSources
      `Prelude.hashWithSalt` utilizationMetrics

instance Prelude.NFData InstanceRecommendation where
  rnf InstanceRecommendation' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf currentInstanceType
      `Prelude.seq` Prelude.rnf currentPerformanceRisk
      `Prelude.seq` Prelude.rnf effectiveRecommendationPreferences
      `Prelude.seq` Prelude.rnf finding
      `Prelude.seq` Prelude.rnf findingReasonCodes
      `Prelude.seq` Prelude.rnf inferredWorkloadTypes
      `Prelude.seq` Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf instanceName
      `Prelude.seq` Prelude.rnf lastRefreshTimestamp
      `Prelude.seq` Prelude.rnf lookBackPeriodInDays
      `Prelude.seq` Prelude.rnf recommendationOptions
      `Prelude.seq` Prelude.rnf recommendationSources
      `Prelude.seq` Prelude.rnf utilizationMetrics
