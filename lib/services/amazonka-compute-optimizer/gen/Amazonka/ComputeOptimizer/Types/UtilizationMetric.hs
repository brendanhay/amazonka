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
-- Module      : Amazonka.ComputeOptimizer.Types.UtilizationMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.UtilizationMetric where

import Amazonka.ComputeOptimizer.Types.MetricName
import Amazonka.ComputeOptimizer.Types.MetricStatistic
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a utilization metric of a resource, such as an Amazon EC2
-- instance.
--
-- Compare the utilization metric data of your resource against its
-- projected utilization metric data to determine the performance
-- difference between your current resource and the recommended option.
--
-- /See:/ 'newUtilizationMetric' smart constructor.
data UtilizationMetric = UtilizationMetric'
  { -- | The name of the utilization metric.
    --
    -- The following utilization metrics are available:
    --
    -- -   @Cpu@ - The percentage of allocated EC2 compute units that are
    --     currently in use on the instance. This metric identifies the
    --     processing power required to run an application on the instance.
    --
    --     Depending on the instance type, tools in your operating system can
    --     show a lower percentage than CloudWatch when the instance is not
    --     allocated a full processor core.
    --
    --     Units: Percent
    --
    -- -   @Memory@ - The percentage of memory that is currently in use on the
    --     instance. This metric identifies the amount of memory required to
    --     run an application on the instance.
    --
    --     Units: Percent
    --
    --     The @Memory@ metric is returned only for resources that have the
    --     unified CloudWatch agent installed on them. For more information,
    --     see
    --     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling Memory Utilization with the CloudWatch Agent>.
    --
    -- -   @EBS_READ_OPS_PER_SECOND@ - The completed read operations from all
    --     EBS volumes attached to the instance in a specified period of time.
    --
    --     Unit: Count
    --
    -- -   @EBS_WRITE_OPS_PER_SECOND@ - The completed write operations to all
    --     EBS volumes attached to the instance in a specified period of time.
    --
    --     Unit: Count
    --
    -- -   @EBS_READ_BYTES_PER_SECOND@ - The bytes read from all EBS volumes
    --     attached to the instance in a specified period of time.
    --
    --     Unit: Bytes
    --
    -- -   @EBS_WRITE_BYTES_PER_SECOND@ - The bytes written to all EBS volumes
    --     attached to the instance in a specified period of time.
    --
    --     Unit: Bytes
    --
    -- -   @DISK_READ_OPS_PER_SECOND@ - The completed read operations from all
    --     instance store volumes available to the instance in a specified
    --     period of time.
    --
    --     If there are no instance store volumes, either the value is @0@ or
    --     the metric is not reported.
    --
    -- -   @DISK_WRITE_OPS_PER_SECOND@ - The completed write operations from
    --     all instance store volumes available to the instance in a specified
    --     period of time.
    --
    --     If there are no instance store volumes, either the value is @0@ or
    --     the metric is not reported.
    --
    -- -   @DISK_READ_BYTES_PER_SECOND@ - The bytes read from all instance
    --     store volumes available to the instance. This metric is used to
    --     determine the volume of the data the application reads from the disk
    --     of the instance. This can be used to determine the speed of the
    --     application.
    --
    --     If there are no instance store volumes, either the value is @0@ or
    --     the metric is not reported.
    --
    -- -   @DISK_WRITE_BYTES_PER_SECOND@ - The bytes written to all instance
    --     store volumes available to the instance. This metric is used to
    --     determine the volume of the data the application writes onto the
    --     disk of the instance. This can be used to determine the speed of the
    --     application.
    --
    --     If there are no instance store volumes, either the value is @0@ or
    --     the metric is not reported.
    --
    -- -   @NETWORK_IN_BYTES_PER_SECOND@ - The number of bytes received by the
    --     instance on all network interfaces. This metric identifies the
    --     volume of incoming network traffic to a single instance.
    --
    -- -   @NETWORK_OUT_BYTES_PER_SECOND@ - The number of bytes sent out by the
    --     instance on all network interfaces. This metric identifies the
    --     volume of outgoing network traffic from a single instance.
    --
    -- -   @NETWORK_PACKETS_IN_PER_SECOND@ - The number of packets received by
    --     the instance on all network interfaces. This metric identifies the
    --     volume of incoming traffic in terms of the number of packets on a
    --     single instance.
    --
    -- -   @NETWORK_PACKETS_OUT_PER_SECOND@ - The number of packets sent out by
    --     the instance on all network interfaces. This metric identifies the
    --     volume of outgoing traffic in terms of the number of packets on a
    --     single instance.
    name :: Prelude.Maybe MetricName,
    -- | The statistic of the utilization metric.
    --
    -- The Compute Optimizer API, Command Line Interface (CLI), and SDKs return
    -- utilization metrics using only the @Maximum@ statistic, which is the
    -- highest value observed during the specified period.
    --
    -- The Compute Optimizer console displays graphs for some utilization
    -- metrics using the @Average@ statistic, which is the value of @Sum@ \/
    -- @SampleCount@ during the specified period. For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/viewing-recommendations.html Viewing resource recommendations>
    -- in the /Compute Optimizer User Guide/. You can also get averaged
    -- utilization metric data for your resources using Amazon CloudWatch. For
    -- more information, see the
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/WhatIsCloudWatch.html Amazon CloudWatch User Guide>.
    statistic :: Prelude.Maybe MetricStatistic,
    -- | The value of the utilization metric.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UtilizationMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'utilizationMetric_name' - The name of the utilization metric.
--
-- The following utilization metrics are available:
--
-- -   @Cpu@ - The percentage of allocated EC2 compute units that are
--     currently in use on the instance. This metric identifies the
--     processing power required to run an application on the instance.
--
--     Depending on the instance type, tools in your operating system can
--     show a lower percentage than CloudWatch when the instance is not
--     allocated a full processor core.
--
--     Units: Percent
--
-- -   @Memory@ - The percentage of memory that is currently in use on the
--     instance. This metric identifies the amount of memory required to
--     run an application on the instance.
--
--     Units: Percent
--
--     The @Memory@ metric is returned only for resources that have the
--     unified CloudWatch agent installed on them. For more information,
--     see
--     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling Memory Utilization with the CloudWatch Agent>.
--
-- -   @EBS_READ_OPS_PER_SECOND@ - The completed read operations from all
--     EBS volumes attached to the instance in a specified period of time.
--
--     Unit: Count
--
-- -   @EBS_WRITE_OPS_PER_SECOND@ - The completed write operations to all
--     EBS volumes attached to the instance in a specified period of time.
--
--     Unit: Count
--
-- -   @EBS_READ_BYTES_PER_SECOND@ - The bytes read from all EBS volumes
--     attached to the instance in a specified period of time.
--
--     Unit: Bytes
--
-- -   @EBS_WRITE_BYTES_PER_SECOND@ - The bytes written to all EBS volumes
--     attached to the instance in a specified period of time.
--
--     Unit: Bytes
--
-- -   @DISK_READ_OPS_PER_SECOND@ - The completed read operations from all
--     instance store volumes available to the instance in a specified
--     period of time.
--
--     If there are no instance store volumes, either the value is @0@ or
--     the metric is not reported.
--
-- -   @DISK_WRITE_OPS_PER_SECOND@ - The completed write operations from
--     all instance store volumes available to the instance in a specified
--     period of time.
--
--     If there are no instance store volumes, either the value is @0@ or
--     the metric is not reported.
--
-- -   @DISK_READ_BYTES_PER_SECOND@ - The bytes read from all instance
--     store volumes available to the instance. This metric is used to
--     determine the volume of the data the application reads from the disk
--     of the instance. This can be used to determine the speed of the
--     application.
--
--     If there are no instance store volumes, either the value is @0@ or
--     the metric is not reported.
--
-- -   @DISK_WRITE_BYTES_PER_SECOND@ - The bytes written to all instance
--     store volumes available to the instance. This metric is used to
--     determine the volume of the data the application writes onto the
--     disk of the instance. This can be used to determine the speed of the
--     application.
--
--     If there are no instance store volumes, either the value is @0@ or
--     the metric is not reported.
--
-- -   @NETWORK_IN_BYTES_PER_SECOND@ - The number of bytes received by the
--     instance on all network interfaces. This metric identifies the
--     volume of incoming network traffic to a single instance.
--
-- -   @NETWORK_OUT_BYTES_PER_SECOND@ - The number of bytes sent out by the
--     instance on all network interfaces. This metric identifies the
--     volume of outgoing network traffic from a single instance.
--
-- -   @NETWORK_PACKETS_IN_PER_SECOND@ - The number of packets received by
--     the instance on all network interfaces. This metric identifies the
--     volume of incoming traffic in terms of the number of packets on a
--     single instance.
--
-- -   @NETWORK_PACKETS_OUT_PER_SECOND@ - The number of packets sent out by
--     the instance on all network interfaces. This metric identifies the
--     volume of outgoing traffic in terms of the number of packets on a
--     single instance.
--
-- 'statistic', 'utilizationMetric_statistic' - The statistic of the utilization metric.
--
-- The Compute Optimizer API, Command Line Interface (CLI), and SDKs return
-- utilization metrics using only the @Maximum@ statistic, which is the
-- highest value observed during the specified period.
--
-- The Compute Optimizer console displays graphs for some utilization
-- metrics using the @Average@ statistic, which is the value of @Sum@ \/
-- @SampleCount@ during the specified period. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/viewing-recommendations.html Viewing resource recommendations>
-- in the /Compute Optimizer User Guide/. You can also get averaged
-- utilization metric data for your resources using Amazon CloudWatch. For
-- more information, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/WhatIsCloudWatch.html Amazon CloudWatch User Guide>.
--
-- 'value', 'utilizationMetric_value' - The value of the utilization metric.
newUtilizationMetric ::
  UtilizationMetric
newUtilizationMetric =
  UtilizationMetric'
    { name = Prelude.Nothing,
      statistic = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the utilization metric.
--
-- The following utilization metrics are available:
--
-- -   @Cpu@ - The percentage of allocated EC2 compute units that are
--     currently in use on the instance. This metric identifies the
--     processing power required to run an application on the instance.
--
--     Depending on the instance type, tools in your operating system can
--     show a lower percentage than CloudWatch when the instance is not
--     allocated a full processor core.
--
--     Units: Percent
--
-- -   @Memory@ - The percentage of memory that is currently in use on the
--     instance. This metric identifies the amount of memory required to
--     run an application on the instance.
--
--     Units: Percent
--
--     The @Memory@ metric is returned only for resources that have the
--     unified CloudWatch agent installed on them. For more information,
--     see
--     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling Memory Utilization with the CloudWatch Agent>.
--
-- -   @EBS_READ_OPS_PER_SECOND@ - The completed read operations from all
--     EBS volumes attached to the instance in a specified period of time.
--
--     Unit: Count
--
-- -   @EBS_WRITE_OPS_PER_SECOND@ - The completed write operations to all
--     EBS volumes attached to the instance in a specified period of time.
--
--     Unit: Count
--
-- -   @EBS_READ_BYTES_PER_SECOND@ - The bytes read from all EBS volumes
--     attached to the instance in a specified period of time.
--
--     Unit: Bytes
--
-- -   @EBS_WRITE_BYTES_PER_SECOND@ - The bytes written to all EBS volumes
--     attached to the instance in a specified period of time.
--
--     Unit: Bytes
--
-- -   @DISK_READ_OPS_PER_SECOND@ - The completed read operations from all
--     instance store volumes available to the instance in a specified
--     period of time.
--
--     If there are no instance store volumes, either the value is @0@ or
--     the metric is not reported.
--
-- -   @DISK_WRITE_OPS_PER_SECOND@ - The completed write operations from
--     all instance store volumes available to the instance in a specified
--     period of time.
--
--     If there are no instance store volumes, either the value is @0@ or
--     the metric is not reported.
--
-- -   @DISK_READ_BYTES_PER_SECOND@ - The bytes read from all instance
--     store volumes available to the instance. This metric is used to
--     determine the volume of the data the application reads from the disk
--     of the instance. This can be used to determine the speed of the
--     application.
--
--     If there are no instance store volumes, either the value is @0@ or
--     the metric is not reported.
--
-- -   @DISK_WRITE_BYTES_PER_SECOND@ - The bytes written to all instance
--     store volumes available to the instance. This metric is used to
--     determine the volume of the data the application writes onto the
--     disk of the instance. This can be used to determine the speed of the
--     application.
--
--     If there are no instance store volumes, either the value is @0@ or
--     the metric is not reported.
--
-- -   @NETWORK_IN_BYTES_PER_SECOND@ - The number of bytes received by the
--     instance on all network interfaces. This metric identifies the
--     volume of incoming network traffic to a single instance.
--
-- -   @NETWORK_OUT_BYTES_PER_SECOND@ - The number of bytes sent out by the
--     instance on all network interfaces. This metric identifies the
--     volume of outgoing network traffic from a single instance.
--
-- -   @NETWORK_PACKETS_IN_PER_SECOND@ - The number of packets received by
--     the instance on all network interfaces. This metric identifies the
--     volume of incoming traffic in terms of the number of packets on a
--     single instance.
--
-- -   @NETWORK_PACKETS_OUT_PER_SECOND@ - The number of packets sent out by
--     the instance on all network interfaces. This metric identifies the
--     volume of outgoing traffic in terms of the number of packets on a
--     single instance.
utilizationMetric_name :: Lens.Lens' UtilizationMetric (Prelude.Maybe MetricName)
utilizationMetric_name = Lens.lens (\UtilizationMetric' {name} -> name) (\s@UtilizationMetric' {} a -> s {name = a} :: UtilizationMetric)

-- | The statistic of the utilization metric.
--
-- The Compute Optimizer API, Command Line Interface (CLI), and SDKs return
-- utilization metrics using only the @Maximum@ statistic, which is the
-- highest value observed during the specified period.
--
-- The Compute Optimizer console displays graphs for some utilization
-- metrics using the @Average@ statistic, which is the value of @Sum@ \/
-- @SampleCount@ during the specified period. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/viewing-recommendations.html Viewing resource recommendations>
-- in the /Compute Optimizer User Guide/. You can also get averaged
-- utilization metric data for your resources using Amazon CloudWatch. For
-- more information, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/WhatIsCloudWatch.html Amazon CloudWatch User Guide>.
utilizationMetric_statistic :: Lens.Lens' UtilizationMetric (Prelude.Maybe MetricStatistic)
utilizationMetric_statistic = Lens.lens (\UtilizationMetric' {statistic} -> statistic) (\s@UtilizationMetric' {} a -> s {statistic = a} :: UtilizationMetric)

-- | The value of the utilization metric.
utilizationMetric_value :: Lens.Lens' UtilizationMetric (Prelude.Maybe Prelude.Double)
utilizationMetric_value = Lens.lens (\UtilizationMetric' {value} -> value) (\s@UtilizationMetric' {} a -> s {value = a} :: UtilizationMetric)

instance Data.FromJSON UtilizationMetric where
  parseJSON =
    Data.withObject
      "UtilizationMetric"
      ( \x ->
          UtilizationMetric'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "statistic")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable UtilizationMetric where
  hashWithSalt _salt UtilizationMetric' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` statistic
      `Prelude.hashWithSalt` value

instance Prelude.NFData UtilizationMetric where
  rnf UtilizationMetric' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf statistic `Prelude.seq`
        Prelude.rnf value
