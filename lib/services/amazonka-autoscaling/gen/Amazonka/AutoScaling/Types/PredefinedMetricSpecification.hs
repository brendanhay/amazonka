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
-- Module      : Amazonka.AutoScaling.Types.PredefinedMetricSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredefinedMetricSpecification where

import Amazonka.AutoScaling.Types.MetricType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a predefined metric for a target tracking scaling policy to
-- use with Amazon EC2 Auto Scaling.
--
-- /See:/ 'newPredefinedMetricSpecification' smart constructor.
data PredefinedMetricSpecification = PredefinedMetricSpecification'
  { -- | A label that uniquely identifies a specific Application Load Balancer
    -- target group from which to determine the average request count served by
    -- your Auto Scaling group. You can\'t specify a resource label unless the
    -- target group is attached to the Auto Scaling group.
    --
    -- You create the resource label by appending the final portion of the load
    -- balancer ARN and the final portion of the target group ARN into a single
    -- value, separated by a forward slash (\/). The format of the resource
    -- label is:
    --
    -- @app\/my-alb\/778d41231b141a0f\/targetgroup\/my-alb-target-group\/943f017f100becff@.
    --
    -- Where:
    --
    -- -   app\/\<load-balancer-name>\/\<load-balancer-id> is the final portion
    --     of the load balancer ARN
    --
    -- -   targetgroup\/\<target-group-name>\/\<target-group-id> is the final
    --     portion of the target group ARN.
    --
    -- To find the ARN for an Application Load Balancer, use the
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>
    -- API operation. To find the ARN for the target group, use the
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
    -- API operation.
    resourceLabel :: Prelude.Maybe Prelude.Text,
    -- | The metric type. The following predefined metrics are available:
    --
    -- -   @ASGAverageCPUUtilization@ - Average CPU utilization of the Auto
    --     Scaling group.
    --
    -- -   @ASGAverageNetworkIn@ - Average number of bytes received on all
    --     network interfaces by the Auto Scaling group.
    --
    -- -   @ASGAverageNetworkOut@ - Average number of bytes sent out on all
    --     network interfaces by the Auto Scaling group.
    --
    -- -   @ALBRequestCountPerTarget@ - Average Application Load Balancer
    --     request count per target for your Auto Scaling group.
    predefinedMetricType :: MetricType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredefinedMetricSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceLabel', 'predefinedMetricSpecification_resourceLabel' - A label that uniquely identifies a specific Application Load Balancer
-- target group from which to determine the average request count served by
-- your Auto Scaling group. You can\'t specify a resource label unless the
-- target group is attached to the Auto Scaling group.
--
-- You create the resource label by appending the final portion of the load
-- balancer ARN and the final portion of the target group ARN into a single
-- value, separated by a forward slash (\/). The format of the resource
-- label is:
--
-- @app\/my-alb\/778d41231b141a0f\/targetgroup\/my-alb-target-group\/943f017f100becff@.
--
-- Where:
--
-- -   app\/\<load-balancer-name>\/\<load-balancer-id> is the final portion
--     of the load balancer ARN
--
-- -   targetgroup\/\<target-group-name>\/\<target-group-id> is the final
--     portion of the target group ARN.
--
-- To find the ARN for an Application Load Balancer, use the
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>
-- API operation. To find the ARN for the target group, use the
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
-- API operation.
--
-- 'predefinedMetricType', 'predefinedMetricSpecification_predefinedMetricType' - The metric type. The following predefined metrics are available:
--
-- -   @ASGAverageCPUUtilization@ - Average CPU utilization of the Auto
--     Scaling group.
--
-- -   @ASGAverageNetworkIn@ - Average number of bytes received on all
--     network interfaces by the Auto Scaling group.
--
-- -   @ASGAverageNetworkOut@ - Average number of bytes sent out on all
--     network interfaces by the Auto Scaling group.
--
-- -   @ALBRequestCountPerTarget@ - Average Application Load Balancer
--     request count per target for your Auto Scaling group.
newPredefinedMetricSpecification ::
  -- | 'predefinedMetricType'
  MetricType ->
  PredefinedMetricSpecification
newPredefinedMetricSpecification
  pPredefinedMetricType_ =
    PredefinedMetricSpecification'
      { resourceLabel =
          Prelude.Nothing,
        predefinedMetricType =
          pPredefinedMetricType_
      }

-- | A label that uniquely identifies a specific Application Load Balancer
-- target group from which to determine the average request count served by
-- your Auto Scaling group. You can\'t specify a resource label unless the
-- target group is attached to the Auto Scaling group.
--
-- You create the resource label by appending the final portion of the load
-- balancer ARN and the final portion of the target group ARN into a single
-- value, separated by a forward slash (\/). The format of the resource
-- label is:
--
-- @app\/my-alb\/778d41231b141a0f\/targetgroup\/my-alb-target-group\/943f017f100becff@.
--
-- Where:
--
-- -   app\/\<load-balancer-name>\/\<load-balancer-id> is the final portion
--     of the load balancer ARN
--
-- -   targetgroup\/\<target-group-name>\/\<target-group-id> is the final
--     portion of the target group ARN.
--
-- To find the ARN for an Application Load Balancer, use the
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>
-- API operation. To find the ARN for the target group, use the
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
-- API operation.
predefinedMetricSpecification_resourceLabel :: Lens.Lens' PredefinedMetricSpecification (Prelude.Maybe Prelude.Text)
predefinedMetricSpecification_resourceLabel = Lens.lens (\PredefinedMetricSpecification' {resourceLabel} -> resourceLabel) (\s@PredefinedMetricSpecification' {} a -> s {resourceLabel = a} :: PredefinedMetricSpecification)

-- | The metric type. The following predefined metrics are available:
--
-- -   @ASGAverageCPUUtilization@ - Average CPU utilization of the Auto
--     Scaling group.
--
-- -   @ASGAverageNetworkIn@ - Average number of bytes received on all
--     network interfaces by the Auto Scaling group.
--
-- -   @ASGAverageNetworkOut@ - Average number of bytes sent out on all
--     network interfaces by the Auto Scaling group.
--
-- -   @ALBRequestCountPerTarget@ - Average Application Load Balancer
--     request count per target for your Auto Scaling group.
predefinedMetricSpecification_predefinedMetricType :: Lens.Lens' PredefinedMetricSpecification MetricType
predefinedMetricSpecification_predefinedMetricType = Lens.lens (\PredefinedMetricSpecification' {predefinedMetricType} -> predefinedMetricType) (\s@PredefinedMetricSpecification' {} a -> s {predefinedMetricType = a} :: PredefinedMetricSpecification)

instance Data.FromXML PredefinedMetricSpecification where
  parseXML x =
    PredefinedMetricSpecification'
      Prelude.<$> (x Data..@? "ResourceLabel")
      Prelude.<*> (x Data..@ "PredefinedMetricType")

instance
  Prelude.Hashable
    PredefinedMetricSpecification
  where
  hashWithSalt _salt PredefinedMetricSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` resourceLabel
      `Prelude.hashWithSalt` predefinedMetricType

instance Prelude.NFData PredefinedMetricSpecification where
  rnf PredefinedMetricSpecification' {..} =
    Prelude.rnf resourceLabel
      `Prelude.seq` Prelude.rnf predefinedMetricType

instance Data.ToQuery PredefinedMetricSpecification where
  toQuery PredefinedMetricSpecification' {..} =
    Prelude.mconcat
      [ "ResourceLabel" Data.=: resourceLabel,
        "PredefinedMetricType" Data.=: predefinedMetricType
      ]
