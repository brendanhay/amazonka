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
-- Module      : Amazonka.AutoScaling.Types.PredictiveScalingPredefinedMetricPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredictiveScalingPredefinedMetricPair where

import Amazonka.AutoScaling.Types.PredefinedMetricPairType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a metric pair for a predictive scaling policy.
--
-- /See:/ 'newPredictiveScalingPredefinedMetricPair' smart constructor.
data PredictiveScalingPredefinedMetricPair = PredictiveScalingPredefinedMetricPair'
  { -- | A label that uniquely identifies a specific Application Load Balancer
    -- target group from which to determine the total and average request count
    -- served by your Auto Scaling group. You can\'t specify a resource label
    -- unless the target group is attached to the Auto Scaling group.
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
    -- | Indicates which metrics to use. There are two different types of metrics
    -- for each metric type: one is a load metric and one is a scaling metric.
    -- For example, if the metric type is @ASGCPUUtilization@, the Auto Scaling
    -- group\'s total CPU metric is used as the load metric, and the average
    -- CPU metric is used for the scaling metric.
    predefinedMetricType :: PredefinedMetricPairType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictiveScalingPredefinedMetricPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceLabel', 'predictiveScalingPredefinedMetricPair_resourceLabel' - A label that uniquely identifies a specific Application Load Balancer
-- target group from which to determine the total and average request count
-- served by your Auto Scaling group. You can\'t specify a resource label
-- unless the target group is attached to the Auto Scaling group.
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
-- 'predefinedMetricType', 'predictiveScalingPredefinedMetricPair_predefinedMetricType' - Indicates which metrics to use. There are two different types of metrics
-- for each metric type: one is a load metric and one is a scaling metric.
-- For example, if the metric type is @ASGCPUUtilization@, the Auto Scaling
-- group\'s total CPU metric is used as the load metric, and the average
-- CPU metric is used for the scaling metric.
newPredictiveScalingPredefinedMetricPair ::
  -- | 'predefinedMetricType'
  PredefinedMetricPairType ->
  PredictiveScalingPredefinedMetricPair
newPredictiveScalingPredefinedMetricPair
  pPredefinedMetricType_ =
    PredictiveScalingPredefinedMetricPair'
      { resourceLabel =
          Prelude.Nothing,
        predefinedMetricType =
          pPredefinedMetricType_
      }

-- | A label that uniquely identifies a specific Application Load Balancer
-- target group from which to determine the total and average request count
-- served by your Auto Scaling group. You can\'t specify a resource label
-- unless the target group is attached to the Auto Scaling group.
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
predictiveScalingPredefinedMetricPair_resourceLabel :: Lens.Lens' PredictiveScalingPredefinedMetricPair (Prelude.Maybe Prelude.Text)
predictiveScalingPredefinedMetricPair_resourceLabel = Lens.lens (\PredictiveScalingPredefinedMetricPair' {resourceLabel} -> resourceLabel) (\s@PredictiveScalingPredefinedMetricPair' {} a -> s {resourceLabel = a} :: PredictiveScalingPredefinedMetricPair)

-- | Indicates which metrics to use. There are two different types of metrics
-- for each metric type: one is a load metric and one is a scaling metric.
-- For example, if the metric type is @ASGCPUUtilization@, the Auto Scaling
-- group\'s total CPU metric is used as the load metric, and the average
-- CPU metric is used for the scaling metric.
predictiveScalingPredefinedMetricPair_predefinedMetricType :: Lens.Lens' PredictiveScalingPredefinedMetricPair PredefinedMetricPairType
predictiveScalingPredefinedMetricPair_predefinedMetricType = Lens.lens (\PredictiveScalingPredefinedMetricPair' {predefinedMetricType} -> predefinedMetricType) (\s@PredictiveScalingPredefinedMetricPair' {} a -> s {predefinedMetricType = a} :: PredictiveScalingPredefinedMetricPair)

instance
  Data.FromXML
    PredictiveScalingPredefinedMetricPair
  where
  parseXML x =
    PredictiveScalingPredefinedMetricPair'
      Prelude.<$> (x Data..@? "ResourceLabel")
      Prelude.<*> (x Data..@ "PredefinedMetricType")

instance
  Prelude.Hashable
    PredictiveScalingPredefinedMetricPair
  where
  hashWithSalt
    _salt
    PredictiveScalingPredefinedMetricPair' {..} =
      _salt
        `Prelude.hashWithSalt` resourceLabel
        `Prelude.hashWithSalt` predefinedMetricType

instance
  Prelude.NFData
    PredictiveScalingPredefinedMetricPair
  where
  rnf PredictiveScalingPredefinedMetricPair' {..} =
    Prelude.rnf resourceLabel `Prelude.seq`
      Prelude.rnf predefinedMetricType

instance
  Data.ToQuery
    PredictiveScalingPredefinedMetricPair
  where
  toQuery PredictiveScalingPredefinedMetricPair' {..} =
    Prelude.mconcat
      [ "ResourceLabel" Data.=: resourceLabel,
        "PredefinedMetricType" Data.=: predefinedMetricType
      ]
