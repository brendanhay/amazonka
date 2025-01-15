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
-- Module      : Amazonka.AutoScaling.Types.PredictiveScalingPredefinedLoadMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredictiveScalingPredefinedLoadMetric where

import Amazonka.AutoScaling.Types.PredefinedLoadMetricType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a load metric for a predictive scaling policy.
--
-- When returned in the output of @DescribePolicies@, it indicates that a
-- predictive scaling policy uses individually specified load and scaling
-- metrics instead of a metric pair.
--
-- /See:/ 'newPredictiveScalingPredefinedLoadMetric' smart constructor.
data PredictiveScalingPredefinedLoadMetric = PredictiveScalingPredefinedLoadMetric'
  { -- | A label that uniquely identifies a specific Application Load Balancer
    -- target group from which to determine the request count served by your
    -- Auto Scaling group. You can\'t specify a resource label unless the
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
    -- | The metric type.
    predefinedMetricType :: PredefinedLoadMetricType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictiveScalingPredefinedLoadMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceLabel', 'predictiveScalingPredefinedLoadMetric_resourceLabel' - A label that uniquely identifies a specific Application Load Balancer
-- target group from which to determine the request count served by your
-- Auto Scaling group. You can\'t specify a resource label unless the
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
-- 'predefinedMetricType', 'predictiveScalingPredefinedLoadMetric_predefinedMetricType' - The metric type.
newPredictiveScalingPredefinedLoadMetric ::
  -- | 'predefinedMetricType'
  PredefinedLoadMetricType ->
  PredictiveScalingPredefinedLoadMetric
newPredictiveScalingPredefinedLoadMetric
  pPredefinedMetricType_ =
    PredictiveScalingPredefinedLoadMetric'
      { resourceLabel =
          Prelude.Nothing,
        predefinedMetricType =
          pPredefinedMetricType_
      }

-- | A label that uniquely identifies a specific Application Load Balancer
-- target group from which to determine the request count served by your
-- Auto Scaling group. You can\'t specify a resource label unless the
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
predictiveScalingPredefinedLoadMetric_resourceLabel :: Lens.Lens' PredictiveScalingPredefinedLoadMetric (Prelude.Maybe Prelude.Text)
predictiveScalingPredefinedLoadMetric_resourceLabel = Lens.lens (\PredictiveScalingPredefinedLoadMetric' {resourceLabel} -> resourceLabel) (\s@PredictiveScalingPredefinedLoadMetric' {} a -> s {resourceLabel = a} :: PredictiveScalingPredefinedLoadMetric)

-- | The metric type.
predictiveScalingPredefinedLoadMetric_predefinedMetricType :: Lens.Lens' PredictiveScalingPredefinedLoadMetric PredefinedLoadMetricType
predictiveScalingPredefinedLoadMetric_predefinedMetricType = Lens.lens (\PredictiveScalingPredefinedLoadMetric' {predefinedMetricType} -> predefinedMetricType) (\s@PredictiveScalingPredefinedLoadMetric' {} a -> s {predefinedMetricType = a} :: PredictiveScalingPredefinedLoadMetric)

instance
  Data.FromXML
    PredictiveScalingPredefinedLoadMetric
  where
  parseXML x =
    PredictiveScalingPredefinedLoadMetric'
      Prelude.<$> (x Data..@? "ResourceLabel")
      Prelude.<*> (x Data..@ "PredefinedMetricType")

instance
  Prelude.Hashable
    PredictiveScalingPredefinedLoadMetric
  where
  hashWithSalt
    _salt
    PredictiveScalingPredefinedLoadMetric' {..} =
      _salt
        `Prelude.hashWithSalt` resourceLabel
        `Prelude.hashWithSalt` predefinedMetricType

instance
  Prelude.NFData
    PredictiveScalingPredefinedLoadMetric
  where
  rnf PredictiveScalingPredefinedLoadMetric' {..} =
    Prelude.rnf resourceLabel `Prelude.seq`
      Prelude.rnf predefinedMetricType

instance
  Data.ToQuery
    PredictiveScalingPredefinedLoadMetric
  where
  toQuery PredictiveScalingPredefinedLoadMetric' {..} =
    Prelude.mconcat
      [ "ResourceLabel" Data.=: resourceLabel,
        "PredefinedMetricType" Data.=: predefinedMetricType
      ]
