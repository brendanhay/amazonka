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
-- Module      : Amazonka.ApplicationAutoScaling.Types.PredefinedMetricSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.PredefinedMetricSpecification where

import Amazonka.ApplicationAutoScaling.Types.MetricType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a predefined metric for a target tracking scaling policy to
-- use with Application Auto Scaling.
--
-- Only the Amazon Web Services that you\'re using send metrics to Amazon
-- CloudWatch. To determine whether a desired metric already exists by
-- looking up its namespace and dimension using the CloudWatch metrics
-- dashboard in the console, follow the procedure in
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/monitoring-cloudwatch.html Building dashboards with CloudWatch>
-- in the /Application Auto Scaling User Guide/.
--
-- /See:/ 'newPredefinedMetricSpecification' smart constructor.
data PredefinedMetricSpecification = PredefinedMetricSpecification'
  { -- | Identifies the resource associated with the metric type. You can\'t
    -- specify a resource label unless the metric type is
    -- @ALBRequestCountPerTarget@ and there is a target group attached to the
    -- Spot Fleet or ECS service.
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
    -- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only
    -- to Spot Fleets and ECS services.
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
-- 'resourceLabel', 'predefinedMetricSpecification_resourceLabel' - Identifies the resource associated with the metric type. You can\'t
-- specify a resource label unless the metric type is
-- @ALBRequestCountPerTarget@ and there is a target group attached to the
-- Spot Fleet or ECS service.
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
-- 'predefinedMetricType', 'predefinedMetricSpecification_predefinedMetricType' - The metric type. The @ALBRequestCountPerTarget@ metric type applies only
-- to Spot Fleets and ECS services.
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

-- | Identifies the resource associated with the metric type. You can\'t
-- specify a resource label unless the metric type is
-- @ALBRequestCountPerTarget@ and there is a target group attached to the
-- Spot Fleet or ECS service.
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

-- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only
-- to Spot Fleets and ECS services.
predefinedMetricSpecification_predefinedMetricType :: Lens.Lens' PredefinedMetricSpecification MetricType
predefinedMetricSpecification_predefinedMetricType = Lens.lens (\PredefinedMetricSpecification' {predefinedMetricType} -> predefinedMetricType) (\s@PredefinedMetricSpecification' {} a -> s {predefinedMetricType = a} :: PredefinedMetricSpecification)

instance Data.FromJSON PredefinedMetricSpecification where
  parseJSON =
    Data.withObject
      "PredefinedMetricSpecification"
      ( \x ->
          PredefinedMetricSpecification'
            Prelude.<$> (x Data..:? "ResourceLabel")
            Prelude.<*> (x Data..: "PredefinedMetricType")
      )

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

instance Data.ToJSON PredefinedMetricSpecification where
  toJSON PredefinedMetricSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceLabel" Data..=) Prelude.<$> resourceLabel,
            Prelude.Just
              ( "PredefinedMetricType"
                  Data..= predefinedMetricType
              )
          ]
      )
