{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ApplicationAutoScaling.Types.PredefinedMetricSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.PredefinedMetricSpecification where

import Network.AWS.ApplicationAutoScaling.Types.MetricType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a predefined metric for a target tracking scaling policy to
-- use with Application Auto Scaling.
--
-- Only the AWS services that you\'re using send metrics to Amazon
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
    -- Spot Fleet request or ECS service.
    --
    -- You create the resource label by appending the final portion of the load
    -- balancer ARN and the final portion of the target group ARN into a single
    -- value, separated by a forward slash (\/). The format is
    -- app\/\<load-balancer-name>\/\<load-balancer-id>\/targetgroup\/\<target-group-name>\/\<target-group-id>,
    -- where:
    --
    -- -   app\/\<load-balancer-name>\/\<load-balancer-id> is the final portion
    --     of the load balancer ARN
    --
    -- -   targetgroup\/\<target-group-name>\/\<target-group-id> is the final
    --     portion of the target group ARN.
    --
    -- This is an example:
    -- app\/EC2Co-EcsEl-1TKLTMITMM0EO\/f37c06a68c1748aa\/targetgroup\/EC2Co-Defau-LDNM7Q3ZH1ZN\/6d4ea56ca2d6a18d.
    --
    -- To find the ARN for an Application Load Balancer, use the
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>
    -- API operation. To find the ARN for the target group, use the
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
    -- API operation.
    resourceLabel :: Prelude.Maybe Prelude.Text,
    -- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only
    -- to Spot Fleet requests and ECS services.
    predefinedMetricType :: MetricType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- Spot Fleet request or ECS service.
--
-- You create the resource label by appending the final portion of the load
-- balancer ARN and the final portion of the target group ARN into a single
-- value, separated by a forward slash (\/). The format is
-- app\/\<load-balancer-name>\/\<load-balancer-id>\/targetgroup\/\<target-group-name>\/\<target-group-id>,
-- where:
--
-- -   app\/\<load-balancer-name>\/\<load-balancer-id> is the final portion
--     of the load balancer ARN
--
-- -   targetgroup\/\<target-group-name>\/\<target-group-id> is the final
--     portion of the target group ARN.
--
-- This is an example:
-- app\/EC2Co-EcsEl-1TKLTMITMM0EO\/f37c06a68c1748aa\/targetgroup\/EC2Co-Defau-LDNM7Q3ZH1ZN\/6d4ea56ca2d6a18d.
--
-- To find the ARN for an Application Load Balancer, use the
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>
-- API operation. To find the ARN for the target group, use the
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
-- API operation.
--
-- 'predefinedMetricType', 'predefinedMetricSpecification_predefinedMetricType' - The metric type. The @ALBRequestCountPerTarget@ metric type applies only
-- to Spot Fleet requests and ECS services.
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
-- Spot Fleet request or ECS service.
--
-- You create the resource label by appending the final portion of the load
-- balancer ARN and the final portion of the target group ARN into a single
-- value, separated by a forward slash (\/). The format is
-- app\/\<load-balancer-name>\/\<load-balancer-id>\/targetgroup\/\<target-group-name>\/\<target-group-id>,
-- where:
--
-- -   app\/\<load-balancer-name>\/\<load-balancer-id> is the final portion
--     of the load balancer ARN
--
-- -   targetgroup\/\<target-group-name>\/\<target-group-id> is the final
--     portion of the target group ARN.
--
-- This is an example:
-- app\/EC2Co-EcsEl-1TKLTMITMM0EO\/f37c06a68c1748aa\/targetgroup\/EC2Co-Defau-LDNM7Q3ZH1ZN\/6d4ea56ca2d6a18d.
--
-- To find the ARN for an Application Load Balancer, use the
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>
-- API operation. To find the ARN for the target group, use the
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
-- API operation.
predefinedMetricSpecification_resourceLabel :: Lens.Lens' PredefinedMetricSpecification (Prelude.Maybe Prelude.Text)
predefinedMetricSpecification_resourceLabel = Lens.lens (\PredefinedMetricSpecification' {resourceLabel} -> resourceLabel) (\s@PredefinedMetricSpecification' {} a -> s {resourceLabel = a} :: PredefinedMetricSpecification)

-- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only
-- to Spot Fleet requests and ECS services.
predefinedMetricSpecification_predefinedMetricType :: Lens.Lens' PredefinedMetricSpecification MetricType
predefinedMetricSpecification_predefinedMetricType = Lens.lens (\PredefinedMetricSpecification' {predefinedMetricType} -> predefinedMetricType) (\s@PredefinedMetricSpecification' {} a -> s {predefinedMetricType = a} :: PredefinedMetricSpecification)

instance
  Prelude.FromJSON
    PredefinedMetricSpecification
  where
  parseJSON =
    Prelude.withObject
      "PredefinedMetricSpecification"
      ( \x ->
          PredefinedMetricSpecification'
            Prelude.<$> (x Prelude..:? "ResourceLabel")
            Prelude.<*> (x Prelude..: "PredefinedMetricType")
      )

instance
  Prelude.Hashable
    PredefinedMetricSpecification

instance Prelude.NFData PredefinedMetricSpecification

instance Prelude.ToJSON PredefinedMetricSpecification where
  toJSON PredefinedMetricSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceLabel" Prelude..=)
              Prelude.<$> resourceLabel,
            Prelude.Just
              ( "PredefinedMetricType"
                  Prelude..= predefinedMetricType
              )
          ]
      )
