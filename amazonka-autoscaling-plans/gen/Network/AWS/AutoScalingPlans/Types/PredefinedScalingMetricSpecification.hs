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
-- Module      : Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification where

import Network.AWS.AutoScalingPlans.Types.ScalingMetricType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a predefined metric that can be used for dynamic scaling as
-- part of a target tracking scaling policy.
--
-- /See:/ 'newPredefinedScalingMetricSpecification' smart constructor.
data PredefinedScalingMetricSpecification = PredefinedScalingMetricSpecification'
  { -- | Identifies the resource associated with the metric type. You can\'t
    -- specify a resource label unless the metric type is
    -- @ALBRequestCountPerTarget@ and there is a target group for an
    -- Application Load Balancer attached to the Auto Scaling group, Spot Fleet
    -- request, or ECS service.
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
    -- to Auto Scaling groups, Spot Fleet requests, and ECS services.
    predefinedScalingMetricType :: ScalingMetricType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PredefinedScalingMetricSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceLabel', 'predefinedScalingMetricSpecification_resourceLabel' - Identifies the resource associated with the metric type. You can\'t
-- specify a resource label unless the metric type is
-- @ALBRequestCountPerTarget@ and there is a target group for an
-- Application Load Balancer attached to the Auto Scaling group, Spot Fleet
-- request, or ECS service.
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
-- 'predefinedScalingMetricType', 'predefinedScalingMetricSpecification_predefinedScalingMetricType' - The metric type. The @ALBRequestCountPerTarget@ metric type applies only
-- to Auto Scaling groups, Spot Fleet requests, and ECS services.
newPredefinedScalingMetricSpecification ::
  -- | 'predefinedScalingMetricType'
  ScalingMetricType ->
  PredefinedScalingMetricSpecification
newPredefinedScalingMetricSpecification
  pPredefinedScalingMetricType_ =
    PredefinedScalingMetricSpecification'
      { resourceLabel =
          Prelude.Nothing,
        predefinedScalingMetricType =
          pPredefinedScalingMetricType_
      }

-- | Identifies the resource associated with the metric type. You can\'t
-- specify a resource label unless the metric type is
-- @ALBRequestCountPerTarget@ and there is a target group for an
-- Application Load Balancer attached to the Auto Scaling group, Spot Fleet
-- request, or ECS service.
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
predefinedScalingMetricSpecification_resourceLabel :: Lens.Lens' PredefinedScalingMetricSpecification (Prelude.Maybe Prelude.Text)
predefinedScalingMetricSpecification_resourceLabel = Lens.lens (\PredefinedScalingMetricSpecification' {resourceLabel} -> resourceLabel) (\s@PredefinedScalingMetricSpecification' {} a -> s {resourceLabel = a} :: PredefinedScalingMetricSpecification)

-- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only
-- to Auto Scaling groups, Spot Fleet requests, and ECS services.
predefinedScalingMetricSpecification_predefinedScalingMetricType :: Lens.Lens' PredefinedScalingMetricSpecification ScalingMetricType
predefinedScalingMetricSpecification_predefinedScalingMetricType = Lens.lens (\PredefinedScalingMetricSpecification' {predefinedScalingMetricType} -> predefinedScalingMetricType) (\s@PredefinedScalingMetricSpecification' {} a -> s {predefinedScalingMetricType = a} :: PredefinedScalingMetricSpecification)

instance
  Prelude.FromJSON
    PredefinedScalingMetricSpecification
  where
  parseJSON =
    Prelude.withObject
      "PredefinedScalingMetricSpecification"
      ( \x ->
          PredefinedScalingMetricSpecification'
            Prelude.<$> (x Prelude..:? "ResourceLabel")
            Prelude.<*> (x Prelude..: "PredefinedScalingMetricType")
      )

instance
  Prelude.Hashable
    PredefinedScalingMetricSpecification

instance
  Prelude.NFData
    PredefinedScalingMetricSpecification

instance
  Prelude.ToJSON
    PredefinedScalingMetricSpecification
  where
  toJSON PredefinedScalingMetricSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceLabel" Prelude..=)
              Prelude.<$> resourceLabel,
            Prelude.Just
              ( "PredefinedScalingMetricType"
                  Prelude..= predefinedScalingMetricType
              )
          ]
      )
