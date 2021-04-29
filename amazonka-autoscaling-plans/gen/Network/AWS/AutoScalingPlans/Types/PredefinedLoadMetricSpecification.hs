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
-- Module      : Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification where

import Network.AWS.AutoScalingPlans.Types.LoadMetricType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a predefined metric that can be used for predictive scaling.
--
-- After creating your scaling plan, you can use the AWS Auto Scaling
-- console to visualize forecasts for the specified metric. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/userguide/gs-create-scaling-plan.html#gs-view-resource View Scaling Information for a Resource>
-- in the /AWS Auto Scaling User Guide/.
--
-- /See:/ 'newPredefinedLoadMetricSpecification' smart constructor.
data PredefinedLoadMetricSpecification = PredefinedLoadMetricSpecification'
  { -- | Identifies the resource associated with the metric type. You can\'t
    -- specify a resource label unless the metric type is
    -- @ALBTargetGroupRequestCount@ and there is a target group for an
    -- Application Load Balancer attached to the Auto Scaling group.
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
    -- | The metric type.
    predefinedLoadMetricType :: LoadMetricType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PredefinedLoadMetricSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceLabel', 'predefinedLoadMetricSpecification_resourceLabel' - Identifies the resource associated with the metric type. You can\'t
-- specify a resource label unless the metric type is
-- @ALBTargetGroupRequestCount@ and there is a target group for an
-- Application Load Balancer attached to the Auto Scaling group.
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
-- 'predefinedLoadMetricType', 'predefinedLoadMetricSpecification_predefinedLoadMetricType' - The metric type.
newPredefinedLoadMetricSpecification ::
  -- | 'predefinedLoadMetricType'
  LoadMetricType ->
  PredefinedLoadMetricSpecification
newPredefinedLoadMetricSpecification
  pPredefinedLoadMetricType_ =
    PredefinedLoadMetricSpecification'
      { resourceLabel =
          Prelude.Nothing,
        predefinedLoadMetricType =
          pPredefinedLoadMetricType_
      }

-- | Identifies the resource associated with the metric type. You can\'t
-- specify a resource label unless the metric type is
-- @ALBTargetGroupRequestCount@ and there is a target group for an
-- Application Load Balancer attached to the Auto Scaling group.
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
predefinedLoadMetricSpecification_resourceLabel :: Lens.Lens' PredefinedLoadMetricSpecification (Prelude.Maybe Prelude.Text)
predefinedLoadMetricSpecification_resourceLabel = Lens.lens (\PredefinedLoadMetricSpecification' {resourceLabel} -> resourceLabel) (\s@PredefinedLoadMetricSpecification' {} a -> s {resourceLabel = a} :: PredefinedLoadMetricSpecification)

-- | The metric type.
predefinedLoadMetricSpecification_predefinedLoadMetricType :: Lens.Lens' PredefinedLoadMetricSpecification LoadMetricType
predefinedLoadMetricSpecification_predefinedLoadMetricType = Lens.lens (\PredefinedLoadMetricSpecification' {predefinedLoadMetricType} -> predefinedLoadMetricType) (\s@PredefinedLoadMetricSpecification' {} a -> s {predefinedLoadMetricType = a} :: PredefinedLoadMetricSpecification)

instance
  Prelude.FromJSON
    PredefinedLoadMetricSpecification
  where
  parseJSON =
    Prelude.withObject
      "PredefinedLoadMetricSpecification"
      ( \x ->
          PredefinedLoadMetricSpecification'
            Prelude.<$> (x Prelude..:? "ResourceLabel")
            Prelude.<*> (x Prelude..: "PredefinedLoadMetricType")
      )

instance
  Prelude.Hashable
    PredefinedLoadMetricSpecification

instance
  Prelude.NFData
    PredefinedLoadMetricSpecification

instance
  Prelude.ToJSON
    PredefinedLoadMetricSpecification
  where
  toJSON PredefinedLoadMetricSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceLabel" Prelude..=)
              Prelude.<$> resourceLabel,
            Prelude.Just
              ( "PredefinedLoadMetricType"
                  Prelude..= predefinedLoadMetricType
              )
          ]
      )
