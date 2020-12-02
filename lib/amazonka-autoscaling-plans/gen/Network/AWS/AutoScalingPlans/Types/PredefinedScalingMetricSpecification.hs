{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification where

import Network.AWS.AutoScalingPlans.Types.ScalingMetricType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a predefined metric that can be used for dynamic scaling as part of a target tracking scaling policy.
--
--
--
-- /See:/ 'predefinedScalingMetricSpecification' smart constructor.
data PredefinedScalingMetricSpecification = PredefinedScalingMetricSpecification'
  { _psmsResourceLabel ::
      !(Maybe Text),
    _psmsPredefinedScalingMetricType ::
      !ScalingMetricType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PredefinedScalingMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psmsResourceLabel' - Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group, Spot Fleet request, or ECS service. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
-- * 'psmsPredefinedScalingMetricType' - The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Auto Scaling groups, Spot Fleet requests, and ECS services.
predefinedScalingMetricSpecification ::
  -- | 'psmsPredefinedScalingMetricType'
  ScalingMetricType ->
  PredefinedScalingMetricSpecification
predefinedScalingMetricSpecification pPredefinedScalingMetricType_ =
  PredefinedScalingMetricSpecification'
    { _psmsResourceLabel =
        Nothing,
      _psmsPredefinedScalingMetricType =
        pPredefinedScalingMetricType_
    }

-- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group, Spot Fleet request, or ECS service. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
psmsResourceLabel :: Lens' PredefinedScalingMetricSpecification (Maybe Text)
psmsResourceLabel = lens _psmsResourceLabel (\s a -> s {_psmsResourceLabel = a})

-- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Auto Scaling groups, Spot Fleet requests, and ECS services.
psmsPredefinedScalingMetricType :: Lens' PredefinedScalingMetricSpecification ScalingMetricType
psmsPredefinedScalingMetricType = lens _psmsPredefinedScalingMetricType (\s a -> s {_psmsPredefinedScalingMetricType = a})

instance FromJSON PredefinedScalingMetricSpecification where
  parseJSON =
    withObject
      "PredefinedScalingMetricSpecification"
      ( \x ->
          PredefinedScalingMetricSpecification'
            <$> (x .:? "ResourceLabel") <*> (x .: "PredefinedScalingMetricType")
      )

instance Hashable PredefinedScalingMetricSpecification

instance NFData PredefinedScalingMetricSpecification

instance ToJSON PredefinedScalingMetricSpecification where
  toJSON PredefinedScalingMetricSpecification' {..} =
    object
      ( catMaybes
          [ ("ResourceLabel" .=) <$> _psmsResourceLabel,
            Just
              ( "PredefinedScalingMetricType"
                  .= _psmsPredefinedScalingMetricType
              )
          ]
      )
