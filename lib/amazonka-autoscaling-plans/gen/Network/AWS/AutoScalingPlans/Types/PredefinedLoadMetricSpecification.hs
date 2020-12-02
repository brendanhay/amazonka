{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification where

import Network.AWS.AutoScalingPlans.Types.LoadMetricType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a predefined metric that can be used for predictive scaling.
--
--
--
-- /See:/ 'predefinedLoadMetricSpecification' smart constructor.
data PredefinedLoadMetricSpecification = PredefinedLoadMetricSpecification'
  { _plmsResourceLabel ::
      !(Maybe Text),
    _plmsPredefinedLoadMetricType ::
      !LoadMetricType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PredefinedLoadMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plmsResourceLabel' - Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
-- * 'plmsPredefinedLoadMetricType' - The metric type.
predefinedLoadMetricSpecification ::
  -- | 'plmsPredefinedLoadMetricType'
  LoadMetricType ->
  PredefinedLoadMetricSpecification
predefinedLoadMetricSpecification pPredefinedLoadMetricType_ =
  PredefinedLoadMetricSpecification'
    { _plmsResourceLabel = Nothing,
      _plmsPredefinedLoadMetricType = pPredefinedLoadMetricType_
    }

-- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
plmsResourceLabel :: Lens' PredefinedLoadMetricSpecification (Maybe Text)
plmsResourceLabel = lens _plmsResourceLabel (\s a -> s {_plmsResourceLabel = a})

-- | The metric type.
plmsPredefinedLoadMetricType :: Lens' PredefinedLoadMetricSpecification LoadMetricType
plmsPredefinedLoadMetricType = lens _plmsPredefinedLoadMetricType (\s a -> s {_plmsPredefinedLoadMetricType = a})

instance FromJSON PredefinedLoadMetricSpecification where
  parseJSON =
    withObject
      "PredefinedLoadMetricSpecification"
      ( \x ->
          PredefinedLoadMetricSpecification'
            <$> (x .:? "ResourceLabel") <*> (x .: "PredefinedLoadMetricType")
      )

instance Hashable PredefinedLoadMetricSpecification

instance NFData PredefinedLoadMetricSpecification

instance ToJSON PredefinedLoadMetricSpecification where
  toJSON PredefinedLoadMetricSpecification' {..} =
    object
      ( catMaybes
          [ ("ResourceLabel" .=) <$> _plmsResourceLabel,
            Just
              ("PredefinedLoadMetricType" .= _plmsPredefinedLoadMetricType)
          ]
      )
