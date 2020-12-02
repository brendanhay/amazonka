{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Behavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Behavior where

import Network.AWS.IoT.Types.BehaviorCriteria
import Network.AWS.IoT.Types.MetricDimension
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A Device Defender security profile behavior.
--
--
--
-- /See:/ 'behavior' smart constructor.
data Behavior = Behavior'
  { _bMetricDimension ::
      !(Maybe MetricDimension),
    _bMetric :: !(Maybe Text),
    _bCriteria :: !(Maybe BehaviorCriteria),
    _bName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Behavior' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bMetricDimension' - The dimension for a metric in your behavior. For example, using a @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric only to MQTT topics whose name match the pattern specified in the dimension.
--
-- * 'bMetric' - What is measured by the behavior.
--
-- * 'bCriteria' - The criteria that determine if a device is behaving normally in regard to the @metric@ .
--
-- * 'bName' - The name you have given to the behavior.
behavior ::
  -- | 'bName'
  Text ->
  Behavior
behavior pName_ =
  Behavior'
    { _bMetricDimension = Nothing,
      _bMetric = Nothing,
      _bCriteria = Nothing,
      _bName = pName_
    }

-- | The dimension for a metric in your behavior. For example, using a @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric only to MQTT topics whose name match the pattern specified in the dimension.
bMetricDimension :: Lens' Behavior (Maybe MetricDimension)
bMetricDimension = lens _bMetricDimension (\s a -> s {_bMetricDimension = a})

-- | What is measured by the behavior.
bMetric :: Lens' Behavior (Maybe Text)
bMetric = lens _bMetric (\s a -> s {_bMetric = a})

-- | The criteria that determine if a device is behaving normally in regard to the @metric@ .
bCriteria :: Lens' Behavior (Maybe BehaviorCriteria)
bCriteria = lens _bCriteria (\s a -> s {_bCriteria = a})

-- | The name you have given to the behavior.
bName :: Lens' Behavior Text
bName = lens _bName (\s a -> s {_bName = a})

instance FromJSON Behavior where
  parseJSON =
    withObject
      "Behavior"
      ( \x ->
          Behavior'
            <$> (x .:? "metricDimension")
            <*> (x .:? "metric")
            <*> (x .:? "criteria")
            <*> (x .: "name")
      )

instance Hashable Behavior

instance NFData Behavior

instance ToJSON Behavior where
  toJSON Behavior' {..} =
    object
      ( catMaybes
          [ ("metricDimension" .=) <$> _bMetricDimension,
            ("metric" .=) <$> _bMetric,
            ("criteria" .=) <$> _bCriteria,
            Just ("name" .= _bName)
          ]
      )
