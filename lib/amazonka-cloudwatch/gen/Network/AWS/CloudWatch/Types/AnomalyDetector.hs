{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.AnomalyDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AnomalyDetector where

import Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration
import Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue
import Network.AWS.CloudWatch.Types.Dimension
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An anomaly detection model associated with a particular CloudWatch metric and statistic. You can use the model to display a band of expected normal values when the metric is graphed.
--
--
--
-- /See:/ 'anomalyDetector' smart constructor.
data AnomalyDetector = AnomalyDetector'
  { _adMetricName ::
      !(Maybe Text),
    _adNamespace :: !(Maybe Text),
    _adStateValue :: !(Maybe AnomalyDetectorStateValue),
    _adStat :: !(Maybe Text),
    _adConfiguration :: !(Maybe AnomalyDetectorConfiguration),
    _adDimensions :: !(Maybe [Dimension])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnomalyDetector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adMetricName' - The name of the metric associated with the anomaly detection model.
--
-- * 'adNamespace' - The namespace of the metric associated with the anomaly detection model.
--
-- * 'adStateValue' - The current status of the anomaly detector's training. The possible values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
--
-- * 'adStat' - The statistic associated with the anomaly detection model.
--
-- * 'adConfiguration' - The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model, and the time zone to use for the metric.
--
-- * 'adDimensions' - The metric dimensions associated with the anomaly detection model.
anomalyDetector ::
  AnomalyDetector
anomalyDetector =
  AnomalyDetector'
    { _adMetricName = Nothing,
      _adNamespace = Nothing,
      _adStateValue = Nothing,
      _adStat = Nothing,
      _adConfiguration = Nothing,
      _adDimensions = Nothing
    }

-- | The name of the metric associated with the anomaly detection model.
adMetricName :: Lens' AnomalyDetector (Maybe Text)
adMetricName = lens _adMetricName (\s a -> s {_adMetricName = a})

-- | The namespace of the metric associated with the anomaly detection model.
adNamespace :: Lens' AnomalyDetector (Maybe Text)
adNamespace = lens _adNamespace (\s a -> s {_adNamespace = a})

-- | The current status of the anomaly detector's training. The possible values are @TRAINED | PENDING_TRAINING | TRAINED_INSUFFICIENT_DATA@
adStateValue :: Lens' AnomalyDetector (Maybe AnomalyDetectorStateValue)
adStateValue = lens _adStateValue (\s a -> s {_adStateValue = a})

-- | The statistic associated with the anomaly detection model.
adStat :: Lens' AnomalyDetector (Maybe Text)
adStat = lens _adStat (\s a -> s {_adStat = a})

-- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model, and the time zone to use for the metric.
adConfiguration :: Lens' AnomalyDetector (Maybe AnomalyDetectorConfiguration)
adConfiguration = lens _adConfiguration (\s a -> s {_adConfiguration = a})

-- | The metric dimensions associated with the anomaly detection model.
adDimensions :: Lens' AnomalyDetector [Dimension]
adDimensions = lens _adDimensions (\s a -> s {_adDimensions = a}) . _Default . _Coerce

instance FromXML AnomalyDetector where
  parseXML x =
    AnomalyDetector'
      <$> (x .@? "MetricName")
      <*> (x .@? "Namespace")
      <*> (x .@? "StateValue")
      <*> (x .@? "Stat")
      <*> (x .@? "Configuration")
      <*> (x .@? "Dimensions" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable AnomalyDetector

instance NFData AnomalyDetector
