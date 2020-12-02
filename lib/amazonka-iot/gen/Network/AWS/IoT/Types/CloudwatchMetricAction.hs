{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CloudwatchMetricAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CloudwatchMetricAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action that captures a CloudWatch metric.
--
--
--
-- /See:/ 'cloudwatchMetricAction' smart constructor.
data CloudwatchMetricAction = CloudwatchMetricAction'
  { _cmaMetricTimestamp ::
      !(Maybe Text),
    _cmaRoleARN :: !Text,
    _cmaMetricNamespace :: !Text,
    _cmaMetricName :: !Text,
    _cmaMetricValue :: !Text,
    _cmaMetricUnit :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudwatchMetricAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmaMetricTimestamp' - An optional <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp> .
--
-- * 'cmaRoleARN' - The IAM role that allows access to the CloudWatch metric.
--
-- * 'cmaMetricNamespace' - The CloudWatch metric namespace name.
--
-- * 'cmaMetricName' - The CloudWatch metric name.
--
-- * 'cmaMetricValue' - The CloudWatch metric value.
--
-- * 'cmaMetricUnit' - The <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit> supported by CloudWatch.
cloudwatchMetricAction ::
  -- | 'cmaRoleARN'
  Text ->
  -- | 'cmaMetricNamespace'
  Text ->
  -- | 'cmaMetricName'
  Text ->
  -- | 'cmaMetricValue'
  Text ->
  -- | 'cmaMetricUnit'
  Text ->
  CloudwatchMetricAction
cloudwatchMetricAction
  pRoleARN_
  pMetricNamespace_
  pMetricName_
  pMetricValue_
  pMetricUnit_ =
    CloudwatchMetricAction'
      { _cmaMetricTimestamp = Nothing,
        _cmaRoleARN = pRoleARN_,
        _cmaMetricNamespace = pMetricNamespace_,
        _cmaMetricName = pMetricName_,
        _cmaMetricValue = pMetricValue_,
        _cmaMetricUnit = pMetricUnit_
      }

-- | An optional <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Unix timestamp> .
cmaMetricTimestamp :: Lens' CloudwatchMetricAction (Maybe Text)
cmaMetricTimestamp = lens _cmaMetricTimestamp (\s a -> s {_cmaMetricTimestamp = a})

-- | The IAM role that allows access to the CloudWatch metric.
cmaRoleARN :: Lens' CloudwatchMetricAction Text
cmaRoleARN = lens _cmaRoleARN (\s a -> s {_cmaRoleARN = a})

-- | The CloudWatch metric namespace name.
cmaMetricNamespace :: Lens' CloudwatchMetricAction Text
cmaMetricNamespace = lens _cmaMetricNamespace (\s a -> s {_cmaMetricNamespace = a})

-- | The CloudWatch metric name.
cmaMetricName :: Lens' CloudwatchMetricAction Text
cmaMetricName = lens _cmaMetricName (\s a -> s {_cmaMetricName = a})

-- | The CloudWatch metric value.
cmaMetricValue :: Lens' CloudwatchMetricAction Text
cmaMetricValue = lens _cmaMetricValue (\s a -> s {_cmaMetricValue = a})

-- | The <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#Unit metric unit> supported by CloudWatch.
cmaMetricUnit :: Lens' CloudwatchMetricAction Text
cmaMetricUnit = lens _cmaMetricUnit (\s a -> s {_cmaMetricUnit = a})

instance FromJSON CloudwatchMetricAction where
  parseJSON =
    withObject
      "CloudwatchMetricAction"
      ( \x ->
          CloudwatchMetricAction'
            <$> (x .:? "metricTimestamp")
            <*> (x .: "roleArn")
            <*> (x .: "metricNamespace")
            <*> (x .: "metricName")
            <*> (x .: "metricValue")
            <*> (x .: "metricUnit")
      )

instance Hashable CloudwatchMetricAction

instance NFData CloudwatchMetricAction

instance ToJSON CloudwatchMetricAction where
  toJSON CloudwatchMetricAction' {..} =
    object
      ( catMaybes
          [ ("metricTimestamp" .=) <$> _cmaMetricTimestamp,
            Just ("roleArn" .= _cmaRoleARN),
            Just ("metricNamespace" .= _cmaMetricNamespace),
            Just ("metricName" .= _cmaMetricName),
            Just ("metricValue" .= _cmaMetricValue),
            Just ("metricUnit" .= _cmaMetricUnit)
          ]
      )
