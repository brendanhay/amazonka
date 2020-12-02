{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MetricDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MetricDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a metric that the training algorithm writes to @stderr@ or @stdout@ . Amazon SageMakerhyperparameter tuning captures all defined metrics. You specify one metric that a hyperparameter tuning job uses as its objective metric to choose the best training job.
--
--
--
-- /See:/ 'metricDefinition' smart constructor.
data MetricDefinition = MetricDefinition'
  { _mdName :: !Text,
    _mdRegex :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdName' - The name of the metric.
--
-- * 'mdRegex' - A regular expression that searches the output of a training job and gets the value of the metric. For more information about using regular expressions to define metrics, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics.html Defining Objective Metrics> .
metricDefinition ::
  -- | 'mdName'
  Text ->
  -- | 'mdRegex'
  Text ->
  MetricDefinition
metricDefinition pName_ pRegex_ =
  MetricDefinition' {_mdName = pName_, _mdRegex = pRegex_}

-- | The name of the metric.
mdName :: Lens' MetricDefinition Text
mdName = lens _mdName (\s a -> s {_mdName = a})

-- | A regular expression that searches the output of a training job and gets the value of the metric. For more information about using regular expressions to define metrics, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics.html Defining Objective Metrics> .
mdRegex :: Lens' MetricDefinition Text
mdRegex = lens _mdRegex (\s a -> s {_mdRegex = a})

instance FromJSON MetricDefinition where
  parseJSON =
    withObject
      "MetricDefinition"
      (\x -> MetricDefinition' <$> (x .: "Name") <*> (x .: "Regex"))

instance Hashable MetricDefinition

instance NFData MetricDefinition

instance ToJSON MetricDefinition where
  toJSON MetricDefinition' {..} =
    object
      (catMaybes [Just ("Name" .= _mdName), Just ("Regex" .= _mdRegex)])
