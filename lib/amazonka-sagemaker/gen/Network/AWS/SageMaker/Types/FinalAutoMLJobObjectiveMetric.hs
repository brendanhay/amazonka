{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AutoMLJobObjectiveType
import Network.AWS.SageMaker.Types.AutoMLMetricEnum

-- | The best candidate result from an AutoML training job.
--
--
--
-- /See:/ 'finalAutoMLJobObjectiveMetric' smart constructor.
data FinalAutoMLJobObjectiveMetric = FinalAutoMLJobObjectiveMetric'
  { _famljomType ::
      !(Maybe AutoMLJobObjectiveType),
    _famljomMetricName ::
      !AutoMLMetricEnum,
    _famljomValue :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FinalAutoMLJobObjectiveMetric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'famljomType' - The type of metric with the best result.
--
-- * 'famljomMetricName' - The name of the metric with the best result. For a description of the possible objective metrics, see 'AutoMLJobObjective$MetricName' .
--
-- * 'famljomValue' - The value of the metric with the best result.
finalAutoMLJobObjectiveMetric ::
  -- | 'famljomMetricName'
  AutoMLMetricEnum ->
  -- | 'famljomValue'
  Double ->
  FinalAutoMLJobObjectiveMetric
finalAutoMLJobObjectiveMetric pMetricName_ pValue_ =
  FinalAutoMLJobObjectiveMetric'
    { _famljomType = Nothing,
      _famljomMetricName = pMetricName_,
      _famljomValue = pValue_
    }

-- | The type of metric with the best result.
famljomType :: Lens' FinalAutoMLJobObjectiveMetric (Maybe AutoMLJobObjectiveType)
famljomType = lens _famljomType (\s a -> s {_famljomType = a})

-- | The name of the metric with the best result. For a description of the possible objective metrics, see 'AutoMLJobObjective$MetricName' .
famljomMetricName :: Lens' FinalAutoMLJobObjectiveMetric AutoMLMetricEnum
famljomMetricName = lens _famljomMetricName (\s a -> s {_famljomMetricName = a})

-- | The value of the metric with the best result.
famljomValue :: Lens' FinalAutoMLJobObjectiveMetric Double
famljomValue = lens _famljomValue (\s a -> s {_famljomValue = a})

instance FromJSON FinalAutoMLJobObjectiveMetric where
  parseJSON =
    withObject
      "FinalAutoMLJobObjectiveMetric"
      ( \x ->
          FinalAutoMLJobObjectiveMetric'
            <$> (x .:? "Type") <*> (x .: "MetricName") <*> (x .: "Value")
      )

instance Hashable FinalAutoMLJobObjectiveMetric

instance NFData FinalAutoMLJobObjectiveMetric
