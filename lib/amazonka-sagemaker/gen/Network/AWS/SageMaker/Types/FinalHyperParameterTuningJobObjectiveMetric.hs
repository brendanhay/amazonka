{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType

-- | Shows the final value for the objective metric for a training job that was launched by a hyperparameter tuning job. You define the objective metric in the @HyperParameterTuningJobObjective@ parameter of 'HyperParameterTuningJobConfig' .
--
--
--
-- /See:/ 'finalHyperParameterTuningJobObjectiveMetric' smart constructor.
data FinalHyperParameterTuningJobObjectiveMetric = FinalHyperParameterTuningJobObjectiveMetric'
  { _fhptjomType ::
      !( Maybe
           HyperParameterTuningJobObjectiveType
       ),
    _fhptjomMetricName ::
      !Text,
    _fhptjomValue ::
      !Double
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'FinalHyperParameterTuningJobObjectiveMetric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fhptjomType' - Whether to minimize or maximize the objective metric. Valid values are Minimize and Maximize.
--
-- * 'fhptjomMetricName' - The name of the objective metric.
--
-- * 'fhptjomValue' - The value of the objective metric.
finalHyperParameterTuningJobObjectiveMetric ::
  -- | 'fhptjomMetricName'
  Text ->
  -- | 'fhptjomValue'
  Double ->
  FinalHyperParameterTuningJobObjectiveMetric
finalHyperParameterTuningJobObjectiveMetric pMetricName_ pValue_ =
  FinalHyperParameterTuningJobObjectiveMetric'
    { _fhptjomType =
        Nothing,
      _fhptjomMetricName = pMetricName_,
      _fhptjomValue = pValue_
    }

-- | Whether to minimize or maximize the objective metric. Valid values are Minimize and Maximize.
fhptjomType :: Lens' FinalHyperParameterTuningJobObjectiveMetric (Maybe HyperParameterTuningJobObjectiveType)
fhptjomType = lens _fhptjomType (\s a -> s {_fhptjomType = a})

-- | The name of the objective metric.
fhptjomMetricName :: Lens' FinalHyperParameterTuningJobObjectiveMetric Text
fhptjomMetricName = lens _fhptjomMetricName (\s a -> s {_fhptjomMetricName = a})

-- | The value of the objective metric.
fhptjomValue :: Lens' FinalHyperParameterTuningJobObjectiveMetric Double
fhptjomValue = lens _fhptjomValue (\s a -> s {_fhptjomValue = a})

instance FromJSON FinalHyperParameterTuningJobObjectiveMetric where
  parseJSON =
    withObject
      "FinalHyperParameterTuningJobObjectiveMetric"
      ( \x ->
          FinalHyperParameterTuningJobObjectiveMetric'
            <$> (x .:? "Type") <*> (x .: "MetricName") <*> (x .: "Value")
      )

instance Hashable FinalHyperParameterTuningJobObjectiveMetric

instance NFData FinalHyperParameterTuningJobObjectiveMetric
