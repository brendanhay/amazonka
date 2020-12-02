{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType

-- | Defines the objective metric for a hyperparameter tuning job. Hyperparameter tuning uses the value of this metric to evaluate the training jobs it launches, and returns the training job that results in either the highest or lowest value for this metric, depending on the value you specify for the @Type@ parameter.
--
--
--
-- /See:/ 'hyperParameterTuningJobObjective' smart constructor.
data HyperParameterTuningJobObjective = HyperParameterTuningJobObjective'
  { _hptjoType ::
      !HyperParameterTuningJobObjectiveType,
    _hptjoMetricName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HyperParameterTuningJobObjective' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hptjoType' - Whether to minimize or maximize the objective metric.
--
-- * 'hptjoMetricName' - The name of the metric to use for the objective metric.
hyperParameterTuningJobObjective ::
  -- | 'hptjoType'
  HyperParameterTuningJobObjectiveType ->
  -- | 'hptjoMetricName'
  Text ->
  HyperParameterTuningJobObjective
hyperParameterTuningJobObjective pType_ pMetricName_ =
  HyperParameterTuningJobObjective'
    { _hptjoType = pType_,
      _hptjoMetricName = pMetricName_
    }

-- | Whether to minimize or maximize the objective metric.
hptjoType :: Lens' HyperParameterTuningJobObjective HyperParameterTuningJobObjectiveType
hptjoType = lens _hptjoType (\s a -> s {_hptjoType = a})

-- | The name of the metric to use for the objective metric.
hptjoMetricName :: Lens' HyperParameterTuningJobObjective Text
hptjoMetricName = lens _hptjoMetricName (\s a -> s {_hptjoMetricName = a})

instance FromJSON HyperParameterTuningJobObjective where
  parseJSON =
    withObject
      "HyperParameterTuningJobObjective"
      ( \x ->
          HyperParameterTuningJobObjective'
            <$> (x .: "Type") <*> (x .: "MetricName")
      )

instance Hashable HyperParameterTuningJobObjective

instance NFData HyperParameterTuningJobObjective

instance ToJSON HyperParameterTuningJobObjective where
  toJSON HyperParameterTuningJobObjective' {..} =
    object
      ( catMaybes
          [ Just ("Type" .= _hptjoType),
            Just ("MetricName" .= _hptjoMetricName)
          ]
      )
