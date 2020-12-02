{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ParameterRanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ParameterRanges where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CategoricalParameterRange
import Network.AWS.SageMaker.Types.ContinuousParameterRange
import Network.AWS.SageMaker.Types.IntegerParameterRange

-- | Specifies ranges of integer, continuous, and categorical hyperparameters that a hyperparameter tuning job searches. The hyperparameter tuning job launches training jobs with hyperparameter values within these ranges to find the combination of values that result in the training job with the best performance as measured by the objective metric of the hyperparameter tuning job.
--
--
--
-- /See:/ 'parameterRanges' smart constructor.
data ParameterRanges = ParameterRanges'
  { _prCategoricalParameterRanges ::
      !(Maybe [CategoricalParameterRange]),
    _prIntegerParameterRanges ::
      !(Maybe [IntegerParameterRange]),
    _prContinuousParameterRanges ::
      !(Maybe [ContinuousParameterRange])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterRanges' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prCategoricalParameterRanges' - The array of 'CategoricalParameterRange' objects that specify ranges of categorical hyperparameters that a hyperparameter tuning job searches.
--
-- * 'prIntegerParameterRanges' - The array of 'IntegerParameterRange' objects that specify ranges of integer hyperparameters that a hyperparameter tuning job searches.
--
-- * 'prContinuousParameterRanges' - The array of 'ContinuousParameterRange' objects that specify ranges of continuous hyperparameters that a hyperparameter tuning job searches.
parameterRanges ::
  ParameterRanges
parameterRanges =
  ParameterRanges'
    { _prCategoricalParameterRanges = Nothing,
      _prIntegerParameterRanges = Nothing,
      _prContinuousParameterRanges = Nothing
    }

-- | The array of 'CategoricalParameterRange' objects that specify ranges of categorical hyperparameters that a hyperparameter tuning job searches.
prCategoricalParameterRanges :: Lens' ParameterRanges [CategoricalParameterRange]
prCategoricalParameterRanges = lens _prCategoricalParameterRanges (\s a -> s {_prCategoricalParameterRanges = a}) . _Default . _Coerce

-- | The array of 'IntegerParameterRange' objects that specify ranges of integer hyperparameters that a hyperparameter tuning job searches.
prIntegerParameterRanges :: Lens' ParameterRanges [IntegerParameterRange]
prIntegerParameterRanges = lens _prIntegerParameterRanges (\s a -> s {_prIntegerParameterRanges = a}) . _Default . _Coerce

-- | The array of 'ContinuousParameterRange' objects that specify ranges of continuous hyperparameters that a hyperparameter tuning job searches.
prContinuousParameterRanges :: Lens' ParameterRanges [ContinuousParameterRange]
prContinuousParameterRanges = lens _prContinuousParameterRanges (\s a -> s {_prContinuousParameterRanges = a}) . _Default . _Coerce

instance FromJSON ParameterRanges where
  parseJSON =
    withObject
      "ParameterRanges"
      ( \x ->
          ParameterRanges'
            <$> (x .:? "CategoricalParameterRanges" .!= mempty)
            <*> (x .:? "IntegerParameterRanges" .!= mempty)
            <*> (x .:? "ContinuousParameterRanges" .!= mempty)
      )

instance Hashable ParameterRanges

instance NFData ParameterRanges

instance ToJSON ParameterRanges where
  toJSON ParameterRanges' {..} =
    object
      ( catMaybes
          [ ("CategoricalParameterRanges" .=)
              <$> _prCategoricalParameterRanges,
            ("IntegerParameterRanges" .=) <$> _prIntegerParameterRanges,
            ("ContinuousParameterRanges" .=) <$> _prContinuousParameterRanges
          ]
      )
