{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ForecastResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ForecastResult where

import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The forecast created for your query.
--
--
--
-- /See:/ 'forecastResult' smart constructor.
data ForecastResult = ForecastResult'
  { _frTimePeriod ::
      !(Maybe DateInterval),
    _frMeanValue :: !(Maybe Text),
    _frPredictionIntervalUpperBound :: !(Maybe Text),
    _frPredictionIntervalLowerBound :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ForecastResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frTimePeriod' - The period of time that the forecast covers.
--
-- * 'frMeanValue' - The mean value of the forecast.
--
-- * 'frPredictionIntervalUpperBound' - The upper limit for the prediction interval.
--
-- * 'frPredictionIntervalLowerBound' - The lower limit for the prediction interval.
forecastResult ::
  ForecastResult
forecastResult =
  ForecastResult'
    { _frTimePeriod = Nothing,
      _frMeanValue = Nothing,
      _frPredictionIntervalUpperBound = Nothing,
      _frPredictionIntervalLowerBound = Nothing
    }

-- | The period of time that the forecast covers.
frTimePeriod :: Lens' ForecastResult (Maybe DateInterval)
frTimePeriod = lens _frTimePeriod (\s a -> s {_frTimePeriod = a})

-- | The mean value of the forecast.
frMeanValue :: Lens' ForecastResult (Maybe Text)
frMeanValue = lens _frMeanValue (\s a -> s {_frMeanValue = a})

-- | The upper limit for the prediction interval.
frPredictionIntervalUpperBound :: Lens' ForecastResult (Maybe Text)
frPredictionIntervalUpperBound = lens _frPredictionIntervalUpperBound (\s a -> s {_frPredictionIntervalUpperBound = a})

-- | The lower limit for the prediction interval.
frPredictionIntervalLowerBound :: Lens' ForecastResult (Maybe Text)
frPredictionIntervalLowerBound = lens _frPredictionIntervalLowerBound (\s a -> s {_frPredictionIntervalLowerBound = a})

instance FromJSON ForecastResult where
  parseJSON =
    withObject
      "ForecastResult"
      ( \x ->
          ForecastResult'
            <$> (x .:? "TimePeriod")
            <*> (x .:? "MeanValue")
            <*> (x .:? "PredictionIntervalUpperBound")
            <*> (x .:? "PredictionIntervalLowerBound")
      )

instance Hashable ForecastResult

instance NFData ForecastResult
