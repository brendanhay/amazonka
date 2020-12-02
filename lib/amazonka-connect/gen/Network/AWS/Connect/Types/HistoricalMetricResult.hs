{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HistoricalMetricResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetricResult where

import Network.AWS.Connect.Types.Dimensions
import Network.AWS.Connect.Types.HistoricalMetricData
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the historical metrics retrieved.
--
--
--
-- /See:/ 'historicalMetricResult' smart constructor.
data HistoricalMetricResult = HistoricalMetricResult'
  { _hmrCollections ::
      !(Maybe [HistoricalMetricData]),
    _hmrDimensions :: !(Maybe Dimensions)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HistoricalMetricResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmrCollections' - The set of metrics.
--
-- * 'hmrDimensions' - The dimension for the metrics.
historicalMetricResult ::
  HistoricalMetricResult
historicalMetricResult =
  HistoricalMetricResult'
    { _hmrCollections = Nothing,
      _hmrDimensions = Nothing
    }

-- | The set of metrics.
hmrCollections :: Lens' HistoricalMetricResult [HistoricalMetricData]
hmrCollections = lens _hmrCollections (\s a -> s {_hmrCollections = a}) . _Default . _Coerce

-- | The dimension for the metrics.
hmrDimensions :: Lens' HistoricalMetricResult (Maybe Dimensions)
hmrDimensions = lens _hmrDimensions (\s a -> s {_hmrDimensions = a})

instance FromJSON HistoricalMetricResult where
  parseJSON =
    withObject
      "HistoricalMetricResult"
      ( \x ->
          HistoricalMetricResult'
            <$> (x .:? "Collections" .!= mempty) <*> (x .:? "Dimensions")
      )

instance Hashable HistoricalMetricResult

instance NFData HistoricalMetricResult
