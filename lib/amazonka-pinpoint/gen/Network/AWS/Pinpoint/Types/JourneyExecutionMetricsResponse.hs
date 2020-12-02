{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyExecutionMetricsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyExecutionMetricsResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the results of a query that retrieved the data for a standard execution metric that applies to a journey, and provides information about that query.
--
--
--
-- /See:/ 'journeyExecutionMetricsResponse' smart constructor.
data JourneyExecutionMetricsResponse = JourneyExecutionMetricsResponse'
  { _jemMetrics ::
      !(Map Text (Text)),
    _jemJourneyId :: !Text,
    _jemLastEvaluatedTime ::
      !Text,
    _jemApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneyExecutionMetricsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jemMetrics' - A JSON object that contains the results of the query. For information about the structure and contents of the results, see the <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- * 'jemJourneyId' - The unique identifier for the journey that the metric applies to.
--
-- * 'jemLastEvaluatedTime' - The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the journey and updated the data for the metric.
--
-- * 'jemApplicationId' - The unique identifier for the application that the metric applies to.
journeyExecutionMetricsResponse ::
  -- | 'jemJourneyId'
  Text ->
  -- | 'jemLastEvaluatedTime'
  Text ->
  -- | 'jemApplicationId'
  Text ->
  JourneyExecutionMetricsResponse
journeyExecutionMetricsResponse
  pJourneyId_
  pLastEvaluatedTime_
  pApplicationId_ =
    JourneyExecutionMetricsResponse'
      { _jemMetrics = mempty,
        _jemJourneyId = pJourneyId_,
        _jemLastEvaluatedTime = pLastEvaluatedTime_,
        _jemApplicationId = pApplicationId_
      }

-- | A JSON object that contains the results of the query. For information about the structure and contents of the results, see the <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
jemMetrics :: Lens' JourneyExecutionMetricsResponse (HashMap Text (Text))
jemMetrics = lens _jemMetrics (\s a -> s {_jemMetrics = a}) . _Map

-- | The unique identifier for the journey that the metric applies to.
jemJourneyId :: Lens' JourneyExecutionMetricsResponse Text
jemJourneyId = lens _jemJourneyId (\s a -> s {_jemJourneyId = a})

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the journey and updated the data for the metric.
jemLastEvaluatedTime :: Lens' JourneyExecutionMetricsResponse Text
jemLastEvaluatedTime = lens _jemLastEvaluatedTime (\s a -> s {_jemLastEvaluatedTime = a})

-- | The unique identifier for the application that the metric applies to.
jemApplicationId :: Lens' JourneyExecutionMetricsResponse Text
jemApplicationId = lens _jemApplicationId (\s a -> s {_jemApplicationId = a})

instance FromJSON JourneyExecutionMetricsResponse where
  parseJSON =
    withObject
      "JourneyExecutionMetricsResponse"
      ( \x ->
          JourneyExecutionMetricsResponse'
            <$> (x .:? "Metrics" .!= mempty)
            <*> (x .: "JourneyId")
            <*> (x .: "LastEvaluatedTime")
            <*> (x .: "ApplicationId")
      )

instance Hashable JourneyExecutionMetricsResponse

instance NFData JourneyExecutionMetricsResponse
