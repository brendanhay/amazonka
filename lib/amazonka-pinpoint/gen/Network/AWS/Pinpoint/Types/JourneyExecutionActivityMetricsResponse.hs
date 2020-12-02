{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyExecutionActivityMetricsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyExecutionActivityMetricsResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the results of a query that retrieved the data for a standard execution metric that applies to a journey activity, and provides information about that query.
--
--
--
-- /See:/ 'journeyExecutionActivityMetricsResponse' smart constructor.
data JourneyExecutionActivityMetricsResponse = JourneyExecutionActivityMetricsResponse'
  { _jeamMetrics ::
      !( Map
           Text
           (Text)
       ),
    _jeamJourneyId ::
      !Text,
    _jeamLastEvaluatedTime ::
      !Text,
    _jeamJourneyActivityId ::
      !Text,
    _jeamActivityType ::
      !Text,
    _jeamApplicationId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneyExecutionActivityMetricsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jeamMetrics' - A JSON object that contains the results of the query. The results vary depending on the type of activity (ActivityType). For information about the structure and contents of the results, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- * 'jeamJourneyId' - The unique identifier for the journey that the metric applies to.
--
-- * 'jeamLastEvaluatedTime' - The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the execution status of the activity and updated the data for the metric.
--
-- * 'jeamJourneyActivityId' - The unique identifier for the activity that the metric applies to.
--
-- * 'jeamActivityType' - The type of activity that the metric applies to. Possible values are:     * CONDITIONAL_SPLIT - For a yes/no split activity, which is an activity that sends participants down one of two paths in a journey.     * HOLDOUT - For a holdout activity, which is an activity that stops a journey for a specified percentage of participants.     * MESSAGE - For an email activity, which is an activity that sends an email message to participants.     * MULTI_CONDITIONAL_SPLIT - For a multivariate split activity, which is an activity that sends participants down one of as many as five paths in a journey.     * RANDOM_SPLIT - For a random split activity, which is an activity that sends specified percentages of participants down one of as many as five paths in a journey.     * WAIT - For a wait activity, which is an activity that waits for a certain amount of time or until a specific date and time before moving participants to the next activity in a journey.
--
-- * 'jeamApplicationId' - The unique identifier for the application that the metric applies to.
journeyExecutionActivityMetricsResponse ::
  -- | 'jeamJourneyId'
  Text ->
  -- | 'jeamLastEvaluatedTime'
  Text ->
  -- | 'jeamJourneyActivityId'
  Text ->
  -- | 'jeamActivityType'
  Text ->
  -- | 'jeamApplicationId'
  Text ->
  JourneyExecutionActivityMetricsResponse
journeyExecutionActivityMetricsResponse
  pJourneyId_
  pLastEvaluatedTime_
  pJourneyActivityId_
  pActivityType_
  pApplicationId_ =
    JourneyExecutionActivityMetricsResponse'
      { _jeamMetrics = mempty,
        _jeamJourneyId = pJourneyId_,
        _jeamLastEvaluatedTime = pLastEvaluatedTime_,
        _jeamJourneyActivityId = pJourneyActivityId_,
        _jeamActivityType = pActivityType_,
        _jeamApplicationId = pApplicationId_
      }

-- | A JSON object that contains the results of the query. The results vary depending on the type of activity (ActivityType). For information about the structure and contents of the results, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
jeamMetrics :: Lens' JourneyExecutionActivityMetricsResponse (HashMap Text (Text))
jeamMetrics = lens _jeamMetrics (\s a -> s {_jeamMetrics = a}) . _Map

-- | The unique identifier for the journey that the metric applies to.
jeamJourneyId :: Lens' JourneyExecutionActivityMetricsResponse Text
jeamJourneyId = lens _jeamJourneyId (\s a -> s {_jeamJourneyId = a})

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the execution status of the activity and updated the data for the metric.
jeamLastEvaluatedTime :: Lens' JourneyExecutionActivityMetricsResponse Text
jeamLastEvaluatedTime = lens _jeamLastEvaluatedTime (\s a -> s {_jeamLastEvaluatedTime = a})

-- | The unique identifier for the activity that the metric applies to.
jeamJourneyActivityId :: Lens' JourneyExecutionActivityMetricsResponse Text
jeamJourneyActivityId = lens _jeamJourneyActivityId (\s a -> s {_jeamJourneyActivityId = a})

-- | The type of activity that the metric applies to. Possible values are:     * CONDITIONAL_SPLIT - For a yes/no split activity, which is an activity that sends participants down one of two paths in a journey.     * HOLDOUT - For a holdout activity, which is an activity that stops a journey for a specified percentage of participants.     * MESSAGE - For an email activity, which is an activity that sends an email message to participants.     * MULTI_CONDITIONAL_SPLIT - For a multivariate split activity, which is an activity that sends participants down one of as many as five paths in a journey.     * RANDOM_SPLIT - For a random split activity, which is an activity that sends specified percentages of participants down one of as many as five paths in a journey.     * WAIT - For a wait activity, which is an activity that waits for a certain amount of time or until a specific date and time before moving participants to the next activity in a journey.
jeamActivityType :: Lens' JourneyExecutionActivityMetricsResponse Text
jeamActivityType = lens _jeamActivityType (\s a -> s {_jeamActivityType = a})

-- | The unique identifier for the application that the metric applies to.
jeamApplicationId :: Lens' JourneyExecutionActivityMetricsResponse Text
jeamApplicationId = lens _jeamApplicationId (\s a -> s {_jeamApplicationId = a})

instance FromJSON JourneyExecutionActivityMetricsResponse where
  parseJSON =
    withObject
      "JourneyExecutionActivityMetricsResponse"
      ( \x ->
          JourneyExecutionActivityMetricsResponse'
            <$> (x .:? "Metrics" .!= mempty)
            <*> (x .: "JourneyId")
            <*> (x .: "LastEvaluatedTime")
            <*> (x .: "JourneyActivityId")
            <*> (x .: "ActivityType")
            <*> (x .: "ApplicationId")
      )

instance Hashable JourneyExecutionActivityMetricsResponse

instance NFData JourneyExecutionActivityMetricsResponse
