{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyDateRangeKpiResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyDateRangeKpiResponse
  ( JourneyDateRangeKpiResponse (..),

    -- * Smart constructor
    mkJourneyDateRangeKpiResponse,

    -- * Lenses
    jdrkNextToken,
    jdrkKpiResult,
    jdrkKpiName,
    jdrkJourneyId,
    jdrkEndTime,
    jdrkStartTime,
    jdrkApplicationId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.BaseKpiResult
import qualified Network.AWS.Prelude as Lude

-- | Provides the results of a query that retrieved the data for a standard engagement metric that applies to a journey, and provides information about that query.
--
-- /See:/ 'mkJourneyDateRangeKpiResponse' smart constructor.
data JourneyDateRangeKpiResponse = JourneyDateRangeKpiResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    kpiResult :: BaseKpiResult,
    kpiName :: Lude.Text,
    journeyId :: Lude.Text,
    endTime :: Lude.Timestamp,
    startTime :: Lude.Timestamp,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneyDateRangeKpiResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the metric applies to.
-- * 'endTime' - The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
-- * 'journeyId' - The unique identifier for the journey that the metric applies to.
-- * 'kpiName' - The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
-- * 'kpiResult' - An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
-- * 'nextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Journey Engagement Metrics resource because the resource returns all results in a single page.
-- * 'startTime' - The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
mkJourneyDateRangeKpiResponse ::
  -- | 'kpiResult'
  BaseKpiResult ->
  -- | 'kpiName'
  Lude.Text ->
  -- | 'journeyId'
  Lude.Text ->
  -- | 'endTime'
  Lude.Timestamp ->
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'applicationId'
  Lude.Text ->
  JourneyDateRangeKpiResponse
mkJourneyDateRangeKpiResponse
  pKpiResult_
  pKpiName_
  pJourneyId_
  pEndTime_
  pStartTime_
  pApplicationId_ =
    JourneyDateRangeKpiResponse'
      { nextToken = Lude.Nothing,
        kpiResult = pKpiResult_,
        kpiName = pKpiName_,
        journeyId = pJourneyId_,
        endTime = pEndTime_,
        startTime = pStartTime_,
        applicationId = pApplicationId_
      }

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Journey Engagement Metrics resource because the resource returns all results in a single page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrkNextToken :: Lens.Lens' JourneyDateRangeKpiResponse (Lude.Maybe Lude.Text)
jdrkNextToken = Lens.lens (nextToken :: JourneyDateRangeKpiResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: JourneyDateRangeKpiResponse)
{-# DEPRECATED jdrkNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
--
-- /Note:/ Consider using 'kpiResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrkKpiResult :: Lens.Lens' JourneyDateRangeKpiResponse BaseKpiResult
jdrkKpiResult = Lens.lens (kpiResult :: JourneyDateRangeKpiResponse -> BaseKpiResult) (\s a -> s {kpiResult = a} :: JourneyDateRangeKpiResponse)
{-# DEPRECATED jdrkKpiResult "Use generic-lens or generic-optics with 'kpiResult' instead." #-}

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'kpiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrkKpiName :: Lens.Lens' JourneyDateRangeKpiResponse Lude.Text
jdrkKpiName = Lens.lens (kpiName :: JourneyDateRangeKpiResponse -> Lude.Text) (\s a -> s {kpiName = a} :: JourneyDateRangeKpiResponse)
{-# DEPRECATED jdrkKpiName "Use generic-lens or generic-optics with 'kpiName' instead." #-}

-- | The unique identifier for the journey that the metric applies to.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrkJourneyId :: Lens.Lens' JourneyDateRangeKpiResponse Lude.Text
jdrkJourneyId = Lens.lens (journeyId :: JourneyDateRangeKpiResponse -> Lude.Text) (\s a -> s {journeyId = a} :: JourneyDateRangeKpiResponse)
{-# DEPRECATED jdrkJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

-- | The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrkEndTime :: Lens.Lens' JourneyDateRangeKpiResponse Lude.Timestamp
jdrkEndTime = Lens.lens (endTime :: JourneyDateRangeKpiResponse -> Lude.Timestamp) (\s a -> s {endTime = a} :: JourneyDateRangeKpiResponse)
{-# DEPRECATED jdrkEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrkStartTime :: Lens.Lens' JourneyDateRangeKpiResponse Lude.Timestamp
jdrkStartTime = Lens.lens (startTime :: JourneyDateRangeKpiResponse -> Lude.Timestamp) (\s a -> s {startTime = a} :: JourneyDateRangeKpiResponse)
{-# DEPRECATED jdrkStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The unique identifier for the application that the metric applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdrkApplicationId :: Lens.Lens' JourneyDateRangeKpiResponse Lude.Text
jdrkApplicationId = Lens.lens (applicationId :: JourneyDateRangeKpiResponse -> Lude.Text) (\s a -> s {applicationId = a} :: JourneyDateRangeKpiResponse)
{-# DEPRECATED jdrkApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.FromJSON JourneyDateRangeKpiResponse where
  parseJSON =
    Lude.withObject
      "JourneyDateRangeKpiResponse"
      ( \x ->
          JourneyDateRangeKpiResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..: "KpiResult")
            Lude.<*> (x Lude..: "KpiName")
            Lude.<*> (x Lude..: "JourneyId")
            Lude.<*> (x Lude..: "EndTime")
            Lude.<*> (x Lude..: "StartTime")
            Lude.<*> (x Lude..: "ApplicationId")
      )
