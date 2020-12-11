-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignDateRangeKpiResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignDateRangeKpiResponse
  ( CampaignDateRangeKpiResponse (..),

    -- * Smart constructor
    mkCampaignDateRangeKpiResponse,

    -- * Lenses
    cdrkNextToken,
    cdrkKpiResult,
    cdrkKpiName,
    cdrkEndTime,
    cdrkCampaignId,
    cdrkStartTime,
    cdrkApplicationId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.BaseKpiResult
import qualified Network.AWS.Prelude as Lude

-- | Provides the results of a query that retrieved the data for a standard metric that applies to a campaign, and provides information about that query.
--
-- /See:/ 'mkCampaignDateRangeKpiResponse' smart constructor.
data CampaignDateRangeKpiResponse = CampaignDateRangeKpiResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    kpiResult :: BaseKpiResult,
    kpiName :: Lude.Text,
    endTime :: Lude.Timestamp,
    campaignId :: Lude.Text,
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

-- | Creates a value of 'CampaignDateRangeKpiResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the metric applies to.
-- * 'campaignId' - The unique identifier for the campaign that the metric applies to.
-- * 'endTime' - The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
-- * 'kpiName' - The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
-- * 'kpiResult' - An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
-- * 'nextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Campaign Metrics resource because the resource returns all results in a single page.
-- * 'startTime' - The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
mkCampaignDateRangeKpiResponse ::
  -- | 'kpiResult'
  BaseKpiResult ->
  -- | 'kpiName'
  Lude.Text ->
  -- | 'endTime'
  Lude.Timestamp ->
  -- | 'campaignId'
  Lude.Text ->
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'applicationId'
  Lude.Text ->
  CampaignDateRangeKpiResponse
mkCampaignDateRangeKpiResponse
  pKpiResult_
  pKpiName_
  pEndTime_
  pCampaignId_
  pStartTime_
  pApplicationId_ =
    CampaignDateRangeKpiResponse'
      { nextToken = Lude.Nothing,
        kpiResult = pKpiResult_,
        kpiName = pKpiName_,
        endTime = pEndTime_,
        campaignId = pCampaignId_,
        startTime = pStartTime_,
        applicationId = pApplicationId_
      }

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Campaign Metrics resource because the resource returns all results in a single page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrkNextToken :: Lens.Lens' CampaignDateRangeKpiResponse (Lude.Maybe Lude.Text)
cdrkNextToken = Lens.lens (nextToken :: CampaignDateRangeKpiResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: CampaignDateRangeKpiResponse)
{-# DEPRECATED cdrkNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
--
-- /Note:/ Consider using 'kpiResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrkKpiResult :: Lens.Lens' CampaignDateRangeKpiResponse BaseKpiResult
cdrkKpiResult = Lens.lens (kpiResult :: CampaignDateRangeKpiResponse -> BaseKpiResult) (\s a -> s {kpiResult = a} :: CampaignDateRangeKpiResponse)
{-# DEPRECATED cdrkKpiResult "Use generic-lens or generic-optics with 'kpiResult' instead." #-}

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'kpiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrkKpiName :: Lens.Lens' CampaignDateRangeKpiResponse Lude.Text
cdrkKpiName = Lens.lens (kpiName :: CampaignDateRangeKpiResponse -> Lude.Text) (\s a -> s {kpiName = a} :: CampaignDateRangeKpiResponse)
{-# DEPRECATED cdrkKpiName "Use generic-lens or generic-optics with 'kpiName' instead." #-}

-- | The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrkEndTime :: Lens.Lens' CampaignDateRangeKpiResponse Lude.Timestamp
cdrkEndTime = Lens.lens (endTime :: CampaignDateRangeKpiResponse -> Lude.Timestamp) (\s a -> s {endTime = a} :: CampaignDateRangeKpiResponse)
{-# DEPRECATED cdrkEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The unique identifier for the campaign that the metric applies to.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrkCampaignId :: Lens.Lens' CampaignDateRangeKpiResponse Lude.Text
cdrkCampaignId = Lens.lens (campaignId :: CampaignDateRangeKpiResponse -> Lude.Text) (\s a -> s {campaignId = a} :: CampaignDateRangeKpiResponse)
{-# DEPRECATED cdrkCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

-- | The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrkStartTime :: Lens.Lens' CampaignDateRangeKpiResponse Lude.Timestamp
cdrkStartTime = Lens.lens (startTime :: CampaignDateRangeKpiResponse -> Lude.Timestamp) (\s a -> s {startTime = a} :: CampaignDateRangeKpiResponse)
{-# DEPRECATED cdrkStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The unique identifier for the application that the metric applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrkApplicationId :: Lens.Lens' CampaignDateRangeKpiResponse Lude.Text
cdrkApplicationId = Lens.lens (applicationId :: CampaignDateRangeKpiResponse -> Lude.Text) (\s a -> s {applicationId = a} :: CampaignDateRangeKpiResponse)
{-# DEPRECATED cdrkApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.FromJSON CampaignDateRangeKpiResponse where
  parseJSON =
    Lude.withObject
      "CampaignDateRangeKpiResponse"
      ( \x ->
          CampaignDateRangeKpiResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..: "KpiResult")
            Lude.<*> (x Lude..: "KpiName")
            Lude.<*> (x Lude..: "EndTime")
            Lude.<*> (x Lude..: "CampaignId")
            Lude.<*> (x Lude..: "StartTime")
            Lude.<*> (x Lude..: "ApplicationId")
      )
