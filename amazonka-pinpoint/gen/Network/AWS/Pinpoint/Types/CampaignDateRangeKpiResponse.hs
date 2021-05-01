{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignDateRangeKpiResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignDateRangeKpiResponse where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.BaseKpiResult
import qualified Network.AWS.Prelude as Prelude

-- | Provides the results of a query that retrieved the data for a standard
-- metric that applies to a campaign, and provides information about that
-- query.
--
-- /See:/ 'newCampaignDateRangeKpiResponse' smart constructor.
data CampaignDateRangeKpiResponse = CampaignDateRangeKpiResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null for the Campaign
    -- Metrics resource because the resource returns all results in a single
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contains the results of the query. Each object
    -- contains the value for the metric and metadata about that value.
    kpiResult :: BaseKpiResult,
    -- | The name of the metric, also referred to as a /key performance indicator
    -- (KPI)/, that the data was retrieved for. This value describes the
    -- associated metric and consists of two or more terms, which are comprised
    -- of lowercase alphanumeric characters, separated by a hyphen. For a list
    -- of possible values, see the
    -- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
    kpiName :: Prelude.Text,
    -- | The last date and time of the date range that was used to filter the
    -- query results, in extended ISO 8601 format. The date range is inclusive.
    endTime :: Prelude.POSIX,
    -- | The unique identifier for the campaign that the metric applies to.
    campaignId :: Prelude.Text,
    -- | The first date and time of the date range that was used to filter the
    -- query results, in extended ISO 8601 format. The date range is inclusive.
    startTime :: Prelude.POSIX,
    -- | The unique identifier for the application that the metric applies to.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CampaignDateRangeKpiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'campaignDateRangeKpiResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null for the Campaign
-- Metrics resource because the resource returns all results in a single
-- page.
--
-- 'kpiResult', 'campaignDateRangeKpiResponse_kpiResult' - An array of objects that contains the results of the query. Each object
-- contains the value for the metric and metadata about that value.
--
-- 'kpiName', 'campaignDateRangeKpiResponse_kpiName' - The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, that the data was retrieved for. This value describes the
-- associated metric and consists of two or more terms, which are comprised
-- of lowercase alphanumeric characters, separated by a hyphen. For a list
-- of possible values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
--
-- 'endTime', 'campaignDateRangeKpiResponse_endTime' - The last date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
--
-- 'campaignId', 'campaignDateRangeKpiResponse_campaignId' - The unique identifier for the campaign that the metric applies to.
--
-- 'startTime', 'campaignDateRangeKpiResponse_startTime' - The first date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
--
-- 'applicationId', 'campaignDateRangeKpiResponse_applicationId' - The unique identifier for the application that the metric applies to.
newCampaignDateRangeKpiResponse ::
  -- | 'kpiResult'
  BaseKpiResult ->
  -- | 'kpiName'
  Prelude.Text ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'campaignId'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'applicationId'
  Prelude.Text ->
  CampaignDateRangeKpiResponse
newCampaignDateRangeKpiResponse
  pKpiResult_
  pKpiName_
  pEndTime_
  pCampaignId_
  pStartTime_
  pApplicationId_ =
    CampaignDateRangeKpiResponse'
      { nextToken =
          Prelude.Nothing,
        kpiResult = pKpiResult_,
        kpiName = pKpiName_,
        endTime = Prelude._Time Lens.# pEndTime_,
        campaignId = pCampaignId_,
        startTime = Prelude._Time Lens.# pStartTime_,
        applicationId = pApplicationId_
      }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null for the Campaign
-- Metrics resource because the resource returns all results in a single
-- page.
campaignDateRangeKpiResponse_nextToken :: Lens.Lens' CampaignDateRangeKpiResponse (Prelude.Maybe Prelude.Text)
campaignDateRangeKpiResponse_nextToken = Lens.lens (\CampaignDateRangeKpiResponse' {nextToken} -> nextToken) (\s@CampaignDateRangeKpiResponse' {} a -> s {nextToken = a} :: CampaignDateRangeKpiResponse)

-- | An array of objects that contains the results of the query. Each object
-- contains the value for the metric and metadata about that value.
campaignDateRangeKpiResponse_kpiResult :: Lens.Lens' CampaignDateRangeKpiResponse BaseKpiResult
campaignDateRangeKpiResponse_kpiResult = Lens.lens (\CampaignDateRangeKpiResponse' {kpiResult} -> kpiResult) (\s@CampaignDateRangeKpiResponse' {} a -> s {kpiResult = a} :: CampaignDateRangeKpiResponse)

-- | The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, that the data was retrieved for. This value describes the
-- associated metric and consists of two or more terms, which are comprised
-- of lowercase alphanumeric characters, separated by a hyphen. For a list
-- of possible values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
campaignDateRangeKpiResponse_kpiName :: Lens.Lens' CampaignDateRangeKpiResponse Prelude.Text
campaignDateRangeKpiResponse_kpiName = Lens.lens (\CampaignDateRangeKpiResponse' {kpiName} -> kpiName) (\s@CampaignDateRangeKpiResponse' {} a -> s {kpiName = a} :: CampaignDateRangeKpiResponse)

-- | The last date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
campaignDateRangeKpiResponse_endTime :: Lens.Lens' CampaignDateRangeKpiResponse Prelude.UTCTime
campaignDateRangeKpiResponse_endTime = Lens.lens (\CampaignDateRangeKpiResponse' {endTime} -> endTime) (\s@CampaignDateRangeKpiResponse' {} a -> s {endTime = a} :: CampaignDateRangeKpiResponse) Prelude.. Prelude._Time

-- | The unique identifier for the campaign that the metric applies to.
campaignDateRangeKpiResponse_campaignId :: Lens.Lens' CampaignDateRangeKpiResponse Prelude.Text
campaignDateRangeKpiResponse_campaignId = Lens.lens (\CampaignDateRangeKpiResponse' {campaignId} -> campaignId) (\s@CampaignDateRangeKpiResponse' {} a -> s {campaignId = a} :: CampaignDateRangeKpiResponse)

-- | The first date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
campaignDateRangeKpiResponse_startTime :: Lens.Lens' CampaignDateRangeKpiResponse Prelude.UTCTime
campaignDateRangeKpiResponse_startTime = Lens.lens (\CampaignDateRangeKpiResponse' {startTime} -> startTime) (\s@CampaignDateRangeKpiResponse' {} a -> s {startTime = a} :: CampaignDateRangeKpiResponse) Prelude.. Prelude._Time

-- | The unique identifier for the application that the metric applies to.
campaignDateRangeKpiResponse_applicationId :: Lens.Lens' CampaignDateRangeKpiResponse Prelude.Text
campaignDateRangeKpiResponse_applicationId = Lens.lens (\CampaignDateRangeKpiResponse' {applicationId} -> applicationId) (\s@CampaignDateRangeKpiResponse' {} a -> s {applicationId = a} :: CampaignDateRangeKpiResponse)

instance
  Prelude.FromJSON
    CampaignDateRangeKpiResponse
  where
  parseJSON =
    Prelude.withObject
      "CampaignDateRangeKpiResponse"
      ( \x ->
          CampaignDateRangeKpiResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> (x Prelude..: "KpiResult")
            Prelude.<*> (x Prelude..: "KpiName")
            Prelude.<*> (x Prelude..: "EndTime")
            Prelude.<*> (x Prelude..: "CampaignId")
            Prelude.<*> (x Prelude..: "StartTime")
            Prelude.<*> (x Prelude..: "ApplicationId")
      )

instance
  Prelude.Hashable
    CampaignDateRangeKpiResponse

instance Prelude.NFData CampaignDateRangeKpiResponse
