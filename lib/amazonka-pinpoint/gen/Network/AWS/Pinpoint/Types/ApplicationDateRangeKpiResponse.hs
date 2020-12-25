{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ApplicationDateRangeKpiResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ApplicationDateRangeKpiResponse
  ( ApplicationDateRangeKpiResponse (..),

    -- * Smart constructor
    mkApplicationDateRangeKpiResponse,

    -- * Lenses
    adrkrKpiResult,
    adrkrKpiName,
    adrkrEndTime,
    adrkrStartTime,
    adrkrApplicationId,
    adrkrNextToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.BaseKpiResult as Types
import qualified Network.AWS.Prelude as Core

-- | Provides the results of a query that retrieved the data for a standard metric that applies to an application, and provides information about that query.
--
-- /See:/ 'mkApplicationDateRangeKpiResponse' smart constructor.
data ApplicationDateRangeKpiResponse = ApplicationDateRangeKpiResponse'
  { -- | An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
    kpiResult :: Types.BaseKpiResult,
    -- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
    kpiName :: Core.Text,
    -- | The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
    endTime :: Core.UTCTime,
    -- | The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
    startTime :: Core.UTCTime,
    -- | The unique identifier for the application that the metric applies to.
    applicationId :: Core.Text,
    -- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Application Metrics resource because the resource returns all results in a single page.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ApplicationDateRangeKpiResponse' value with any optional fields omitted.
mkApplicationDateRangeKpiResponse ::
  -- | 'kpiResult'
  Types.BaseKpiResult ->
  -- | 'kpiName'
  Core.Text ->
  -- | 'endTime'
  Core.UTCTime ->
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'applicationId'
  Core.Text ->
  ApplicationDateRangeKpiResponse
mkApplicationDateRangeKpiResponse
  kpiResult
  kpiName
  endTime
  startTime
  applicationId =
    ApplicationDateRangeKpiResponse'
      { kpiResult,
        kpiName,
        endTime,
        startTime,
        applicationId,
        nextToken = Core.Nothing
      }

-- | An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
--
-- /Note:/ Consider using 'kpiResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrkrKpiResult :: Lens.Lens' ApplicationDateRangeKpiResponse Types.BaseKpiResult
adrkrKpiResult = Lens.field @"kpiResult"
{-# DEPRECATED adrkrKpiResult "Use generic-lens or generic-optics with 'kpiResult' instead." #-}

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'kpiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrkrKpiName :: Lens.Lens' ApplicationDateRangeKpiResponse Core.Text
adrkrKpiName = Lens.field @"kpiName"
{-# DEPRECATED adrkrKpiName "Use generic-lens or generic-optics with 'kpiName' instead." #-}

-- | The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrkrEndTime :: Lens.Lens' ApplicationDateRangeKpiResponse Core.UTCTime
adrkrEndTime = Lens.field @"endTime"
{-# DEPRECATED adrkrEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrkrStartTime :: Lens.Lens' ApplicationDateRangeKpiResponse Core.UTCTime
adrkrStartTime = Lens.field @"startTime"
{-# DEPRECATED adrkrStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The unique identifier for the application that the metric applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrkrApplicationId :: Lens.Lens' ApplicationDateRangeKpiResponse Core.Text
adrkrApplicationId = Lens.field @"applicationId"
{-# DEPRECATED adrkrApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Application Metrics resource because the resource returns all results in a single page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrkrNextToken :: Lens.Lens' ApplicationDateRangeKpiResponse (Core.Maybe Core.Text)
adrkrNextToken = Lens.field @"nextToken"
{-# DEPRECATED adrkrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ApplicationDateRangeKpiResponse where
  parseJSON =
    Core.withObject "ApplicationDateRangeKpiResponse" Core.$
      \x ->
        ApplicationDateRangeKpiResponse'
          Core.<$> (x Core..: "KpiResult")
          Core.<*> (x Core..: "KpiName")
          Core.<*> (x Core..: "EndTime")
          Core.<*> (x Core..: "StartTime")
          Core.<*> (x Core..: "ApplicationId")
          Core.<*> (x Core..:? "NextToken")
