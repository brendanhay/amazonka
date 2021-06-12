{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetUsageStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty usage statistics over the last 30 days for the
-- specified detector ID. For newly enabled detectors or data sources the
-- cost returned will include only the usage so far under 30 days, this may
-- differ from the cost metrics in the console, which projects usage over
-- 30 days to provide a monthly cost estimate. For more information see
-- <https://docs.aws.amazon.com/guardduty/latest/ug/monitoring_costs.html#usage-calculations Understanding How Usage Costs are Calculated>.
module Network.AWS.GuardDuty.GetUsageStatistics
  ( -- * Creating a Request
    GetUsageStatistics (..),
    newGetUsageStatistics,

    -- * Request Lenses
    getUsageStatistics_nextToken,
    getUsageStatistics_unit,
    getUsageStatistics_maxResults,
    getUsageStatistics_detectorId,
    getUsageStatistics_usageStatisticType,
    getUsageStatistics_usageCriteria,

    -- * Destructuring the Response
    GetUsageStatisticsResponse (..),
    newGetUsageStatisticsResponse,

    -- * Response Lenses
    getUsageStatisticsResponse_nextToken,
    getUsageStatisticsResponse_usageStatistics,
    getUsageStatisticsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetUsageStatistics' smart constructor.
data GetUsageStatistics = GetUsageStatistics'
  { -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the NextToken value returned from the
    -- previous request to continue listing results after the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The currency unit you would like to view your usage statistics in.
    -- Current valid values are USD.
    unit :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the detector that specifies the GuardDuty service whose usage
    -- statistics you want to retrieve.
    detectorId :: Core.Text,
    -- | The type of usage statistics to retrieve.
    usageStatisticType :: UsageStatisticType,
    -- | Represents the criteria used for querying usage.
    usageCriteria :: UsageCriteria
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUsageStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getUsageStatistics_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the NextToken value returned from the
-- previous request to continue listing results after the first page.
--
-- 'unit', 'getUsageStatistics_unit' - The currency unit you would like to view your usage statistics in.
-- Current valid values are USD.
--
-- 'maxResults', 'getUsageStatistics_maxResults' - The maximum number of results to return in the response.
--
-- 'detectorId', 'getUsageStatistics_detectorId' - The ID of the detector that specifies the GuardDuty service whose usage
-- statistics you want to retrieve.
--
-- 'usageStatisticType', 'getUsageStatistics_usageStatisticType' - The type of usage statistics to retrieve.
--
-- 'usageCriteria', 'getUsageStatistics_usageCriteria' - Represents the criteria used for querying usage.
newGetUsageStatistics ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'usageStatisticType'
  UsageStatisticType ->
  -- | 'usageCriteria'
  UsageCriteria ->
  GetUsageStatistics
newGetUsageStatistics
  pDetectorId_
  pUsageStatisticType_
  pUsageCriteria_ =
    GetUsageStatistics'
      { nextToken = Core.Nothing,
        unit = Core.Nothing,
        maxResults = Core.Nothing,
        detectorId = pDetectorId_,
        usageStatisticType = pUsageStatisticType_,
        usageCriteria = pUsageCriteria_
      }

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the NextToken value returned from the
-- previous request to continue listing results after the first page.
getUsageStatistics_nextToken :: Lens.Lens' GetUsageStatistics (Core.Maybe Core.Text)
getUsageStatistics_nextToken = Lens.lens (\GetUsageStatistics' {nextToken} -> nextToken) (\s@GetUsageStatistics' {} a -> s {nextToken = a} :: GetUsageStatistics)

-- | The currency unit you would like to view your usage statistics in.
-- Current valid values are USD.
getUsageStatistics_unit :: Lens.Lens' GetUsageStatistics (Core.Maybe Core.Text)
getUsageStatistics_unit = Lens.lens (\GetUsageStatistics' {unit} -> unit) (\s@GetUsageStatistics' {} a -> s {unit = a} :: GetUsageStatistics)

-- | The maximum number of results to return in the response.
getUsageStatistics_maxResults :: Lens.Lens' GetUsageStatistics (Core.Maybe Core.Natural)
getUsageStatistics_maxResults = Lens.lens (\GetUsageStatistics' {maxResults} -> maxResults) (\s@GetUsageStatistics' {} a -> s {maxResults = a} :: GetUsageStatistics)

-- | The ID of the detector that specifies the GuardDuty service whose usage
-- statistics you want to retrieve.
getUsageStatistics_detectorId :: Lens.Lens' GetUsageStatistics Core.Text
getUsageStatistics_detectorId = Lens.lens (\GetUsageStatistics' {detectorId} -> detectorId) (\s@GetUsageStatistics' {} a -> s {detectorId = a} :: GetUsageStatistics)

-- | The type of usage statistics to retrieve.
getUsageStatistics_usageStatisticType :: Lens.Lens' GetUsageStatistics UsageStatisticType
getUsageStatistics_usageStatisticType = Lens.lens (\GetUsageStatistics' {usageStatisticType} -> usageStatisticType) (\s@GetUsageStatistics' {} a -> s {usageStatisticType = a} :: GetUsageStatistics)

-- | Represents the criteria used for querying usage.
getUsageStatistics_usageCriteria :: Lens.Lens' GetUsageStatistics UsageCriteria
getUsageStatistics_usageCriteria = Lens.lens (\GetUsageStatistics' {usageCriteria} -> usageCriteria) (\s@GetUsageStatistics' {} a -> s {usageCriteria = a} :: GetUsageStatistics)

instance Core.AWSRequest GetUsageStatistics where
  type
    AWSResponse GetUsageStatistics =
      GetUsageStatisticsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsageStatisticsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "usageStatistics")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetUsageStatistics

instance Core.NFData GetUsageStatistics

instance Core.ToHeaders GetUsageStatistics where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetUsageStatistics where
  toJSON GetUsageStatistics' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("unit" Core..=) Core.<$> unit,
            ("maxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("usageStatisticsType" Core..= usageStatisticType),
            Core.Just ("usageCriteria" Core..= usageCriteria)
          ]
      )

instance Core.ToPath GetUsageStatistics where
  toPath GetUsageStatistics' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/usage/statistics"
      ]

instance Core.ToQuery GetUsageStatistics where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetUsageStatisticsResponse' smart constructor.
data GetUsageStatisticsResponse = GetUsageStatisticsResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Core.Maybe Core.Text,
    -- | The usage statistics object. If a UsageStatisticType was provided, the
    -- objects representing other types will be null.
    usageStatistics :: Core.Maybe UsageStatistics,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUsageStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getUsageStatisticsResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'usageStatistics', 'getUsageStatisticsResponse_usageStatistics' - The usage statistics object. If a UsageStatisticType was provided, the
-- objects representing other types will be null.
--
-- 'httpStatus', 'getUsageStatisticsResponse_httpStatus' - The response's http status code.
newGetUsageStatisticsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetUsageStatisticsResponse
newGetUsageStatisticsResponse pHttpStatus_ =
  GetUsageStatisticsResponse'
    { nextToken =
        Core.Nothing,
      usageStatistics = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
getUsageStatisticsResponse_nextToken :: Lens.Lens' GetUsageStatisticsResponse (Core.Maybe Core.Text)
getUsageStatisticsResponse_nextToken = Lens.lens (\GetUsageStatisticsResponse' {nextToken} -> nextToken) (\s@GetUsageStatisticsResponse' {} a -> s {nextToken = a} :: GetUsageStatisticsResponse)

-- | The usage statistics object. If a UsageStatisticType was provided, the
-- objects representing other types will be null.
getUsageStatisticsResponse_usageStatistics :: Lens.Lens' GetUsageStatisticsResponse (Core.Maybe UsageStatistics)
getUsageStatisticsResponse_usageStatistics = Lens.lens (\GetUsageStatisticsResponse' {usageStatistics} -> usageStatistics) (\s@GetUsageStatisticsResponse' {} a -> s {usageStatistics = a} :: GetUsageStatisticsResponse)

-- | The response's http status code.
getUsageStatisticsResponse_httpStatus :: Lens.Lens' GetUsageStatisticsResponse Core.Int
getUsageStatisticsResponse_httpStatus = Lens.lens (\GetUsageStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetUsageStatisticsResponse' {} a -> s {httpStatus = a} :: GetUsageStatisticsResponse)

instance Core.NFData GetUsageStatisticsResponse
