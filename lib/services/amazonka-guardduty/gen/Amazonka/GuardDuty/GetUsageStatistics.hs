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
-- Module      : Amazonka.GuardDuty.GetUsageStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty usage statistics over the last 30 days for the
-- specified detector ID. For newly enabled detectors or data sources, the
-- cost returned will include only the usage so far under 30 days. This may
-- differ from the cost metrics in the console, which project usage over 30
-- days to provide a monthly cost estimate. For more information, see
-- <https://docs.aws.amazon.com/guardduty/latest/ug/monitoring_costs.html#usage-calculations Understanding How Usage Costs are Calculated>.
module Amazonka.GuardDuty.GetUsageStatistics
  ( -- * Creating a Request
    GetUsageStatistics (..),
    newGetUsageStatistics,

    -- * Request Lenses
    getUsageStatistics_maxResults,
    getUsageStatistics_nextToken,
    getUsageStatistics_unit,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUsageStatistics' smart constructor.
data GetUsageStatistics = GetUsageStatistics'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the NextToken value returned from the
    -- previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The currency unit you would like to view your usage statistics in.
    -- Current valid values are USD.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The ID of the detector that specifies the GuardDuty service whose usage
    -- statistics you want to retrieve.
    detectorId :: Prelude.Text,
    -- | The type of usage statistics to retrieve.
    usageStatisticType :: UsageStatisticType,
    -- | Represents the criteria used for querying usage.
    usageCriteria :: UsageCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsageStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getUsageStatistics_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'getUsageStatistics_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the NextToken value returned from the
-- previous request to continue listing results after the first page.
--
-- 'unit', 'getUsageStatistics_unit' - The currency unit you would like to view your usage statistics in.
-- Current valid values are USD.
--
-- 'detectorId', 'getUsageStatistics_detectorId' - The ID of the detector that specifies the GuardDuty service whose usage
-- statistics you want to retrieve.
--
-- 'usageStatisticType', 'getUsageStatistics_usageStatisticType' - The type of usage statistics to retrieve.
--
-- 'usageCriteria', 'getUsageStatistics_usageCriteria' - Represents the criteria used for querying usage.
newGetUsageStatistics ::
  -- | 'detectorId'
  Prelude.Text ->
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
      { maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        unit = Prelude.Nothing,
        detectorId = pDetectorId_,
        usageStatisticType = pUsageStatisticType_,
        usageCriteria = pUsageCriteria_
      }

-- | The maximum number of results to return in the response.
getUsageStatistics_maxResults :: Lens.Lens' GetUsageStatistics (Prelude.Maybe Prelude.Natural)
getUsageStatistics_maxResults = Lens.lens (\GetUsageStatistics' {maxResults} -> maxResults) (\s@GetUsageStatistics' {} a -> s {maxResults = a} :: GetUsageStatistics)

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the NextToken value returned from the
-- previous request to continue listing results after the first page.
getUsageStatistics_nextToken :: Lens.Lens' GetUsageStatistics (Prelude.Maybe Prelude.Text)
getUsageStatistics_nextToken = Lens.lens (\GetUsageStatistics' {nextToken} -> nextToken) (\s@GetUsageStatistics' {} a -> s {nextToken = a} :: GetUsageStatistics)

-- | The currency unit you would like to view your usage statistics in.
-- Current valid values are USD.
getUsageStatistics_unit :: Lens.Lens' GetUsageStatistics (Prelude.Maybe Prelude.Text)
getUsageStatistics_unit = Lens.lens (\GetUsageStatistics' {unit} -> unit) (\s@GetUsageStatistics' {} a -> s {unit = a} :: GetUsageStatistics)

-- | The ID of the detector that specifies the GuardDuty service whose usage
-- statistics you want to retrieve.
getUsageStatistics_detectorId :: Lens.Lens' GetUsageStatistics Prelude.Text
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsageStatisticsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "usageStatistics")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUsageStatistics where
  hashWithSalt _salt GetUsageStatistics' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` usageStatisticType
      `Prelude.hashWithSalt` usageCriteria

instance Prelude.NFData GetUsageStatistics where
  rnf GetUsageStatistics' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf usageStatisticType
      `Prelude.seq` Prelude.rnf usageCriteria

instance Data.ToHeaders GetUsageStatistics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetUsageStatistics where
  toJSON GetUsageStatistics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("unit" Data..=) Prelude.<$> unit,
            Prelude.Just
              ("usageStatisticsType" Data..= usageStatisticType),
            Prelude.Just
              ("usageCriteria" Data..= usageCriteria)
          ]
      )

instance Data.ToPath GetUsageStatistics where
  toPath GetUsageStatistics' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/usage/statistics"
      ]

instance Data.ToQuery GetUsageStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUsageStatisticsResponse' smart constructor.
data GetUsageStatisticsResponse = GetUsageStatisticsResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The usage statistics object. If a UsageStatisticType was provided, the
    -- objects representing other types will be null.
    usageStatistics :: Prelude.Maybe UsageStatistics,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetUsageStatisticsResponse
newGetUsageStatisticsResponse pHttpStatus_ =
  GetUsageStatisticsResponse'
    { nextToken =
        Prelude.Nothing,
      usageStatistics = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
getUsageStatisticsResponse_nextToken :: Lens.Lens' GetUsageStatisticsResponse (Prelude.Maybe Prelude.Text)
getUsageStatisticsResponse_nextToken = Lens.lens (\GetUsageStatisticsResponse' {nextToken} -> nextToken) (\s@GetUsageStatisticsResponse' {} a -> s {nextToken = a} :: GetUsageStatisticsResponse)

-- | The usage statistics object. If a UsageStatisticType was provided, the
-- objects representing other types will be null.
getUsageStatisticsResponse_usageStatistics :: Lens.Lens' GetUsageStatisticsResponse (Prelude.Maybe UsageStatistics)
getUsageStatisticsResponse_usageStatistics = Lens.lens (\GetUsageStatisticsResponse' {usageStatistics} -> usageStatistics) (\s@GetUsageStatisticsResponse' {} a -> s {usageStatistics = a} :: GetUsageStatisticsResponse)

-- | The response's http status code.
getUsageStatisticsResponse_httpStatus :: Lens.Lens' GetUsageStatisticsResponse Prelude.Int
getUsageStatisticsResponse_httpStatus = Lens.lens (\GetUsageStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetUsageStatisticsResponse' {} a -> s {httpStatus = a} :: GetUsageStatisticsResponse)

instance Prelude.NFData GetUsageStatisticsResponse where
  rnf GetUsageStatisticsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf usageStatistics
      `Prelude.seq` Prelude.rnf httpStatus
