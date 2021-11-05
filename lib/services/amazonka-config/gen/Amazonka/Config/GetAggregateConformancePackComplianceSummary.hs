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
-- Module      : Amazonka.Config.GetAggregateConformancePackComplianceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the count of compliant and noncompliant conformance packs across
-- all Amazon Web Services accounts and Amazon Web Services Regions in an
-- aggregator. You can filter based on Amazon Web Services account ID or
-- Amazon Web Services Region.
--
-- The results can return an empty result page, but if you have a
-- nextToken, the results are displayed on the next page.
module Amazonka.Config.GetAggregateConformancePackComplianceSummary
  ( -- * Creating a Request
    GetAggregateConformancePackComplianceSummary (..),
    newGetAggregateConformancePackComplianceSummary,

    -- * Request Lenses
    getAggregateConformancePackComplianceSummary_filters,
    getAggregateConformancePackComplianceSummary_nextToken,
    getAggregateConformancePackComplianceSummary_limit,
    getAggregateConformancePackComplianceSummary_groupByKey,
    getAggregateConformancePackComplianceSummary_configurationAggregatorName,

    -- * Destructuring the Response
    GetAggregateConformancePackComplianceSummaryResponse (..),
    newGetAggregateConformancePackComplianceSummaryResponse,

    -- * Response Lenses
    getAggregateConformancePackComplianceSummaryResponse_nextToken,
    getAggregateConformancePackComplianceSummaryResponse_aggregateConformancePackComplianceSummaries,
    getAggregateConformancePackComplianceSummaryResponse_groupByKey,
    getAggregateConformancePackComplianceSummaryResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAggregateConformancePackComplianceSummary' smart constructor.
data GetAggregateConformancePackComplianceSummary = GetAggregateConformancePackComplianceSummary'
  { -- | Filters the results based on the
    -- @AggregateConformancePackComplianceSummaryFilters@ object.
    filters :: Prelude.Maybe AggregateConformancePackComplianceSummaryFilters,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned on each page. The default is
    -- maximum. If you specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Groups the result based on Amazon Web Services account ID or Amazon Web
    -- Services Region.
    groupByKey :: Prelude.Maybe AggregateConformancePackComplianceSummaryGroupKey,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAggregateConformancePackComplianceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'getAggregateConformancePackComplianceSummary_filters' - Filters the results based on the
-- @AggregateConformancePackComplianceSummaryFilters@ object.
--
-- 'nextToken', 'getAggregateConformancePackComplianceSummary_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'limit', 'getAggregateConformancePackComplianceSummary_limit' - The maximum number of results returned on each page. The default is
-- maximum. If you specify 0, Config uses the default.
--
-- 'groupByKey', 'getAggregateConformancePackComplianceSummary_groupByKey' - Groups the result based on Amazon Web Services account ID or Amazon Web
-- Services Region.
--
-- 'configurationAggregatorName', 'getAggregateConformancePackComplianceSummary_configurationAggregatorName' - The name of the configuration aggregator.
newGetAggregateConformancePackComplianceSummary ::
  -- | 'configurationAggregatorName'
  Prelude.Text ->
  GetAggregateConformancePackComplianceSummary
newGetAggregateConformancePackComplianceSummary
  pConfigurationAggregatorName_ =
    GetAggregateConformancePackComplianceSummary'
      { filters =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        limit = Prelude.Nothing,
        groupByKey = Prelude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | Filters the results based on the
-- @AggregateConformancePackComplianceSummaryFilters@ object.
getAggregateConformancePackComplianceSummary_filters :: Lens.Lens' GetAggregateConformancePackComplianceSummary (Prelude.Maybe AggregateConformancePackComplianceSummaryFilters)
getAggregateConformancePackComplianceSummary_filters = Lens.lens (\GetAggregateConformancePackComplianceSummary' {filters} -> filters) (\s@GetAggregateConformancePackComplianceSummary' {} a -> s {filters = a} :: GetAggregateConformancePackComplianceSummary)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateConformancePackComplianceSummary_nextToken :: Lens.Lens' GetAggregateConformancePackComplianceSummary (Prelude.Maybe Prelude.Text)
getAggregateConformancePackComplianceSummary_nextToken = Lens.lens (\GetAggregateConformancePackComplianceSummary' {nextToken} -> nextToken) (\s@GetAggregateConformancePackComplianceSummary' {} a -> s {nextToken = a} :: GetAggregateConformancePackComplianceSummary)

-- | The maximum number of results returned on each page. The default is
-- maximum. If you specify 0, Config uses the default.
getAggregateConformancePackComplianceSummary_limit :: Lens.Lens' GetAggregateConformancePackComplianceSummary (Prelude.Maybe Prelude.Natural)
getAggregateConformancePackComplianceSummary_limit = Lens.lens (\GetAggregateConformancePackComplianceSummary' {limit} -> limit) (\s@GetAggregateConformancePackComplianceSummary' {} a -> s {limit = a} :: GetAggregateConformancePackComplianceSummary)

-- | Groups the result based on Amazon Web Services account ID or Amazon Web
-- Services Region.
getAggregateConformancePackComplianceSummary_groupByKey :: Lens.Lens' GetAggregateConformancePackComplianceSummary (Prelude.Maybe AggregateConformancePackComplianceSummaryGroupKey)
getAggregateConformancePackComplianceSummary_groupByKey = Lens.lens (\GetAggregateConformancePackComplianceSummary' {groupByKey} -> groupByKey) (\s@GetAggregateConformancePackComplianceSummary' {} a -> s {groupByKey = a} :: GetAggregateConformancePackComplianceSummary)

-- | The name of the configuration aggregator.
getAggregateConformancePackComplianceSummary_configurationAggregatorName :: Lens.Lens' GetAggregateConformancePackComplianceSummary Prelude.Text
getAggregateConformancePackComplianceSummary_configurationAggregatorName = Lens.lens (\GetAggregateConformancePackComplianceSummary' {configurationAggregatorName} -> configurationAggregatorName) (\s@GetAggregateConformancePackComplianceSummary' {} a -> s {configurationAggregatorName = a} :: GetAggregateConformancePackComplianceSummary)

instance
  Core.AWSRequest
    GetAggregateConformancePackComplianceSummary
  where
  type
    AWSResponse
      GetAggregateConformancePackComplianceSummary =
      GetAggregateConformancePackComplianceSummaryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAggregateConformancePackComplianceSummaryResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x
                              Core..?> "AggregateConformancePackComplianceSummaries"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (x Core..?> "GroupByKey")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAggregateConformancePackComplianceSummary

instance
  Prelude.NFData
    GetAggregateConformancePackComplianceSummary

instance
  Core.ToHeaders
    GetAggregateConformancePackComplianceSummary
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetAggregateConformancePackComplianceSummary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    GetAggregateConformancePackComplianceSummary
  where
  toJSON
    GetAggregateConformancePackComplianceSummary' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Filters" Core..=) Prelude.<$> filters,
              ("NextToken" Core..=) Prelude.<$> nextToken,
              ("Limit" Core..=) Prelude.<$> limit,
              ("GroupByKey" Core..=) Prelude.<$> groupByKey,
              Prelude.Just
                ( "ConfigurationAggregatorName"
                    Core..= configurationAggregatorName
                )
            ]
        )

instance
  Core.ToPath
    GetAggregateConformancePackComplianceSummary
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetAggregateConformancePackComplianceSummary
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAggregateConformancePackComplianceSummaryResponse' smart constructor.
data GetAggregateConformancePackComplianceSummaryResponse = GetAggregateConformancePackComplianceSummaryResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of @AggregateConformancePackComplianceSummary@ object.
    aggregateConformancePackComplianceSummaries :: Prelude.Maybe [AggregateConformancePackComplianceSummary],
    -- | Groups the result based on Amazon Web Services account ID or Amazon Web
    -- Services Region.
    groupByKey :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAggregateConformancePackComplianceSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAggregateConformancePackComplianceSummaryResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'aggregateConformancePackComplianceSummaries', 'getAggregateConformancePackComplianceSummaryResponse_aggregateConformancePackComplianceSummaries' - Returns a list of @AggregateConformancePackComplianceSummary@ object.
--
-- 'groupByKey', 'getAggregateConformancePackComplianceSummaryResponse_groupByKey' - Groups the result based on Amazon Web Services account ID or Amazon Web
-- Services Region.
--
-- 'httpStatus', 'getAggregateConformancePackComplianceSummaryResponse_httpStatus' - The response's http status code.
newGetAggregateConformancePackComplianceSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAggregateConformancePackComplianceSummaryResponse
newGetAggregateConformancePackComplianceSummaryResponse
  pHttpStatus_ =
    GetAggregateConformancePackComplianceSummaryResponse'
      { nextToken =
          Prelude.Nothing,
        aggregateConformancePackComplianceSummaries =
          Prelude.Nothing,
        groupByKey =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateConformancePackComplianceSummaryResponse_nextToken :: Lens.Lens' GetAggregateConformancePackComplianceSummaryResponse (Prelude.Maybe Prelude.Text)
getAggregateConformancePackComplianceSummaryResponse_nextToken = Lens.lens (\GetAggregateConformancePackComplianceSummaryResponse' {nextToken} -> nextToken) (\s@GetAggregateConformancePackComplianceSummaryResponse' {} a -> s {nextToken = a} :: GetAggregateConformancePackComplianceSummaryResponse)

-- | Returns a list of @AggregateConformancePackComplianceSummary@ object.
getAggregateConformancePackComplianceSummaryResponse_aggregateConformancePackComplianceSummaries :: Lens.Lens' GetAggregateConformancePackComplianceSummaryResponse (Prelude.Maybe [AggregateConformancePackComplianceSummary])
getAggregateConformancePackComplianceSummaryResponse_aggregateConformancePackComplianceSummaries = Lens.lens (\GetAggregateConformancePackComplianceSummaryResponse' {aggregateConformancePackComplianceSummaries} -> aggregateConformancePackComplianceSummaries) (\s@GetAggregateConformancePackComplianceSummaryResponse' {} a -> s {aggregateConformancePackComplianceSummaries = a} :: GetAggregateConformancePackComplianceSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | Groups the result based on Amazon Web Services account ID or Amazon Web
-- Services Region.
getAggregateConformancePackComplianceSummaryResponse_groupByKey :: Lens.Lens' GetAggregateConformancePackComplianceSummaryResponse (Prelude.Maybe Prelude.Text)
getAggregateConformancePackComplianceSummaryResponse_groupByKey = Lens.lens (\GetAggregateConformancePackComplianceSummaryResponse' {groupByKey} -> groupByKey) (\s@GetAggregateConformancePackComplianceSummaryResponse' {} a -> s {groupByKey = a} :: GetAggregateConformancePackComplianceSummaryResponse)

-- | The response's http status code.
getAggregateConformancePackComplianceSummaryResponse_httpStatus :: Lens.Lens' GetAggregateConformancePackComplianceSummaryResponse Prelude.Int
getAggregateConformancePackComplianceSummaryResponse_httpStatus = Lens.lens (\GetAggregateConformancePackComplianceSummaryResponse' {httpStatus} -> httpStatus) (\s@GetAggregateConformancePackComplianceSummaryResponse' {} a -> s {httpStatus = a} :: GetAggregateConformancePackComplianceSummaryResponse)

instance
  Prelude.NFData
    GetAggregateConformancePackComplianceSummaryResponse
