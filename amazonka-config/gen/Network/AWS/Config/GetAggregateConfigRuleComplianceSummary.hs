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
-- Module      : Network.AWS.Config.GetAggregateConfigRuleComplianceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of compliant and noncompliant rules for one or more
-- accounts and regions in an aggregator.
--
-- The results can return an empty result page, but if you have a
-- nextToken, the results are displayed on the next page.
module Network.AWS.Config.GetAggregateConfigRuleComplianceSummary
  ( -- * Creating a Request
    GetAggregateConfigRuleComplianceSummary (..),
    newGetAggregateConfigRuleComplianceSummary,

    -- * Request Lenses
    getAggregateConfigRuleComplianceSummary_nextToken,
    getAggregateConfigRuleComplianceSummary_filters,
    getAggregateConfigRuleComplianceSummary_groupByKey,
    getAggregateConfigRuleComplianceSummary_limit,
    getAggregateConfigRuleComplianceSummary_configurationAggregatorName,

    -- * Destructuring the Response
    GetAggregateConfigRuleComplianceSummaryResponse (..),
    newGetAggregateConfigRuleComplianceSummaryResponse,

    -- * Response Lenses
    getAggregateConfigRuleComplianceSummaryResponse_nextToken,
    getAggregateConfigRuleComplianceSummaryResponse_aggregateComplianceCounts,
    getAggregateConfigRuleComplianceSummaryResponse_groupByKey,
    getAggregateConfigRuleComplianceSummaryResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAggregateConfigRuleComplianceSummary' smart constructor.
data GetAggregateConfigRuleComplianceSummary = GetAggregateConfigRuleComplianceSummary'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Filters the results based on the ConfigRuleComplianceSummaryFilters
    -- object.
    filters :: Core.Maybe ConfigRuleComplianceSummaryFilters,
    -- | Groups the result based on ACCOUNT_ID or AWS_REGION.
    groupByKey :: Core.Maybe ConfigRuleComplianceSummaryGroupKey,
    -- | The maximum number of evaluation results returned on each page. The
    -- default is 1000. You cannot specify a number greater than 1000. If you
    -- specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAggregateConfigRuleComplianceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAggregateConfigRuleComplianceSummary_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'filters', 'getAggregateConfigRuleComplianceSummary_filters' - Filters the results based on the ConfigRuleComplianceSummaryFilters
-- object.
--
-- 'groupByKey', 'getAggregateConfigRuleComplianceSummary_groupByKey' - Groups the result based on ACCOUNT_ID or AWS_REGION.
--
-- 'limit', 'getAggregateConfigRuleComplianceSummary_limit' - The maximum number of evaluation results returned on each page. The
-- default is 1000. You cannot specify a number greater than 1000. If you
-- specify 0, AWS Config uses the default.
--
-- 'configurationAggregatorName', 'getAggregateConfigRuleComplianceSummary_configurationAggregatorName' - The name of the configuration aggregator.
newGetAggregateConfigRuleComplianceSummary ::
  -- | 'configurationAggregatorName'
  Core.Text ->
  GetAggregateConfigRuleComplianceSummary
newGetAggregateConfigRuleComplianceSummary
  pConfigurationAggregatorName_ =
    GetAggregateConfigRuleComplianceSummary'
      { nextToken =
          Core.Nothing,
        filters = Core.Nothing,
        groupByKey = Core.Nothing,
        limit = Core.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateConfigRuleComplianceSummary_nextToken :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Core.Maybe Core.Text)
getAggregateConfigRuleComplianceSummary_nextToken = Lens.lens (\GetAggregateConfigRuleComplianceSummary' {nextToken} -> nextToken) (\s@GetAggregateConfigRuleComplianceSummary' {} a -> s {nextToken = a} :: GetAggregateConfigRuleComplianceSummary)

-- | Filters the results based on the ConfigRuleComplianceSummaryFilters
-- object.
getAggregateConfigRuleComplianceSummary_filters :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Core.Maybe ConfigRuleComplianceSummaryFilters)
getAggregateConfigRuleComplianceSummary_filters = Lens.lens (\GetAggregateConfigRuleComplianceSummary' {filters} -> filters) (\s@GetAggregateConfigRuleComplianceSummary' {} a -> s {filters = a} :: GetAggregateConfigRuleComplianceSummary)

-- | Groups the result based on ACCOUNT_ID or AWS_REGION.
getAggregateConfigRuleComplianceSummary_groupByKey :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Core.Maybe ConfigRuleComplianceSummaryGroupKey)
getAggregateConfigRuleComplianceSummary_groupByKey = Lens.lens (\GetAggregateConfigRuleComplianceSummary' {groupByKey} -> groupByKey) (\s@GetAggregateConfigRuleComplianceSummary' {} a -> s {groupByKey = a} :: GetAggregateConfigRuleComplianceSummary)

-- | The maximum number of evaluation results returned on each page. The
-- default is 1000. You cannot specify a number greater than 1000. If you
-- specify 0, AWS Config uses the default.
getAggregateConfigRuleComplianceSummary_limit :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Core.Maybe Core.Natural)
getAggregateConfigRuleComplianceSummary_limit = Lens.lens (\GetAggregateConfigRuleComplianceSummary' {limit} -> limit) (\s@GetAggregateConfigRuleComplianceSummary' {} a -> s {limit = a} :: GetAggregateConfigRuleComplianceSummary)

-- | The name of the configuration aggregator.
getAggregateConfigRuleComplianceSummary_configurationAggregatorName :: Lens.Lens' GetAggregateConfigRuleComplianceSummary Core.Text
getAggregateConfigRuleComplianceSummary_configurationAggregatorName = Lens.lens (\GetAggregateConfigRuleComplianceSummary' {configurationAggregatorName} -> configurationAggregatorName) (\s@GetAggregateConfigRuleComplianceSummary' {} a -> s {configurationAggregatorName = a} :: GetAggregateConfigRuleComplianceSummary)

instance
  Core.AWSRequest
    GetAggregateConfigRuleComplianceSummary
  where
  type
    AWSResponse
      GetAggregateConfigRuleComplianceSummary =
      GetAggregateConfigRuleComplianceSummaryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAggregateConfigRuleComplianceSummaryResponse'
            Core.<$> (x Core..?> "NextToken")
              Core.<*> ( x Core..?> "AggregateComplianceCounts"
                           Core..!@ Core.mempty
                       )
              Core.<*> (x Core..?> "GroupByKey")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetAggregateConfigRuleComplianceSummary

instance
  Core.NFData
    GetAggregateConfigRuleComplianceSummary

instance
  Core.ToHeaders
    GetAggregateConfigRuleComplianceSummary
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetAggregateConfigRuleComplianceSummary" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetAggregateConfigRuleComplianceSummary
  where
  toJSON GetAggregateConfigRuleComplianceSummary' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Filters" Core..=) Core.<$> filters,
            ("GroupByKey" Core..=) Core.<$> groupByKey,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              )
          ]
      )

instance
  Core.ToPath
    GetAggregateConfigRuleComplianceSummary
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetAggregateConfigRuleComplianceSummary
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAggregateConfigRuleComplianceSummaryResponse' smart constructor.
data GetAggregateConfigRuleComplianceSummaryResponse = GetAggregateConfigRuleComplianceSummaryResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a list of AggregateComplianceCounts object.
    aggregateComplianceCounts :: Core.Maybe [AggregateComplianceCount],
    -- | Groups the result based on ACCOUNT_ID or AWS_REGION.
    groupByKey :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAggregateConfigRuleComplianceSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAggregateConfigRuleComplianceSummaryResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'aggregateComplianceCounts', 'getAggregateConfigRuleComplianceSummaryResponse_aggregateComplianceCounts' - Returns a list of AggregateComplianceCounts object.
--
-- 'groupByKey', 'getAggregateConfigRuleComplianceSummaryResponse_groupByKey' - Groups the result based on ACCOUNT_ID or AWS_REGION.
--
-- 'httpStatus', 'getAggregateConfigRuleComplianceSummaryResponse_httpStatus' - The response's http status code.
newGetAggregateConfigRuleComplianceSummaryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAggregateConfigRuleComplianceSummaryResponse
newGetAggregateConfigRuleComplianceSummaryResponse
  pHttpStatus_ =
    GetAggregateConfigRuleComplianceSummaryResponse'
      { nextToken =
          Core.Nothing,
        aggregateComplianceCounts =
          Core.Nothing,
        groupByKey = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateConfigRuleComplianceSummaryResponse_nextToken :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Core.Maybe Core.Text)
getAggregateConfigRuleComplianceSummaryResponse_nextToken = Lens.lens (\GetAggregateConfigRuleComplianceSummaryResponse' {nextToken} -> nextToken) (\s@GetAggregateConfigRuleComplianceSummaryResponse' {} a -> s {nextToken = a} :: GetAggregateConfigRuleComplianceSummaryResponse)

-- | Returns a list of AggregateComplianceCounts object.
getAggregateConfigRuleComplianceSummaryResponse_aggregateComplianceCounts :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Core.Maybe [AggregateComplianceCount])
getAggregateConfigRuleComplianceSummaryResponse_aggregateComplianceCounts = Lens.lens (\GetAggregateConfigRuleComplianceSummaryResponse' {aggregateComplianceCounts} -> aggregateComplianceCounts) (\s@GetAggregateConfigRuleComplianceSummaryResponse' {} a -> s {aggregateComplianceCounts = a} :: GetAggregateConfigRuleComplianceSummaryResponse) Core.. Lens.mapping Lens._Coerce

-- | Groups the result based on ACCOUNT_ID or AWS_REGION.
getAggregateConfigRuleComplianceSummaryResponse_groupByKey :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Core.Maybe Core.Text)
getAggregateConfigRuleComplianceSummaryResponse_groupByKey = Lens.lens (\GetAggregateConfigRuleComplianceSummaryResponse' {groupByKey} -> groupByKey) (\s@GetAggregateConfigRuleComplianceSummaryResponse' {} a -> s {groupByKey = a} :: GetAggregateConfigRuleComplianceSummaryResponse)

-- | The response's http status code.
getAggregateConfigRuleComplianceSummaryResponse_httpStatus :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse Core.Int
getAggregateConfigRuleComplianceSummaryResponse_httpStatus = Lens.lens (\GetAggregateConfigRuleComplianceSummaryResponse' {httpStatus} -> httpStatus) (\s@GetAggregateConfigRuleComplianceSummaryResponse' {} a -> s {httpStatus = a} :: GetAggregateConfigRuleComplianceSummaryResponse)

instance
  Core.NFData
    GetAggregateConfigRuleComplianceSummaryResponse
