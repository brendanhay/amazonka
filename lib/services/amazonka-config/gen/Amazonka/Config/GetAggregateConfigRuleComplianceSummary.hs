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
-- Module      : Amazonka.Config.GetAggregateConfigRuleComplianceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of compliant and noncompliant rules for one or more
-- accounts and regions in an aggregator.
--
-- The results can return an empty result page, but if you have a
-- nextToken, the results are displayed on the next page.
module Amazonka.Config.GetAggregateConfigRuleComplianceSummary
  ( -- * Creating a Request
    GetAggregateConfigRuleComplianceSummary (..),
    newGetAggregateConfigRuleComplianceSummary,

    -- * Request Lenses
    getAggregateConfigRuleComplianceSummary_filters,
    getAggregateConfigRuleComplianceSummary_groupByKey,
    getAggregateConfigRuleComplianceSummary_limit,
    getAggregateConfigRuleComplianceSummary_nextToken,
    getAggregateConfigRuleComplianceSummary_configurationAggregatorName,

    -- * Destructuring the Response
    GetAggregateConfigRuleComplianceSummaryResponse (..),
    newGetAggregateConfigRuleComplianceSummaryResponse,

    -- * Response Lenses
    getAggregateConfigRuleComplianceSummaryResponse_aggregateComplianceCounts,
    getAggregateConfigRuleComplianceSummaryResponse_groupByKey,
    getAggregateConfigRuleComplianceSummaryResponse_nextToken,
    getAggregateConfigRuleComplianceSummaryResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAggregateConfigRuleComplianceSummary' smart constructor.
data GetAggregateConfigRuleComplianceSummary = GetAggregateConfigRuleComplianceSummary'
  { -- | Filters the results based on the ConfigRuleComplianceSummaryFilters
    -- object.
    filters :: Prelude.Maybe ConfigRuleComplianceSummaryFilters,
    -- | Groups the result based on ACCOUNT_ID or AWS_REGION.
    groupByKey :: Prelude.Maybe ConfigRuleComplianceSummaryGroupKey,
    -- | The maximum number of evaluation results returned on each page. The
    -- default is 1000. You cannot specify a number greater than 1000. If you
    -- specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAggregateConfigRuleComplianceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'getAggregateConfigRuleComplianceSummary_filters' - Filters the results based on the ConfigRuleComplianceSummaryFilters
-- object.
--
-- 'groupByKey', 'getAggregateConfigRuleComplianceSummary_groupByKey' - Groups the result based on ACCOUNT_ID or AWS_REGION.
--
-- 'limit', 'getAggregateConfigRuleComplianceSummary_limit' - The maximum number of evaluation results returned on each page. The
-- default is 1000. You cannot specify a number greater than 1000. If you
-- specify 0, Config uses the default.
--
-- 'nextToken', 'getAggregateConfigRuleComplianceSummary_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'configurationAggregatorName', 'getAggregateConfigRuleComplianceSummary_configurationAggregatorName' - The name of the configuration aggregator.
newGetAggregateConfigRuleComplianceSummary ::
  -- | 'configurationAggregatorName'
  Prelude.Text ->
  GetAggregateConfigRuleComplianceSummary
newGetAggregateConfigRuleComplianceSummary
  pConfigurationAggregatorName_ =
    GetAggregateConfigRuleComplianceSummary'
      { filters =
          Prelude.Nothing,
        groupByKey = Prelude.Nothing,
        limit = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | Filters the results based on the ConfigRuleComplianceSummaryFilters
-- object.
getAggregateConfigRuleComplianceSummary_filters :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Prelude.Maybe ConfigRuleComplianceSummaryFilters)
getAggregateConfigRuleComplianceSummary_filters = Lens.lens (\GetAggregateConfigRuleComplianceSummary' {filters} -> filters) (\s@GetAggregateConfigRuleComplianceSummary' {} a -> s {filters = a} :: GetAggregateConfigRuleComplianceSummary)

-- | Groups the result based on ACCOUNT_ID or AWS_REGION.
getAggregateConfigRuleComplianceSummary_groupByKey :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Prelude.Maybe ConfigRuleComplianceSummaryGroupKey)
getAggregateConfigRuleComplianceSummary_groupByKey = Lens.lens (\GetAggregateConfigRuleComplianceSummary' {groupByKey} -> groupByKey) (\s@GetAggregateConfigRuleComplianceSummary' {} a -> s {groupByKey = a} :: GetAggregateConfigRuleComplianceSummary)

-- | The maximum number of evaluation results returned on each page. The
-- default is 1000. You cannot specify a number greater than 1000. If you
-- specify 0, Config uses the default.
getAggregateConfigRuleComplianceSummary_limit :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Prelude.Maybe Prelude.Natural)
getAggregateConfigRuleComplianceSummary_limit = Lens.lens (\GetAggregateConfigRuleComplianceSummary' {limit} -> limit) (\s@GetAggregateConfigRuleComplianceSummary' {} a -> s {limit = a} :: GetAggregateConfigRuleComplianceSummary)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateConfigRuleComplianceSummary_nextToken :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Prelude.Maybe Prelude.Text)
getAggregateConfigRuleComplianceSummary_nextToken = Lens.lens (\GetAggregateConfigRuleComplianceSummary' {nextToken} -> nextToken) (\s@GetAggregateConfigRuleComplianceSummary' {} a -> s {nextToken = a} :: GetAggregateConfigRuleComplianceSummary)

-- | The name of the configuration aggregator.
getAggregateConfigRuleComplianceSummary_configurationAggregatorName :: Lens.Lens' GetAggregateConfigRuleComplianceSummary Prelude.Text
getAggregateConfigRuleComplianceSummary_configurationAggregatorName = Lens.lens (\GetAggregateConfigRuleComplianceSummary' {configurationAggregatorName} -> configurationAggregatorName) (\s@GetAggregateConfigRuleComplianceSummary' {} a -> s {configurationAggregatorName = a} :: GetAggregateConfigRuleComplianceSummary)

instance
  Core.AWSRequest
    GetAggregateConfigRuleComplianceSummary
  where
  type
    AWSResponse
      GetAggregateConfigRuleComplianceSummary =
      GetAggregateConfigRuleComplianceSummaryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAggregateConfigRuleComplianceSummaryResponse'
            Prelude.<$> ( x
                            Data..?> "AggregateComplianceCounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "GroupByKey")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAggregateConfigRuleComplianceSummary
  where
  hashWithSalt
    _salt
    GetAggregateConfigRuleComplianceSummary' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` groupByKey
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` configurationAggregatorName

instance
  Prelude.NFData
    GetAggregateConfigRuleComplianceSummary
  where
  rnf GetAggregateConfigRuleComplianceSummary' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf groupByKey
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf configurationAggregatorName

instance
  Data.ToHeaders
    GetAggregateConfigRuleComplianceSummary
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.GetAggregateConfigRuleComplianceSummary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetAggregateConfigRuleComplianceSummary
  where
  toJSON GetAggregateConfigRuleComplianceSummary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("GroupByKey" Data..=) Prelude.<$> groupByKey,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "ConfigurationAggregatorName"
                  Data..= configurationAggregatorName
              )
          ]
      )

instance
  Data.ToPath
    GetAggregateConfigRuleComplianceSummary
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetAggregateConfigRuleComplianceSummary
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAggregateConfigRuleComplianceSummaryResponse' smart constructor.
data GetAggregateConfigRuleComplianceSummaryResponse = GetAggregateConfigRuleComplianceSummaryResponse'
  { -- | Returns a list of AggregateComplianceCounts object.
    aggregateComplianceCounts :: Prelude.Maybe [AggregateComplianceCount],
    -- | Groups the result based on ACCOUNT_ID or AWS_REGION.
    groupByKey :: Prelude.Maybe Prelude.Text,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAggregateConfigRuleComplianceSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregateComplianceCounts', 'getAggregateConfigRuleComplianceSummaryResponse_aggregateComplianceCounts' - Returns a list of AggregateComplianceCounts object.
--
-- 'groupByKey', 'getAggregateConfigRuleComplianceSummaryResponse_groupByKey' - Groups the result based on ACCOUNT_ID or AWS_REGION.
--
-- 'nextToken', 'getAggregateConfigRuleComplianceSummaryResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'httpStatus', 'getAggregateConfigRuleComplianceSummaryResponse_httpStatus' - The response's http status code.
newGetAggregateConfigRuleComplianceSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAggregateConfigRuleComplianceSummaryResponse
newGetAggregateConfigRuleComplianceSummaryResponse
  pHttpStatus_ =
    GetAggregateConfigRuleComplianceSummaryResponse'
      { aggregateComplianceCounts =
          Prelude.Nothing,
        groupByKey =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a list of AggregateComplianceCounts object.
getAggregateConfigRuleComplianceSummaryResponse_aggregateComplianceCounts :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Prelude.Maybe [AggregateComplianceCount])
getAggregateConfigRuleComplianceSummaryResponse_aggregateComplianceCounts = Lens.lens (\GetAggregateConfigRuleComplianceSummaryResponse' {aggregateComplianceCounts} -> aggregateComplianceCounts) (\s@GetAggregateConfigRuleComplianceSummaryResponse' {} a -> s {aggregateComplianceCounts = a} :: GetAggregateConfigRuleComplianceSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | Groups the result based on ACCOUNT_ID or AWS_REGION.
getAggregateConfigRuleComplianceSummaryResponse_groupByKey :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Prelude.Maybe Prelude.Text)
getAggregateConfigRuleComplianceSummaryResponse_groupByKey = Lens.lens (\GetAggregateConfigRuleComplianceSummaryResponse' {groupByKey} -> groupByKey) (\s@GetAggregateConfigRuleComplianceSummaryResponse' {} a -> s {groupByKey = a} :: GetAggregateConfigRuleComplianceSummaryResponse)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateConfigRuleComplianceSummaryResponse_nextToken :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Prelude.Maybe Prelude.Text)
getAggregateConfigRuleComplianceSummaryResponse_nextToken = Lens.lens (\GetAggregateConfigRuleComplianceSummaryResponse' {nextToken} -> nextToken) (\s@GetAggregateConfigRuleComplianceSummaryResponse' {} a -> s {nextToken = a} :: GetAggregateConfigRuleComplianceSummaryResponse)

-- | The response's http status code.
getAggregateConfigRuleComplianceSummaryResponse_httpStatus :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse Prelude.Int
getAggregateConfigRuleComplianceSummaryResponse_httpStatus = Lens.lens (\GetAggregateConfigRuleComplianceSummaryResponse' {httpStatus} -> httpStatus) (\s@GetAggregateConfigRuleComplianceSummaryResponse' {} a -> s {httpStatus = a} :: GetAggregateConfigRuleComplianceSummaryResponse)

instance
  Prelude.NFData
    GetAggregateConfigRuleComplianceSummaryResponse
  where
  rnf
    GetAggregateConfigRuleComplianceSummaryResponse' {..} =
      Prelude.rnf aggregateComplianceCounts
        `Prelude.seq` Prelude.rnf groupByKey
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
