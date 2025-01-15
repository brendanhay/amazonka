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
-- Module      : Amazonka.Config.DescribeAggregateComplianceByConfigRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of compliant and noncompliant rules with the number of
-- resources for compliant and noncompliant rules. Does not display rules
-- that do not have compliance results.
--
-- The results can return an empty result page, but if you have a
-- @nextToken@, the results are displayed on the next page.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeAggregateComplianceByConfigRules
  ( -- * Creating a Request
    DescribeAggregateComplianceByConfigRules (..),
    newDescribeAggregateComplianceByConfigRules,

    -- * Request Lenses
    describeAggregateComplianceByConfigRules_filters,
    describeAggregateComplianceByConfigRules_limit,
    describeAggregateComplianceByConfigRules_nextToken,
    describeAggregateComplianceByConfigRules_configurationAggregatorName,

    -- * Destructuring the Response
    DescribeAggregateComplianceByConfigRulesResponse (..),
    newDescribeAggregateComplianceByConfigRulesResponse,

    -- * Response Lenses
    describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules,
    describeAggregateComplianceByConfigRulesResponse_nextToken,
    describeAggregateComplianceByConfigRulesResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAggregateComplianceByConfigRules' smart constructor.
data DescribeAggregateComplianceByConfigRules = DescribeAggregateComplianceByConfigRules'
  { -- | Filters the results by ConfigRuleComplianceFilters object.
    filters :: Prelude.Maybe ConfigRuleComplianceFilters,
    -- | The maximum number of evaluation results returned on each page. The
    -- default is maximum. If you specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAggregateComplianceByConfigRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeAggregateComplianceByConfigRules_filters' - Filters the results by ConfigRuleComplianceFilters object.
--
-- 'limit', 'describeAggregateComplianceByConfigRules_limit' - The maximum number of evaluation results returned on each page. The
-- default is maximum. If you specify 0, Config uses the default.
--
-- 'nextToken', 'describeAggregateComplianceByConfigRules_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'configurationAggregatorName', 'describeAggregateComplianceByConfigRules_configurationAggregatorName' - The name of the configuration aggregator.
newDescribeAggregateComplianceByConfigRules ::
  -- | 'configurationAggregatorName'
  Prelude.Text ->
  DescribeAggregateComplianceByConfigRules
newDescribeAggregateComplianceByConfigRules
  pConfigurationAggregatorName_ =
    DescribeAggregateComplianceByConfigRules'
      { filters =
          Prelude.Nothing,
        limit = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | Filters the results by ConfigRuleComplianceFilters object.
describeAggregateComplianceByConfigRules_filters :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Prelude.Maybe ConfigRuleComplianceFilters)
describeAggregateComplianceByConfigRules_filters = Lens.lens (\DescribeAggregateComplianceByConfigRules' {filters} -> filters) (\s@DescribeAggregateComplianceByConfigRules' {} a -> s {filters = a} :: DescribeAggregateComplianceByConfigRules)

-- | The maximum number of evaluation results returned on each page. The
-- default is maximum. If you specify 0, Config uses the default.
describeAggregateComplianceByConfigRules_limit :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Prelude.Maybe Prelude.Natural)
describeAggregateComplianceByConfigRules_limit = Lens.lens (\DescribeAggregateComplianceByConfigRules' {limit} -> limit) (\s@DescribeAggregateComplianceByConfigRules' {} a -> s {limit = a} :: DescribeAggregateComplianceByConfigRules)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregateComplianceByConfigRules_nextToken :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Prelude.Maybe Prelude.Text)
describeAggregateComplianceByConfigRules_nextToken = Lens.lens (\DescribeAggregateComplianceByConfigRules' {nextToken} -> nextToken) (\s@DescribeAggregateComplianceByConfigRules' {} a -> s {nextToken = a} :: DescribeAggregateComplianceByConfigRules)

-- | The name of the configuration aggregator.
describeAggregateComplianceByConfigRules_configurationAggregatorName :: Lens.Lens' DescribeAggregateComplianceByConfigRules Prelude.Text
describeAggregateComplianceByConfigRules_configurationAggregatorName = Lens.lens (\DescribeAggregateComplianceByConfigRules' {configurationAggregatorName} -> configurationAggregatorName) (\s@DescribeAggregateComplianceByConfigRules' {} a -> s {configurationAggregatorName = a} :: DescribeAggregateComplianceByConfigRules)

instance
  Core.AWSPager
    DescribeAggregateComplianceByConfigRules
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAggregateComplianceByConfigRulesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeAggregateComplianceByConfigRules_nextToken
              Lens..~ rs
              Lens.^? describeAggregateComplianceByConfigRulesResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeAggregateComplianceByConfigRules
  where
  type
    AWSResponse
      DescribeAggregateComplianceByConfigRules =
      DescribeAggregateComplianceByConfigRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAggregateComplianceByConfigRulesResponse'
            Prelude.<$> ( x
                            Data..?> "AggregateComplianceByConfigRules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAggregateComplianceByConfigRules
  where
  hashWithSalt
    _salt
    DescribeAggregateComplianceByConfigRules' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` configurationAggregatorName

instance
  Prelude.NFData
    DescribeAggregateComplianceByConfigRules
  where
  rnf DescribeAggregateComplianceByConfigRules' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf limit `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf configurationAggregatorName

instance
  Data.ToHeaders
    DescribeAggregateComplianceByConfigRules
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeAggregateComplianceByConfigRules" ::
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
    DescribeAggregateComplianceByConfigRules
  where
  toJSON DescribeAggregateComplianceByConfigRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
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
    DescribeAggregateComplianceByConfigRules
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeAggregateComplianceByConfigRules
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAggregateComplianceByConfigRulesResponse' smart constructor.
data DescribeAggregateComplianceByConfigRulesResponse = DescribeAggregateComplianceByConfigRulesResponse'
  { -- | Returns a list of AggregateComplianceByConfigRule object.
    aggregateComplianceByConfigRules :: Prelude.Maybe [AggregateComplianceByConfigRule],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAggregateComplianceByConfigRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregateComplianceByConfigRules', 'describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules' - Returns a list of AggregateComplianceByConfigRule object.
--
-- 'nextToken', 'describeAggregateComplianceByConfigRulesResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'httpStatus', 'describeAggregateComplianceByConfigRulesResponse_httpStatus' - The response's http status code.
newDescribeAggregateComplianceByConfigRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAggregateComplianceByConfigRulesResponse
newDescribeAggregateComplianceByConfigRulesResponse
  pHttpStatus_ =
    DescribeAggregateComplianceByConfigRulesResponse'
      { aggregateComplianceByConfigRules =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a list of AggregateComplianceByConfigRule object.
describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse (Prelude.Maybe [AggregateComplianceByConfigRule])
describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules = Lens.lens (\DescribeAggregateComplianceByConfigRulesResponse' {aggregateComplianceByConfigRules} -> aggregateComplianceByConfigRules) (\s@DescribeAggregateComplianceByConfigRulesResponse' {} a -> s {aggregateComplianceByConfigRules = a} :: DescribeAggregateComplianceByConfigRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregateComplianceByConfigRulesResponse_nextToken :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse (Prelude.Maybe Prelude.Text)
describeAggregateComplianceByConfigRulesResponse_nextToken = Lens.lens (\DescribeAggregateComplianceByConfigRulesResponse' {nextToken} -> nextToken) (\s@DescribeAggregateComplianceByConfigRulesResponse' {} a -> s {nextToken = a} :: DescribeAggregateComplianceByConfigRulesResponse)

-- | The response's http status code.
describeAggregateComplianceByConfigRulesResponse_httpStatus :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse Prelude.Int
describeAggregateComplianceByConfigRulesResponse_httpStatus = Lens.lens (\DescribeAggregateComplianceByConfigRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeAggregateComplianceByConfigRulesResponse' {} a -> s {httpStatus = a} :: DescribeAggregateComplianceByConfigRulesResponse)

instance
  Prelude.NFData
    DescribeAggregateComplianceByConfigRulesResponse
  where
  rnf
    DescribeAggregateComplianceByConfigRulesResponse' {..} =
      Prelude.rnf aggregateComplianceByConfigRules `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf httpStatus
