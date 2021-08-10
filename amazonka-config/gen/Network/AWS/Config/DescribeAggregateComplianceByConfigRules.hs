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
-- Module      : Network.AWS.Config.DescribeAggregateComplianceByConfigRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of compliant and noncompliant rules with the number of
-- resources for compliant and noncompliant rules.
--
-- The results can return an empty result page, but if you have a
-- @nextToken@, the results are displayed on the next page.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeAggregateComplianceByConfigRules
  ( -- * Creating a Request
    DescribeAggregateComplianceByConfigRules (..),
    newDescribeAggregateComplianceByConfigRules,

    -- * Request Lenses
    describeAggregateComplianceByConfigRules_nextToken,
    describeAggregateComplianceByConfigRules_filters,
    describeAggregateComplianceByConfigRules_limit,
    describeAggregateComplianceByConfigRules_configurationAggregatorName,

    -- * Destructuring the Response
    DescribeAggregateComplianceByConfigRulesResponse (..),
    newDescribeAggregateComplianceByConfigRulesResponse,

    -- * Response Lenses
    describeAggregateComplianceByConfigRulesResponse_nextToken,
    describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules,
    describeAggregateComplianceByConfigRulesResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAggregateComplianceByConfigRules' smart constructor.
data DescribeAggregateComplianceByConfigRules = DescribeAggregateComplianceByConfigRules'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the results by ConfigRuleComplianceFilters object.
    filters :: Prelude.Maybe ConfigRuleComplianceFilters,
    -- | The maximum number of evaluation results returned on each page. The
    -- default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'describeAggregateComplianceByConfigRules_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'filters', 'describeAggregateComplianceByConfigRules_filters' - Filters the results by ConfigRuleComplianceFilters object.
--
-- 'limit', 'describeAggregateComplianceByConfigRules_limit' - The maximum number of evaluation results returned on each page. The
-- default is maximum. If you specify 0, AWS Config uses the default.
--
-- 'configurationAggregatorName', 'describeAggregateComplianceByConfigRules_configurationAggregatorName' - The name of the configuration aggregator.
newDescribeAggregateComplianceByConfigRules ::
  -- | 'configurationAggregatorName'
  Prelude.Text ->
  DescribeAggregateComplianceByConfigRules
newDescribeAggregateComplianceByConfigRules
  pConfigurationAggregatorName_ =
    DescribeAggregateComplianceByConfigRules'
      { nextToken =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        limit = Prelude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregateComplianceByConfigRules_nextToken :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Prelude.Maybe Prelude.Text)
describeAggregateComplianceByConfigRules_nextToken = Lens.lens (\DescribeAggregateComplianceByConfigRules' {nextToken} -> nextToken) (\s@DescribeAggregateComplianceByConfigRules' {} a -> s {nextToken = a} :: DescribeAggregateComplianceByConfigRules)

-- | Filters the results by ConfigRuleComplianceFilters object.
describeAggregateComplianceByConfigRules_filters :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Prelude.Maybe ConfigRuleComplianceFilters)
describeAggregateComplianceByConfigRules_filters = Lens.lens (\DescribeAggregateComplianceByConfigRules' {filters} -> filters) (\s@DescribeAggregateComplianceByConfigRules' {} a -> s {filters = a} :: DescribeAggregateComplianceByConfigRules)

-- | The maximum number of evaluation results returned on each page. The
-- default is maximum. If you specify 0, AWS Config uses the default.
describeAggregateComplianceByConfigRules_limit :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Prelude.Maybe Prelude.Natural)
describeAggregateComplianceByConfigRules_limit = Lens.lens (\DescribeAggregateComplianceByConfigRules' {limit} -> limit) (\s@DescribeAggregateComplianceByConfigRules' {} a -> s {limit = a} :: DescribeAggregateComplianceByConfigRules)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAggregateComplianceByConfigRulesResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "AggregateComplianceByConfigRules"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAggregateComplianceByConfigRules

instance
  Prelude.NFData
    DescribeAggregateComplianceByConfigRules

instance
  Core.ToHeaders
    DescribeAggregateComplianceByConfigRules
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeAggregateComplianceByConfigRules" ::
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
    DescribeAggregateComplianceByConfigRules
  where
  toJSON DescribeAggregateComplianceByConfigRules' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              )
          ]
      )

instance
  Core.ToPath
    DescribeAggregateComplianceByConfigRules
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeAggregateComplianceByConfigRules
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAggregateComplianceByConfigRulesResponse' smart constructor.
data DescribeAggregateComplianceByConfigRulesResponse = DescribeAggregateComplianceByConfigRulesResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of AggregateComplianceByConfigRule object.
    aggregateComplianceByConfigRules :: Prelude.Maybe [AggregateComplianceByConfigRule],
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
-- 'nextToken', 'describeAggregateComplianceByConfigRulesResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'aggregateComplianceByConfigRules', 'describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules' - Returns a list of AggregateComplianceByConfigRule object.
--
-- 'httpStatus', 'describeAggregateComplianceByConfigRulesResponse_httpStatus' - The response's http status code.
newDescribeAggregateComplianceByConfigRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAggregateComplianceByConfigRulesResponse
newDescribeAggregateComplianceByConfigRulesResponse
  pHttpStatus_ =
    DescribeAggregateComplianceByConfigRulesResponse'
      { nextToken =
          Prelude.Nothing,
        aggregateComplianceByConfigRules =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregateComplianceByConfigRulesResponse_nextToken :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse (Prelude.Maybe Prelude.Text)
describeAggregateComplianceByConfigRulesResponse_nextToken = Lens.lens (\DescribeAggregateComplianceByConfigRulesResponse' {nextToken} -> nextToken) (\s@DescribeAggregateComplianceByConfigRulesResponse' {} a -> s {nextToken = a} :: DescribeAggregateComplianceByConfigRulesResponse)

-- | Returns a list of AggregateComplianceByConfigRule object.
describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse (Prelude.Maybe [AggregateComplianceByConfigRule])
describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules = Lens.lens (\DescribeAggregateComplianceByConfigRulesResponse' {aggregateComplianceByConfigRules} -> aggregateComplianceByConfigRules) (\s@DescribeAggregateComplianceByConfigRulesResponse' {} a -> s {aggregateComplianceByConfigRules = a} :: DescribeAggregateComplianceByConfigRulesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAggregateComplianceByConfigRulesResponse_httpStatus :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse Prelude.Int
describeAggregateComplianceByConfigRulesResponse_httpStatus = Lens.lens (\DescribeAggregateComplianceByConfigRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeAggregateComplianceByConfigRulesResponse' {} a -> s {httpStatus = a} :: DescribeAggregateComplianceByConfigRulesResponse)

instance
  Prelude.NFData
    DescribeAggregateComplianceByConfigRulesResponse
