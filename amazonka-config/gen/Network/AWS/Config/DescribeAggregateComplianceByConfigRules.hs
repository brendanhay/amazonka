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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAggregateComplianceByConfigRules' smart constructor.
data DescribeAggregateComplianceByConfigRules = DescribeAggregateComplianceByConfigRules'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Filters the results by ConfigRuleComplianceFilters object.
    filters :: Core.Maybe ConfigRuleComplianceFilters,
    -- | The maximum number of evaluation results returned on each page. The
    -- default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeAggregateComplianceByConfigRules
newDescribeAggregateComplianceByConfigRules
  pConfigurationAggregatorName_ =
    DescribeAggregateComplianceByConfigRules'
      { nextToken =
          Core.Nothing,
        filters = Core.Nothing,
        limit = Core.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregateComplianceByConfigRules_nextToken :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Core.Maybe Core.Text)
describeAggregateComplianceByConfigRules_nextToken = Lens.lens (\DescribeAggregateComplianceByConfigRules' {nextToken} -> nextToken) (\s@DescribeAggregateComplianceByConfigRules' {} a -> s {nextToken = a} :: DescribeAggregateComplianceByConfigRules)

-- | Filters the results by ConfigRuleComplianceFilters object.
describeAggregateComplianceByConfigRules_filters :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Core.Maybe ConfigRuleComplianceFilters)
describeAggregateComplianceByConfigRules_filters = Lens.lens (\DescribeAggregateComplianceByConfigRules' {filters} -> filters) (\s@DescribeAggregateComplianceByConfigRules' {} a -> s {filters = a} :: DescribeAggregateComplianceByConfigRules)

-- | The maximum number of evaluation results returned on each page. The
-- default is maximum. If you specify 0, AWS Config uses the default.
describeAggregateComplianceByConfigRules_limit :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Core.Maybe Core.Natural)
describeAggregateComplianceByConfigRules_limit = Lens.lens (\DescribeAggregateComplianceByConfigRules' {limit} -> limit) (\s@DescribeAggregateComplianceByConfigRules' {} a -> s {limit = a} :: DescribeAggregateComplianceByConfigRules)

-- | The name of the configuration aggregator.
describeAggregateComplianceByConfigRules_configurationAggregatorName :: Lens.Lens' DescribeAggregateComplianceByConfigRules Core.Text
describeAggregateComplianceByConfigRules_configurationAggregatorName = Lens.lens (\DescribeAggregateComplianceByConfigRules' {configurationAggregatorName} -> configurationAggregatorName) (\s@DescribeAggregateComplianceByConfigRules' {} a -> s {configurationAggregatorName = a} :: DescribeAggregateComplianceByConfigRules)

instance
  Core.AWSPager
    DescribeAggregateComplianceByConfigRules
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAggregateComplianceByConfigRulesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAggregateComplianceByConfigRules_nextToken
          Lens..~ rs
            Lens.^? describeAggregateComplianceByConfigRulesResponse_nextToken
              Core.. Lens._Just

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
            Core.<$> (x Core..?> "NextToken")
              Core.<*> ( x Core..?> "AggregateComplianceByConfigRules"
                           Core..!@ Core.mempty
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeAggregateComplianceByConfigRules

instance
  Core.NFData
    DescribeAggregateComplianceByConfigRules

instance
  Core.ToHeaders
    DescribeAggregateComplianceByConfigRules
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeAggregateComplianceByConfigRules" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeAggregateComplianceByConfigRules
  where
  toJSON DescribeAggregateComplianceByConfigRules' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Filters" Core..=) Core.<$> filters,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              )
          ]
      )

instance
  Core.ToPath
    DescribeAggregateComplianceByConfigRules
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeAggregateComplianceByConfigRules
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAggregateComplianceByConfigRulesResponse' smart constructor.
data DescribeAggregateComplianceByConfigRulesResponse = DescribeAggregateComplianceByConfigRulesResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a list of AggregateComplianceByConfigRule object.
    aggregateComplianceByConfigRules :: Core.Maybe [AggregateComplianceByConfigRule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeAggregateComplianceByConfigRulesResponse
newDescribeAggregateComplianceByConfigRulesResponse
  pHttpStatus_ =
    DescribeAggregateComplianceByConfigRulesResponse'
      { nextToken =
          Core.Nothing,
        aggregateComplianceByConfigRules =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregateComplianceByConfigRulesResponse_nextToken :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse (Core.Maybe Core.Text)
describeAggregateComplianceByConfigRulesResponse_nextToken = Lens.lens (\DescribeAggregateComplianceByConfigRulesResponse' {nextToken} -> nextToken) (\s@DescribeAggregateComplianceByConfigRulesResponse' {} a -> s {nextToken = a} :: DescribeAggregateComplianceByConfigRulesResponse)

-- | Returns a list of AggregateComplianceByConfigRule object.
describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse (Core.Maybe [AggregateComplianceByConfigRule])
describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules = Lens.lens (\DescribeAggregateComplianceByConfigRulesResponse' {aggregateComplianceByConfigRules} -> aggregateComplianceByConfigRules) (\s@DescribeAggregateComplianceByConfigRulesResponse' {} a -> s {aggregateComplianceByConfigRules = a} :: DescribeAggregateComplianceByConfigRulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAggregateComplianceByConfigRulesResponse_httpStatus :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse Core.Int
describeAggregateComplianceByConfigRulesResponse_httpStatus = Lens.lens (\DescribeAggregateComplianceByConfigRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeAggregateComplianceByConfigRulesResponse' {} a -> s {httpStatus = a} :: DescribeAggregateComplianceByConfigRulesResponse)

instance
  Core.NFData
    DescribeAggregateComplianceByConfigRulesResponse
