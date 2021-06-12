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
-- Module      : Network.AWS.Config.SelectAggregateResourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a structured query language (SQL) SELECT command and an
-- aggregator to query configuration state of AWS resources across multiple
-- accounts and regions, performs the corresponding search, and returns
-- resource configurations matching the properties.
--
-- For more information about query components, see the
-- <https://docs.aws.amazon.com/config/latest/developerguide/query-components.html Query Components>
-- section in the AWS Config Developer Guide.
module Network.AWS.Config.SelectAggregateResourceConfig
  ( -- * Creating a Request
    SelectAggregateResourceConfig (..),
    newSelectAggregateResourceConfig,

    -- * Request Lenses
    selectAggregateResourceConfig_nextToken,
    selectAggregateResourceConfig_maxResults,
    selectAggregateResourceConfig_limit,
    selectAggregateResourceConfig_expression,
    selectAggregateResourceConfig_configurationAggregatorName,

    -- * Destructuring the Response
    SelectAggregateResourceConfigResponse (..),
    newSelectAggregateResourceConfigResponse,

    -- * Response Lenses
    selectAggregateResourceConfigResponse_nextToken,
    selectAggregateResourceConfigResponse_queryInfo,
    selectAggregateResourceConfigResponse_results,
    selectAggregateResourceConfigResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSelectAggregateResourceConfig' smart constructor.
data SelectAggregateResourceConfig = SelectAggregateResourceConfig'
  { -- | The nextToken string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of query results returned on each page. AWS Config
    -- also allows the Limit request parameter.
    maxResults :: Core.Maybe Core.Natural,
    -- | The maximum number of query results returned on each page.
    limit :: Core.Maybe Core.Natural,
    -- | The SQL query SELECT command.
    expression :: Core.Text,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SelectAggregateResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'selectAggregateResourceConfig_nextToken' - The nextToken string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'maxResults', 'selectAggregateResourceConfig_maxResults' - The maximum number of query results returned on each page. AWS Config
-- also allows the Limit request parameter.
--
-- 'limit', 'selectAggregateResourceConfig_limit' - The maximum number of query results returned on each page.
--
-- 'expression', 'selectAggregateResourceConfig_expression' - The SQL query SELECT command.
--
-- 'configurationAggregatorName', 'selectAggregateResourceConfig_configurationAggregatorName' - The name of the configuration aggregator.
newSelectAggregateResourceConfig ::
  -- | 'expression'
  Core.Text ->
  -- | 'configurationAggregatorName'
  Core.Text ->
  SelectAggregateResourceConfig
newSelectAggregateResourceConfig
  pExpression_
  pConfigurationAggregatorName_ =
    SelectAggregateResourceConfig'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        limit = Core.Nothing,
        expression = pExpression_,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | The nextToken string returned in a previous request that you use to
-- request the next page of results in a paginated response.
selectAggregateResourceConfig_nextToken :: Lens.Lens' SelectAggregateResourceConfig (Core.Maybe Core.Text)
selectAggregateResourceConfig_nextToken = Lens.lens (\SelectAggregateResourceConfig' {nextToken} -> nextToken) (\s@SelectAggregateResourceConfig' {} a -> s {nextToken = a} :: SelectAggregateResourceConfig)

-- | The maximum number of query results returned on each page. AWS Config
-- also allows the Limit request parameter.
selectAggregateResourceConfig_maxResults :: Lens.Lens' SelectAggregateResourceConfig (Core.Maybe Core.Natural)
selectAggregateResourceConfig_maxResults = Lens.lens (\SelectAggregateResourceConfig' {maxResults} -> maxResults) (\s@SelectAggregateResourceConfig' {} a -> s {maxResults = a} :: SelectAggregateResourceConfig)

-- | The maximum number of query results returned on each page.
selectAggregateResourceConfig_limit :: Lens.Lens' SelectAggregateResourceConfig (Core.Maybe Core.Natural)
selectAggregateResourceConfig_limit = Lens.lens (\SelectAggregateResourceConfig' {limit} -> limit) (\s@SelectAggregateResourceConfig' {} a -> s {limit = a} :: SelectAggregateResourceConfig)

-- | The SQL query SELECT command.
selectAggregateResourceConfig_expression :: Lens.Lens' SelectAggregateResourceConfig Core.Text
selectAggregateResourceConfig_expression = Lens.lens (\SelectAggregateResourceConfig' {expression} -> expression) (\s@SelectAggregateResourceConfig' {} a -> s {expression = a} :: SelectAggregateResourceConfig)

-- | The name of the configuration aggregator.
selectAggregateResourceConfig_configurationAggregatorName :: Lens.Lens' SelectAggregateResourceConfig Core.Text
selectAggregateResourceConfig_configurationAggregatorName = Lens.lens (\SelectAggregateResourceConfig' {configurationAggregatorName} -> configurationAggregatorName) (\s@SelectAggregateResourceConfig' {} a -> s {configurationAggregatorName = a} :: SelectAggregateResourceConfig)

instance
  Core.AWSRequest
    SelectAggregateResourceConfig
  where
  type
    AWSResponse SelectAggregateResourceConfig =
      SelectAggregateResourceConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SelectAggregateResourceConfigResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "QueryInfo")
            Core.<*> (x Core..?> "Results" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SelectAggregateResourceConfig

instance Core.NFData SelectAggregateResourceConfig

instance Core.ToHeaders SelectAggregateResourceConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.SelectAggregateResourceConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SelectAggregateResourceConfig where
  toJSON SelectAggregateResourceConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("Expression" Core..= expression),
            Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              )
          ]
      )

instance Core.ToPath SelectAggregateResourceConfig where
  toPath = Core.const "/"

instance Core.ToQuery SelectAggregateResourceConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSelectAggregateResourceConfigResponse' smart constructor.
data SelectAggregateResourceConfigResponse = SelectAggregateResourceConfigResponse'
  { -- | The nextToken string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    queryInfo :: Core.Maybe QueryInfo,
    -- | Returns the results for the SQL query.
    results :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SelectAggregateResourceConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'selectAggregateResourceConfigResponse_nextToken' - The nextToken string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'queryInfo', 'selectAggregateResourceConfigResponse_queryInfo' - Undocumented member.
--
-- 'results', 'selectAggregateResourceConfigResponse_results' - Returns the results for the SQL query.
--
-- 'httpStatus', 'selectAggregateResourceConfigResponse_httpStatus' - The response's http status code.
newSelectAggregateResourceConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SelectAggregateResourceConfigResponse
newSelectAggregateResourceConfigResponse pHttpStatus_ =
  SelectAggregateResourceConfigResponse'
    { nextToken =
        Core.Nothing,
      queryInfo = Core.Nothing,
      results = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The nextToken string returned in a previous request that you use to
-- request the next page of results in a paginated response.
selectAggregateResourceConfigResponse_nextToken :: Lens.Lens' SelectAggregateResourceConfigResponse (Core.Maybe Core.Text)
selectAggregateResourceConfigResponse_nextToken = Lens.lens (\SelectAggregateResourceConfigResponse' {nextToken} -> nextToken) (\s@SelectAggregateResourceConfigResponse' {} a -> s {nextToken = a} :: SelectAggregateResourceConfigResponse)

-- | Undocumented member.
selectAggregateResourceConfigResponse_queryInfo :: Lens.Lens' SelectAggregateResourceConfigResponse (Core.Maybe QueryInfo)
selectAggregateResourceConfigResponse_queryInfo = Lens.lens (\SelectAggregateResourceConfigResponse' {queryInfo} -> queryInfo) (\s@SelectAggregateResourceConfigResponse' {} a -> s {queryInfo = a} :: SelectAggregateResourceConfigResponse)

-- | Returns the results for the SQL query.
selectAggregateResourceConfigResponse_results :: Lens.Lens' SelectAggregateResourceConfigResponse (Core.Maybe [Core.Text])
selectAggregateResourceConfigResponse_results = Lens.lens (\SelectAggregateResourceConfigResponse' {results} -> results) (\s@SelectAggregateResourceConfigResponse' {} a -> s {results = a} :: SelectAggregateResourceConfigResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
selectAggregateResourceConfigResponse_httpStatus :: Lens.Lens' SelectAggregateResourceConfigResponse Core.Int
selectAggregateResourceConfigResponse_httpStatus = Lens.lens (\SelectAggregateResourceConfigResponse' {httpStatus} -> httpStatus) (\s@SelectAggregateResourceConfigResponse' {} a -> s {httpStatus = a} :: SelectAggregateResourceConfigResponse)

instance
  Core.NFData
    SelectAggregateResourceConfigResponse
