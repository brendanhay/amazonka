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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSelectAggregateResourceConfig' smart constructor.
data SelectAggregateResourceConfig = SelectAggregateResourceConfig'
  { -- | The nextToken string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of query results returned on each page. AWS Config
    -- also allows the Limit request parameter.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of query results returned on each page.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The SQL query SELECT command.
    expression :: Prelude.Text,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'configurationAggregatorName'
  Prelude.Text ->
  SelectAggregateResourceConfig
newSelectAggregateResourceConfig
  pExpression_
  pConfigurationAggregatorName_ =
    SelectAggregateResourceConfig'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        limit = Prelude.Nothing,
        expression = pExpression_,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | The nextToken string returned in a previous request that you use to
-- request the next page of results in a paginated response.
selectAggregateResourceConfig_nextToken :: Lens.Lens' SelectAggregateResourceConfig (Prelude.Maybe Prelude.Text)
selectAggregateResourceConfig_nextToken = Lens.lens (\SelectAggregateResourceConfig' {nextToken} -> nextToken) (\s@SelectAggregateResourceConfig' {} a -> s {nextToken = a} :: SelectAggregateResourceConfig)

-- | The maximum number of query results returned on each page. AWS Config
-- also allows the Limit request parameter.
selectAggregateResourceConfig_maxResults :: Lens.Lens' SelectAggregateResourceConfig (Prelude.Maybe Prelude.Natural)
selectAggregateResourceConfig_maxResults = Lens.lens (\SelectAggregateResourceConfig' {maxResults} -> maxResults) (\s@SelectAggregateResourceConfig' {} a -> s {maxResults = a} :: SelectAggregateResourceConfig)

-- | The maximum number of query results returned on each page.
selectAggregateResourceConfig_limit :: Lens.Lens' SelectAggregateResourceConfig (Prelude.Maybe Prelude.Natural)
selectAggregateResourceConfig_limit = Lens.lens (\SelectAggregateResourceConfig' {limit} -> limit) (\s@SelectAggregateResourceConfig' {} a -> s {limit = a} :: SelectAggregateResourceConfig)

-- | The SQL query SELECT command.
selectAggregateResourceConfig_expression :: Lens.Lens' SelectAggregateResourceConfig Prelude.Text
selectAggregateResourceConfig_expression = Lens.lens (\SelectAggregateResourceConfig' {expression} -> expression) (\s@SelectAggregateResourceConfig' {} a -> s {expression = a} :: SelectAggregateResourceConfig)

-- | The name of the configuration aggregator.
selectAggregateResourceConfig_configurationAggregatorName :: Lens.Lens' SelectAggregateResourceConfig Prelude.Text
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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "QueryInfo")
            Prelude.<*> (x Core..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SelectAggregateResourceConfig

instance Prelude.NFData SelectAggregateResourceConfig

instance Core.ToHeaders SelectAggregateResourceConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.SelectAggregateResourceConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SelectAggregateResourceConfig where
  toJSON SelectAggregateResourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("Expression" Core..= expression),
            Prelude.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              )
          ]
      )

instance Core.ToPath SelectAggregateResourceConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery SelectAggregateResourceConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSelectAggregateResourceConfigResponse' smart constructor.
data SelectAggregateResourceConfigResponse = SelectAggregateResourceConfigResponse'
  { -- | The nextToken string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    queryInfo :: Prelude.Maybe QueryInfo,
    -- | Returns the results for the SQL query.
    results :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SelectAggregateResourceConfigResponse
newSelectAggregateResourceConfigResponse pHttpStatus_ =
  SelectAggregateResourceConfigResponse'
    { nextToken =
        Prelude.Nothing,
      queryInfo = Prelude.Nothing,
      results = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The nextToken string returned in a previous request that you use to
-- request the next page of results in a paginated response.
selectAggregateResourceConfigResponse_nextToken :: Lens.Lens' SelectAggregateResourceConfigResponse (Prelude.Maybe Prelude.Text)
selectAggregateResourceConfigResponse_nextToken = Lens.lens (\SelectAggregateResourceConfigResponse' {nextToken} -> nextToken) (\s@SelectAggregateResourceConfigResponse' {} a -> s {nextToken = a} :: SelectAggregateResourceConfigResponse)

-- | Undocumented member.
selectAggregateResourceConfigResponse_queryInfo :: Lens.Lens' SelectAggregateResourceConfigResponse (Prelude.Maybe QueryInfo)
selectAggregateResourceConfigResponse_queryInfo = Lens.lens (\SelectAggregateResourceConfigResponse' {queryInfo} -> queryInfo) (\s@SelectAggregateResourceConfigResponse' {} a -> s {queryInfo = a} :: SelectAggregateResourceConfigResponse)

-- | Returns the results for the SQL query.
selectAggregateResourceConfigResponse_results :: Lens.Lens' SelectAggregateResourceConfigResponse (Prelude.Maybe [Prelude.Text])
selectAggregateResourceConfigResponse_results = Lens.lens (\SelectAggregateResourceConfigResponse' {results} -> results) (\s@SelectAggregateResourceConfigResponse' {} a -> s {results = a} :: SelectAggregateResourceConfigResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
selectAggregateResourceConfigResponse_httpStatus :: Lens.Lens' SelectAggregateResourceConfigResponse Prelude.Int
selectAggregateResourceConfigResponse_httpStatus = Lens.lens (\SelectAggregateResourceConfigResponse' {httpStatus} -> httpStatus) (\s@SelectAggregateResourceConfigResponse' {} a -> s {httpStatus = a} :: SelectAggregateResourceConfigResponse)

instance
  Prelude.NFData
    SelectAggregateResourceConfigResponse
