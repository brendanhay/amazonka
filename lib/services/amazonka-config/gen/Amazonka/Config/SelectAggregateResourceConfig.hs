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
-- Module      : Amazonka.Config.SelectAggregateResourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a structured query language (SQL) SELECT command and an
-- aggregator to query configuration state of Amazon Web Services resources
-- across multiple accounts and regions, performs the corresponding search,
-- and returns resource configurations matching the properties.
--
-- For more information about query components, see the
-- <https://docs.aws.amazon.com/config/latest/developerguide/query-components.html Query Components>
-- section in the Config Developer Guide.
--
-- If you run an aggregation query (i.e., using @GROUP BY@ or using
-- aggregate functions such as @COUNT@; e.g.,
-- @SELECT resourceId, COUNT(*) WHERE resourceType = \'AWS::IAM::Role\' GROUP BY resourceId@)
-- and do not specify the @MaxResults@ or the @Limit@ query parameters, the
-- default page size is set to 500.
--
-- If you run a non-aggregation query (i.e., not using @GROUP BY@ or
-- aggregate function; e.g.,
-- @SELECT * WHERE resourceType = \'AWS::IAM::Role\'@) and do not specify
-- the @MaxResults@ or the @Limit@ query parameters, the default page size
-- is set to 25.
--
-- This operation returns paginated results.
module Amazonka.Config.SelectAggregateResourceConfig
  ( -- * Creating a Request
    SelectAggregateResourceConfig (..),
    newSelectAggregateResourceConfig,

    -- * Request Lenses
    selectAggregateResourceConfig_limit,
    selectAggregateResourceConfig_maxResults,
    selectAggregateResourceConfig_nextToken,
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSelectAggregateResourceConfig' smart constructor.
data SelectAggregateResourceConfig = SelectAggregateResourceConfig'
  { -- | The maximum number of query results returned on each page.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of query results returned on each page. Config also
    -- allows the Limit request parameter.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The nextToken string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'limit', 'selectAggregateResourceConfig_limit' - The maximum number of query results returned on each page.
--
-- 'maxResults', 'selectAggregateResourceConfig_maxResults' - The maximum number of query results returned on each page. Config also
-- allows the Limit request parameter.
--
-- 'nextToken', 'selectAggregateResourceConfig_nextToken' - The nextToken string returned in a previous request that you use to
-- request the next page of results in a paginated response.
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
      { limit =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        expression = pExpression_,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | The maximum number of query results returned on each page.
selectAggregateResourceConfig_limit :: Lens.Lens' SelectAggregateResourceConfig (Prelude.Maybe Prelude.Natural)
selectAggregateResourceConfig_limit = Lens.lens (\SelectAggregateResourceConfig' {limit} -> limit) (\s@SelectAggregateResourceConfig' {} a -> s {limit = a} :: SelectAggregateResourceConfig)

-- | The maximum number of query results returned on each page. Config also
-- allows the Limit request parameter.
selectAggregateResourceConfig_maxResults :: Lens.Lens' SelectAggregateResourceConfig (Prelude.Maybe Prelude.Natural)
selectAggregateResourceConfig_maxResults = Lens.lens (\SelectAggregateResourceConfig' {maxResults} -> maxResults) (\s@SelectAggregateResourceConfig' {} a -> s {maxResults = a} :: SelectAggregateResourceConfig)

-- | The nextToken string returned in a previous request that you use to
-- request the next page of results in a paginated response.
selectAggregateResourceConfig_nextToken :: Lens.Lens' SelectAggregateResourceConfig (Prelude.Maybe Prelude.Text)
selectAggregateResourceConfig_nextToken = Lens.lens (\SelectAggregateResourceConfig' {nextToken} -> nextToken) (\s@SelectAggregateResourceConfig' {} a -> s {nextToken = a} :: SelectAggregateResourceConfig)

-- | The SQL query SELECT command.
selectAggregateResourceConfig_expression :: Lens.Lens' SelectAggregateResourceConfig Prelude.Text
selectAggregateResourceConfig_expression = Lens.lens (\SelectAggregateResourceConfig' {expression} -> expression) (\s@SelectAggregateResourceConfig' {} a -> s {expression = a} :: SelectAggregateResourceConfig)

-- | The name of the configuration aggregator.
selectAggregateResourceConfig_configurationAggregatorName :: Lens.Lens' SelectAggregateResourceConfig Prelude.Text
selectAggregateResourceConfig_configurationAggregatorName = Lens.lens (\SelectAggregateResourceConfig' {configurationAggregatorName} -> configurationAggregatorName) (\s@SelectAggregateResourceConfig' {} a -> s {configurationAggregatorName = a} :: SelectAggregateResourceConfig)

instance Core.AWSPager SelectAggregateResourceConfig where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? selectAggregateResourceConfigResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? selectAggregateResourceConfigResponse_results
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& selectAggregateResourceConfig_nextToken
          Lens..~ rs
          Lens.^? selectAggregateResourceConfigResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    SelectAggregateResourceConfig
  where
  type
    AWSResponse SelectAggregateResourceConfig =
      SelectAggregateResourceConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SelectAggregateResourceConfigResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "QueryInfo")
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SelectAggregateResourceConfig
  where
  hashWithSalt _salt SelectAggregateResourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` configurationAggregatorName

instance Prelude.NFData SelectAggregateResourceConfig where
  rnf SelectAggregateResourceConfig' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf configurationAggregatorName

instance Data.ToHeaders SelectAggregateResourceConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.SelectAggregateResourceConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SelectAggregateResourceConfig where
  toJSON SelectAggregateResourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("Expression" Data..= expression),
            Prelude.Just
              ( "ConfigurationAggregatorName"
                  Data..= configurationAggregatorName
              )
          ]
      )

instance Data.ToPath SelectAggregateResourceConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery SelectAggregateResourceConfig where
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
selectAggregateResourceConfigResponse_results = Lens.lens (\SelectAggregateResourceConfigResponse' {results} -> results) (\s@SelectAggregateResourceConfigResponse' {} a -> s {results = a} :: SelectAggregateResourceConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
selectAggregateResourceConfigResponse_httpStatus :: Lens.Lens' SelectAggregateResourceConfigResponse Prelude.Int
selectAggregateResourceConfigResponse_httpStatus = Lens.lens (\SelectAggregateResourceConfigResponse' {httpStatus} -> httpStatus) (\s@SelectAggregateResourceConfigResponse' {} a -> s {httpStatus = a} :: SelectAggregateResourceConfigResponse)

instance
  Prelude.NFData
    SelectAggregateResourceConfigResponse
  where
  rnf SelectAggregateResourceConfigResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf queryInfo
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf httpStatus
