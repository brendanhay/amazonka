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
-- Module      : Amazonka.Config.SelectResourceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a structured query language (SQL) @SELECT@ command, performs the
-- corresponding search, and returns resource configurations matching the
-- properties.
--
-- For more information about query components, see the
-- <https://docs.aws.amazon.com/config/latest/developerguide/query-components.html Query Components>
-- section in the /Config Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.Config.SelectResourceConfig
  ( -- * Creating a Request
    SelectResourceConfig (..),
    newSelectResourceConfig,

    -- * Request Lenses
    selectResourceConfig_nextToken,
    selectResourceConfig_limit,
    selectResourceConfig_expression,

    -- * Destructuring the Response
    SelectResourceConfigResponse (..),
    newSelectResourceConfigResponse,

    -- * Response Lenses
    selectResourceConfigResponse_queryInfo,
    selectResourceConfigResponse_nextToken,
    selectResourceConfigResponse_results,
    selectResourceConfigResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSelectResourceConfig' smart constructor.
data SelectResourceConfig = SelectResourceConfig'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of query results returned on each page.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The SQL query @SELECT@ command.
    expression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'selectResourceConfig_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'limit', 'selectResourceConfig_limit' - The maximum number of query results returned on each page.
--
-- 'expression', 'selectResourceConfig_expression' - The SQL query @SELECT@ command.
newSelectResourceConfig ::
  -- | 'expression'
  Prelude.Text ->
  SelectResourceConfig
newSelectResourceConfig pExpression_ =
  SelectResourceConfig'
    { nextToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      expression = pExpression_
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
selectResourceConfig_nextToken :: Lens.Lens' SelectResourceConfig (Prelude.Maybe Prelude.Text)
selectResourceConfig_nextToken = Lens.lens (\SelectResourceConfig' {nextToken} -> nextToken) (\s@SelectResourceConfig' {} a -> s {nextToken = a} :: SelectResourceConfig)

-- | The maximum number of query results returned on each page.
selectResourceConfig_limit :: Lens.Lens' SelectResourceConfig (Prelude.Maybe Prelude.Natural)
selectResourceConfig_limit = Lens.lens (\SelectResourceConfig' {limit} -> limit) (\s@SelectResourceConfig' {} a -> s {limit = a} :: SelectResourceConfig)

-- | The SQL query @SELECT@ command.
selectResourceConfig_expression :: Lens.Lens' SelectResourceConfig Prelude.Text
selectResourceConfig_expression = Lens.lens (\SelectResourceConfig' {expression} -> expression) (\s@SelectResourceConfig' {} a -> s {expression = a} :: SelectResourceConfig)

instance Core.AWSPager SelectResourceConfig where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? selectResourceConfigResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? selectResourceConfigResponse_results
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& selectResourceConfig_nextToken
          Lens..~ rs
          Lens.^? selectResourceConfigResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SelectResourceConfig where
  type
    AWSResponse SelectResourceConfig =
      SelectResourceConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SelectResourceConfigResponse'
            Prelude.<$> (x Core..?> "QueryInfo")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SelectResourceConfig where
  hashWithSalt _salt SelectResourceConfig' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` expression

instance Prelude.NFData SelectResourceConfig where
  rnf SelectResourceConfig' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf expression

instance Core.ToHeaders SelectResourceConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.SelectResourceConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SelectResourceConfig where
  toJSON SelectResourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("Expression" Core..= expression)
          ]
      )

instance Core.ToPath SelectResourceConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery SelectResourceConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSelectResourceConfigResponse' smart constructor.
data SelectResourceConfigResponse = SelectResourceConfigResponse'
  { -- | Returns the @QueryInfo@ object.
    queryInfo :: Prelude.Maybe QueryInfo,
    -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns the results for the SQL query.
    results :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectResourceConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryInfo', 'selectResourceConfigResponse_queryInfo' - Returns the @QueryInfo@ object.
--
-- 'nextToken', 'selectResourceConfigResponse_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'results', 'selectResourceConfigResponse_results' - Returns the results for the SQL query.
--
-- 'httpStatus', 'selectResourceConfigResponse_httpStatus' - The response's http status code.
newSelectResourceConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SelectResourceConfigResponse
newSelectResourceConfigResponse pHttpStatus_ =
  SelectResourceConfigResponse'
    { queryInfo =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      results = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the @QueryInfo@ object.
selectResourceConfigResponse_queryInfo :: Lens.Lens' SelectResourceConfigResponse (Prelude.Maybe QueryInfo)
selectResourceConfigResponse_queryInfo = Lens.lens (\SelectResourceConfigResponse' {queryInfo} -> queryInfo) (\s@SelectResourceConfigResponse' {} a -> s {queryInfo = a} :: SelectResourceConfigResponse)

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
selectResourceConfigResponse_nextToken :: Lens.Lens' SelectResourceConfigResponse (Prelude.Maybe Prelude.Text)
selectResourceConfigResponse_nextToken = Lens.lens (\SelectResourceConfigResponse' {nextToken} -> nextToken) (\s@SelectResourceConfigResponse' {} a -> s {nextToken = a} :: SelectResourceConfigResponse)

-- | Returns the results for the SQL query.
selectResourceConfigResponse_results :: Lens.Lens' SelectResourceConfigResponse (Prelude.Maybe [Prelude.Text])
selectResourceConfigResponse_results = Lens.lens (\SelectResourceConfigResponse' {results} -> results) (\s@SelectResourceConfigResponse' {} a -> s {results = a} :: SelectResourceConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
selectResourceConfigResponse_httpStatus :: Lens.Lens' SelectResourceConfigResponse Prelude.Int
selectResourceConfigResponse_httpStatus = Lens.lens (\SelectResourceConfigResponse' {httpStatus} -> httpStatus) (\s@SelectResourceConfigResponse' {} a -> s {httpStatus = a} :: SelectResourceConfigResponse)

instance Prelude.NFData SelectResourceConfigResponse where
  rnf SelectResourceConfigResponse' {..} =
    Prelude.rnf queryInfo
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf httpStatus
