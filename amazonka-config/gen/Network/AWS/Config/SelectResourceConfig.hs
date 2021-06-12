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
-- Module      : Network.AWS.Config.SelectResourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- section in the AWS Config Developer Guide.
module Network.AWS.Config.SelectResourceConfig
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
    selectResourceConfigResponse_nextToken,
    selectResourceConfigResponse_queryInfo,
    selectResourceConfigResponse_results,
    selectResourceConfigResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSelectResourceConfig' smart constructor.
data SelectResourceConfig = SelectResourceConfig'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of query results returned on each page.
    limit :: Core.Maybe Core.Natural,
    -- | The SQL query @SELECT@ command.
    expression :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  SelectResourceConfig
newSelectResourceConfig pExpression_ =
  SelectResourceConfig'
    { nextToken = Core.Nothing,
      limit = Core.Nothing,
      expression = pExpression_
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
selectResourceConfig_nextToken :: Lens.Lens' SelectResourceConfig (Core.Maybe Core.Text)
selectResourceConfig_nextToken = Lens.lens (\SelectResourceConfig' {nextToken} -> nextToken) (\s@SelectResourceConfig' {} a -> s {nextToken = a} :: SelectResourceConfig)

-- | The maximum number of query results returned on each page.
selectResourceConfig_limit :: Lens.Lens' SelectResourceConfig (Core.Maybe Core.Natural)
selectResourceConfig_limit = Lens.lens (\SelectResourceConfig' {limit} -> limit) (\s@SelectResourceConfig' {} a -> s {limit = a} :: SelectResourceConfig)

-- | The SQL query @SELECT@ command.
selectResourceConfig_expression :: Lens.Lens' SelectResourceConfig Core.Text
selectResourceConfig_expression = Lens.lens (\SelectResourceConfig' {expression} -> expression) (\s@SelectResourceConfig' {} a -> s {expression = a} :: SelectResourceConfig)

instance Core.AWSRequest SelectResourceConfig where
  type
    AWSResponse SelectResourceConfig =
      SelectResourceConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SelectResourceConfigResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "QueryInfo")
            Core.<*> (x Core..?> "Results" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SelectResourceConfig

instance Core.NFData SelectResourceConfig

instance Core.ToHeaders SelectResourceConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.SelectResourceConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SelectResourceConfig where
  toJSON SelectResourceConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("Expression" Core..= expression)
          ]
      )

instance Core.ToPath SelectResourceConfig where
  toPath = Core.const "/"

instance Core.ToQuery SelectResourceConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSelectResourceConfigResponse' smart constructor.
data SelectResourceConfigResponse = SelectResourceConfigResponse'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns the @QueryInfo@ object.
    queryInfo :: Core.Maybe QueryInfo,
    -- | Returns the results for the SQL query.
    results :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SelectResourceConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'selectResourceConfigResponse_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'queryInfo', 'selectResourceConfigResponse_queryInfo' - Returns the @QueryInfo@ object.
--
-- 'results', 'selectResourceConfigResponse_results' - Returns the results for the SQL query.
--
-- 'httpStatus', 'selectResourceConfigResponse_httpStatus' - The response's http status code.
newSelectResourceConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SelectResourceConfigResponse
newSelectResourceConfigResponse pHttpStatus_ =
  SelectResourceConfigResponse'
    { nextToken =
        Core.Nothing,
      queryInfo = Core.Nothing,
      results = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
selectResourceConfigResponse_nextToken :: Lens.Lens' SelectResourceConfigResponse (Core.Maybe Core.Text)
selectResourceConfigResponse_nextToken = Lens.lens (\SelectResourceConfigResponse' {nextToken} -> nextToken) (\s@SelectResourceConfigResponse' {} a -> s {nextToken = a} :: SelectResourceConfigResponse)

-- | Returns the @QueryInfo@ object.
selectResourceConfigResponse_queryInfo :: Lens.Lens' SelectResourceConfigResponse (Core.Maybe QueryInfo)
selectResourceConfigResponse_queryInfo = Lens.lens (\SelectResourceConfigResponse' {queryInfo} -> queryInfo) (\s@SelectResourceConfigResponse' {} a -> s {queryInfo = a} :: SelectResourceConfigResponse)

-- | Returns the results for the SQL query.
selectResourceConfigResponse_results :: Lens.Lens' SelectResourceConfigResponse (Core.Maybe [Core.Text])
selectResourceConfigResponse_results = Lens.lens (\SelectResourceConfigResponse' {results} -> results) (\s@SelectResourceConfigResponse' {} a -> s {results = a} :: SelectResourceConfigResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
selectResourceConfigResponse_httpStatus :: Lens.Lens' SelectResourceConfigResponse Core.Int
selectResourceConfigResponse_httpStatus = Lens.lens (\SelectResourceConfigResponse' {httpStatus} -> httpStatus) (\s@SelectResourceConfigResponse' {} a -> s {httpStatus = a} :: SelectResourceConfigResponse)

instance Core.NFData SelectResourceConfigResponse
