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
-- Module      : Amazonka.Inspector2.ListFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the filters associated with your account.
--
-- This operation returns paginated results.
module Amazonka.Inspector2.ListFilters
  ( -- * Creating a Request
    ListFilters (..),
    newListFilters,

    -- * Request Lenses
    listFilters_action,
    listFilters_arns,
    listFilters_maxResults,
    listFilters_nextToken,

    -- * Destructuring the Response
    ListFiltersResponse (..),
    newListFiltersResponse,

    -- * Response Lenses
    listFiltersResponse_nextToken,
    listFiltersResponse_httpStatus,
    listFiltersResponse_filters,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFilters' smart constructor.
data ListFilters = ListFilters'
  { -- | The action the filter applies to matched findings.
    action :: Prelude.Maybe FilterAction,
    -- | The Amazon resource number (ARN) of the filter.
    arns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'listFilters_action' - The action the filter applies to matched findings.
--
-- 'arns', 'listFilters_arns' - The Amazon resource number (ARN) of the filter.
--
-- 'maxResults', 'listFilters_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listFilters_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
newListFilters ::
  ListFilters
newListFilters =
  ListFilters'
    { action = Prelude.Nothing,
      arns = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The action the filter applies to matched findings.
listFilters_action :: Lens.Lens' ListFilters (Prelude.Maybe FilterAction)
listFilters_action = Lens.lens (\ListFilters' {action} -> action) (\s@ListFilters' {} a -> s {action = a} :: ListFilters)

-- | The Amazon resource number (ARN) of the filter.
listFilters_arns :: Lens.Lens' ListFilters (Prelude.Maybe [Prelude.Text])
listFilters_arns = Lens.lens (\ListFilters' {arns} -> arns) (\s@ListFilters' {} a -> s {arns = a} :: ListFilters) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in the response.
listFilters_maxResults :: Lens.Lens' ListFilters (Prelude.Maybe Prelude.Natural)
listFilters_maxResults = Lens.lens (\ListFilters' {maxResults} -> maxResults) (\s@ListFilters' {} a -> s {maxResults = a} :: ListFilters)

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listFilters_nextToken :: Lens.Lens' ListFilters (Prelude.Maybe Prelude.Text)
listFilters_nextToken = Lens.lens (\ListFilters' {nextToken} -> nextToken) (\s@ListFilters' {} a -> s {nextToken = a} :: ListFilters)

instance Core.AWSPager ListFilters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFiltersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listFiltersResponse_filters) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFilters_nextToken
          Lens..~ rs
          Lens.^? listFiltersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListFilters where
  type AWSResponse ListFilters = ListFiltersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFiltersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "filters" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListFilters where
  hashWithSalt _salt ListFilters' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` arns
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListFilters where
  rnf ListFilters' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf arns
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListFilters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFilters where
  toJSON ListFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("action" Data..=) Prelude.<$> action,
            ("arns" Data..=) Prelude.<$> arns,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListFilters where
  toPath = Prelude.const "/filters/list"

instance Data.ToQuery ListFilters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFiltersResponse' smart constructor.
data ListFiltersResponse = ListFiltersResponse'
  { -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains details on the filters associated with your account.
    filters :: [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFiltersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFiltersResponse_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'httpStatus', 'listFiltersResponse_httpStatus' - The response's http status code.
--
-- 'filters', 'listFiltersResponse_filters' - Contains details on the filters associated with your account.
newListFiltersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFiltersResponse
newListFiltersResponse pHttpStatus_ =
  ListFiltersResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      filters = Prelude.mempty
    }

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listFiltersResponse_nextToken :: Lens.Lens' ListFiltersResponse (Prelude.Maybe Prelude.Text)
listFiltersResponse_nextToken = Lens.lens (\ListFiltersResponse' {nextToken} -> nextToken) (\s@ListFiltersResponse' {} a -> s {nextToken = a} :: ListFiltersResponse)

-- | The response's http status code.
listFiltersResponse_httpStatus :: Lens.Lens' ListFiltersResponse Prelude.Int
listFiltersResponse_httpStatus = Lens.lens (\ListFiltersResponse' {httpStatus} -> httpStatus) (\s@ListFiltersResponse' {} a -> s {httpStatus = a} :: ListFiltersResponse)

-- | Contains details on the filters associated with your account.
listFiltersResponse_filters :: Lens.Lens' ListFiltersResponse [Filter]
listFiltersResponse_filters = Lens.lens (\ListFiltersResponse' {filters} -> filters) (\s@ListFiltersResponse' {} a -> s {filters = a} :: ListFiltersResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListFiltersResponse where
  rnf ListFiltersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf filters
