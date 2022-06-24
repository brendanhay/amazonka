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
-- Module      : Amazonka.RobOMaker.ListFleets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of fleets. You can optionally provide filters to retrieve
-- specific fleets.
--
-- This operation returns paginated results.
module Amazonka.RobOMaker.ListFleets
  ( -- * Creating a Request
    ListFleets (..),
    newListFleets,

    -- * Request Lenses
    listFleets_nextToken,
    listFleets_filters,
    listFleets_maxResults,

    -- * Destructuring the Response
    ListFleetsResponse (..),
    newListFleetsResponse,

    -- * Response Lenses
    listFleetsResponse_nextToken,
    listFleetsResponse_fleetDetails,
    listFleetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListFleets' smart constructor.
data ListFleets = ListFleets'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListFleets@ again and
    -- assign that token to the request object\'s @nextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- NextToken parameter is set to null.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional filters to limit results.
    --
    -- The filter name @name@ is supported. When filtering, you must use the
    -- complete value of the filtered item. You can use up to three filters.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | When this parameter is used, @ListFleets@ only returns @maxResults@
    -- results in a single page along with a @nextToken@ response element. The
    -- remaining results of the initial request can be seen by sending another
    -- @ListFleets@ request with the returned @nextToken@ value. This value can
    -- be between 1 and 200. If this parameter is not used, then @ListFleets@
    -- returns up to 200 results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFleets_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListFleets@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'filters', 'listFleets_filters' - Optional filters to limit results.
--
-- The filter name @name@ is supported. When filtering, you must use the
-- complete value of the filtered item. You can use up to three filters.
--
-- 'maxResults', 'listFleets_maxResults' - When this parameter is used, @ListFleets@ only returns @maxResults@
-- results in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @ListFleets@ request with the returned @nextToken@ value. This value can
-- be between 1 and 200. If this parameter is not used, then @ListFleets@
-- returns up to 200 results and a @nextToken@ value if applicable.
newListFleets ::
  ListFleets
newListFleets =
  ListFleets'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListFleets@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listFleets_nextToken :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Text)
listFleets_nextToken = Lens.lens (\ListFleets' {nextToken} -> nextToken) (\s@ListFleets' {} a -> s {nextToken = a} :: ListFleets)

-- | Optional filters to limit results.
--
-- The filter name @name@ is supported. When filtering, you must use the
-- complete value of the filtered item. You can use up to three filters.
listFleets_filters :: Lens.Lens' ListFleets (Prelude.Maybe (Prelude.NonEmpty Filter))
listFleets_filters = Lens.lens (\ListFleets' {filters} -> filters) (\s@ListFleets' {} a -> s {filters = a} :: ListFleets) Prelude.. Lens.mapping Lens.coerced

-- | When this parameter is used, @ListFleets@ only returns @maxResults@
-- results in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @ListFleets@ request with the returned @nextToken@ value. This value can
-- be between 1 and 200. If this parameter is not used, then @ListFleets@
-- returns up to 200 results and a @nextToken@ value if applicable.
listFleets_maxResults :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Int)
listFleets_maxResults = Lens.lens (\ListFleets' {maxResults} -> maxResults) (\s@ListFleets' {} a -> s {maxResults = a} :: ListFleets)

instance Core.AWSPager ListFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFleetsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFleetsResponse_fleetDetails Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFleets_nextToken
          Lens..~ rs
          Lens.^? listFleetsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFleets where
  type AWSResponse ListFleets = ListFleetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFleetsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "fleetDetails" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFleets where
  hashWithSalt _salt ListFleets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListFleets where
  rnf ListFleets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListFleets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListFleets where
  toJSON ListFleets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListFleets where
  toPath = Prelude.const "/listFleets"

instance Core.ToQuery ListFleets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFleetsResponse' smart constructor.
data ListFleetsResponse = ListFleetsResponse'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListFleets@ again and
    -- assign that token to the request object\'s @nextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of fleet details meeting the request criteria.
    fleetDetails :: Prelude.Maybe [Fleet],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFleetsResponse_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListFleets@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
--
-- 'fleetDetails', 'listFleetsResponse_fleetDetails' - A list of fleet details meeting the request criteria.
--
-- 'httpStatus', 'listFleetsResponse_httpStatus' - The response's http status code.
newListFleetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFleetsResponse
newListFleetsResponse pHttpStatus_ =
  ListFleetsResponse'
    { nextToken = Prelude.Nothing,
      fleetDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListFleets@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
listFleetsResponse_nextToken :: Lens.Lens' ListFleetsResponse (Prelude.Maybe Prelude.Text)
listFleetsResponse_nextToken = Lens.lens (\ListFleetsResponse' {nextToken} -> nextToken) (\s@ListFleetsResponse' {} a -> s {nextToken = a} :: ListFleetsResponse)

-- | A list of fleet details meeting the request criteria.
listFleetsResponse_fleetDetails :: Lens.Lens' ListFleetsResponse (Prelude.Maybe [Fleet])
listFleetsResponse_fleetDetails = Lens.lens (\ListFleetsResponse' {fleetDetails} -> fleetDetails) (\s@ListFleetsResponse' {} a -> s {fleetDetails = a} :: ListFleetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFleetsResponse_httpStatus :: Lens.Lens' ListFleetsResponse Prelude.Int
listFleetsResponse_httpStatus = Lens.lens (\ListFleetsResponse' {httpStatus} -> httpStatus) (\s@ListFleetsResponse' {} a -> s {httpStatus = a} :: ListFleetsResponse)

instance Prelude.NFData ListFleetsResponse where
  rnf ListFleetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fleetDetails
      `Prelude.seq` Prelude.rnf httpStatus
