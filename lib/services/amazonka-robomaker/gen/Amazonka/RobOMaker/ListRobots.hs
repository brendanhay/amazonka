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
-- Module      : Amazonka.RobOMaker.ListRobots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of robots. You can optionally provide filters to retrieve
-- specific robots.
--
-- This operation returns paginated results.
module Amazonka.RobOMaker.ListRobots
  ( -- * Creating a Request
    ListRobots (..),
    newListRobots,

    -- * Request Lenses
    listRobots_nextToken,
    listRobots_filters,
    listRobots_maxResults,

    -- * Destructuring the Response
    ListRobotsResponse (..),
    newListRobotsResponse,

    -- * Response Lenses
    listRobotsResponse_nextToken,
    listRobotsResponse_robots,
    listRobotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListRobots' smart constructor.
data ListRobots = ListRobots'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListRobots@ again and
    -- assign that token to the request object\'s @nextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional filters to limit results.
    --
    -- The filter names @status@ and @fleetName@ are supported. When filtering,
    -- you must use the complete value of the filtered item. You can use up to
    -- three filters, but they must be for the same named item. For example, if
    -- you are looking for items with the status @Registered@ or the status
    -- @Available@.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | When this parameter is used, @ListRobots@ only returns @maxResults@
    -- results in a single page along with a @nextToken@ response element. The
    -- remaining results of the initial request can be seen by sending another
    -- @ListRobots@ request with the returned @nextToken@ value. This value can
    -- be between 1 and 200. If this parameter is not used, then @ListRobots@
    -- returns up to 200 results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRobots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRobots_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListRobots@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
--
-- 'filters', 'listRobots_filters' - Optional filters to limit results.
--
-- The filter names @status@ and @fleetName@ are supported. When filtering,
-- you must use the complete value of the filtered item. You can use up to
-- three filters, but they must be for the same named item. For example, if
-- you are looking for items with the status @Registered@ or the status
-- @Available@.
--
-- 'maxResults', 'listRobots_maxResults' - When this parameter is used, @ListRobots@ only returns @maxResults@
-- results in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @ListRobots@ request with the returned @nextToken@ value. This value can
-- be between 1 and 200. If this parameter is not used, then @ListRobots@
-- returns up to 200 results and a @nextToken@ value if applicable.
newListRobots ::
  ListRobots
newListRobots =
  ListRobots'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListRobots@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
listRobots_nextToken :: Lens.Lens' ListRobots (Prelude.Maybe Prelude.Text)
listRobots_nextToken = Lens.lens (\ListRobots' {nextToken} -> nextToken) (\s@ListRobots' {} a -> s {nextToken = a} :: ListRobots)

-- | Optional filters to limit results.
--
-- The filter names @status@ and @fleetName@ are supported. When filtering,
-- you must use the complete value of the filtered item. You can use up to
-- three filters, but they must be for the same named item. For example, if
-- you are looking for items with the status @Registered@ or the status
-- @Available@.
listRobots_filters :: Lens.Lens' ListRobots (Prelude.Maybe (Prelude.NonEmpty Filter))
listRobots_filters = Lens.lens (\ListRobots' {filters} -> filters) (\s@ListRobots' {} a -> s {filters = a} :: ListRobots) Prelude.. Lens.mapping Lens.coerced

-- | When this parameter is used, @ListRobots@ only returns @maxResults@
-- results in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @ListRobots@ request with the returned @nextToken@ value. This value can
-- be between 1 and 200. If this parameter is not used, then @ListRobots@
-- returns up to 200 results and a @nextToken@ value if applicable.
listRobots_maxResults :: Lens.Lens' ListRobots (Prelude.Maybe Prelude.Int)
listRobots_maxResults = Lens.lens (\ListRobots' {maxResults} -> maxResults) (\s@ListRobots' {} a -> s {maxResults = a} :: ListRobots)

instance Core.AWSPager ListRobots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRobotsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRobotsResponse_robots Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRobots_nextToken
          Lens..~ rs
          Lens.^? listRobotsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListRobots where
  type AWSResponse ListRobots = ListRobotsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRobotsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "robots" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRobots where
  hashWithSalt _salt ListRobots' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListRobots where
  rnf ListRobots' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListRobots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRobots where
  toJSON ListRobots' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListRobots where
  toPath = Prelude.const "/listRobots"

instance Core.ToQuery ListRobots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRobotsResponse' smart constructor.
data ListRobotsResponse = ListRobotsResponse'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListRobots@ again and
    -- assign that token to the request object\'s @nextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of robots that meet the criteria of the request.
    robots :: Prelude.Maybe [Robot],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRobotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRobotsResponse_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListRobots@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
--
-- 'robots', 'listRobotsResponse_robots' - A list of robots that meet the criteria of the request.
--
-- 'httpStatus', 'listRobotsResponse_httpStatus' - The response's http status code.
newListRobotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRobotsResponse
newListRobotsResponse pHttpStatus_ =
  ListRobotsResponse'
    { nextToken = Prelude.Nothing,
      robots = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListRobots@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
listRobotsResponse_nextToken :: Lens.Lens' ListRobotsResponse (Prelude.Maybe Prelude.Text)
listRobotsResponse_nextToken = Lens.lens (\ListRobotsResponse' {nextToken} -> nextToken) (\s@ListRobotsResponse' {} a -> s {nextToken = a} :: ListRobotsResponse)

-- | A list of robots that meet the criteria of the request.
listRobotsResponse_robots :: Lens.Lens' ListRobotsResponse (Prelude.Maybe [Robot])
listRobotsResponse_robots = Lens.lens (\ListRobotsResponse' {robots} -> robots) (\s@ListRobotsResponse' {} a -> s {robots = a} :: ListRobotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRobotsResponse_httpStatus :: Lens.Lens' ListRobotsResponse Prelude.Int
listRobotsResponse_httpStatus = Lens.lens (\ListRobotsResponse' {httpStatus} -> httpStatus) (\s@ListRobotsResponse' {} a -> s {httpStatus = a} :: ListRobotsResponse)

instance Prelude.NFData ListRobotsResponse where
  rnf ListRobotsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf robots
      `Prelude.seq` Prelude.rnf httpStatus
