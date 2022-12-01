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
-- Module      : Amazonka.RobOMaker.ListRobotApplications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of robot application. You can optionally provide filters
-- to retrieve specific robot applications.
--
-- This operation returns paginated results.
module Amazonka.RobOMaker.ListRobotApplications
  ( -- * Creating a Request
    ListRobotApplications (..),
    newListRobotApplications,

    -- * Request Lenses
    listRobotApplications_nextToken,
    listRobotApplications_filters,
    listRobotApplications_versionQualifier,
    listRobotApplications_maxResults,

    -- * Destructuring the Response
    ListRobotApplicationsResponse (..),
    newListRobotApplicationsResponse,

    -- * Response Lenses
    listRobotApplicationsResponse_nextToken,
    listRobotApplicationsResponse_robotApplicationSummaries,
    listRobotApplicationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListRobotApplications' smart constructor.
data ListRobotApplications = ListRobotApplications'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListRobotApplications@
    -- again and assign that token to the request object\'s @nextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional filters to limit results.
    --
    -- The filter name @name@ is supported. When filtering, you must use the
    -- complete value of the filtered item. You can use up to three filters.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | The version qualifier of the robot application.
    versionQualifier :: Prelude.Maybe Prelude.Text,
    -- | When this parameter is used, @ListRobotApplications@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListRobotApplications@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter is not used, then @ListRobotApplications@ returns up to 100
    -- results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRobotApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRobotApplications_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListRobotApplications@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
--
-- 'filters', 'listRobotApplications_filters' - Optional filters to limit results.
--
-- The filter name @name@ is supported. When filtering, you must use the
-- complete value of the filtered item. You can use up to three filters.
--
-- 'versionQualifier', 'listRobotApplications_versionQualifier' - The version qualifier of the robot application.
--
-- 'maxResults', 'listRobotApplications_maxResults' - When this parameter is used, @ListRobotApplications@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListRobotApplications@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListRobotApplications@ returns up to 100
-- results and a @nextToken@ value if applicable.
newListRobotApplications ::
  ListRobotApplications
newListRobotApplications =
  ListRobotApplications'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      versionQualifier = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListRobotApplications@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
listRobotApplications_nextToken :: Lens.Lens' ListRobotApplications (Prelude.Maybe Prelude.Text)
listRobotApplications_nextToken = Lens.lens (\ListRobotApplications' {nextToken} -> nextToken) (\s@ListRobotApplications' {} a -> s {nextToken = a} :: ListRobotApplications)

-- | Optional filters to limit results.
--
-- The filter name @name@ is supported. When filtering, you must use the
-- complete value of the filtered item. You can use up to three filters.
listRobotApplications_filters :: Lens.Lens' ListRobotApplications (Prelude.Maybe (Prelude.NonEmpty Filter))
listRobotApplications_filters = Lens.lens (\ListRobotApplications' {filters} -> filters) (\s@ListRobotApplications' {} a -> s {filters = a} :: ListRobotApplications) Prelude.. Lens.mapping Lens.coerced

-- | The version qualifier of the robot application.
listRobotApplications_versionQualifier :: Lens.Lens' ListRobotApplications (Prelude.Maybe Prelude.Text)
listRobotApplications_versionQualifier = Lens.lens (\ListRobotApplications' {versionQualifier} -> versionQualifier) (\s@ListRobotApplications' {} a -> s {versionQualifier = a} :: ListRobotApplications)

-- | When this parameter is used, @ListRobotApplications@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListRobotApplications@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListRobotApplications@ returns up to 100
-- results and a @nextToken@ value if applicable.
listRobotApplications_maxResults :: Lens.Lens' ListRobotApplications (Prelude.Maybe Prelude.Int)
listRobotApplications_maxResults = Lens.lens (\ListRobotApplications' {maxResults} -> maxResults) (\s@ListRobotApplications' {} a -> s {maxResults = a} :: ListRobotApplications)

instance Core.AWSPager ListRobotApplications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRobotApplicationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRobotApplicationsResponse_robotApplicationSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRobotApplications_nextToken
          Lens..~ rs
          Lens.^? listRobotApplicationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRobotApplications where
  type
    AWSResponse ListRobotApplications =
      ListRobotApplicationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRobotApplicationsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "robotApplicationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRobotApplications where
  hashWithSalt _salt ListRobotApplications' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` versionQualifier
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListRobotApplications where
  rnf ListRobotApplications' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf versionQualifier
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListRobotApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRobotApplications where
  toJSON ListRobotApplications' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filters" Core..=) Prelude.<$> filters,
            ("versionQualifier" Core..=)
              Prelude.<$> versionQualifier,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListRobotApplications where
  toPath = Prelude.const "/listRobotApplications"

instance Core.ToQuery ListRobotApplications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRobotApplicationsResponse' smart constructor.
data ListRobotApplicationsResponse = ListRobotApplicationsResponse'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListRobotApplications@
    -- again and assign that token to the request object\'s @nextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of robot application summaries that meet the criteria of the
    -- request.
    robotApplicationSummaries :: Prelude.Maybe [RobotApplicationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRobotApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRobotApplicationsResponse_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListRobotApplications@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
--
-- 'robotApplicationSummaries', 'listRobotApplicationsResponse_robotApplicationSummaries' - A list of robot application summaries that meet the criteria of the
-- request.
--
-- 'httpStatus', 'listRobotApplicationsResponse_httpStatus' - The response's http status code.
newListRobotApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRobotApplicationsResponse
newListRobotApplicationsResponse pHttpStatus_ =
  ListRobotApplicationsResponse'
    { nextToken =
        Prelude.Nothing,
      robotApplicationSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListRobotApplications@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
listRobotApplicationsResponse_nextToken :: Lens.Lens' ListRobotApplicationsResponse (Prelude.Maybe Prelude.Text)
listRobotApplicationsResponse_nextToken = Lens.lens (\ListRobotApplicationsResponse' {nextToken} -> nextToken) (\s@ListRobotApplicationsResponse' {} a -> s {nextToken = a} :: ListRobotApplicationsResponse)

-- | A list of robot application summaries that meet the criteria of the
-- request.
listRobotApplicationsResponse_robotApplicationSummaries :: Lens.Lens' ListRobotApplicationsResponse (Prelude.Maybe [RobotApplicationSummary])
listRobotApplicationsResponse_robotApplicationSummaries = Lens.lens (\ListRobotApplicationsResponse' {robotApplicationSummaries} -> robotApplicationSummaries) (\s@ListRobotApplicationsResponse' {} a -> s {robotApplicationSummaries = a} :: ListRobotApplicationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRobotApplicationsResponse_httpStatus :: Lens.Lens' ListRobotApplicationsResponse Prelude.Int
listRobotApplicationsResponse_httpStatus = Lens.lens (\ListRobotApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListRobotApplicationsResponse' {} a -> s {httpStatus = a} :: ListRobotApplicationsResponse)

instance Prelude.NFData ListRobotApplicationsResponse where
  rnf ListRobotApplicationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf robotApplicationSummaries
      `Prelude.seq` Prelude.rnf httpStatus
