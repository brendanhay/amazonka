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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    listRobotApplications_filters,
    listRobotApplications_maxResults,
    listRobotApplications_nextToken,
    listRobotApplications_versionQualifier,

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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListRobotApplications' smart constructor.
data ListRobotApplications = ListRobotApplications'
  { -- | Optional filters to limit results.
    --
    -- The filter name @name@ is supported. When filtering, you must use the
    -- complete value of the filtered item. You can use up to three filters.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | When this parameter is used, @ListRobotApplications@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListRobotApplications@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter is not used, then @ListRobotApplications@ returns up to 100
    -- results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListRobotApplications@
    -- again and assign that token to the request object\'s @nextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The version qualifier of the robot application.
    versionQualifier :: Prelude.Maybe Prelude.Text
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
-- 'filters', 'listRobotApplications_filters' - Optional filters to limit results.
--
-- The filter name @name@ is supported. When filtering, you must use the
-- complete value of the filtered item. You can use up to three filters.
--
-- 'maxResults', 'listRobotApplications_maxResults' - When this parameter is used, @ListRobotApplications@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListRobotApplications@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListRobotApplications@ returns up to 100
-- results and a @nextToken@ value if applicable.
--
-- 'nextToken', 'listRobotApplications_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListRobotApplications@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
--
-- 'versionQualifier', 'listRobotApplications_versionQualifier' - The version qualifier of the robot application.
newListRobotApplications ::
  ListRobotApplications
newListRobotApplications =
  ListRobotApplications'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      versionQualifier = Prelude.Nothing
    }

-- | Optional filters to limit results.
--
-- The filter name @name@ is supported. When filtering, you must use the
-- complete value of the filtered item. You can use up to three filters.
listRobotApplications_filters :: Lens.Lens' ListRobotApplications (Prelude.Maybe (Prelude.NonEmpty Filter))
listRobotApplications_filters = Lens.lens (\ListRobotApplications' {filters} -> filters) (\s@ListRobotApplications' {} a -> s {filters = a} :: ListRobotApplications) Prelude.. Lens.mapping Lens.coerced

-- | When this parameter is used, @ListRobotApplications@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListRobotApplications@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListRobotApplications@ returns up to 100
-- results and a @nextToken@ value if applicable.
listRobotApplications_maxResults :: Lens.Lens' ListRobotApplications (Prelude.Maybe Prelude.Int)
listRobotApplications_maxResults = Lens.lens (\ListRobotApplications' {maxResults} -> maxResults) (\s@ListRobotApplications' {} a -> s {maxResults = a} :: ListRobotApplications)

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListRobotApplications@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
listRobotApplications_nextToken :: Lens.Lens' ListRobotApplications (Prelude.Maybe Prelude.Text)
listRobotApplications_nextToken = Lens.lens (\ListRobotApplications' {nextToken} -> nextToken) (\s@ListRobotApplications' {} a -> s {nextToken = a} :: ListRobotApplications)

-- | The version qualifier of the robot application.
listRobotApplications_versionQualifier :: Lens.Lens' ListRobotApplications (Prelude.Maybe Prelude.Text)
listRobotApplications_versionQualifier = Lens.lens (\ListRobotApplications' {versionQualifier} -> versionQualifier) (\s@ListRobotApplications' {} a -> s {versionQualifier = a} :: ListRobotApplications)

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
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "robotApplicationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRobotApplications where
  hashWithSalt _salt ListRobotApplications' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` versionQualifier

instance Prelude.NFData ListRobotApplications where
  rnf ListRobotApplications' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf versionQualifier

instance Data.ToHeaders ListRobotApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRobotApplications where
  toJSON ListRobotApplications' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("versionQualifier" Data..=)
              Prelude.<$> versionQualifier
          ]
      )

instance Data.ToPath ListRobotApplications where
  toPath = Prelude.const "/listRobotApplications"

instance Data.ToQuery ListRobotApplications where
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
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf robotApplicationSummaries `Prelude.seq`
        Prelude.rnf httpStatus
