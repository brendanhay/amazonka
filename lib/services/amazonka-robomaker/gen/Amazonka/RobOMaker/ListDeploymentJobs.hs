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
-- Module      : Amazonka.RobOMaker.ListDeploymentJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of deployment jobs for a fleet. You can optionally
-- provide filters to retrieve specific deployment jobs.
--
-- This operation returns paginated results.
module Amazonka.RobOMaker.ListDeploymentJobs
  ( -- * Creating a Request
    ListDeploymentJobs (..),
    newListDeploymentJobs,

    -- * Request Lenses
    listDeploymentJobs_filters,
    listDeploymentJobs_nextToken,
    listDeploymentJobs_maxResults,

    -- * Destructuring the Response
    ListDeploymentJobsResponse (..),
    newListDeploymentJobsResponse,

    -- * Response Lenses
    listDeploymentJobsResponse_deploymentJobs,
    listDeploymentJobsResponse_nextToken,
    listDeploymentJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListDeploymentJobs' smart constructor.
data ListDeploymentJobs = ListDeploymentJobs'
  { -- | Optional filters to limit results.
    --
    -- The filter names @status@ and @fleetName@ are supported. When filtering,
    -- you must use the complete value of the filtered item. You can use up to
    -- three filters, but they must be for the same named item. For example, if
    -- you are looking for items with the status @InProgress@ or the status
    -- @Pending@.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListDeploymentJobs@
    -- again and assign that token to the request object\'s @nextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | When this parameter is used, @ListDeploymentJobs@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListDeploymentJobs@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 200. If this
    -- parameter is not used, then @ListDeploymentJobs@ returns up to 200
    -- results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listDeploymentJobs_filters' - Optional filters to limit results.
--
-- The filter names @status@ and @fleetName@ are supported. When filtering,
-- you must use the complete value of the filtered item. You can use up to
-- three filters, but they must be for the same named item. For example, if
-- you are looking for items with the status @InProgress@ or the status
-- @Pending@.
--
-- 'nextToken', 'listDeploymentJobs_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListDeploymentJobs@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
--
-- 'maxResults', 'listDeploymentJobs_maxResults' - When this parameter is used, @ListDeploymentJobs@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListDeploymentJobs@ request with the returned
-- @nextToken@ value. This value can be between 1 and 200. If this
-- parameter is not used, then @ListDeploymentJobs@ returns up to 200
-- results and a @nextToken@ value if applicable.
newListDeploymentJobs ::
  ListDeploymentJobs
newListDeploymentJobs =
  ListDeploymentJobs'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Optional filters to limit results.
--
-- The filter names @status@ and @fleetName@ are supported. When filtering,
-- you must use the complete value of the filtered item. You can use up to
-- three filters, but they must be for the same named item. For example, if
-- you are looking for items with the status @InProgress@ or the status
-- @Pending@.
listDeploymentJobs_filters :: Lens.Lens' ListDeploymentJobs (Prelude.Maybe (Prelude.NonEmpty Filter))
listDeploymentJobs_filters = Lens.lens (\ListDeploymentJobs' {filters} -> filters) (\s@ListDeploymentJobs' {} a -> s {filters = a} :: ListDeploymentJobs) Prelude.. Lens.mapping Lens.coerced

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListDeploymentJobs@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
listDeploymentJobs_nextToken :: Lens.Lens' ListDeploymentJobs (Prelude.Maybe Prelude.Text)
listDeploymentJobs_nextToken = Lens.lens (\ListDeploymentJobs' {nextToken} -> nextToken) (\s@ListDeploymentJobs' {} a -> s {nextToken = a} :: ListDeploymentJobs)

-- | When this parameter is used, @ListDeploymentJobs@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListDeploymentJobs@ request with the returned
-- @nextToken@ value. This value can be between 1 and 200. If this
-- parameter is not used, then @ListDeploymentJobs@ returns up to 200
-- results and a @nextToken@ value if applicable.
listDeploymentJobs_maxResults :: Lens.Lens' ListDeploymentJobs (Prelude.Maybe Prelude.Int)
listDeploymentJobs_maxResults = Lens.lens (\ListDeploymentJobs' {maxResults} -> maxResults) (\s@ListDeploymentJobs' {} a -> s {maxResults = a} :: ListDeploymentJobs)

instance Core.AWSPager ListDeploymentJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeploymentJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeploymentJobsResponse_deploymentJobs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDeploymentJobs_nextToken
          Lens..~ rs
          Lens.^? listDeploymentJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDeploymentJobs where
  type
    AWSResponse ListDeploymentJobs =
      ListDeploymentJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentJobsResponse'
            Prelude.<$> (x Core..?> "deploymentJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeploymentJobs where
  hashWithSalt salt' ListDeploymentJobs' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters

instance Prelude.NFData ListDeploymentJobs where
  rnf ListDeploymentJobs' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders ListDeploymentJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDeploymentJobs where
  toJSON ListDeploymentJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListDeploymentJobs where
  toPath = Prelude.const "/listDeploymentJobs"

instance Core.ToQuery ListDeploymentJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDeploymentJobsResponse' smart constructor.
data ListDeploymentJobsResponse = ListDeploymentJobsResponse'
  { -- | A list of deployment jobs that meet the criteria of the request.
    deploymentJobs :: Prelude.Maybe [DeploymentJob],
    -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListDeploymentJobs@
    -- again and assign that token to the request object\'s @nextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentJobs', 'listDeploymentJobsResponse_deploymentJobs' - A list of deployment jobs that meet the criteria of the request.
--
-- 'nextToken', 'listDeploymentJobsResponse_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListDeploymentJobs@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
--
-- 'httpStatus', 'listDeploymentJobsResponse_httpStatus' - The response's http status code.
newListDeploymentJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentJobsResponse
newListDeploymentJobsResponse pHttpStatus_ =
  ListDeploymentJobsResponse'
    { deploymentJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of deployment jobs that meet the criteria of the request.
listDeploymentJobsResponse_deploymentJobs :: Lens.Lens' ListDeploymentJobsResponse (Prelude.Maybe [DeploymentJob])
listDeploymentJobsResponse_deploymentJobs = Lens.lens (\ListDeploymentJobsResponse' {deploymentJobs} -> deploymentJobs) (\s@ListDeploymentJobsResponse' {} a -> s {deploymentJobs = a} :: ListDeploymentJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListDeploymentJobs@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
listDeploymentJobsResponse_nextToken :: Lens.Lens' ListDeploymentJobsResponse (Prelude.Maybe Prelude.Text)
listDeploymentJobsResponse_nextToken = Lens.lens (\ListDeploymentJobsResponse' {nextToken} -> nextToken) (\s@ListDeploymentJobsResponse' {} a -> s {nextToken = a} :: ListDeploymentJobsResponse)

-- | The response's http status code.
listDeploymentJobsResponse_httpStatus :: Lens.Lens' ListDeploymentJobsResponse Prelude.Int
listDeploymentJobsResponse_httpStatus = Lens.lens (\ListDeploymentJobsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentJobsResponse' {} a -> s {httpStatus = a} :: ListDeploymentJobsResponse)

instance Prelude.NFData ListDeploymentJobsResponse where
  rnf ListDeploymentJobsResponse' {..} =
    Prelude.rnf deploymentJobs
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
