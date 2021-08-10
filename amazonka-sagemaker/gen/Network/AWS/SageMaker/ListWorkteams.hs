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
-- Module      : Network.AWS.SageMaker.ListWorkteams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of private work teams that you have defined in a region. The
-- list may be empty if no work team satisfies the filter specified in the
-- @NameContains@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListWorkteams
  ( -- * Creating a Request
    ListWorkteams (..),
    newListWorkteams,

    -- * Request Lenses
    listWorkteams_sortOrder,
    listWorkteams_nextToken,
    listWorkteams_nameContains,
    listWorkteams_maxResults,
    listWorkteams_sortBy,

    -- * Destructuring the Response
    ListWorkteamsResponse (..),
    newListWorkteamsResponse,

    -- * Response Lenses
    listWorkteamsResponse_nextToken,
    listWorkteamsResponse_httpStatus,
    listWorkteamsResponse_workteams,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListWorkteams' smart constructor.
data ListWorkteams = ListWorkteams'
  { -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the result of the previous @ListWorkteams@ request was truncated, the
    -- response includes a @NextToken@. To retrieve the next set of labeling
    -- jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string in the work team\'s name. This filter returns only work teams
    -- whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of work teams to return in each page of the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe ListWorkteamsSortByOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkteams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listWorkteams_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'nextToken', 'listWorkteams_nextToken' - If the result of the previous @ListWorkteams@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of labeling
-- jobs, use the token in the next request.
--
-- 'nameContains', 'listWorkteams_nameContains' - A string in the work team\'s name. This filter returns only work teams
-- whose name contains the specified string.
--
-- 'maxResults', 'listWorkteams_maxResults' - The maximum number of work teams to return in each page of the response.
--
-- 'sortBy', 'listWorkteams_sortBy' - The field to sort results by. The default is @CreationTime@.
newListWorkteams ::
  ListWorkteams
newListWorkteams =
  ListWorkteams'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | The sort order for results. The default is @Ascending@.
listWorkteams_sortOrder :: Lens.Lens' ListWorkteams (Prelude.Maybe SortOrder)
listWorkteams_sortOrder = Lens.lens (\ListWorkteams' {sortOrder} -> sortOrder) (\s@ListWorkteams' {} a -> s {sortOrder = a} :: ListWorkteams)

-- | If the result of the previous @ListWorkteams@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of labeling
-- jobs, use the token in the next request.
listWorkteams_nextToken :: Lens.Lens' ListWorkteams (Prelude.Maybe Prelude.Text)
listWorkteams_nextToken = Lens.lens (\ListWorkteams' {nextToken} -> nextToken) (\s@ListWorkteams' {} a -> s {nextToken = a} :: ListWorkteams)

-- | A string in the work team\'s name. This filter returns only work teams
-- whose name contains the specified string.
listWorkteams_nameContains :: Lens.Lens' ListWorkteams (Prelude.Maybe Prelude.Text)
listWorkteams_nameContains = Lens.lens (\ListWorkteams' {nameContains} -> nameContains) (\s@ListWorkteams' {} a -> s {nameContains = a} :: ListWorkteams)

-- | The maximum number of work teams to return in each page of the response.
listWorkteams_maxResults :: Lens.Lens' ListWorkteams (Prelude.Maybe Prelude.Natural)
listWorkteams_maxResults = Lens.lens (\ListWorkteams' {maxResults} -> maxResults) (\s@ListWorkteams' {} a -> s {maxResults = a} :: ListWorkteams)

-- | The field to sort results by. The default is @CreationTime@.
listWorkteams_sortBy :: Lens.Lens' ListWorkteams (Prelude.Maybe ListWorkteamsSortByOptions)
listWorkteams_sortBy = Lens.lens (\ListWorkteams' {sortBy} -> sortBy) (\s@ListWorkteams' {} a -> s {sortBy = a} :: ListWorkteams)

instance Core.AWSPager ListWorkteams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkteamsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listWorkteamsResponse_workteams) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorkteams_nextToken
          Lens..~ rs
          Lens.^? listWorkteamsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListWorkteams where
  type
    AWSResponse ListWorkteams =
      ListWorkteamsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkteamsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Workteams" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListWorkteams

instance Prelude.NFData ListWorkteams

instance Core.ToHeaders ListWorkteams where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListWorkteams" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListWorkteams where
  toJSON ListWorkteams' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListWorkteams where
  toPath = Prelude.const "/"

instance Core.ToQuery ListWorkteams where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorkteamsResponse' smart constructor.
data ListWorkteamsResponse = ListWorkteamsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of work teams, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @Workteam@ objects, each describing a work team.
    workteams :: [Workteam]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkteamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkteamsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of work teams, use it in the subsequent request.
--
-- 'httpStatus', 'listWorkteamsResponse_httpStatus' - The response's http status code.
--
-- 'workteams', 'listWorkteamsResponse_workteams' - An array of @Workteam@ objects, each describing a work team.
newListWorkteamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkteamsResponse
newListWorkteamsResponse pHttpStatus_ =
  ListWorkteamsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      workteams = Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of work teams, use it in the subsequent request.
listWorkteamsResponse_nextToken :: Lens.Lens' ListWorkteamsResponse (Prelude.Maybe Prelude.Text)
listWorkteamsResponse_nextToken = Lens.lens (\ListWorkteamsResponse' {nextToken} -> nextToken) (\s@ListWorkteamsResponse' {} a -> s {nextToken = a} :: ListWorkteamsResponse)

-- | The response's http status code.
listWorkteamsResponse_httpStatus :: Lens.Lens' ListWorkteamsResponse Prelude.Int
listWorkteamsResponse_httpStatus = Lens.lens (\ListWorkteamsResponse' {httpStatus} -> httpStatus) (\s@ListWorkteamsResponse' {} a -> s {httpStatus = a} :: ListWorkteamsResponse)

-- | An array of @Workteam@ objects, each describing a work team.
listWorkteamsResponse_workteams :: Lens.Lens' ListWorkteamsResponse [Workteam]
listWorkteamsResponse_workteams = Lens.lens (\ListWorkteamsResponse' {workteams} -> workteams) (\s@ListWorkteamsResponse' {} a -> s {workteams = a} :: ListWorkteamsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListWorkteamsResponse
