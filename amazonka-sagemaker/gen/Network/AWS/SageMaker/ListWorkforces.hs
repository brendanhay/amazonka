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
-- Module      : Network.AWS.SageMaker.ListWorkforces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to list all private and vendor workforces in an AWS
-- Region. Note that you can only have one private workforce per AWS
-- Region.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListWorkforces
  ( -- * Creating a Request
    ListWorkforces (..),
    newListWorkforces,

    -- * Request Lenses
    listWorkforces_sortOrder,
    listWorkforces_nextToken,
    listWorkforces_nameContains,
    listWorkforces_maxResults,
    listWorkforces_sortBy,

    -- * Destructuring the Response
    ListWorkforcesResponse (..),
    newListWorkforcesResponse,

    -- * Response Lenses
    listWorkforcesResponse_nextToken,
    listWorkforcesResponse_httpStatus,
    listWorkforcesResponse_workforces,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListWorkforces' smart constructor.
data ListWorkforces = ListWorkforces'
  { -- | Sort workforces in ascending or descending order.
    sortOrder :: Core.Maybe SortOrder,
    -- | A token to resume pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter you can use to search for workforces using part of the
    -- workforce name.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of workforces returned in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | Sort workforces using the workforce name or creation date.
    sortBy :: Core.Maybe ListWorkforcesSortByOptions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListWorkforces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listWorkforces_sortOrder' - Sort workforces in ascending or descending order.
--
-- 'nextToken', 'listWorkforces_nextToken' - A token to resume pagination.
--
-- 'nameContains', 'listWorkforces_nameContains' - A filter you can use to search for workforces using part of the
-- workforce name.
--
-- 'maxResults', 'listWorkforces_maxResults' - The maximum number of workforces returned in the response.
--
-- 'sortBy', 'listWorkforces_sortBy' - Sort workforces using the workforce name or creation date.
newListWorkforces ::
  ListWorkforces
newListWorkforces =
  ListWorkforces'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | Sort workforces in ascending or descending order.
listWorkforces_sortOrder :: Lens.Lens' ListWorkforces (Core.Maybe SortOrder)
listWorkforces_sortOrder = Lens.lens (\ListWorkforces' {sortOrder} -> sortOrder) (\s@ListWorkforces' {} a -> s {sortOrder = a} :: ListWorkforces)

-- | A token to resume pagination.
listWorkforces_nextToken :: Lens.Lens' ListWorkforces (Core.Maybe Core.Text)
listWorkforces_nextToken = Lens.lens (\ListWorkforces' {nextToken} -> nextToken) (\s@ListWorkforces' {} a -> s {nextToken = a} :: ListWorkforces)

-- | A filter you can use to search for workforces using part of the
-- workforce name.
listWorkforces_nameContains :: Lens.Lens' ListWorkforces (Core.Maybe Core.Text)
listWorkforces_nameContains = Lens.lens (\ListWorkforces' {nameContains} -> nameContains) (\s@ListWorkforces' {} a -> s {nameContains = a} :: ListWorkforces)

-- | The maximum number of workforces returned in the response.
listWorkforces_maxResults :: Lens.Lens' ListWorkforces (Core.Maybe Core.Natural)
listWorkforces_maxResults = Lens.lens (\ListWorkforces' {maxResults} -> maxResults) (\s@ListWorkforces' {} a -> s {maxResults = a} :: ListWorkforces)

-- | Sort workforces using the workforce name or creation date.
listWorkforces_sortBy :: Lens.Lens' ListWorkforces (Core.Maybe ListWorkforcesSortByOptions)
listWorkforces_sortBy = Lens.lens (\ListWorkforces' {sortBy} -> sortBy) (\s@ListWorkforces' {} a -> s {sortBy = a} :: ListWorkforces)

instance Core.AWSPager ListWorkforces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkforcesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listWorkforcesResponse_workforces) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listWorkforces_nextToken
          Lens..~ rs
          Lens.^? listWorkforcesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListWorkforces where
  type
    AWSResponse ListWorkforces =
      ListWorkforcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkforcesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "Workforces" Core..!@ Core.mempty)
      )

instance Core.Hashable ListWorkforces

instance Core.NFData ListWorkforces

instance Core.ToHeaders ListWorkforces where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListWorkforces" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListWorkforces where
  toJSON ListWorkforces' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.ToPath ListWorkforces where
  toPath = Core.const "/"

instance Core.ToQuery ListWorkforces where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListWorkforcesResponse' smart constructor.
data ListWorkforcesResponse = ListWorkforcesResponse'
  { -- | A token to resume pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list containing information about your workforce.
    workforces :: [Workforce]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListWorkforcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkforcesResponse_nextToken' - A token to resume pagination.
--
-- 'httpStatus', 'listWorkforcesResponse_httpStatus' - The response's http status code.
--
-- 'workforces', 'listWorkforcesResponse_workforces' - A list containing information about your workforce.
newListWorkforcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListWorkforcesResponse
newListWorkforcesResponse pHttpStatus_ =
  ListWorkforcesResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      workforces = Core.mempty
    }

-- | A token to resume pagination.
listWorkforcesResponse_nextToken :: Lens.Lens' ListWorkforcesResponse (Core.Maybe Core.Text)
listWorkforcesResponse_nextToken = Lens.lens (\ListWorkforcesResponse' {nextToken} -> nextToken) (\s@ListWorkforcesResponse' {} a -> s {nextToken = a} :: ListWorkforcesResponse)

-- | The response's http status code.
listWorkforcesResponse_httpStatus :: Lens.Lens' ListWorkforcesResponse Core.Int
listWorkforcesResponse_httpStatus = Lens.lens (\ListWorkforcesResponse' {httpStatus} -> httpStatus) (\s@ListWorkforcesResponse' {} a -> s {httpStatus = a} :: ListWorkforcesResponse)

-- | A list containing information about your workforce.
listWorkforcesResponse_workforces :: Lens.Lens' ListWorkforcesResponse [Workforce]
listWorkforcesResponse_workforces = Lens.lens (\ListWorkforcesResponse' {workforces} -> workforces) (\s@ListWorkforcesResponse' {} a -> s {workforces = a} :: ListWorkforcesResponse) Core.. Lens._Coerce

instance Core.NFData ListWorkforcesResponse
