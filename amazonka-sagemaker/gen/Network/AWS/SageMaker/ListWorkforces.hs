{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListWorkforces' smart constructor.
data ListWorkforces = ListWorkforces'
  { -- | Sort workforces in ascending or descending order.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A token to resume pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter you can use to search for workforces using part of the
    -- workforce name.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of workforces returned in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Sort workforces using the workforce name or creation date.
    sortBy :: Prelude.Maybe ListWorkforcesSortByOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | Sort workforces in ascending or descending order.
listWorkforces_sortOrder :: Lens.Lens' ListWorkforces (Prelude.Maybe SortOrder)
listWorkforces_sortOrder = Lens.lens (\ListWorkforces' {sortOrder} -> sortOrder) (\s@ListWorkforces' {} a -> s {sortOrder = a} :: ListWorkforces)

-- | A token to resume pagination.
listWorkforces_nextToken :: Lens.Lens' ListWorkforces (Prelude.Maybe Prelude.Text)
listWorkforces_nextToken = Lens.lens (\ListWorkforces' {nextToken} -> nextToken) (\s@ListWorkforces' {} a -> s {nextToken = a} :: ListWorkforces)

-- | A filter you can use to search for workforces using part of the
-- workforce name.
listWorkforces_nameContains :: Lens.Lens' ListWorkforces (Prelude.Maybe Prelude.Text)
listWorkforces_nameContains = Lens.lens (\ListWorkforces' {nameContains} -> nameContains) (\s@ListWorkforces' {} a -> s {nameContains = a} :: ListWorkforces)

-- | The maximum number of workforces returned in the response.
listWorkforces_maxResults :: Lens.Lens' ListWorkforces (Prelude.Maybe Prelude.Natural)
listWorkforces_maxResults = Lens.lens (\ListWorkforces' {maxResults} -> maxResults) (\s@ListWorkforces' {} a -> s {maxResults = a} :: ListWorkforces)

-- | Sort workforces using the workforce name or creation date.
listWorkforces_sortBy :: Lens.Lens' ListWorkforces (Prelude.Maybe ListWorkforcesSortByOptions)
listWorkforces_sortBy = Lens.lens (\ListWorkforces' {sortBy} -> sortBy) (\s@ListWorkforces' {} a -> s {sortBy = a} :: ListWorkforces)

instance Pager.AWSPager ListWorkforces where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listWorkforcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        (rs Lens.^. listWorkforcesResponse_workforces) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listWorkforces_nextToken
          Lens..~ rs
          Lens.^? listWorkforcesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListWorkforces where
  type Rs ListWorkforces = ListWorkforcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkforcesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "Workforces"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListWorkforces

instance Prelude.NFData ListWorkforces

instance Prelude.ToHeaders ListWorkforces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.ListWorkforces" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListWorkforces where
  toJSON ListWorkforces' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SortOrder" Prelude..=) Prelude.<$> sortOrder,
            ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("NameContains" Prelude..=) Prelude.<$> nameContains,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("SortBy" Prelude..=) Prelude.<$> sortBy
          ]
      )

instance Prelude.ToPath ListWorkforces where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListWorkforces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorkforcesResponse' smart constructor.
data ListWorkforcesResponse = ListWorkforcesResponse'
  { -- | A token to resume pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list containing information about your workforce.
    workforces :: [Workforce]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListWorkforcesResponse
newListWorkforcesResponse pHttpStatus_ =
  ListWorkforcesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      workforces = Prelude.mempty
    }

-- | A token to resume pagination.
listWorkforcesResponse_nextToken :: Lens.Lens' ListWorkforcesResponse (Prelude.Maybe Prelude.Text)
listWorkforcesResponse_nextToken = Lens.lens (\ListWorkforcesResponse' {nextToken} -> nextToken) (\s@ListWorkforcesResponse' {} a -> s {nextToken = a} :: ListWorkforcesResponse)

-- | The response's http status code.
listWorkforcesResponse_httpStatus :: Lens.Lens' ListWorkforcesResponse Prelude.Int
listWorkforcesResponse_httpStatus = Lens.lens (\ListWorkforcesResponse' {httpStatus} -> httpStatus) (\s@ListWorkforcesResponse' {} a -> s {httpStatus = a} :: ListWorkforcesResponse)

-- | A list containing information about your workforce.
listWorkforcesResponse_workforces :: Lens.Lens' ListWorkforcesResponse [Workforce]
listWorkforcesResponse_workforces = Lens.lens (\ListWorkforcesResponse' {workforces} -> workforces) (\s@ListWorkforcesResponse' {} a -> s {workforces = a} :: ListWorkforcesResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListWorkforcesResponse
