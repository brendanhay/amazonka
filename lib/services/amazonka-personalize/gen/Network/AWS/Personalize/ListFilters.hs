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
-- Module      : Network.AWS.Personalize.ListFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all filters that belong to a given dataset group.
--
-- This operation returns paginated results.
module Network.AWS.Personalize.ListFilters
  ( -- * Creating a Request
    ListFilters (..),
    newListFilters,

    -- * Request Lenses
    listFilters_nextToken,
    listFilters_datasetGroupArn,
    listFilters_maxResults,

    -- * Destructuring the Response
    ListFiltersResponse (..),
    newListFiltersResponse,

    -- * Response Lenses
    listFiltersResponse_filters,
    listFiltersResponse_nextToken,
    listFiltersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Personalize.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFilters' smart constructor.
data ListFilters = ListFilters'
  { -- | A token returned from the previous call to @ListFilters@ for getting the
    -- next set of filters (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the dataset group that contains the filters.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of filters to return.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listFilters_nextToken' - A token returned from the previous call to @ListFilters@ for getting the
-- next set of filters (if they exist).
--
-- 'datasetGroupArn', 'listFilters_datasetGroupArn' - The ARN of the dataset group that contains the filters.
--
-- 'maxResults', 'listFilters_maxResults' - The maximum number of filters to return.
newListFilters ::
  ListFilters
newListFilters =
  ListFilters'
    { nextToken = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token returned from the previous call to @ListFilters@ for getting the
-- next set of filters (if they exist).
listFilters_nextToken :: Lens.Lens' ListFilters (Prelude.Maybe Prelude.Text)
listFilters_nextToken = Lens.lens (\ListFilters' {nextToken} -> nextToken) (\s@ListFilters' {} a -> s {nextToken = a} :: ListFilters)

-- | The ARN of the dataset group that contains the filters.
listFilters_datasetGroupArn :: Lens.Lens' ListFilters (Prelude.Maybe Prelude.Text)
listFilters_datasetGroupArn = Lens.lens (\ListFilters' {datasetGroupArn} -> datasetGroupArn) (\s@ListFilters' {} a -> s {datasetGroupArn = a} :: ListFilters)

-- | The maximum number of filters to return.
listFilters_maxResults :: Lens.Lens' ListFilters (Prelude.Maybe Prelude.Natural)
listFilters_maxResults = Lens.lens (\ListFilters' {maxResults} -> maxResults) (\s@ListFilters' {} a -> s {maxResults = a} :: ListFilters)

instance Core.AWSPager ListFilters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFiltersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFiltersResponse_filters Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFilters_nextToken
          Lens..~ rs
          Lens.^? listFiltersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFilters where
  type AWSResponse ListFilters = ListFiltersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFiltersResponse'
            Prelude.<$> (x Core..?> "Filters" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFilters

instance Prelude.NFData ListFilters

instance Core.ToHeaders ListFilters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.ListFilters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListFilters where
  toJSON ListFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("datasetGroupArn" Core..=)
              Prelude.<$> datasetGroupArn,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListFilters where
  toPath = Prelude.const "/"

instance Core.ToQuery ListFilters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFiltersResponse' smart constructor.
data ListFiltersResponse = ListFiltersResponse'
  { -- | A list of returned filters.
    filters :: Prelude.Maybe [FilterSummary],
    -- | A token for getting the next set of filters (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'filters', 'listFiltersResponse_filters' - A list of returned filters.
--
-- 'nextToken', 'listFiltersResponse_nextToken' - A token for getting the next set of filters (if they exist).
--
-- 'httpStatus', 'listFiltersResponse_httpStatus' - The response's http status code.
newListFiltersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFiltersResponse
newListFiltersResponse pHttpStatus_ =
  ListFiltersResponse'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of returned filters.
listFiltersResponse_filters :: Lens.Lens' ListFiltersResponse (Prelude.Maybe [FilterSummary])
listFiltersResponse_filters = Lens.lens (\ListFiltersResponse' {filters} -> filters) (\s@ListFiltersResponse' {} a -> s {filters = a} :: ListFiltersResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token for getting the next set of filters (if they exist).
listFiltersResponse_nextToken :: Lens.Lens' ListFiltersResponse (Prelude.Maybe Prelude.Text)
listFiltersResponse_nextToken = Lens.lens (\ListFiltersResponse' {nextToken} -> nextToken) (\s@ListFiltersResponse' {} a -> s {nextToken = a} :: ListFiltersResponse)

-- | The response's http status code.
listFiltersResponse_httpStatus :: Lens.Lens' ListFiltersResponse Prelude.Int
listFiltersResponse_httpStatus = Lens.lens (\ListFiltersResponse' {httpStatus} -> httpStatus) (\s@ListFiltersResponse' {} a -> s {httpStatus = a} :: ListFiltersResponse)

instance Prelude.NFData ListFiltersResponse
