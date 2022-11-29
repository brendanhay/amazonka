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
-- Module      : Amazonka.MacieV2.SearchResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) statistical data and other information about Amazon
-- Web Services resources that Amazon Macie monitors and analyzes.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.SearchResources
  ( -- * Creating a Request
    SearchResources (..),
    newSearchResources,

    -- * Request Lenses
    searchResources_sortCriteria,
    searchResources_bucketCriteria,
    searchResources_nextToken,
    searchResources_maxResults,

    -- * Destructuring the Response
    SearchResourcesResponse (..),
    newSearchResourcesResponse,

    -- * Response Lenses
    searchResourcesResponse_nextToken,
    searchResourcesResponse_matchingResources,
    searchResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchResources' smart constructor.
data SearchResources = SearchResources'
  { -- | The criteria to use to sort the results.
    sortCriteria :: Prelude.Maybe SearchResourcesSortCriteria,
    -- | The filter conditions that determine which S3 buckets to include or
    -- exclude from the query results.
    bucketCriteria :: Prelude.Maybe SearchResourcesBucketCriteria,
    -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to include in each page of the response. The
    -- default value is 50.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortCriteria', 'searchResources_sortCriteria' - The criteria to use to sort the results.
--
-- 'bucketCriteria', 'searchResources_bucketCriteria' - The filter conditions that determine which S3 buckets to include or
-- exclude from the query results.
--
-- 'nextToken', 'searchResources_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'maxResults', 'searchResources_maxResults' - The maximum number of items to include in each page of the response. The
-- default value is 50.
newSearchResources ::
  SearchResources
newSearchResources =
  SearchResources'
    { sortCriteria = Prelude.Nothing,
      bucketCriteria = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The criteria to use to sort the results.
searchResources_sortCriteria :: Lens.Lens' SearchResources (Prelude.Maybe SearchResourcesSortCriteria)
searchResources_sortCriteria = Lens.lens (\SearchResources' {sortCriteria} -> sortCriteria) (\s@SearchResources' {} a -> s {sortCriteria = a} :: SearchResources)

-- | The filter conditions that determine which S3 buckets to include or
-- exclude from the query results.
searchResources_bucketCriteria :: Lens.Lens' SearchResources (Prelude.Maybe SearchResourcesBucketCriteria)
searchResources_bucketCriteria = Lens.lens (\SearchResources' {bucketCriteria} -> bucketCriteria) (\s@SearchResources' {} a -> s {bucketCriteria = a} :: SearchResources)

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
searchResources_nextToken :: Lens.Lens' SearchResources (Prelude.Maybe Prelude.Text)
searchResources_nextToken = Lens.lens (\SearchResources' {nextToken} -> nextToken) (\s@SearchResources' {} a -> s {nextToken = a} :: SearchResources)

-- | The maximum number of items to include in each page of the response. The
-- default value is 50.
searchResources_maxResults :: Lens.Lens' SearchResources (Prelude.Maybe Prelude.Int)
searchResources_maxResults = Lens.lens (\SearchResources' {maxResults} -> maxResults) (\s@SearchResources' {} a -> s {maxResults = a} :: SearchResources)

instance Core.AWSPager SearchResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchResourcesResponse_matchingResources
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchResources_nextToken
          Lens..~ rs
          Lens.^? searchResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchResources where
  type
    AWSResponse SearchResources =
      SearchResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchResourcesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "matchingResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchResources where
  hashWithSalt _salt SearchResources' {..} =
    _salt `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` bucketCriteria
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData SearchResources where
  rnf SearchResources' {..} =
    Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf bucketCriteria
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders SearchResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchResources where
  toJSON SearchResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sortCriteria" Core..=) Prelude.<$> sortCriteria,
            ("bucketCriteria" Core..=)
              Prelude.<$> bucketCriteria,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath SearchResources where
  toPath =
    Prelude.const "/datasources/search-resources"

instance Core.ToQuery SearchResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchResourcesResponse' smart constructor.
data SearchResourcesResponse = SearchResourcesResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects, one for each resource that meets the filter
    -- criteria specified in the request.
    matchingResources :: Prelude.Maybe [MatchingResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchResourcesResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'matchingResources', 'searchResourcesResponse_matchingResources' - An array of objects, one for each resource that meets the filter
-- criteria specified in the request.
--
-- 'httpStatus', 'searchResourcesResponse_httpStatus' - The response's http status code.
newSearchResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchResourcesResponse
newSearchResourcesResponse pHttpStatus_ =
  SearchResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      matchingResources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
searchResourcesResponse_nextToken :: Lens.Lens' SearchResourcesResponse (Prelude.Maybe Prelude.Text)
searchResourcesResponse_nextToken = Lens.lens (\SearchResourcesResponse' {nextToken} -> nextToken) (\s@SearchResourcesResponse' {} a -> s {nextToken = a} :: SearchResourcesResponse)

-- | An array of objects, one for each resource that meets the filter
-- criteria specified in the request.
searchResourcesResponse_matchingResources :: Lens.Lens' SearchResourcesResponse (Prelude.Maybe [MatchingResource])
searchResourcesResponse_matchingResources = Lens.lens (\SearchResourcesResponse' {matchingResources} -> matchingResources) (\s@SearchResourcesResponse' {} a -> s {matchingResources = a} :: SearchResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchResourcesResponse_httpStatus :: Lens.Lens' SearchResourcesResponse Prelude.Int
searchResourcesResponse_httpStatus = Lens.lens (\SearchResourcesResponse' {httpStatus} -> httpStatus) (\s@SearchResourcesResponse' {} a -> s {httpStatus = a} :: SearchResourcesResponse)

instance Prelude.NFData SearchResourcesResponse where
  rnf SearchResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf matchingResources
      `Prelude.seq` Prelude.rnf httpStatus
