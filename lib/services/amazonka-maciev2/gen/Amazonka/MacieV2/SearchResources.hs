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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    searchResources_bucketCriteria,
    searchResources_maxResults,
    searchResources_nextToken,
    searchResources_sortCriteria,

    -- * Destructuring the Response
    SearchResourcesResponse (..),
    newSearchResourcesResponse,

    -- * Response Lenses
    searchResourcesResponse_matchingResources,
    searchResourcesResponse_nextToken,
    searchResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchResources' smart constructor.
data SearchResources = SearchResources'
  { -- | The filter conditions that determine which S3 buckets to include or
    -- exclude from the query results.
    bucketCriteria :: Prelude.Maybe SearchResourcesBucketCriteria,
    -- | The maximum number of items to include in each page of the response. The
    -- default value is 50.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The criteria to use to sort the results.
    sortCriteria :: Prelude.Maybe SearchResourcesSortCriteria
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
-- 'bucketCriteria', 'searchResources_bucketCriteria' - The filter conditions that determine which S3 buckets to include or
-- exclude from the query results.
--
-- 'maxResults', 'searchResources_maxResults' - The maximum number of items to include in each page of the response. The
-- default value is 50.
--
-- 'nextToken', 'searchResources_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'sortCriteria', 'searchResources_sortCriteria' - The criteria to use to sort the results.
newSearchResources ::
  SearchResources
newSearchResources =
  SearchResources'
    { bucketCriteria = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortCriteria = Prelude.Nothing
    }

-- | The filter conditions that determine which S3 buckets to include or
-- exclude from the query results.
searchResources_bucketCriteria :: Lens.Lens' SearchResources (Prelude.Maybe SearchResourcesBucketCriteria)
searchResources_bucketCriteria = Lens.lens (\SearchResources' {bucketCriteria} -> bucketCriteria) (\s@SearchResources' {} a -> s {bucketCriteria = a} :: SearchResources)

-- | The maximum number of items to include in each page of the response. The
-- default value is 50.
searchResources_maxResults :: Lens.Lens' SearchResources (Prelude.Maybe Prelude.Int)
searchResources_maxResults = Lens.lens (\SearchResources' {maxResults} -> maxResults) (\s@SearchResources' {} a -> s {maxResults = a} :: SearchResources)

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
searchResources_nextToken :: Lens.Lens' SearchResources (Prelude.Maybe Prelude.Text)
searchResources_nextToken = Lens.lens (\SearchResources' {nextToken} -> nextToken) (\s@SearchResources' {} a -> s {nextToken = a} :: SearchResources)

-- | The criteria to use to sort the results.
searchResources_sortCriteria :: Lens.Lens' SearchResources (Prelude.Maybe SearchResourcesSortCriteria)
searchResources_sortCriteria = Lens.lens (\SearchResources' {sortCriteria} -> sortCriteria) (\s@SearchResources' {} a -> s {sortCriteria = a} :: SearchResources)

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
            Prelude.<$> ( x Data..?> "matchingResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchResources where
  hashWithSalt _salt SearchResources' {..} =
    _salt `Prelude.hashWithSalt` bucketCriteria
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortCriteria

instance Prelude.NFData SearchResources where
  rnf SearchResources' {..} =
    Prelude.rnf bucketCriteria
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortCriteria

instance Data.ToHeaders SearchResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchResources where
  toJSON SearchResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucketCriteria" Data..=)
              Prelude.<$> bucketCriteria,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortCriteria" Data..=) Prelude.<$> sortCriteria
          ]
      )

instance Data.ToPath SearchResources where
  toPath =
    Prelude.const "/datasources/search-resources"

instance Data.ToQuery SearchResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchResourcesResponse' smart constructor.
data SearchResourcesResponse = SearchResourcesResponse'
  { -- | An array of objects, one for each resource that matches the filter
    -- criteria specified in the request.
    matchingResources :: Prelude.Maybe [MatchingResource],
    -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'matchingResources', 'searchResourcesResponse_matchingResources' - An array of objects, one for each resource that matches the filter
-- criteria specified in the request.
--
-- 'nextToken', 'searchResourcesResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'httpStatus', 'searchResourcesResponse_httpStatus' - The response's http status code.
newSearchResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchResourcesResponse
newSearchResourcesResponse pHttpStatus_ =
  SearchResourcesResponse'
    { matchingResources =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each resource that matches the filter
-- criteria specified in the request.
searchResourcesResponse_matchingResources :: Lens.Lens' SearchResourcesResponse (Prelude.Maybe [MatchingResource])
searchResourcesResponse_matchingResources = Lens.lens (\SearchResourcesResponse' {matchingResources} -> matchingResources) (\s@SearchResourcesResponse' {} a -> s {matchingResources = a} :: SearchResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
searchResourcesResponse_nextToken :: Lens.Lens' SearchResourcesResponse (Prelude.Maybe Prelude.Text)
searchResourcesResponse_nextToken = Lens.lens (\SearchResourcesResponse' {nextToken} -> nextToken) (\s@SearchResourcesResponse' {} a -> s {nextToken = a} :: SearchResourcesResponse)

-- | The response's http status code.
searchResourcesResponse_httpStatus :: Lens.Lens' SearchResourcesResponse Prelude.Int
searchResourcesResponse_httpStatus = Lens.lens (\SearchResourcesResponse' {httpStatus} -> httpStatus) (\s@SearchResourcesResponse' {} a -> s {httpStatus = a} :: SearchResourcesResponse)

instance Prelude.NFData SearchResourcesResponse where
  rnf SearchResourcesResponse' {..} =
    Prelude.rnf matchingResources
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
