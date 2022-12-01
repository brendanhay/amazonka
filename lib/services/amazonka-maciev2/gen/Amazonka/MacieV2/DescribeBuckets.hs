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
-- Module      : Amazonka.MacieV2.DescribeBuckets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) statistical data and other information about one or
-- more S3 buckets that Amazon Macie monitors and analyzes.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.DescribeBuckets
  ( -- * Creating a Request
    DescribeBuckets (..),
    newDescribeBuckets,

    -- * Request Lenses
    describeBuckets_sortCriteria,
    describeBuckets_nextToken,
    describeBuckets_criteria,
    describeBuckets_maxResults,

    -- * Destructuring the Response
    DescribeBucketsResponse (..),
    newDescribeBucketsResponse,

    -- * Response Lenses
    describeBucketsResponse_nextToken,
    describeBucketsResponse_buckets,
    describeBucketsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBuckets' smart constructor.
data DescribeBuckets = DescribeBuckets'
  { -- | The criteria to use to sort the query results.
    sortCriteria :: Prelude.Maybe BucketSortCriteria,
    -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The criteria to use to filter the query results.
    criteria :: Prelude.Maybe (Prelude.HashMap Prelude.Text BucketCriteriaAdditionalProperties),
    -- | The maximum number of items to include in each page of the response. The
    -- default value is 50.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBuckets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortCriteria', 'describeBuckets_sortCriteria' - The criteria to use to sort the query results.
--
-- 'nextToken', 'describeBuckets_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'criteria', 'describeBuckets_criteria' - The criteria to use to filter the query results.
--
-- 'maxResults', 'describeBuckets_maxResults' - The maximum number of items to include in each page of the response. The
-- default value is 50.
newDescribeBuckets ::
  DescribeBuckets
newDescribeBuckets =
  DescribeBuckets'
    { sortCriteria = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      criteria = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The criteria to use to sort the query results.
describeBuckets_sortCriteria :: Lens.Lens' DescribeBuckets (Prelude.Maybe BucketSortCriteria)
describeBuckets_sortCriteria = Lens.lens (\DescribeBuckets' {sortCriteria} -> sortCriteria) (\s@DescribeBuckets' {} a -> s {sortCriteria = a} :: DescribeBuckets)

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
describeBuckets_nextToken :: Lens.Lens' DescribeBuckets (Prelude.Maybe Prelude.Text)
describeBuckets_nextToken = Lens.lens (\DescribeBuckets' {nextToken} -> nextToken) (\s@DescribeBuckets' {} a -> s {nextToken = a} :: DescribeBuckets)

-- | The criteria to use to filter the query results.
describeBuckets_criteria :: Lens.Lens' DescribeBuckets (Prelude.Maybe (Prelude.HashMap Prelude.Text BucketCriteriaAdditionalProperties))
describeBuckets_criteria = Lens.lens (\DescribeBuckets' {criteria} -> criteria) (\s@DescribeBuckets' {} a -> s {criteria = a} :: DescribeBuckets) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to include in each page of the response. The
-- default value is 50.
describeBuckets_maxResults :: Lens.Lens' DescribeBuckets (Prelude.Maybe Prelude.Int)
describeBuckets_maxResults = Lens.lens (\DescribeBuckets' {maxResults} -> maxResults) (\s@DescribeBuckets' {} a -> s {maxResults = a} :: DescribeBuckets)

instance Core.AWSPager DescribeBuckets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBucketsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeBucketsResponse_buckets Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeBuckets_nextToken
          Lens..~ rs
          Lens.^? describeBucketsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeBuckets where
  type
    AWSResponse DescribeBuckets =
      DescribeBucketsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBucketsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "buckets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBuckets where
  hashWithSalt _salt DescribeBuckets' {..} =
    _salt `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` criteria
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeBuckets where
  rnf DescribeBuckets' {..} =
    Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf criteria
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeBuckets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeBuckets where
  toJSON DescribeBuckets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sortCriteria" Core..=) Prelude.<$> sortCriteria,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("criteria" Core..=) Prelude.<$> criteria,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeBuckets where
  toPath = Prelude.const "/datasources/s3"

instance Core.ToQuery DescribeBuckets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBucketsResponse' smart constructor.
data DescribeBucketsResponse = DescribeBucketsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects, one for each bucket that meets the filter criteria
    -- specified in the request.
    buckets :: Prelude.Maybe [BucketMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBucketsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBucketsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'buckets', 'describeBucketsResponse_buckets' - An array of objects, one for each bucket that meets the filter criteria
-- specified in the request.
--
-- 'httpStatus', 'describeBucketsResponse_httpStatus' - The response's http status code.
newDescribeBucketsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBucketsResponse
newDescribeBucketsResponse pHttpStatus_ =
  DescribeBucketsResponse'
    { nextToken =
        Prelude.Nothing,
      buckets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
describeBucketsResponse_nextToken :: Lens.Lens' DescribeBucketsResponse (Prelude.Maybe Prelude.Text)
describeBucketsResponse_nextToken = Lens.lens (\DescribeBucketsResponse' {nextToken} -> nextToken) (\s@DescribeBucketsResponse' {} a -> s {nextToken = a} :: DescribeBucketsResponse)

-- | An array of objects, one for each bucket that meets the filter criteria
-- specified in the request.
describeBucketsResponse_buckets :: Lens.Lens' DescribeBucketsResponse (Prelude.Maybe [BucketMetadata])
describeBucketsResponse_buckets = Lens.lens (\DescribeBucketsResponse' {buckets} -> buckets) (\s@DescribeBucketsResponse' {} a -> s {buckets = a} :: DescribeBucketsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeBucketsResponse_httpStatus :: Lens.Lens' DescribeBucketsResponse Prelude.Int
describeBucketsResponse_httpStatus = Lens.lens (\DescribeBucketsResponse' {httpStatus} -> httpStatus) (\s@DescribeBucketsResponse' {} a -> s {httpStatus = a} :: DescribeBucketsResponse)

instance Prelude.NFData DescribeBucketsResponse where
  rnf DescribeBucketsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf buckets
      `Prelude.seq` Prelude.rnf httpStatus
