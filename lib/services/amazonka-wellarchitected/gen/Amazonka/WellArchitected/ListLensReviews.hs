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
-- Module      : Amazonka.WellArchitected.ListLensReviews
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List lens reviews.
module Amazonka.WellArchitected.ListLensReviews
  ( -- * Creating a Request
    ListLensReviews (..),
    newListLensReviews,

    -- * Request Lenses
    listLensReviews_milestoneNumber,
    listLensReviews_nextToken,
    listLensReviews_maxResults,
    listLensReviews_workloadId,

    -- * Destructuring the Response
    ListLensReviewsResponse (..),
    newListLensReviewsResponse,

    -- * Response Lenses
    listLensReviewsResponse_milestoneNumber,
    listLensReviewsResponse_nextToken,
    listLensReviewsResponse_lensReviewSummaries,
    listLensReviewsResponse_workloadId,
    listLensReviewsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to list lens reviews.
--
-- /See:/ 'newListLensReviews' smart constructor.
data ListLensReviews = ListLensReviews'
  { milestoneNumber :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    workloadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLensReviews' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'milestoneNumber', 'listLensReviews_milestoneNumber' - Undocumented member.
--
-- 'nextToken', 'listLensReviews_nextToken' - Undocumented member.
--
-- 'maxResults', 'listLensReviews_maxResults' - Undocumented member.
--
-- 'workloadId', 'listLensReviews_workloadId' - Undocumented member.
newListLensReviews ::
  -- | 'workloadId'
  Prelude.Text ->
  ListLensReviews
newListLensReviews pWorkloadId_ =
  ListLensReviews'
    { milestoneNumber = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      workloadId = pWorkloadId_
    }

-- | Undocumented member.
listLensReviews_milestoneNumber :: Lens.Lens' ListLensReviews (Prelude.Maybe Prelude.Natural)
listLensReviews_milestoneNumber = Lens.lens (\ListLensReviews' {milestoneNumber} -> milestoneNumber) (\s@ListLensReviews' {} a -> s {milestoneNumber = a} :: ListLensReviews)

-- | Undocumented member.
listLensReviews_nextToken :: Lens.Lens' ListLensReviews (Prelude.Maybe Prelude.Text)
listLensReviews_nextToken = Lens.lens (\ListLensReviews' {nextToken} -> nextToken) (\s@ListLensReviews' {} a -> s {nextToken = a} :: ListLensReviews)

-- | Undocumented member.
listLensReviews_maxResults :: Lens.Lens' ListLensReviews (Prelude.Maybe Prelude.Natural)
listLensReviews_maxResults = Lens.lens (\ListLensReviews' {maxResults} -> maxResults) (\s@ListLensReviews' {} a -> s {maxResults = a} :: ListLensReviews)

-- | Undocumented member.
listLensReviews_workloadId :: Lens.Lens' ListLensReviews Prelude.Text
listLensReviews_workloadId = Lens.lens (\ListLensReviews' {workloadId} -> workloadId) (\s@ListLensReviews' {} a -> s {workloadId = a} :: ListLensReviews)

instance Core.AWSRequest ListLensReviews where
  type
    AWSResponse ListLensReviews =
      ListLensReviewsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLensReviewsResponse'
            Prelude.<$> (x Core..?> "MilestoneNumber")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "LensReviewSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "WorkloadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLensReviews where
  hashWithSalt _salt ListLensReviews' {..} =
    _salt `Prelude.hashWithSalt` milestoneNumber
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` workloadId

instance Prelude.NFData ListLensReviews where
  rnf ListLensReviews' {..} =
    Prelude.rnf milestoneNumber
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf workloadId

instance Core.ToHeaders ListLensReviews where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListLensReviews where
  toPath ListLensReviews' {..} =
    Prelude.mconcat
      ["/workloads/", Core.toBS workloadId, "/lensReviews"]

instance Core.ToQuery ListLensReviews where
  toQuery ListLensReviews' {..} =
    Prelude.mconcat
      [ "MilestoneNumber" Core.=: milestoneNumber,
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | Output of a list lens reviews call.
--
-- /See:/ 'newListLensReviewsResponse' smart constructor.
data ListLensReviewsResponse = ListLensReviewsResponse'
  { milestoneNumber :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    lensReviewSummaries :: Prelude.Maybe [LensReviewSummary],
    workloadId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLensReviewsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'milestoneNumber', 'listLensReviewsResponse_milestoneNumber' - Undocumented member.
--
-- 'nextToken', 'listLensReviewsResponse_nextToken' - Undocumented member.
--
-- 'lensReviewSummaries', 'listLensReviewsResponse_lensReviewSummaries' - Undocumented member.
--
-- 'workloadId', 'listLensReviewsResponse_workloadId' - Undocumented member.
--
-- 'httpStatus', 'listLensReviewsResponse_httpStatus' - The response's http status code.
newListLensReviewsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLensReviewsResponse
newListLensReviewsResponse pHttpStatus_ =
  ListLensReviewsResponse'
    { milestoneNumber =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      lensReviewSummaries = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listLensReviewsResponse_milestoneNumber :: Lens.Lens' ListLensReviewsResponse (Prelude.Maybe Prelude.Natural)
listLensReviewsResponse_milestoneNumber = Lens.lens (\ListLensReviewsResponse' {milestoneNumber} -> milestoneNumber) (\s@ListLensReviewsResponse' {} a -> s {milestoneNumber = a} :: ListLensReviewsResponse)

-- | Undocumented member.
listLensReviewsResponse_nextToken :: Lens.Lens' ListLensReviewsResponse (Prelude.Maybe Prelude.Text)
listLensReviewsResponse_nextToken = Lens.lens (\ListLensReviewsResponse' {nextToken} -> nextToken) (\s@ListLensReviewsResponse' {} a -> s {nextToken = a} :: ListLensReviewsResponse)

-- | Undocumented member.
listLensReviewsResponse_lensReviewSummaries :: Lens.Lens' ListLensReviewsResponse (Prelude.Maybe [LensReviewSummary])
listLensReviewsResponse_lensReviewSummaries = Lens.lens (\ListLensReviewsResponse' {lensReviewSummaries} -> lensReviewSummaries) (\s@ListLensReviewsResponse' {} a -> s {lensReviewSummaries = a} :: ListLensReviewsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listLensReviewsResponse_workloadId :: Lens.Lens' ListLensReviewsResponse (Prelude.Maybe Prelude.Text)
listLensReviewsResponse_workloadId = Lens.lens (\ListLensReviewsResponse' {workloadId} -> workloadId) (\s@ListLensReviewsResponse' {} a -> s {workloadId = a} :: ListLensReviewsResponse)

-- | The response's http status code.
listLensReviewsResponse_httpStatus :: Lens.Lens' ListLensReviewsResponse Prelude.Int
listLensReviewsResponse_httpStatus = Lens.lens (\ListLensReviewsResponse' {httpStatus} -> httpStatus) (\s@ListLensReviewsResponse' {} a -> s {httpStatus = a} :: ListLensReviewsResponse)

instance Prelude.NFData ListLensReviewsResponse where
  rnf ListLensReviewsResponse' {..} =
    Prelude.rnf milestoneNumber
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf lensReviewSummaries
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf httpStatus
