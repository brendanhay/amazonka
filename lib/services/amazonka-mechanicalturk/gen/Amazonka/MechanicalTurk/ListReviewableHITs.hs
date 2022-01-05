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
-- Module      : Amazonka.MechanicalTurk.ListReviewableHITs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListReviewableHITs@ operation retrieves the HITs with Status equal
-- to Reviewable or Status equal to Reviewing that belong to the Requester
-- calling the operation.
--
-- This operation returns paginated results.
module Amazonka.MechanicalTurk.ListReviewableHITs
  ( -- * Creating a Request
    ListReviewableHITs (..),
    newListReviewableHITs,

    -- * Request Lenses
    listReviewableHITs_status,
    listReviewableHITs_hITTypeId,
    listReviewableHITs_nextToken,
    listReviewableHITs_maxResults,

    -- * Destructuring the Response
    ListReviewableHITsResponse (..),
    newListReviewableHITsResponse,

    -- * Response Lenses
    listReviewableHITsResponse_nextToken,
    listReviewableHITsResponse_numResults,
    listReviewableHITsResponse_hITs,
    listReviewableHITsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReviewableHITs' smart constructor.
data ListReviewableHITs = ListReviewableHITs'
  { -- | Can be either @Reviewable@ or @Reviewing@. Reviewable is the default
    -- value.
    status :: Prelude.Maybe ReviewableHITStatus,
    -- | The ID of the HIT type of the HITs to consider for the query. If not
    -- specified, all HITs for the Reviewer are considered
    hITTypeId :: Prelude.Maybe Prelude.Text,
    -- | Pagination Token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Limit the number of results returned.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReviewableHITs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listReviewableHITs_status' - Can be either @Reviewable@ or @Reviewing@. Reviewable is the default
-- value.
--
-- 'hITTypeId', 'listReviewableHITs_hITTypeId' - The ID of the HIT type of the HITs to consider for the query. If not
-- specified, all HITs for the Reviewer are considered
--
-- 'nextToken', 'listReviewableHITs_nextToken' - Pagination Token
--
-- 'maxResults', 'listReviewableHITs_maxResults' - Limit the number of results returned.
newListReviewableHITs ::
  ListReviewableHITs
newListReviewableHITs =
  ListReviewableHITs'
    { status = Prelude.Nothing,
      hITTypeId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Can be either @Reviewable@ or @Reviewing@. Reviewable is the default
-- value.
listReviewableHITs_status :: Lens.Lens' ListReviewableHITs (Prelude.Maybe ReviewableHITStatus)
listReviewableHITs_status = Lens.lens (\ListReviewableHITs' {status} -> status) (\s@ListReviewableHITs' {} a -> s {status = a} :: ListReviewableHITs)

-- | The ID of the HIT type of the HITs to consider for the query. If not
-- specified, all HITs for the Reviewer are considered
listReviewableHITs_hITTypeId :: Lens.Lens' ListReviewableHITs (Prelude.Maybe Prelude.Text)
listReviewableHITs_hITTypeId = Lens.lens (\ListReviewableHITs' {hITTypeId} -> hITTypeId) (\s@ListReviewableHITs' {} a -> s {hITTypeId = a} :: ListReviewableHITs)

-- | Pagination Token
listReviewableHITs_nextToken :: Lens.Lens' ListReviewableHITs (Prelude.Maybe Prelude.Text)
listReviewableHITs_nextToken = Lens.lens (\ListReviewableHITs' {nextToken} -> nextToken) (\s@ListReviewableHITs' {} a -> s {nextToken = a} :: ListReviewableHITs)

-- | Limit the number of results returned.
listReviewableHITs_maxResults :: Lens.Lens' ListReviewableHITs (Prelude.Maybe Prelude.Natural)
listReviewableHITs_maxResults = Lens.lens (\ListReviewableHITs' {maxResults} -> maxResults) (\s@ListReviewableHITs' {} a -> s {maxResults = a} :: ListReviewableHITs)

instance Core.AWSPager ListReviewableHITs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReviewableHITsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReviewableHITsResponse_hITs Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listReviewableHITs_nextToken
          Lens..~ rs
          Lens.^? listReviewableHITsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListReviewableHITs where
  type
    AWSResponse ListReviewableHITs =
      ListReviewableHITsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReviewableHITsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "NumResults")
            Prelude.<*> (x Core..?> "HITs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReviewableHITs where
  hashWithSalt _salt ListReviewableHITs' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` hITTypeId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListReviewableHITs where
  rnf ListReviewableHITs' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf hITTypeId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListReviewableHITs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.ListReviewableHITs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListReviewableHITs where
  toJSON ListReviewableHITs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("HITTypeId" Core..=) Prelude.<$> hITTypeId,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListReviewableHITs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListReviewableHITs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReviewableHITsResponse' smart constructor.
data ListReviewableHITsResponse = ListReviewableHITsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of HITs on this page in the filtered results list, equivalent
    -- to the number of HITs being returned by this call.
    numResults :: Prelude.Maybe Prelude.Int,
    -- | The list of HIT elements returned by the query.
    hITs :: Prelude.Maybe [HIT],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReviewableHITsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReviewableHITsResponse_nextToken' - Undocumented member.
--
-- 'numResults', 'listReviewableHITsResponse_numResults' - The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
--
-- 'hITs', 'listReviewableHITsResponse_hITs' - The list of HIT elements returned by the query.
--
-- 'httpStatus', 'listReviewableHITsResponse_httpStatus' - The response's http status code.
newListReviewableHITsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReviewableHITsResponse
newListReviewableHITsResponse pHttpStatus_ =
  ListReviewableHITsResponse'
    { nextToken =
        Prelude.Nothing,
      numResults = Prelude.Nothing,
      hITs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listReviewableHITsResponse_nextToken :: Lens.Lens' ListReviewableHITsResponse (Prelude.Maybe Prelude.Text)
listReviewableHITsResponse_nextToken = Lens.lens (\ListReviewableHITsResponse' {nextToken} -> nextToken) (\s@ListReviewableHITsResponse' {} a -> s {nextToken = a} :: ListReviewableHITsResponse)

-- | The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
listReviewableHITsResponse_numResults :: Lens.Lens' ListReviewableHITsResponse (Prelude.Maybe Prelude.Int)
listReviewableHITsResponse_numResults = Lens.lens (\ListReviewableHITsResponse' {numResults} -> numResults) (\s@ListReviewableHITsResponse' {} a -> s {numResults = a} :: ListReviewableHITsResponse)

-- | The list of HIT elements returned by the query.
listReviewableHITsResponse_hITs :: Lens.Lens' ListReviewableHITsResponse (Prelude.Maybe [HIT])
listReviewableHITsResponse_hITs = Lens.lens (\ListReviewableHITsResponse' {hITs} -> hITs) (\s@ListReviewableHITsResponse' {} a -> s {hITs = a} :: ListReviewableHITsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReviewableHITsResponse_httpStatus :: Lens.Lens' ListReviewableHITsResponse Prelude.Int
listReviewableHITsResponse_httpStatus = Lens.lens (\ListReviewableHITsResponse' {httpStatus} -> httpStatus) (\s@ListReviewableHITsResponse' {} a -> s {httpStatus = a} :: ListReviewableHITsResponse)

instance Prelude.NFData ListReviewableHITsResponse where
  rnf ListReviewableHITsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf numResults
      `Prelude.seq` Prelude.rnf hITs
      `Prelude.seq` Prelude.rnf httpStatus
