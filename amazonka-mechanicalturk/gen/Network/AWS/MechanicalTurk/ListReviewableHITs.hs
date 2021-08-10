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
-- Module      : Network.AWS.MechanicalTurk.ListReviewableHITs
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
module Network.AWS.MechanicalTurk.ListReviewableHITs
  ( -- * Creating a Request
    ListReviewableHITs (..),
    newListReviewableHITs,

    -- * Request Lenses
    listReviewableHITs_status,
    listReviewableHITs_nextToken,
    listReviewableHITs_maxResults,
    listReviewableHITs_hITTypeId,

    -- * Destructuring the Response
    ListReviewableHITsResponse (..),
    newListReviewableHITsResponse,

    -- * Response Lenses
    listReviewableHITsResponse_nextToken,
    listReviewableHITsResponse_hITs,
    listReviewableHITsResponse_numResults,
    listReviewableHITsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListReviewableHITs' smart constructor.
data ListReviewableHITs = ListReviewableHITs'
  { -- | Can be either @Reviewable@ or @Reviewing@. Reviewable is the default
    -- value.
    status :: Prelude.Maybe ReviewableHITStatus,
    -- | Pagination Token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Limit the number of results returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the HIT type of the HITs to consider for the query. If not
    -- specified, all HITs for the Reviewer are considered
    hITTypeId :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'listReviewableHITs_nextToken' - Pagination Token
--
-- 'maxResults', 'listReviewableHITs_maxResults' - Limit the number of results returned.
--
-- 'hITTypeId', 'listReviewableHITs_hITTypeId' - The ID of the HIT type of the HITs to consider for the query. If not
-- specified, all HITs for the Reviewer are considered
newListReviewableHITs ::
  ListReviewableHITs
newListReviewableHITs =
  ListReviewableHITs'
    { status = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      hITTypeId = Prelude.Nothing
    }

-- | Can be either @Reviewable@ or @Reviewing@. Reviewable is the default
-- value.
listReviewableHITs_status :: Lens.Lens' ListReviewableHITs (Prelude.Maybe ReviewableHITStatus)
listReviewableHITs_status = Lens.lens (\ListReviewableHITs' {status} -> status) (\s@ListReviewableHITs' {} a -> s {status = a} :: ListReviewableHITs)

-- | Pagination Token
listReviewableHITs_nextToken :: Lens.Lens' ListReviewableHITs (Prelude.Maybe Prelude.Text)
listReviewableHITs_nextToken = Lens.lens (\ListReviewableHITs' {nextToken} -> nextToken) (\s@ListReviewableHITs' {} a -> s {nextToken = a} :: ListReviewableHITs)

-- | Limit the number of results returned.
listReviewableHITs_maxResults :: Lens.Lens' ListReviewableHITs (Prelude.Maybe Prelude.Natural)
listReviewableHITs_maxResults = Lens.lens (\ListReviewableHITs' {maxResults} -> maxResults) (\s@ListReviewableHITs' {} a -> s {maxResults = a} :: ListReviewableHITs)

-- | The ID of the HIT type of the HITs to consider for the query. If not
-- specified, all HITs for the Reviewer are considered
listReviewableHITs_hITTypeId :: Lens.Lens' ListReviewableHITs (Prelude.Maybe Prelude.Text)
listReviewableHITs_hITTypeId = Lens.lens (\ListReviewableHITs' {hITTypeId} -> hITTypeId) (\s@ListReviewableHITs' {} a -> s {hITTypeId = a} :: ListReviewableHITs)

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
            Prelude.<*> (x Core..?> "HITs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NumResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReviewableHITs

instance Prelude.NFData ListReviewableHITs

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
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("HITTypeId" Core..=) Prelude.<$> hITTypeId
          ]
      )

instance Core.ToPath ListReviewableHITs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListReviewableHITs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReviewableHITsResponse' smart constructor.
data ListReviewableHITsResponse = ListReviewableHITsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of HIT elements returned by the query.
    hITs :: Prelude.Maybe [HIT],
    -- | The number of HITs on this page in the filtered results list, equivalent
    -- to the number of HITs being returned by this call.
    numResults :: Prelude.Maybe Prelude.Int,
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
-- 'hITs', 'listReviewableHITsResponse_hITs' - The list of HIT elements returned by the query.
--
-- 'numResults', 'listReviewableHITsResponse_numResults' - The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
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
      hITs = Prelude.Nothing,
      numResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listReviewableHITsResponse_nextToken :: Lens.Lens' ListReviewableHITsResponse (Prelude.Maybe Prelude.Text)
listReviewableHITsResponse_nextToken = Lens.lens (\ListReviewableHITsResponse' {nextToken} -> nextToken) (\s@ListReviewableHITsResponse' {} a -> s {nextToken = a} :: ListReviewableHITsResponse)

-- | The list of HIT elements returned by the query.
listReviewableHITsResponse_hITs :: Lens.Lens' ListReviewableHITsResponse (Prelude.Maybe [HIT])
listReviewableHITsResponse_hITs = Lens.lens (\ListReviewableHITsResponse' {hITs} -> hITs) (\s@ListReviewableHITsResponse' {} a -> s {hITs = a} :: ListReviewableHITsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
listReviewableHITsResponse_numResults :: Lens.Lens' ListReviewableHITsResponse (Prelude.Maybe Prelude.Int)
listReviewableHITsResponse_numResults = Lens.lens (\ListReviewableHITsResponse' {numResults} -> numResults) (\s@ListReviewableHITsResponse' {} a -> s {numResults = a} :: ListReviewableHITsResponse)

-- | The response's http status code.
listReviewableHITsResponse_httpStatus :: Lens.Lens' ListReviewableHITsResponse Prelude.Int
listReviewableHITsResponse_httpStatus = Lens.lens (\ListReviewableHITsResponse' {httpStatus} -> httpStatus) (\s@ListReviewableHITsResponse' {} a -> s {httpStatus = a} :: ListReviewableHITsResponse)

instance Prelude.NFData ListReviewableHITsResponse
