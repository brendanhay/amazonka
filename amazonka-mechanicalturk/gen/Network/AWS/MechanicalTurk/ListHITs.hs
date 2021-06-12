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
-- Module      : Network.AWS.MechanicalTurk.ListHITs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListHITs@ operation returns all of a Requester\'s HITs. The
-- operation returns HITs of any status, except for HITs that have been
-- deleted of with the DeleteHIT operation or that have been auto-deleted.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListHITs
  ( -- * Creating a Request
    ListHITs (..),
    newListHITs,

    -- * Request Lenses
    listHITs_nextToken,
    listHITs_maxResults,

    -- * Destructuring the Response
    ListHITsResponse (..),
    newListHITsResponse,

    -- * Response Lenses
    listHITsResponse_nextToken,
    listHITsResponse_hITs,
    listHITsResponse_numResults,
    listHITsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListHITs' smart constructor.
data ListHITs = ListHITs'
  { -- | Pagination token
    nextToken :: Core.Maybe Core.Text,
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHITs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHITs_nextToken' - Pagination token
--
-- 'maxResults', 'listHITs_maxResults' - Undocumented member.
newListHITs ::
  ListHITs
newListHITs =
  ListHITs'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Pagination token
listHITs_nextToken :: Lens.Lens' ListHITs (Core.Maybe Core.Text)
listHITs_nextToken = Lens.lens (\ListHITs' {nextToken} -> nextToken) (\s@ListHITs' {} a -> s {nextToken = a} :: ListHITs)

-- | Undocumented member.
listHITs_maxResults :: Lens.Lens' ListHITs (Core.Maybe Core.Natural)
listHITs_maxResults = Lens.lens (\ListHITs' {maxResults} -> maxResults) (\s@ListHITs' {} a -> s {maxResults = a} :: ListHITs)

instance Core.AWSPager ListHITs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHITsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^? listHITsResponse_hITs Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listHITs_nextToken
          Lens..~ rs
          Lens.^? listHITsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListHITs where
  type AWSResponse ListHITs = ListHITsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHITsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "HITs" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NumResults")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListHITs

instance Core.NFData ListHITs

instance Core.ToHeaders ListHITs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.ListHITs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListHITs where
  toJSON ListHITs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListHITs where
  toPath = Core.const "/"

instance Core.ToQuery ListHITs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListHITsResponse' smart constructor.
data ListHITsResponse = ListHITsResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The list of HIT elements returned by the query.
    hITs :: Core.Maybe [HIT],
    -- | The number of HITs on this page in the filtered results list, equivalent
    -- to the number of HITs being returned by this call.
    numResults :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHITsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHITsResponse_nextToken' - Undocumented member.
--
-- 'hITs', 'listHITsResponse_hITs' - The list of HIT elements returned by the query.
--
-- 'numResults', 'listHITsResponse_numResults' - The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
--
-- 'httpStatus', 'listHITsResponse_httpStatus' - The response's http status code.
newListHITsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListHITsResponse
newListHITsResponse pHttpStatus_ =
  ListHITsResponse'
    { nextToken = Core.Nothing,
      hITs = Core.Nothing,
      numResults = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listHITsResponse_nextToken :: Lens.Lens' ListHITsResponse (Core.Maybe Core.Text)
listHITsResponse_nextToken = Lens.lens (\ListHITsResponse' {nextToken} -> nextToken) (\s@ListHITsResponse' {} a -> s {nextToken = a} :: ListHITsResponse)

-- | The list of HIT elements returned by the query.
listHITsResponse_hITs :: Lens.Lens' ListHITsResponse (Core.Maybe [HIT])
listHITsResponse_hITs = Lens.lens (\ListHITsResponse' {hITs} -> hITs) (\s@ListHITsResponse' {} a -> s {hITs = a} :: ListHITsResponse) Core.. Lens.mapping Lens._Coerce

-- | The number of HITs on this page in the filtered results list, equivalent
-- to the number of HITs being returned by this call.
listHITsResponse_numResults :: Lens.Lens' ListHITsResponse (Core.Maybe Core.Int)
listHITsResponse_numResults = Lens.lens (\ListHITsResponse' {numResults} -> numResults) (\s@ListHITsResponse' {} a -> s {numResults = a} :: ListHITsResponse)

-- | The response's http status code.
listHITsResponse_httpStatus :: Lens.Lens' ListHITsResponse Core.Int
listHITsResponse_httpStatus = Lens.lens (\ListHITsResponse' {httpStatus} -> httpStatus) (\s@ListHITsResponse' {} a -> s {httpStatus = a} :: ListHITsResponse)

instance Core.NFData ListHITsResponse
