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
-- Module      : Network.AWS.MechanicalTurk.ListWorkerBlocks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListWorkersBlocks@ operation retrieves a list of Workers who are
-- blocked from working on your HITs.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListWorkerBlocks
  ( -- * Creating a Request
    ListWorkerBlocks (..),
    newListWorkerBlocks,

    -- * Request Lenses
    listWorkerBlocks_nextToken,
    listWorkerBlocks_maxResults,

    -- * Destructuring the Response
    ListWorkerBlocksResponse (..),
    newListWorkerBlocksResponse,

    -- * Response Lenses
    listWorkerBlocksResponse_nextToken,
    listWorkerBlocksResponse_workerBlocks,
    listWorkerBlocksResponse_numResults,
    listWorkerBlocksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListWorkerBlocks' smart constructor.
data ListWorkerBlocks = ListWorkerBlocks'
  { -- | Pagination token
    nextToken :: Core.Maybe Core.Text,
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListWorkerBlocks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkerBlocks_nextToken' - Pagination token
--
-- 'maxResults', 'listWorkerBlocks_maxResults' - Undocumented member.
newListWorkerBlocks ::
  ListWorkerBlocks
newListWorkerBlocks =
  ListWorkerBlocks'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Pagination token
listWorkerBlocks_nextToken :: Lens.Lens' ListWorkerBlocks (Core.Maybe Core.Text)
listWorkerBlocks_nextToken = Lens.lens (\ListWorkerBlocks' {nextToken} -> nextToken) (\s@ListWorkerBlocks' {} a -> s {nextToken = a} :: ListWorkerBlocks)

-- | Undocumented member.
listWorkerBlocks_maxResults :: Lens.Lens' ListWorkerBlocks (Core.Maybe Core.Natural)
listWorkerBlocks_maxResults = Lens.lens (\ListWorkerBlocks' {maxResults} -> maxResults) (\s@ListWorkerBlocks' {} a -> s {maxResults = a} :: ListWorkerBlocks)

instance Core.AWSPager ListWorkerBlocks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkerBlocksResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listWorkerBlocksResponse_workerBlocks
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listWorkerBlocks_nextToken
          Lens..~ rs
          Lens.^? listWorkerBlocksResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListWorkerBlocks where
  type
    AWSResponse ListWorkerBlocks =
      ListWorkerBlocksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkerBlocksResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "WorkerBlocks" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NumResults")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListWorkerBlocks

instance Core.NFData ListWorkerBlocks

instance Core.ToHeaders ListWorkerBlocks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.ListWorkerBlocks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListWorkerBlocks where
  toJSON ListWorkerBlocks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListWorkerBlocks where
  toPath = Core.const "/"

instance Core.ToQuery ListWorkerBlocks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListWorkerBlocksResponse' smart constructor.
data ListWorkerBlocksResponse = ListWorkerBlocksResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The list of WorkerBlocks, containing the collection of Worker IDs and
    -- reasons for blocking.
    workerBlocks :: Core.Maybe [WorkerBlock],
    -- | The number of assignments on the page in the filtered results list,
    -- equivalent to the number of assignments returned by this call.
    numResults :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListWorkerBlocksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkerBlocksResponse_nextToken' - Undocumented member.
--
-- 'workerBlocks', 'listWorkerBlocksResponse_workerBlocks' - The list of WorkerBlocks, containing the collection of Worker IDs and
-- reasons for blocking.
--
-- 'numResults', 'listWorkerBlocksResponse_numResults' - The number of assignments on the page in the filtered results list,
-- equivalent to the number of assignments returned by this call.
--
-- 'httpStatus', 'listWorkerBlocksResponse_httpStatus' - The response's http status code.
newListWorkerBlocksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListWorkerBlocksResponse
newListWorkerBlocksResponse pHttpStatus_ =
  ListWorkerBlocksResponse'
    { nextToken = Core.Nothing,
      workerBlocks = Core.Nothing,
      numResults = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listWorkerBlocksResponse_nextToken :: Lens.Lens' ListWorkerBlocksResponse (Core.Maybe Core.Text)
listWorkerBlocksResponse_nextToken = Lens.lens (\ListWorkerBlocksResponse' {nextToken} -> nextToken) (\s@ListWorkerBlocksResponse' {} a -> s {nextToken = a} :: ListWorkerBlocksResponse)

-- | The list of WorkerBlocks, containing the collection of Worker IDs and
-- reasons for blocking.
listWorkerBlocksResponse_workerBlocks :: Lens.Lens' ListWorkerBlocksResponse (Core.Maybe [WorkerBlock])
listWorkerBlocksResponse_workerBlocks = Lens.lens (\ListWorkerBlocksResponse' {workerBlocks} -> workerBlocks) (\s@ListWorkerBlocksResponse' {} a -> s {workerBlocks = a} :: ListWorkerBlocksResponse) Core.. Lens.mapping Lens._Coerce

-- | The number of assignments on the page in the filtered results list,
-- equivalent to the number of assignments returned by this call.
listWorkerBlocksResponse_numResults :: Lens.Lens' ListWorkerBlocksResponse (Core.Maybe Core.Int)
listWorkerBlocksResponse_numResults = Lens.lens (\ListWorkerBlocksResponse' {numResults} -> numResults) (\s@ListWorkerBlocksResponse' {} a -> s {numResults = a} :: ListWorkerBlocksResponse)

-- | The response's http status code.
listWorkerBlocksResponse_httpStatus :: Lens.Lens' ListWorkerBlocksResponse Core.Int
listWorkerBlocksResponse_httpStatus = Lens.lens (\ListWorkerBlocksResponse' {httpStatus} -> httpStatus) (\s@ListWorkerBlocksResponse' {} a -> s {httpStatus = a} :: ListWorkerBlocksResponse)

instance Core.NFData ListWorkerBlocksResponse
