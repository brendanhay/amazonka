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
-- Module      : Network.AWS.MigrationHub.ListProgressUpdateStreams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists progress update streams associated with the user account making
-- this call.
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListProgressUpdateStreams
  ( -- * Creating a Request
    ListProgressUpdateStreams (..),
    newListProgressUpdateStreams,

    -- * Request Lenses
    listProgressUpdateStreams_nextToken,
    listProgressUpdateStreams_maxResults,

    -- * Destructuring the Response
    ListProgressUpdateStreamsResponse (..),
    newListProgressUpdateStreamsResponse,

    -- * Response Lenses
    listProgressUpdateStreamsResponse_nextToken,
    listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList,
    listProgressUpdateStreamsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListProgressUpdateStreams' smart constructor.
data ListProgressUpdateStreams = ListProgressUpdateStreams'
  { -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Core.Maybe Core.Text,
    -- | Filter to limit the maximum number of results to list per page.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProgressUpdateStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProgressUpdateStreams_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
--
-- 'maxResults', 'listProgressUpdateStreams_maxResults' - Filter to limit the maximum number of results to list per page.
newListProgressUpdateStreams ::
  ListProgressUpdateStreams
newListProgressUpdateStreams =
  ListProgressUpdateStreams'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listProgressUpdateStreams_nextToken :: Lens.Lens' ListProgressUpdateStreams (Core.Maybe Core.Text)
listProgressUpdateStreams_nextToken = Lens.lens (\ListProgressUpdateStreams' {nextToken} -> nextToken) (\s@ListProgressUpdateStreams' {} a -> s {nextToken = a} :: ListProgressUpdateStreams)

-- | Filter to limit the maximum number of results to list per page.
listProgressUpdateStreams_maxResults :: Lens.Lens' ListProgressUpdateStreams (Core.Maybe Core.Natural)
listProgressUpdateStreams_maxResults = Lens.lens (\ListProgressUpdateStreams' {maxResults} -> maxResults) (\s@ListProgressUpdateStreams' {} a -> s {maxResults = a} :: ListProgressUpdateStreams)

instance Core.AWSPager ListProgressUpdateStreams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProgressUpdateStreamsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listProgressUpdateStreams_nextToken
          Lens..~ rs
          Lens.^? listProgressUpdateStreamsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListProgressUpdateStreams where
  type
    AWSResponse ListProgressUpdateStreams =
      ListProgressUpdateStreamsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProgressUpdateStreamsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ProgressUpdateStreamSummaryList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListProgressUpdateStreams

instance Core.NFData ListProgressUpdateStreams

instance Core.ToHeaders ListProgressUpdateStreams where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.ListProgressUpdateStreams" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListProgressUpdateStreams where
  toJSON ListProgressUpdateStreams' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListProgressUpdateStreams where
  toPath = Core.const "/"

instance Core.ToQuery ListProgressUpdateStreams where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListProgressUpdateStreamsResponse' smart constructor.
data ListProgressUpdateStreamsResponse = ListProgressUpdateStreamsResponse'
  { -- | If there are more streams created than the max result, return the next
    -- token to be passed to the next call as a bookmark of where to start
    -- from.
    nextToken :: Core.Maybe Core.Text,
    -- | List of progress update streams up to the max number of results passed
    -- in the input.
    progressUpdateStreamSummaryList :: Core.Maybe [ProgressUpdateStreamSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProgressUpdateStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProgressUpdateStreamsResponse_nextToken' - If there are more streams created than the max result, return the next
-- token to be passed to the next call as a bookmark of where to start
-- from.
--
-- 'progressUpdateStreamSummaryList', 'listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList' - List of progress update streams up to the max number of results passed
-- in the input.
--
-- 'httpStatus', 'listProgressUpdateStreamsResponse_httpStatus' - The response's http status code.
newListProgressUpdateStreamsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListProgressUpdateStreamsResponse
newListProgressUpdateStreamsResponse pHttpStatus_ =
  ListProgressUpdateStreamsResponse'
    { nextToken =
        Core.Nothing,
      progressUpdateStreamSummaryList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more streams created than the max result, return the next
-- token to be passed to the next call as a bookmark of where to start
-- from.
listProgressUpdateStreamsResponse_nextToken :: Lens.Lens' ListProgressUpdateStreamsResponse (Core.Maybe Core.Text)
listProgressUpdateStreamsResponse_nextToken = Lens.lens (\ListProgressUpdateStreamsResponse' {nextToken} -> nextToken) (\s@ListProgressUpdateStreamsResponse' {} a -> s {nextToken = a} :: ListProgressUpdateStreamsResponse)

-- | List of progress update streams up to the max number of results passed
-- in the input.
listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList :: Lens.Lens' ListProgressUpdateStreamsResponse (Core.Maybe [ProgressUpdateStreamSummary])
listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList = Lens.lens (\ListProgressUpdateStreamsResponse' {progressUpdateStreamSummaryList} -> progressUpdateStreamSummaryList) (\s@ListProgressUpdateStreamsResponse' {} a -> s {progressUpdateStreamSummaryList = a} :: ListProgressUpdateStreamsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listProgressUpdateStreamsResponse_httpStatus :: Lens.Lens' ListProgressUpdateStreamsResponse Core.Int
listProgressUpdateStreamsResponse_httpStatus = Lens.lens (\ListProgressUpdateStreamsResponse' {httpStatus} -> httpStatus) (\s@ListProgressUpdateStreamsResponse' {} a -> s {httpStatus = a} :: ListProgressUpdateStreamsResponse)

instance
  Core.NFData
    ListProgressUpdateStreamsResponse
