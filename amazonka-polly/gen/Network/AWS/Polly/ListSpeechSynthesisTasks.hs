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
-- Module      : Network.AWS.Polly.ListSpeechSynthesisTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of SpeechSynthesisTask objects ordered by their creation
-- date. This operation can filter the tasks by their status, for example,
-- allowing users to list only tasks that are completed.
--
-- This operation returns paginated results.
module Network.AWS.Polly.ListSpeechSynthesisTasks
  ( -- * Creating a Request
    ListSpeechSynthesisTasks (..),
    newListSpeechSynthesisTasks,

    -- * Request Lenses
    listSpeechSynthesisTasks_status,
    listSpeechSynthesisTasks_nextToken,
    listSpeechSynthesisTasks_maxResults,

    -- * Destructuring the Response
    ListSpeechSynthesisTasksResponse (..),
    newListSpeechSynthesisTasksResponse,

    -- * Response Lenses
    listSpeechSynthesisTasksResponse_nextToken,
    listSpeechSynthesisTasksResponse_synthesisTasks,
    listSpeechSynthesisTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSpeechSynthesisTasks' smart constructor.
data ListSpeechSynthesisTasks = ListSpeechSynthesisTasks'
  { -- | Status of the speech synthesis tasks returned in a List operation
    status :: Core.Maybe TaskStatus,
    -- | The pagination token to use in the next request to continue the listing
    -- of speech synthesis tasks.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of speech synthesis tasks returned in a List operation.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSpeechSynthesisTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listSpeechSynthesisTasks_status' - Status of the speech synthesis tasks returned in a List operation
--
-- 'nextToken', 'listSpeechSynthesisTasks_nextToken' - The pagination token to use in the next request to continue the listing
-- of speech synthesis tasks.
--
-- 'maxResults', 'listSpeechSynthesisTasks_maxResults' - Maximum number of speech synthesis tasks returned in a List operation.
newListSpeechSynthesisTasks ::
  ListSpeechSynthesisTasks
newListSpeechSynthesisTasks =
  ListSpeechSynthesisTasks'
    { status = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Status of the speech synthesis tasks returned in a List operation
listSpeechSynthesisTasks_status :: Lens.Lens' ListSpeechSynthesisTasks (Core.Maybe TaskStatus)
listSpeechSynthesisTasks_status = Lens.lens (\ListSpeechSynthesisTasks' {status} -> status) (\s@ListSpeechSynthesisTasks' {} a -> s {status = a} :: ListSpeechSynthesisTasks)

-- | The pagination token to use in the next request to continue the listing
-- of speech synthesis tasks.
listSpeechSynthesisTasks_nextToken :: Lens.Lens' ListSpeechSynthesisTasks (Core.Maybe Core.Text)
listSpeechSynthesisTasks_nextToken = Lens.lens (\ListSpeechSynthesisTasks' {nextToken} -> nextToken) (\s@ListSpeechSynthesisTasks' {} a -> s {nextToken = a} :: ListSpeechSynthesisTasks)

-- | Maximum number of speech synthesis tasks returned in a List operation.
listSpeechSynthesisTasks_maxResults :: Lens.Lens' ListSpeechSynthesisTasks (Core.Maybe Core.Natural)
listSpeechSynthesisTasks_maxResults = Lens.lens (\ListSpeechSynthesisTasks' {maxResults} -> maxResults) (\s@ListSpeechSynthesisTasks' {} a -> s {maxResults = a} :: ListSpeechSynthesisTasks)

instance Core.AWSPager ListSpeechSynthesisTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSpeechSynthesisTasksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSpeechSynthesisTasksResponse_synthesisTasks
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSpeechSynthesisTasks_nextToken
          Lens..~ rs
          Lens.^? listSpeechSynthesisTasksResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListSpeechSynthesisTasks where
  type
    AWSResponse ListSpeechSynthesisTasks =
      ListSpeechSynthesisTasksResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSpeechSynthesisTasksResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "SynthesisTasks" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSpeechSynthesisTasks

instance Core.NFData ListSpeechSynthesisTasks

instance Core.ToHeaders ListSpeechSynthesisTasks where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListSpeechSynthesisTasks where
  toPath = Core.const "/v1/synthesisTasks"

instance Core.ToQuery ListSpeechSynthesisTasks where
  toQuery ListSpeechSynthesisTasks' {..} =
    Core.mconcat
      [ "Status" Core.=: status,
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListSpeechSynthesisTasksResponse' smart constructor.
data ListSpeechSynthesisTasksResponse = ListSpeechSynthesisTasksResponse'
  { -- | An opaque pagination token returned from the previous List operation in
    -- this request. If present, this indicates where to continue the listing.
    nextToken :: Core.Maybe Core.Text,
    -- | List of SynthesisTask objects that provides information from the
    -- specified task in the list request, including output format, creation
    -- time, task status, and so on.
    synthesisTasks :: Core.Maybe [SynthesisTask],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSpeechSynthesisTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSpeechSynthesisTasksResponse_nextToken' - An opaque pagination token returned from the previous List operation in
-- this request. If present, this indicates where to continue the listing.
--
-- 'synthesisTasks', 'listSpeechSynthesisTasksResponse_synthesisTasks' - List of SynthesisTask objects that provides information from the
-- specified task in the list request, including output format, creation
-- time, task status, and so on.
--
-- 'httpStatus', 'listSpeechSynthesisTasksResponse_httpStatus' - The response's http status code.
newListSpeechSynthesisTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSpeechSynthesisTasksResponse
newListSpeechSynthesisTasksResponse pHttpStatus_ =
  ListSpeechSynthesisTasksResponse'
    { nextToken =
        Core.Nothing,
      synthesisTasks = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque pagination token returned from the previous List operation in
-- this request. If present, this indicates where to continue the listing.
listSpeechSynthesisTasksResponse_nextToken :: Lens.Lens' ListSpeechSynthesisTasksResponse (Core.Maybe Core.Text)
listSpeechSynthesisTasksResponse_nextToken = Lens.lens (\ListSpeechSynthesisTasksResponse' {nextToken} -> nextToken) (\s@ListSpeechSynthesisTasksResponse' {} a -> s {nextToken = a} :: ListSpeechSynthesisTasksResponse)

-- | List of SynthesisTask objects that provides information from the
-- specified task in the list request, including output format, creation
-- time, task status, and so on.
listSpeechSynthesisTasksResponse_synthesisTasks :: Lens.Lens' ListSpeechSynthesisTasksResponse (Core.Maybe [SynthesisTask])
listSpeechSynthesisTasksResponse_synthesisTasks = Lens.lens (\ListSpeechSynthesisTasksResponse' {synthesisTasks} -> synthesisTasks) (\s@ListSpeechSynthesisTasksResponse' {} a -> s {synthesisTasks = a} :: ListSpeechSynthesisTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSpeechSynthesisTasksResponse_httpStatus :: Lens.Lens' ListSpeechSynthesisTasksResponse Core.Int
listSpeechSynthesisTasksResponse_httpStatus = Lens.lens (\ListSpeechSynthesisTasksResponse' {httpStatus} -> httpStatus) (\s@ListSpeechSynthesisTasksResponse' {} a -> s {httpStatus = a} :: ListSpeechSynthesisTasksResponse)

instance Core.NFData ListSpeechSynthesisTasksResponse
