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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSpeechSynthesisTasks' smart constructor.
data ListSpeechSynthesisTasks = ListSpeechSynthesisTasks'
  { -- | Status of the speech synthesis tasks returned in a List operation
    status :: Prelude.Maybe TaskStatus,
    -- | The pagination token to use in the next request to continue the listing
    -- of speech synthesis tasks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of speech synthesis tasks returned in a List operation.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { status = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Status of the speech synthesis tasks returned in a List operation
listSpeechSynthesisTasks_status :: Lens.Lens' ListSpeechSynthesisTasks (Prelude.Maybe TaskStatus)
listSpeechSynthesisTasks_status = Lens.lens (\ListSpeechSynthesisTasks' {status} -> status) (\s@ListSpeechSynthesisTasks' {} a -> s {status = a} :: ListSpeechSynthesisTasks)

-- | The pagination token to use in the next request to continue the listing
-- of speech synthesis tasks.
listSpeechSynthesisTasks_nextToken :: Lens.Lens' ListSpeechSynthesisTasks (Prelude.Maybe Prelude.Text)
listSpeechSynthesisTasks_nextToken = Lens.lens (\ListSpeechSynthesisTasks' {nextToken} -> nextToken) (\s@ListSpeechSynthesisTasks' {} a -> s {nextToken = a} :: ListSpeechSynthesisTasks)

-- | Maximum number of speech synthesis tasks returned in a List operation.
listSpeechSynthesisTasks_maxResults :: Lens.Lens' ListSpeechSynthesisTasks (Prelude.Maybe Prelude.Natural)
listSpeechSynthesisTasks_maxResults = Lens.lens (\ListSpeechSynthesisTasks' {maxResults} -> maxResults) (\s@ListSpeechSynthesisTasks' {} a -> s {maxResults = a} :: ListSpeechSynthesisTasks)

instance Core.AWSPager ListSpeechSynthesisTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSpeechSynthesisTasksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSpeechSynthesisTasksResponse_synthesisTasks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSpeechSynthesisTasks_nextToken
          Lens..~ rs
          Lens.^? listSpeechSynthesisTasksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSpeechSynthesisTasks where
  type
    AWSResponse ListSpeechSynthesisTasks =
      ListSpeechSynthesisTasksResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSpeechSynthesisTasksResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "SynthesisTasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSpeechSynthesisTasks

instance Prelude.NFData ListSpeechSynthesisTasks

instance Core.ToHeaders ListSpeechSynthesisTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListSpeechSynthesisTasks where
  toPath = Prelude.const "/v1/synthesisTasks"

instance Core.ToQuery ListSpeechSynthesisTasks where
  toQuery ListSpeechSynthesisTasks' {..} =
    Prelude.mconcat
      [ "Status" Core.=: status,
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListSpeechSynthesisTasksResponse' smart constructor.
data ListSpeechSynthesisTasksResponse = ListSpeechSynthesisTasksResponse'
  { -- | An opaque pagination token returned from the previous List operation in
    -- this request. If present, this indicates where to continue the listing.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of SynthesisTask objects that provides information from the
    -- specified task in the list request, including output format, creation
    -- time, task status, and so on.
    synthesisTasks :: Prelude.Maybe [SynthesisTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListSpeechSynthesisTasksResponse
newListSpeechSynthesisTasksResponse pHttpStatus_ =
  ListSpeechSynthesisTasksResponse'
    { nextToken =
        Prelude.Nothing,
      synthesisTasks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque pagination token returned from the previous List operation in
-- this request. If present, this indicates where to continue the listing.
listSpeechSynthesisTasksResponse_nextToken :: Lens.Lens' ListSpeechSynthesisTasksResponse (Prelude.Maybe Prelude.Text)
listSpeechSynthesisTasksResponse_nextToken = Lens.lens (\ListSpeechSynthesisTasksResponse' {nextToken} -> nextToken) (\s@ListSpeechSynthesisTasksResponse' {} a -> s {nextToken = a} :: ListSpeechSynthesisTasksResponse)

-- | List of SynthesisTask objects that provides information from the
-- specified task in the list request, including output format, creation
-- time, task status, and so on.
listSpeechSynthesisTasksResponse_synthesisTasks :: Lens.Lens' ListSpeechSynthesisTasksResponse (Prelude.Maybe [SynthesisTask])
listSpeechSynthesisTasksResponse_synthesisTasks = Lens.lens (\ListSpeechSynthesisTasksResponse' {synthesisTasks} -> synthesisTasks) (\s@ListSpeechSynthesisTasksResponse' {} a -> s {synthesisTasks = a} :: ListSpeechSynthesisTasksResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSpeechSynthesisTasksResponse_httpStatus :: Lens.Lens' ListSpeechSynthesisTasksResponse Prelude.Int
listSpeechSynthesisTasksResponse_httpStatus = Lens.lens (\ListSpeechSynthesisTasksResponse' {httpStatus} -> httpStatus) (\s@ListSpeechSynthesisTasksResponse' {} a -> s {httpStatus = a} :: ListSpeechSynthesisTasksResponse)

instance
  Prelude.NFData
    ListSpeechSynthesisTasksResponse
