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
-- Module      : Amazonka.Polly.ListSpeechSynthesisTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Polly.ListSpeechSynthesisTasks
  ( -- * Creating a Request
    ListSpeechSynthesisTasks (..),
    newListSpeechSynthesisTasks,

    -- * Request Lenses
    listSpeechSynthesisTasks_maxResults,
    listSpeechSynthesisTasks_nextToken,
    listSpeechSynthesisTasks_status,

    -- * Destructuring the Response
    ListSpeechSynthesisTasksResponse (..),
    newListSpeechSynthesisTasksResponse,

    -- * Response Lenses
    listSpeechSynthesisTasksResponse_nextToken,
    listSpeechSynthesisTasksResponse_synthesisTasks,
    listSpeechSynthesisTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Polly.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSpeechSynthesisTasks' smart constructor.
data ListSpeechSynthesisTasks = ListSpeechSynthesisTasks'
  { -- | Maximum number of speech synthesis tasks returned in a List operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to use in the next request to continue the listing
    -- of speech synthesis tasks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Status of the speech synthesis tasks returned in a List operation
    status :: Prelude.Maybe TaskStatus
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
-- 'maxResults', 'listSpeechSynthesisTasks_maxResults' - Maximum number of speech synthesis tasks returned in a List operation.
--
-- 'nextToken', 'listSpeechSynthesisTasks_nextToken' - The pagination token to use in the next request to continue the listing
-- of speech synthesis tasks.
--
-- 'status', 'listSpeechSynthesisTasks_status' - Status of the speech synthesis tasks returned in a List operation
newListSpeechSynthesisTasks ::
  ListSpeechSynthesisTasks
newListSpeechSynthesisTasks =
  ListSpeechSynthesisTasks'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Maximum number of speech synthesis tasks returned in a List operation.
listSpeechSynthesisTasks_maxResults :: Lens.Lens' ListSpeechSynthesisTasks (Prelude.Maybe Prelude.Natural)
listSpeechSynthesisTasks_maxResults = Lens.lens (\ListSpeechSynthesisTasks' {maxResults} -> maxResults) (\s@ListSpeechSynthesisTasks' {} a -> s {maxResults = a} :: ListSpeechSynthesisTasks)

-- | The pagination token to use in the next request to continue the listing
-- of speech synthesis tasks.
listSpeechSynthesisTasks_nextToken :: Lens.Lens' ListSpeechSynthesisTasks (Prelude.Maybe Prelude.Text)
listSpeechSynthesisTasks_nextToken = Lens.lens (\ListSpeechSynthesisTasks' {nextToken} -> nextToken) (\s@ListSpeechSynthesisTasks' {} a -> s {nextToken = a} :: ListSpeechSynthesisTasks)

-- | Status of the speech synthesis tasks returned in a List operation
listSpeechSynthesisTasks_status :: Lens.Lens' ListSpeechSynthesisTasks (Prelude.Maybe TaskStatus)
listSpeechSynthesisTasks_status = Lens.lens (\ListSpeechSynthesisTasks' {status} -> status) (\s@ListSpeechSynthesisTasks' {} a -> s {status = a} :: ListSpeechSynthesisTasks)

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSpeechSynthesisTasksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SynthesisTasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSpeechSynthesisTasks where
  hashWithSalt _salt ListSpeechSynthesisTasks' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListSpeechSynthesisTasks where
  rnf ListSpeechSynthesisTasks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders ListSpeechSynthesisTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListSpeechSynthesisTasks where
  toPath = Prelude.const "/v1/synthesisTasks"

instance Data.ToQuery ListSpeechSynthesisTasks where
  toQuery ListSpeechSynthesisTasks' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "Status" Data.=: status
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
listSpeechSynthesisTasksResponse_synthesisTasks = Lens.lens (\ListSpeechSynthesisTasksResponse' {synthesisTasks} -> synthesisTasks) (\s@ListSpeechSynthesisTasksResponse' {} a -> s {synthesisTasks = a} :: ListSpeechSynthesisTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSpeechSynthesisTasksResponse_httpStatus :: Lens.Lens' ListSpeechSynthesisTasksResponse Prelude.Int
listSpeechSynthesisTasksResponse_httpStatus = Lens.lens (\ListSpeechSynthesisTasksResponse' {httpStatus} -> httpStatus) (\s@ListSpeechSynthesisTasksResponse' {} a -> s {httpStatus = a} :: ListSpeechSynthesisTasksResponse)

instance
  Prelude.NFData
    ListSpeechSynthesisTasksResponse
  where
  rnf ListSpeechSynthesisTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf synthesisTasks
      `Prelude.seq` Prelude.rnf httpStatus
