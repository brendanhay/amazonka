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
-- Module      : Amazonka.MigrationHub.ListProgressUpdateStreams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists progress update streams associated with the user account making
-- this call.
--
-- This operation returns paginated results.
module Amazonka.MigrationHub.ListProgressUpdateStreams
  ( -- * Creating a Request
    ListProgressUpdateStreams (..),
    newListProgressUpdateStreams,

    -- * Request Lenses
    listProgressUpdateStreams_maxResults,
    listProgressUpdateStreams_nextToken,

    -- * Destructuring the Response
    ListProgressUpdateStreamsResponse (..),
    newListProgressUpdateStreamsResponse,

    -- * Response Lenses
    listProgressUpdateStreamsResponse_nextToken,
    listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList,
    listProgressUpdateStreamsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProgressUpdateStreams' smart constructor.
data ListProgressUpdateStreams = ListProgressUpdateStreams'
  { -- | Filter to limit the maximum number of results to list per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProgressUpdateStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProgressUpdateStreams_maxResults' - Filter to limit the maximum number of results to list per page.
--
-- 'nextToken', 'listProgressUpdateStreams_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
newListProgressUpdateStreams ::
  ListProgressUpdateStreams
newListProgressUpdateStreams =
  ListProgressUpdateStreams'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filter to limit the maximum number of results to list per page.
listProgressUpdateStreams_maxResults :: Lens.Lens' ListProgressUpdateStreams (Prelude.Maybe Prelude.Natural)
listProgressUpdateStreams_maxResults = Lens.lens (\ListProgressUpdateStreams' {maxResults} -> maxResults) (\s@ListProgressUpdateStreams' {} a -> s {maxResults = a} :: ListProgressUpdateStreams)

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listProgressUpdateStreams_nextToken :: Lens.Lens' ListProgressUpdateStreams (Prelude.Maybe Prelude.Text)
listProgressUpdateStreams_nextToken = Lens.lens (\ListProgressUpdateStreams' {nextToken} -> nextToken) (\s@ListProgressUpdateStreams' {} a -> s {nextToken = a} :: ListProgressUpdateStreams)

instance Core.AWSPager ListProgressUpdateStreams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProgressUpdateStreamsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listProgressUpdateStreams_nextToken
          Lens..~ rs
          Lens.^? listProgressUpdateStreamsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListProgressUpdateStreams where
  type
    AWSResponse ListProgressUpdateStreams =
      ListProgressUpdateStreamsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProgressUpdateStreamsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ProgressUpdateStreamSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProgressUpdateStreams where
  hashWithSalt _salt ListProgressUpdateStreams' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListProgressUpdateStreams where
  rnf ListProgressUpdateStreams' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListProgressUpdateStreams where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMigrationHub.ListProgressUpdateStreams" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListProgressUpdateStreams where
  toJSON ListProgressUpdateStreams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListProgressUpdateStreams where
  toPath = Prelude.const "/"

instance Data.ToQuery ListProgressUpdateStreams where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProgressUpdateStreamsResponse' smart constructor.
data ListProgressUpdateStreamsResponse = ListProgressUpdateStreamsResponse'
  { -- | If there are more streams created than the max result, return the next
    -- token to be passed to the next call as a bookmark of where to start
    -- from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of progress update streams up to the max number of results passed
    -- in the input.
    progressUpdateStreamSummaryList :: Prelude.Maybe [ProgressUpdateStreamSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListProgressUpdateStreamsResponse
newListProgressUpdateStreamsResponse pHttpStatus_ =
  ListProgressUpdateStreamsResponse'
    { nextToken =
        Prelude.Nothing,
      progressUpdateStreamSummaryList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more streams created than the max result, return the next
-- token to be passed to the next call as a bookmark of where to start
-- from.
listProgressUpdateStreamsResponse_nextToken :: Lens.Lens' ListProgressUpdateStreamsResponse (Prelude.Maybe Prelude.Text)
listProgressUpdateStreamsResponse_nextToken = Lens.lens (\ListProgressUpdateStreamsResponse' {nextToken} -> nextToken) (\s@ListProgressUpdateStreamsResponse' {} a -> s {nextToken = a} :: ListProgressUpdateStreamsResponse)

-- | List of progress update streams up to the max number of results passed
-- in the input.
listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList :: Lens.Lens' ListProgressUpdateStreamsResponse (Prelude.Maybe [ProgressUpdateStreamSummary])
listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList = Lens.lens (\ListProgressUpdateStreamsResponse' {progressUpdateStreamSummaryList} -> progressUpdateStreamSummaryList) (\s@ListProgressUpdateStreamsResponse' {} a -> s {progressUpdateStreamSummaryList = a} :: ListProgressUpdateStreamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProgressUpdateStreamsResponse_httpStatus :: Lens.Lens' ListProgressUpdateStreamsResponse Prelude.Int
listProgressUpdateStreamsResponse_httpStatus = Lens.lens (\ListProgressUpdateStreamsResponse' {httpStatus} -> httpStatus) (\s@ListProgressUpdateStreamsResponse' {} a -> s {httpStatus = a} :: ListProgressUpdateStreamsResponse)

instance
  Prelude.NFData
    ListProgressUpdateStreamsResponse
  where
  rnf ListProgressUpdateStreamsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf progressUpdateStreamSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
