{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListProgressUpdateStreams' smart constructor.
data ListProgressUpdateStreams = ListProgressUpdateStreams'
  { -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter to limit the maximum number of results to list per page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listProgressUpdateStreams_nextToken :: Lens.Lens' ListProgressUpdateStreams (Prelude.Maybe Prelude.Text)
listProgressUpdateStreams_nextToken = Lens.lens (\ListProgressUpdateStreams' {nextToken} -> nextToken) (\s@ListProgressUpdateStreams' {} a -> s {nextToken = a} :: ListProgressUpdateStreams)

-- | Filter to limit the maximum number of results to list per page.
listProgressUpdateStreams_maxResults :: Lens.Lens' ListProgressUpdateStreams (Prelude.Maybe Prelude.Natural)
listProgressUpdateStreams_maxResults = Lens.lens (\ListProgressUpdateStreams' {maxResults} -> maxResults) (\s@ListProgressUpdateStreams' {} a -> s {maxResults = a} :: ListProgressUpdateStreams)

instance Pager.AWSPager ListProgressUpdateStreams where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listProgressUpdateStreamsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listProgressUpdateStreams_nextToken
          Lens..~ rs
          Lens.^? listProgressUpdateStreamsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListProgressUpdateStreams where
  type
    Rs ListProgressUpdateStreams =
      ListProgressUpdateStreamsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProgressUpdateStreamsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ProgressUpdateStreamSummaryList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProgressUpdateStreams

instance Prelude.NFData ListProgressUpdateStreams

instance Prelude.ToHeaders ListProgressUpdateStreams where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSMigrationHub.ListProgressUpdateStreams" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListProgressUpdateStreams where
  toJSON ListProgressUpdateStreams' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListProgressUpdateStreams where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListProgressUpdateStreams where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList = Lens.lens (\ListProgressUpdateStreamsResponse' {progressUpdateStreamSummaryList} -> progressUpdateStreamSummaryList) (\s@ListProgressUpdateStreamsResponse' {} a -> s {progressUpdateStreamSummaryList = a} :: ListProgressUpdateStreamsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listProgressUpdateStreamsResponse_httpStatus :: Lens.Lens' ListProgressUpdateStreamsResponse Prelude.Int
listProgressUpdateStreamsResponse_httpStatus = Lens.lens (\ListProgressUpdateStreamsResponse' {httpStatus} -> httpStatus) (\s@ListProgressUpdateStreamsResponse' {} a -> s {httpStatus = a} :: ListProgressUpdateStreamsResponse)

instance
  Prelude.NFData
    ListProgressUpdateStreamsResponse
