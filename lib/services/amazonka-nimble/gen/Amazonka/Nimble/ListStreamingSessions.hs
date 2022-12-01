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
-- Module      : Amazonka.Nimble.ListStreamingSessions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the streaming sessions in a studio.
--
-- This operation returns paginated results.
module Amazonka.Nimble.ListStreamingSessions
  ( -- * Creating a Request
    ListStreamingSessions (..),
    newListStreamingSessions,

    -- * Request Lenses
    listStreamingSessions_nextToken,
    listStreamingSessions_sessionIds,
    listStreamingSessions_createdBy,
    listStreamingSessions_ownedBy,
    listStreamingSessions_studioId,

    -- * Destructuring the Response
    ListStreamingSessionsResponse (..),
    newListStreamingSessionsResponse,

    -- * Response Lenses
    listStreamingSessionsResponse_nextToken,
    listStreamingSessionsResponse_sessions,
    listStreamingSessionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStreamingSessions' smart constructor.
data ListStreamingSessions = ListStreamingSessions'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the request to only the provided session IDs.
    sessionIds :: Prelude.Maybe Prelude.Text,
    -- | Filters the request to streaming sessions created by the given user.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | Filters the request to streaming session owned by the given user
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamingSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStreamingSessions_nextToken' - The token to request the next page of results.
--
-- 'sessionIds', 'listStreamingSessions_sessionIds' - Filters the request to only the provided session IDs.
--
-- 'createdBy', 'listStreamingSessions_createdBy' - Filters the request to streaming sessions created by the given user.
--
-- 'ownedBy', 'listStreamingSessions_ownedBy' - Filters the request to streaming session owned by the given user
--
-- 'studioId', 'listStreamingSessions_studioId' - The studio ID.
newListStreamingSessions ::
  -- | 'studioId'
  Prelude.Text ->
  ListStreamingSessions
newListStreamingSessions pStudioId_ =
  ListStreamingSessions'
    { nextToken = Prelude.Nothing,
      sessionIds = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      ownedBy = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | The token to request the next page of results.
listStreamingSessions_nextToken :: Lens.Lens' ListStreamingSessions (Prelude.Maybe Prelude.Text)
listStreamingSessions_nextToken = Lens.lens (\ListStreamingSessions' {nextToken} -> nextToken) (\s@ListStreamingSessions' {} a -> s {nextToken = a} :: ListStreamingSessions)

-- | Filters the request to only the provided session IDs.
listStreamingSessions_sessionIds :: Lens.Lens' ListStreamingSessions (Prelude.Maybe Prelude.Text)
listStreamingSessions_sessionIds = Lens.lens (\ListStreamingSessions' {sessionIds} -> sessionIds) (\s@ListStreamingSessions' {} a -> s {sessionIds = a} :: ListStreamingSessions)

-- | Filters the request to streaming sessions created by the given user.
listStreamingSessions_createdBy :: Lens.Lens' ListStreamingSessions (Prelude.Maybe Prelude.Text)
listStreamingSessions_createdBy = Lens.lens (\ListStreamingSessions' {createdBy} -> createdBy) (\s@ListStreamingSessions' {} a -> s {createdBy = a} :: ListStreamingSessions)

-- | Filters the request to streaming session owned by the given user
listStreamingSessions_ownedBy :: Lens.Lens' ListStreamingSessions (Prelude.Maybe Prelude.Text)
listStreamingSessions_ownedBy = Lens.lens (\ListStreamingSessions' {ownedBy} -> ownedBy) (\s@ListStreamingSessions' {} a -> s {ownedBy = a} :: ListStreamingSessions)

-- | The studio ID.
listStreamingSessions_studioId :: Lens.Lens' ListStreamingSessions Prelude.Text
listStreamingSessions_studioId = Lens.lens (\ListStreamingSessions' {studioId} -> studioId) (\s@ListStreamingSessions' {} a -> s {studioId = a} :: ListStreamingSessions)

instance Core.AWSPager ListStreamingSessions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStreamingSessionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStreamingSessionsResponse_sessions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStreamingSessions_nextToken
          Lens..~ rs
          Lens.^? listStreamingSessionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListStreamingSessions where
  type
    AWSResponse ListStreamingSessions =
      ListStreamingSessionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamingSessionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "sessions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStreamingSessions where
  hashWithSalt _salt ListStreamingSessions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sessionIds
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` ownedBy
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData ListStreamingSessions where
  rnf ListStreamingSessions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sessionIds
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf ownedBy
      `Prelude.seq` Prelude.rnf studioId

instance Core.ToHeaders ListStreamingSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListStreamingSessions where
  toPath ListStreamingSessions' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/streaming-sessions"
      ]

instance Core.ToQuery ListStreamingSessions where
  toQuery ListStreamingSessions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "sessionIds" Core.=: sessionIds,
        "createdBy" Core.=: createdBy,
        "ownedBy" Core.=: ownedBy
      ]

-- | /See:/ 'newListStreamingSessionsResponse' smart constructor.
data ListStreamingSessionsResponse = ListStreamingSessionsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of streaming sessions.
    sessions :: Prelude.Maybe [StreamingSession],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamingSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStreamingSessionsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'sessions', 'listStreamingSessionsResponse_sessions' - A collection of streaming sessions.
--
-- 'httpStatus', 'listStreamingSessionsResponse_httpStatus' - The response's http status code.
newListStreamingSessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStreamingSessionsResponse
newListStreamingSessionsResponse pHttpStatus_ =
  ListStreamingSessionsResponse'
    { nextToken =
        Prelude.Nothing,
      sessions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listStreamingSessionsResponse_nextToken :: Lens.Lens' ListStreamingSessionsResponse (Prelude.Maybe Prelude.Text)
listStreamingSessionsResponse_nextToken = Lens.lens (\ListStreamingSessionsResponse' {nextToken} -> nextToken) (\s@ListStreamingSessionsResponse' {} a -> s {nextToken = a} :: ListStreamingSessionsResponse)

-- | A collection of streaming sessions.
listStreamingSessionsResponse_sessions :: Lens.Lens' ListStreamingSessionsResponse (Prelude.Maybe [StreamingSession])
listStreamingSessionsResponse_sessions = Lens.lens (\ListStreamingSessionsResponse' {sessions} -> sessions) (\s@ListStreamingSessionsResponse' {} a -> s {sessions = a} :: ListStreamingSessionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStreamingSessionsResponse_httpStatus :: Lens.Lens' ListStreamingSessionsResponse Prelude.Int
listStreamingSessionsResponse_httpStatus = Lens.lens (\ListStreamingSessionsResponse' {httpStatus} -> httpStatus) (\s@ListStreamingSessionsResponse' {} a -> s {httpStatus = a} :: ListStreamingSessionsResponse)

instance Prelude.NFData ListStreamingSessionsResponse where
  rnf ListStreamingSessionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sessions
      `Prelude.seq` Prelude.rnf httpStatus
