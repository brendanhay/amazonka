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
-- Module      : Amazonka.Glue.ListSessions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of sessions.
module Amazonka.Glue.ListSessions
  ( -- * Creating a Request
    ListSessions (..),
    newListSessions,

    -- * Request Lenses
    listSessions_tags,
    listSessions_nextToken,
    listSessions_requestOrigin,
    listSessions_maxResults,

    -- * Destructuring the Response
    ListSessionsResponse (..),
    newListSessionsResponse,

    -- * Response Lenses
    listSessionsResponse_nextToken,
    listSessionsResponse_sessions,
    listSessionsResponse_ids,
    listSessionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSessions' smart constructor.
data ListSessions = ListSessions'
  { -- | Tags belonging to the session.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The token for the next set of results, or null if there are no more
    -- result.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The origin of the request.
    requestOrigin :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listSessions_tags' - Tags belonging to the session.
--
-- 'nextToken', 'listSessions_nextToken' - The token for the next set of results, or null if there are no more
-- result.
--
-- 'requestOrigin', 'listSessions_requestOrigin' - The origin of the request.
--
-- 'maxResults', 'listSessions_maxResults' - The maximum number of results.
newListSessions ::
  ListSessions
newListSessions =
  ListSessions'
    { tags = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestOrigin = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Tags belonging to the session.
listSessions_tags :: Lens.Lens' ListSessions (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listSessions_tags = Lens.lens (\ListSessions' {tags} -> tags) (\s@ListSessions' {} a -> s {tags = a} :: ListSessions) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- result.
listSessions_nextToken :: Lens.Lens' ListSessions (Prelude.Maybe Prelude.Text)
listSessions_nextToken = Lens.lens (\ListSessions' {nextToken} -> nextToken) (\s@ListSessions' {} a -> s {nextToken = a} :: ListSessions)

-- | The origin of the request.
listSessions_requestOrigin :: Lens.Lens' ListSessions (Prelude.Maybe Prelude.Text)
listSessions_requestOrigin = Lens.lens (\ListSessions' {requestOrigin} -> requestOrigin) (\s@ListSessions' {} a -> s {requestOrigin = a} :: ListSessions)

-- | The maximum number of results.
listSessions_maxResults :: Lens.Lens' ListSessions (Prelude.Maybe Prelude.Natural)
listSessions_maxResults = Lens.lens (\ListSessions' {maxResults} -> maxResults) (\s@ListSessions' {} a -> s {maxResults = a} :: ListSessions)

instance Core.AWSRequest ListSessions where
  type AWSResponse ListSessions = ListSessionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSessionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Sessions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Ids" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSessions where
  hashWithSalt _salt ListSessions' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` requestOrigin
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListSessions where
  rnf ListSessions' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestOrigin
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.ListSessions" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSessions where
  toJSON ListSessions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("RequestOrigin" Data..=) Prelude.<$> requestOrigin,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListSessions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSessions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSessionsResponse' smart constructor.
data ListSessionsResponse = ListSessionsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- result.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns the session object.
    sessions :: Prelude.Maybe [Session],
    -- | Returns the ID of the session.
    ids :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSessionsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- result.
--
-- 'sessions', 'listSessionsResponse_sessions' - Returns the session object.
--
-- 'ids', 'listSessionsResponse_ids' - Returns the ID of the session.
--
-- 'httpStatus', 'listSessionsResponse_httpStatus' - The response's http status code.
newListSessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSessionsResponse
newListSessionsResponse pHttpStatus_ =
  ListSessionsResponse'
    { nextToken = Prelude.Nothing,
      sessions = Prelude.Nothing,
      ids = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- result.
listSessionsResponse_nextToken :: Lens.Lens' ListSessionsResponse (Prelude.Maybe Prelude.Text)
listSessionsResponse_nextToken = Lens.lens (\ListSessionsResponse' {nextToken} -> nextToken) (\s@ListSessionsResponse' {} a -> s {nextToken = a} :: ListSessionsResponse)

-- | Returns the session object.
listSessionsResponse_sessions :: Lens.Lens' ListSessionsResponse (Prelude.Maybe [Session])
listSessionsResponse_sessions = Lens.lens (\ListSessionsResponse' {sessions} -> sessions) (\s@ListSessionsResponse' {} a -> s {sessions = a} :: ListSessionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns the ID of the session.
listSessionsResponse_ids :: Lens.Lens' ListSessionsResponse (Prelude.Maybe [Prelude.Text])
listSessionsResponse_ids = Lens.lens (\ListSessionsResponse' {ids} -> ids) (\s@ListSessionsResponse' {} a -> s {ids = a} :: ListSessionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSessionsResponse_httpStatus :: Lens.Lens' ListSessionsResponse Prelude.Int
listSessionsResponse_httpStatus = Lens.lens (\ListSessionsResponse' {httpStatus} -> httpStatus) (\s@ListSessionsResponse' {} a -> s {httpStatus = a} :: ListSessionsResponse)

instance Prelude.NFData ListSessionsResponse where
  rnf ListSessionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sessions
      `Prelude.seq` Prelude.rnf ids
      `Prelude.seq` Prelude.rnf httpStatus
