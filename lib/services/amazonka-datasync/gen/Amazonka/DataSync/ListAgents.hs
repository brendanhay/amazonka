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
-- Module      : Amazonka.DataSync.ListAgents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of agents owned by an Amazon Web Services account in the
-- Amazon Web Services Region specified in the request. The returned list
-- is ordered by agent Amazon Resource Name (ARN).
--
-- By default, this operation returns a maximum of 100 agents. This
-- operation supports pagination that enables you to optionally reduce the
-- number of agents returned in a response.
--
-- If you have more agents than are returned in a response (that is, the
-- response returns only a truncated list of your agents), the response
-- contains a marker that you can specify in your next request to fetch the
-- next page of agents.
--
-- This operation returns paginated results.
module Amazonka.DataSync.ListAgents
  ( -- * Creating a Request
    ListAgents (..),
    newListAgents,

    -- * Request Lenses
    listAgents_nextToken,
    listAgents_maxResults,

    -- * Destructuring the Response
    ListAgentsResponse (..),
    newListAgentsResponse,

    -- * Response Lenses
    listAgentsResponse_nextToken,
    listAgentsResponse_agents,
    listAgentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | ListAgentsRequest
--
-- /See:/ 'newListAgents' smart constructor.
data ListAgents = ListAgents'
  { -- | An opaque string that indicates the position at which to begin the next
    -- list of agents.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of agents to list.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAgents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAgents_nextToken' - An opaque string that indicates the position at which to begin the next
-- list of agents.
--
-- 'maxResults', 'listAgents_maxResults' - The maximum number of agents to list.
newListAgents ::
  ListAgents
newListAgents =
  ListAgents'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | An opaque string that indicates the position at which to begin the next
-- list of agents.
listAgents_nextToken :: Lens.Lens' ListAgents (Prelude.Maybe Prelude.Text)
listAgents_nextToken = Lens.lens (\ListAgents' {nextToken} -> nextToken) (\s@ListAgents' {} a -> s {nextToken = a} :: ListAgents)

-- | The maximum number of agents to list.
listAgents_maxResults :: Lens.Lens' ListAgents (Prelude.Maybe Prelude.Natural)
listAgents_maxResults = Lens.lens (\ListAgents' {maxResults} -> maxResults) (\s@ListAgents' {} a -> s {maxResults = a} :: ListAgents)

instance Core.AWSPager ListAgents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAgentsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAgentsResponse_agents Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAgents_nextToken
          Lens..~ rs
          Lens.^? listAgentsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListAgents where
  type AWSResponse ListAgents = ListAgentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAgentsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Agents" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAgents where
  hashWithSalt _salt ListAgents' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListAgents where
  rnf ListAgents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListAgents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("FmrsService.ListAgents" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAgents where
  toJSON ListAgents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListAgents where
  toPath = Prelude.const "/"

instance Core.ToQuery ListAgents where
  toQuery = Prelude.const Prelude.mempty

-- | ListAgentsResponse
--
-- /See:/ 'newListAgentsResponse' smart constructor.
data ListAgentsResponse = ListAgentsResponse'
  { -- | An opaque string that indicates the position at which to begin returning
    -- the next list of agents.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of agents in your account.
    agents :: Prelude.Maybe [AgentListEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAgentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAgentsResponse_nextToken' - An opaque string that indicates the position at which to begin returning
-- the next list of agents.
--
-- 'agents', 'listAgentsResponse_agents' - A list of agents in your account.
--
-- 'httpStatus', 'listAgentsResponse_httpStatus' - The response's http status code.
newListAgentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAgentsResponse
newListAgentsResponse pHttpStatus_ =
  ListAgentsResponse'
    { nextToken = Prelude.Nothing,
      agents = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque string that indicates the position at which to begin returning
-- the next list of agents.
listAgentsResponse_nextToken :: Lens.Lens' ListAgentsResponse (Prelude.Maybe Prelude.Text)
listAgentsResponse_nextToken = Lens.lens (\ListAgentsResponse' {nextToken} -> nextToken) (\s@ListAgentsResponse' {} a -> s {nextToken = a} :: ListAgentsResponse)

-- | A list of agents in your account.
listAgentsResponse_agents :: Lens.Lens' ListAgentsResponse (Prelude.Maybe [AgentListEntry])
listAgentsResponse_agents = Lens.lens (\ListAgentsResponse' {agents} -> agents) (\s@ListAgentsResponse' {} a -> s {agents = a} :: ListAgentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAgentsResponse_httpStatus :: Lens.Lens' ListAgentsResponse Prelude.Int
listAgentsResponse_httpStatus = Lens.lens (\ListAgentsResponse' {httpStatus} -> httpStatus) (\s@ListAgentsResponse' {} a -> s {httpStatus = a} :: ListAgentsResponse)

instance Prelude.NFData ListAgentsResponse where
  rnf ListAgentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf agents
      `Prelude.seq` Prelude.rnf httpStatus
