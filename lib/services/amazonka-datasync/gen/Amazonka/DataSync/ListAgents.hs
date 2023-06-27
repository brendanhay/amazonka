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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DataSync agents that belong to an Amazon Web Services
-- account in the Amazon Web Services Region specified in the request.
--
-- With pagination, you can reduce the number of agents returned in a
-- response. If you get a truncated list of agents in a response, the
-- response contains a marker that you can specify in your next request to
-- fetch the next page of agents.
--
-- @ListAgents@ is eventually consistent. This means the result of running
-- the operation might not reflect that you just created or deleted an
-- agent. For example, if you create an agent with
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateAgent.html CreateAgent>
-- and then immediately run @ListAgents@, that agent might not show up in
-- the list right away. In situations like this, you can always confirm
-- whether an agent has been created (or deleted) by using
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_DescribeAgent.html DescribeAgent>.
--
-- This operation returns paginated results.
module Amazonka.DataSync.ListAgents
  ( -- * Creating a Request
    ListAgents (..),
    newListAgents,

    -- * Request Lenses
    listAgents_maxResults,
    listAgents_nextToken,

    -- * Destructuring the Response
    ListAgentsResponse (..),
    newListAgentsResponse,

    -- * Response Lenses
    listAgentsResponse_agents,
    listAgentsResponse_nextToken,
    listAgentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | ListAgentsRequest
--
-- /See:/ 'newListAgents' smart constructor.
data ListAgents = ListAgents'
  { -- | Specifies the maximum number of DataSync agents to list in a response.
    -- By default, a response shows a maximum of 100 agents.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies an opaque string that indicates the position to begin the next
    -- list of results in the response.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listAgents_maxResults' - Specifies the maximum number of DataSync agents to list in a response.
-- By default, a response shows a maximum of 100 agents.
--
-- 'nextToken', 'listAgents_nextToken' - Specifies an opaque string that indicates the position to begin the next
-- list of results in the response.
newListAgents ::
  ListAgents
newListAgents =
  ListAgents'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specifies the maximum number of DataSync agents to list in a response.
-- By default, a response shows a maximum of 100 agents.
listAgents_maxResults :: Lens.Lens' ListAgents (Prelude.Maybe Prelude.Natural)
listAgents_maxResults = Lens.lens (\ListAgents' {maxResults} -> maxResults) (\s@ListAgents' {} a -> s {maxResults = a} :: ListAgents)

-- | Specifies an opaque string that indicates the position to begin the next
-- list of results in the response.
listAgents_nextToken :: Lens.Lens' ListAgents (Prelude.Maybe Prelude.Text)
listAgents_nextToken = Lens.lens (\ListAgents' {nextToken} -> nextToken) (\s@ListAgents' {} a -> s {nextToken = a} :: ListAgents)

instance Core.AWSPager ListAgents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAgentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAgentsResponse_agents
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAgents_nextToken
          Lens..~ rs
          Lens.^? listAgentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAgents where
  type AWSResponse ListAgents = ListAgentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAgentsResponse'
            Prelude.<$> (x Data..?> "Agents" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAgents where
  hashWithSalt _salt ListAgents' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAgents where
  rnf ListAgents' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAgents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("FmrsService.ListAgents" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAgents where
  toJSON ListAgents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListAgents where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAgents where
  toQuery = Prelude.const Prelude.mempty

-- | ListAgentsResponse
--
-- /See:/ 'newListAgentsResponse' smart constructor.
data ListAgentsResponse = ListAgentsResponse'
  { -- | A list of DataSync agents in your Amazon Web Services account in the
    -- Amazon Web Services Region specified in the request. The list is ordered
    -- by the agents\' Amazon Resource Names (ARNs).
    agents :: Prelude.Maybe [AgentListEntry],
    -- | The opaque string that indicates the position to begin the next list of
    -- results in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'agents', 'listAgentsResponse_agents' - A list of DataSync agents in your Amazon Web Services account in the
-- Amazon Web Services Region specified in the request. The list is ordered
-- by the agents\' Amazon Resource Names (ARNs).
--
-- 'nextToken', 'listAgentsResponse_nextToken' - The opaque string that indicates the position to begin the next list of
-- results in the response.
--
-- 'httpStatus', 'listAgentsResponse_httpStatus' - The response's http status code.
newListAgentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAgentsResponse
newListAgentsResponse pHttpStatus_ =
  ListAgentsResponse'
    { agents = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of DataSync agents in your Amazon Web Services account in the
-- Amazon Web Services Region specified in the request. The list is ordered
-- by the agents\' Amazon Resource Names (ARNs).
listAgentsResponse_agents :: Lens.Lens' ListAgentsResponse (Prelude.Maybe [AgentListEntry])
listAgentsResponse_agents = Lens.lens (\ListAgentsResponse' {agents} -> agents) (\s@ListAgentsResponse' {} a -> s {agents = a} :: ListAgentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The opaque string that indicates the position to begin the next list of
-- results in the response.
listAgentsResponse_nextToken :: Lens.Lens' ListAgentsResponse (Prelude.Maybe Prelude.Text)
listAgentsResponse_nextToken = Lens.lens (\ListAgentsResponse' {nextToken} -> nextToken) (\s@ListAgentsResponse' {} a -> s {nextToken = a} :: ListAgentsResponse)

-- | The response's http status code.
listAgentsResponse_httpStatus :: Lens.Lens' ListAgentsResponse Prelude.Int
listAgentsResponse_httpStatus = Lens.lens (\ListAgentsResponse' {httpStatus} -> httpStatus) (\s@ListAgentsResponse' {} a -> s {httpStatus = a} :: ListAgentsResponse)

instance Prelude.NFData ListAgentsResponse where
  rnf ListAgentsResponse' {..} =
    Prelude.rnf agents
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
