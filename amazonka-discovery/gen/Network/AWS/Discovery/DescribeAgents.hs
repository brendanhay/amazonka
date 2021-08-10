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
-- Module      : Network.AWS.Discovery.DescribeAgents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists agents or connectors as specified by ID or other filters. All
-- agents\/connectors associated with your user account can be listed if
-- you call @DescribeAgents@ as is without passing any parameters.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeAgents
  ( -- * Creating a Request
    DescribeAgents (..),
    newDescribeAgents,

    -- * Request Lenses
    describeAgents_agentIds,
    describeAgents_nextToken,
    describeAgents_maxResults,
    describeAgents_filters,

    -- * Destructuring the Response
    DescribeAgentsResponse (..),
    newDescribeAgentsResponse,

    -- * Response Lenses
    describeAgentsResponse_nextToken,
    describeAgentsResponse_agentsInfo,
    describeAgentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAgents' smart constructor.
data DescribeAgents = DescribeAgents'
  { -- | The agent or the Connector IDs for which you want information. If you
    -- specify no IDs, the system returns information about all
    -- agents\/Connectors associated with your AWS user account.
    agentIds :: Prelude.Maybe [Prelude.Text],
    -- | Token to retrieve the next set of results. For example, if you
    -- previously specified 100 IDs for @DescribeAgentsRequest$agentIds@ but
    -- set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10
    -- results along with a token. Use that token in this query to get the next
    -- set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of agents\/Connectors to return in a single page of
    -- output. The maximum value is 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | You can filter the request using various logical operators and a
    -- /key/-/value/ format. For example:
    --
    -- @{\"key\": \"collectionStatus\", \"value\": \"STARTED\"}@
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAgents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentIds', 'describeAgents_agentIds' - The agent or the Connector IDs for which you want information. If you
-- specify no IDs, the system returns information about all
-- agents\/Connectors associated with your AWS user account.
--
-- 'nextToken', 'describeAgents_nextToken' - Token to retrieve the next set of results. For example, if you
-- previously specified 100 IDs for @DescribeAgentsRequest$agentIds@ but
-- set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10
-- results along with a token. Use that token in this query to get the next
-- set of 10.
--
-- 'maxResults', 'describeAgents_maxResults' - The total number of agents\/Connectors to return in a single page of
-- output. The maximum value is 100.
--
-- 'filters', 'describeAgents_filters' - You can filter the request using various logical operators and a
-- /key/-/value/ format. For example:
--
-- @{\"key\": \"collectionStatus\", \"value\": \"STARTED\"}@
newDescribeAgents ::
  DescribeAgents
newDescribeAgents =
  DescribeAgents'
    { agentIds = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The agent or the Connector IDs for which you want information. If you
-- specify no IDs, the system returns information about all
-- agents\/Connectors associated with your AWS user account.
describeAgents_agentIds :: Lens.Lens' DescribeAgents (Prelude.Maybe [Prelude.Text])
describeAgents_agentIds = Lens.lens (\DescribeAgents' {agentIds} -> agentIds) (\s@DescribeAgents' {} a -> s {agentIds = a} :: DescribeAgents) Prelude.. Lens.mapping Lens._Coerce

-- | Token to retrieve the next set of results. For example, if you
-- previously specified 100 IDs for @DescribeAgentsRequest$agentIds@ but
-- set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10
-- results along with a token. Use that token in this query to get the next
-- set of 10.
describeAgents_nextToken :: Lens.Lens' DescribeAgents (Prelude.Maybe Prelude.Text)
describeAgents_nextToken = Lens.lens (\DescribeAgents' {nextToken} -> nextToken) (\s@DescribeAgents' {} a -> s {nextToken = a} :: DescribeAgents)

-- | The total number of agents\/Connectors to return in a single page of
-- output. The maximum value is 100.
describeAgents_maxResults :: Lens.Lens' DescribeAgents (Prelude.Maybe Prelude.Int)
describeAgents_maxResults = Lens.lens (\DescribeAgents' {maxResults} -> maxResults) (\s@DescribeAgents' {} a -> s {maxResults = a} :: DescribeAgents)

-- | You can filter the request using various logical operators and a
-- /key/-/value/ format. For example:
--
-- @{\"key\": \"collectionStatus\", \"value\": \"STARTED\"}@
describeAgents_filters :: Lens.Lens' DescribeAgents (Prelude.Maybe [Filter])
describeAgents_filters = Lens.lens (\DescribeAgents' {filters} -> filters) (\s@DescribeAgents' {} a -> s {filters = a} :: DescribeAgents) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeAgents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAgentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAgentsResponse_agentsInfo
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAgents_nextToken
          Lens..~ rs
          Lens.^? describeAgentsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribeAgents where
  type
    AWSResponse DescribeAgents =
      DescribeAgentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAgentsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "agentsInfo" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAgents

instance Prelude.NFData DescribeAgents

instance Core.ToHeaders DescribeAgents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DescribeAgents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAgents where
  toJSON DescribeAgents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("agentIds" Core..=) Prelude.<$> agentIds,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("filters" Core..=) Prelude.<$> filters
          ]
      )

instance Core.ToPath DescribeAgents where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAgents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAgentsResponse' smart constructor.
data DescribeAgentsResponse = DescribeAgentsResponse'
  { -- | Token to retrieve the next set of results. For example, if you specified
    -- 100 IDs for @DescribeAgentsRequest$agentIds@ but set
    -- @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10
    -- results along with this token. Use this token in the next query to
    -- retrieve the next set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists agents or the Connector by ID or lists all agents\/Connectors
    -- associated with your user account if you did not specify an
    -- agent\/Connector ID. The output includes agent\/Connector IDs, IP
    -- addresses, media access control (MAC) addresses, agent\/Connector
    -- health, host name where the agent\/Connector resides, and the version
    -- number of each agent\/Connector.
    agentsInfo :: Prelude.Maybe [AgentInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAgentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAgentsResponse_nextToken' - Token to retrieve the next set of results. For example, if you specified
-- 100 IDs for @DescribeAgentsRequest$agentIds@ but set
-- @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10
-- results along with this token. Use this token in the next query to
-- retrieve the next set of 10.
--
-- 'agentsInfo', 'describeAgentsResponse_agentsInfo' - Lists agents or the Connector by ID or lists all agents\/Connectors
-- associated with your user account if you did not specify an
-- agent\/Connector ID. The output includes agent\/Connector IDs, IP
-- addresses, media access control (MAC) addresses, agent\/Connector
-- health, host name where the agent\/Connector resides, and the version
-- number of each agent\/Connector.
--
-- 'httpStatus', 'describeAgentsResponse_httpStatus' - The response's http status code.
newDescribeAgentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAgentsResponse
newDescribeAgentsResponse pHttpStatus_ =
  DescribeAgentsResponse'
    { nextToken =
        Prelude.Nothing,
      agentsInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token to retrieve the next set of results. For example, if you specified
-- 100 IDs for @DescribeAgentsRequest$agentIds@ but set
-- @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10
-- results along with this token. Use this token in the next query to
-- retrieve the next set of 10.
describeAgentsResponse_nextToken :: Lens.Lens' DescribeAgentsResponse (Prelude.Maybe Prelude.Text)
describeAgentsResponse_nextToken = Lens.lens (\DescribeAgentsResponse' {nextToken} -> nextToken) (\s@DescribeAgentsResponse' {} a -> s {nextToken = a} :: DescribeAgentsResponse)

-- | Lists agents or the Connector by ID or lists all agents\/Connectors
-- associated with your user account if you did not specify an
-- agent\/Connector ID. The output includes agent\/Connector IDs, IP
-- addresses, media access control (MAC) addresses, agent\/Connector
-- health, host name where the agent\/Connector resides, and the version
-- number of each agent\/Connector.
describeAgentsResponse_agentsInfo :: Lens.Lens' DescribeAgentsResponse (Prelude.Maybe [AgentInfo])
describeAgentsResponse_agentsInfo = Lens.lens (\DescribeAgentsResponse' {agentsInfo} -> agentsInfo) (\s@DescribeAgentsResponse' {} a -> s {agentsInfo = a} :: DescribeAgentsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAgentsResponse_httpStatus :: Lens.Lens' DescribeAgentsResponse Prelude.Int
describeAgentsResponse_httpStatus = Lens.lens (\DescribeAgentsResponse' {httpStatus} -> httpStatus) (\s@DescribeAgentsResponse' {} a -> s {httpStatus = a} :: DescribeAgentsResponse)

instance Prelude.NFData DescribeAgentsResponse
