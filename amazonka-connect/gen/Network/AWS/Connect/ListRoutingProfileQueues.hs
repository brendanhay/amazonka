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
-- Module      : Network.AWS.Connect.ListRoutingProfileQueues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the queues associated with a routing profile.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListRoutingProfileQueues
  ( -- * Creating a Request
    ListRoutingProfileQueues (..),
    newListRoutingProfileQueues,

    -- * Request Lenses
    listRoutingProfileQueues_nextToken,
    listRoutingProfileQueues_maxResults,
    listRoutingProfileQueues_instanceId,
    listRoutingProfileQueues_routingProfileId,

    -- * Destructuring the Response
    ListRoutingProfileQueuesResponse (..),
    newListRoutingProfileQueuesResponse,

    -- * Response Lenses
    listRoutingProfileQueuesResponse_nextToken,
    listRoutingProfileQueuesResponse_routingProfileQueueConfigSummaryList,
    listRoutingProfileQueuesResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRoutingProfileQueues' smart constructor.
data ListRoutingProfileQueues = ListRoutingProfileQueues'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListRoutingProfileQueues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoutingProfileQueues_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listRoutingProfileQueues_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listRoutingProfileQueues_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'routingProfileId', 'listRoutingProfileQueues_routingProfileId' - The identifier of the routing profile.
newListRoutingProfileQueues ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'routingProfileId'
  Prelude.Text ->
  ListRoutingProfileQueues
newListRoutingProfileQueues
  pInstanceId_
  pRoutingProfileId_ =
    ListRoutingProfileQueues'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        instanceId = pInstanceId_,
        routingProfileId = pRoutingProfileId_
      }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listRoutingProfileQueues_nextToken :: Lens.Lens' ListRoutingProfileQueues (Prelude.Maybe Prelude.Text)
listRoutingProfileQueues_nextToken = Lens.lens (\ListRoutingProfileQueues' {nextToken} -> nextToken) (\s@ListRoutingProfileQueues' {} a -> s {nextToken = a} :: ListRoutingProfileQueues)

-- | The maximum number of results to return per page.
listRoutingProfileQueues_maxResults :: Lens.Lens' ListRoutingProfileQueues (Prelude.Maybe Prelude.Natural)
listRoutingProfileQueues_maxResults = Lens.lens (\ListRoutingProfileQueues' {maxResults} -> maxResults) (\s@ListRoutingProfileQueues' {} a -> s {maxResults = a} :: ListRoutingProfileQueues)

-- | The identifier of the Amazon Connect instance.
listRoutingProfileQueues_instanceId :: Lens.Lens' ListRoutingProfileQueues Prelude.Text
listRoutingProfileQueues_instanceId = Lens.lens (\ListRoutingProfileQueues' {instanceId} -> instanceId) (\s@ListRoutingProfileQueues' {} a -> s {instanceId = a} :: ListRoutingProfileQueues)

-- | The identifier of the routing profile.
listRoutingProfileQueues_routingProfileId :: Lens.Lens' ListRoutingProfileQueues Prelude.Text
listRoutingProfileQueues_routingProfileId = Lens.lens (\ListRoutingProfileQueues' {routingProfileId} -> routingProfileId) (\s@ListRoutingProfileQueues' {} a -> s {routingProfileId = a} :: ListRoutingProfileQueues)

instance Pager.AWSPager ListRoutingProfileQueues where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listRoutingProfileQueuesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listRoutingProfileQueuesResponse_routingProfileQueueConfigSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listRoutingProfileQueues_nextToken
          Lens..~ rs
          Lens.^? listRoutingProfileQueuesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListRoutingProfileQueues where
  type
    Rs ListRoutingProfileQueues =
      ListRoutingProfileQueuesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoutingProfileQueuesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "RoutingProfileQueueConfigSummaryList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRoutingProfileQueues

instance Prelude.NFData ListRoutingProfileQueues

instance Prelude.ToHeaders ListRoutingProfileQueues where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListRoutingProfileQueues where
  toPath ListRoutingProfileQueues' {..} =
    Prelude.mconcat
      [ "/routing-profiles/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS routingProfileId,
        "/queues"
      ]

instance Prelude.ToQuery ListRoutingProfileQueues where
  toQuery ListRoutingProfileQueues' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListRoutingProfileQueuesResponse' smart constructor.
data ListRoutingProfileQueuesResponse = ListRoutingProfileQueuesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the routing profiles.
    routingProfileQueueConfigSummaryList :: Prelude.Maybe [RoutingProfileQueueConfigSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListRoutingProfileQueuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoutingProfileQueuesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'routingProfileQueueConfigSummaryList', 'listRoutingProfileQueuesResponse_routingProfileQueueConfigSummaryList' - Information about the routing profiles.
--
-- 'httpStatus', 'listRoutingProfileQueuesResponse_httpStatus' - The response's http status code.
newListRoutingProfileQueuesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRoutingProfileQueuesResponse
newListRoutingProfileQueuesResponse pHttpStatus_ =
  ListRoutingProfileQueuesResponse'
    { nextToken =
        Prelude.Nothing,
      routingProfileQueueConfigSummaryList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listRoutingProfileQueuesResponse_nextToken :: Lens.Lens' ListRoutingProfileQueuesResponse (Prelude.Maybe Prelude.Text)
listRoutingProfileQueuesResponse_nextToken = Lens.lens (\ListRoutingProfileQueuesResponse' {nextToken} -> nextToken) (\s@ListRoutingProfileQueuesResponse' {} a -> s {nextToken = a} :: ListRoutingProfileQueuesResponse)

-- | Information about the routing profiles.
listRoutingProfileQueuesResponse_routingProfileQueueConfigSummaryList :: Lens.Lens' ListRoutingProfileQueuesResponse (Prelude.Maybe [RoutingProfileQueueConfigSummary])
listRoutingProfileQueuesResponse_routingProfileQueueConfigSummaryList = Lens.lens (\ListRoutingProfileQueuesResponse' {routingProfileQueueConfigSummaryList} -> routingProfileQueueConfigSummaryList) (\s@ListRoutingProfileQueuesResponse' {} a -> s {routingProfileQueueConfigSummaryList = a} :: ListRoutingProfileQueuesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listRoutingProfileQueuesResponse_httpStatus :: Lens.Lens' ListRoutingProfileQueuesResponse Prelude.Int
listRoutingProfileQueuesResponse_httpStatus = Lens.lens (\ListRoutingProfileQueuesResponse' {httpStatus} -> httpStatus) (\s@ListRoutingProfileQueuesResponse' {} a -> s {httpStatus = a} :: ListRoutingProfileQueuesResponse)

instance
  Prelude.NFData
    ListRoutingProfileQueuesResponse
