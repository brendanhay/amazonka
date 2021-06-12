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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRoutingProfileQueues' smart constructor.
data ListRoutingProfileQueues = ListRoutingProfileQueues'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'routingProfileId'
  Core.Text ->
  ListRoutingProfileQueues
newListRoutingProfileQueues
  pInstanceId_
  pRoutingProfileId_ =
    ListRoutingProfileQueues'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        instanceId = pInstanceId_,
        routingProfileId = pRoutingProfileId_
      }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listRoutingProfileQueues_nextToken :: Lens.Lens' ListRoutingProfileQueues (Core.Maybe Core.Text)
listRoutingProfileQueues_nextToken = Lens.lens (\ListRoutingProfileQueues' {nextToken} -> nextToken) (\s@ListRoutingProfileQueues' {} a -> s {nextToken = a} :: ListRoutingProfileQueues)

-- | The maximum number of results to return per page.
listRoutingProfileQueues_maxResults :: Lens.Lens' ListRoutingProfileQueues (Core.Maybe Core.Natural)
listRoutingProfileQueues_maxResults = Lens.lens (\ListRoutingProfileQueues' {maxResults} -> maxResults) (\s@ListRoutingProfileQueues' {} a -> s {maxResults = a} :: ListRoutingProfileQueues)

-- | The identifier of the Amazon Connect instance.
listRoutingProfileQueues_instanceId :: Lens.Lens' ListRoutingProfileQueues Core.Text
listRoutingProfileQueues_instanceId = Lens.lens (\ListRoutingProfileQueues' {instanceId} -> instanceId) (\s@ListRoutingProfileQueues' {} a -> s {instanceId = a} :: ListRoutingProfileQueues)

-- | The identifier of the routing profile.
listRoutingProfileQueues_routingProfileId :: Lens.Lens' ListRoutingProfileQueues Core.Text
listRoutingProfileQueues_routingProfileId = Lens.lens (\ListRoutingProfileQueues' {routingProfileId} -> routingProfileId) (\s@ListRoutingProfileQueues' {} a -> s {routingProfileId = a} :: ListRoutingProfileQueues)

instance Core.AWSPager ListRoutingProfileQueues where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRoutingProfileQueuesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listRoutingProfileQueuesResponse_routingProfileQueueConfigSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listRoutingProfileQueues_nextToken
          Lens..~ rs
          Lens.^? listRoutingProfileQueuesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListRoutingProfileQueues where
  type
    AWSResponse ListRoutingProfileQueues =
      ListRoutingProfileQueuesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoutingProfileQueuesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "RoutingProfileQueueConfigSummaryList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRoutingProfileQueues

instance Core.NFData ListRoutingProfileQueues

instance Core.ToHeaders ListRoutingProfileQueues where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListRoutingProfileQueues where
  toPath ListRoutingProfileQueues' {..} =
    Core.mconcat
      [ "/routing-profiles/",
        Core.toBS instanceId,
        "/",
        Core.toBS routingProfileId,
        "/queues"
      ]

instance Core.ToQuery ListRoutingProfileQueues where
  toQuery ListRoutingProfileQueues' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListRoutingProfileQueuesResponse' smart constructor.
data ListRoutingProfileQueuesResponse = ListRoutingProfileQueuesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the routing profiles.
    routingProfileQueueConfigSummaryList :: Core.Maybe [RoutingProfileQueueConfigSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListRoutingProfileQueuesResponse
newListRoutingProfileQueuesResponse pHttpStatus_ =
  ListRoutingProfileQueuesResponse'
    { nextToken =
        Core.Nothing,
      routingProfileQueueConfigSummaryList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listRoutingProfileQueuesResponse_nextToken :: Lens.Lens' ListRoutingProfileQueuesResponse (Core.Maybe Core.Text)
listRoutingProfileQueuesResponse_nextToken = Lens.lens (\ListRoutingProfileQueuesResponse' {nextToken} -> nextToken) (\s@ListRoutingProfileQueuesResponse' {} a -> s {nextToken = a} :: ListRoutingProfileQueuesResponse)

-- | Information about the routing profiles.
listRoutingProfileQueuesResponse_routingProfileQueueConfigSummaryList :: Lens.Lens' ListRoutingProfileQueuesResponse (Core.Maybe [RoutingProfileQueueConfigSummary])
listRoutingProfileQueuesResponse_routingProfileQueueConfigSummaryList = Lens.lens (\ListRoutingProfileQueuesResponse' {routingProfileQueueConfigSummaryList} -> routingProfileQueueConfigSummaryList) (\s@ListRoutingProfileQueuesResponse' {} a -> s {routingProfileQueueConfigSummaryList = a} :: ListRoutingProfileQueuesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRoutingProfileQueuesResponse_httpStatus :: Lens.Lens' ListRoutingProfileQueuesResponse Core.Int
listRoutingProfileQueuesResponse_httpStatus = Lens.lens (\ListRoutingProfileQueuesResponse' {httpStatus} -> httpStatus) (\s@ListRoutingProfileQueuesResponse' {} a -> s {httpStatus = a} :: ListRoutingProfileQueuesResponse)

instance Core.NFData ListRoutingProfileQueuesResponse
