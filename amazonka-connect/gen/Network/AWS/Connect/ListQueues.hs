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
-- Module      : Network.AWS.Connect.ListQueues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the queues for the specified Amazon Connect
-- instance.
--
-- For more information about queues, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-queues-standard-and-agent.html Queues: Standard and Agent>
-- in the /Amazon Connect Administrator Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListQueues
  ( -- * Creating a Request
    ListQueues (..),
    newListQueues,

    -- * Request Lenses
    listQueues_nextToken,
    listQueues_maxResults,
    listQueues_queueTypes,
    listQueues_instanceId,

    -- * Destructuring the Response
    ListQueuesResponse (..),
    newListQueuesResponse,

    -- * Response Lenses
    listQueuesResponse_nextToken,
    listQueuesResponse_queueSummaryList,
    listQueuesResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListQueues' smart constructor.
data ListQueues = ListQueues'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The type of queue.
    queueTypes :: Core.Maybe [QueueType],
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListQueues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQueues_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listQueues_maxResults' - The maximum number of results to return per page.
--
-- 'queueTypes', 'listQueues_queueTypes' - The type of queue.
--
-- 'instanceId', 'listQueues_instanceId' - The identifier of the Amazon Connect instance.
newListQueues ::
  -- | 'instanceId'
  Core.Text ->
  ListQueues
newListQueues pInstanceId_ =
  ListQueues'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      queueTypes = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listQueues_nextToken :: Lens.Lens' ListQueues (Core.Maybe Core.Text)
listQueues_nextToken = Lens.lens (\ListQueues' {nextToken} -> nextToken) (\s@ListQueues' {} a -> s {nextToken = a} :: ListQueues)

-- | The maximum number of results to return per page.
listQueues_maxResults :: Lens.Lens' ListQueues (Core.Maybe Core.Natural)
listQueues_maxResults = Lens.lens (\ListQueues' {maxResults} -> maxResults) (\s@ListQueues' {} a -> s {maxResults = a} :: ListQueues)

-- | The type of queue.
listQueues_queueTypes :: Lens.Lens' ListQueues (Core.Maybe [QueueType])
listQueues_queueTypes = Lens.lens (\ListQueues' {queueTypes} -> queueTypes) (\s@ListQueues' {} a -> s {queueTypes = a} :: ListQueues) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the Amazon Connect instance.
listQueues_instanceId :: Lens.Lens' ListQueues Core.Text
listQueues_instanceId = Lens.lens (\ListQueues' {instanceId} -> instanceId) (\s@ListQueues' {} a -> s {instanceId = a} :: ListQueues)

instance Core.AWSPager ListQueues where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listQueuesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listQueuesResponse_queueSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listQueues_nextToken
          Lens..~ rs
          Lens.^? listQueuesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListQueues where
  type AWSResponse ListQueues = ListQueuesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQueuesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "QueueSummaryList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListQueues

instance Core.NFData ListQueues

instance Core.ToHeaders ListQueues where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListQueues where
  toPath ListQueues' {..} =
    Core.mconcat
      ["/queues-summary/", Core.toBS instanceId]

instance Core.ToQuery ListQueues where
  toQuery ListQueues' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "queueTypes"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> queueTypes)
      ]

-- | /See:/ 'newListQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the queues.
    queueSummaryList :: Core.Maybe [QueueSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListQueuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQueuesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'queueSummaryList', 'listQueuesResponse_queueSummaryList' - Information about the queues.
--
-- 'httpStatus', 'listQueuesResponse_httpStatus' - The response's http status code.
newListQueuesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListQueuesResponse
newListQueuesResponse pHttpStatus_ =
  ListQueuesResponse'
    { nextToken = Core.Nothing,
      queueSummaryList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listQueuesResponse_nextToken :: Lens.Lens' ListQueuesResponse (Core.Maybe Core.Text)
listQueuesResponse_nextToken = Lens.lens (\ListQueuesResponse' {nextToken} -> nextToken) (\s@ListQueuesResponse' {} a -> s {nextToken = a} :: ListQueuesResponse)

-- | Information about the queues.
listQueuesResponse_queueSummaryList :: Lens.Lens' ListQueuesResponse (Core.Maybe [QueueSummary])
listQueuesResponse_queueSummaryList = Lens.lens (\ListQueuesResponse' {queueSummaryList} -> queueSummaryList) (\s@ListQueuesResponse' {} a -> s {queueSummaryList = a} :: ListQueuesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listQueuesResponse_httpStatus :: Lens.Lens' ListQueuesResponse Core.Int
listQueuesResponse_httpStatus = Lens.lens (\ListQueuesResponse' {httpStatus} -> httpStatus) (\s@ListQueuesResponse' {} a -> s {httpStatus = a} :: ListQueuesResponse)

instance Core.NFData ListQueuesResponse
