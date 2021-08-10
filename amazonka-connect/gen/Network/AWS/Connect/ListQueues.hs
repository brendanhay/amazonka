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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListQueues' smart constructor.
data ListQueues = ListQueues'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The type of queue.
    queueTypes :: Prelude.Maybe [QueueType],
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListQueues
newListQueues pInstanceId_ =
  ListQueues'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      queueTypes = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listQueues_nextToken :: Lens.Lens' ListQueues (Prelude.Maybe Prelude.Text)
listQueues_nextToken = Lens.lens (\ListQueues' {nextToken} -> nextToken) (\s@ListQueues' {} a -> s {nextToken = a} :: ListQueues)

-- | The maximum number of results to return per page.
listQueues_maxResults :: Lens.Lens' ListQueues (Prelude.Maybe Prelude.Natural)
listQueues_maxResults = Lens.lens (\ListQueues' {maxResults} -> maxResults) (\s@ListQueues' {} a -> s {maxResults = a} :: ListQueues)

-- | The type of queue.
listQueues_queueTypes :: Lens.Lens' ListQueues (Prelude.Maybe [QueueType])
listQueues_queueTypes = Lens.lens (\ListQueues' {queueTypes} -> queueTypes) (\s@ListQueues' {} a -> s {queueTypes = a} :: ListQueues) Prelude.. Lens.mapping Lens._Coerce

-- | The identifier of the Amazon Connect instance.
listQueues_instanceId :: Lens.Lens' ListQueues Prelude.Text
listQueues_instanceId = Lens.lens (\ListQueues' {instanceId} -> instanceId) (\s@ListQueues' {} a -> s {instanceId = a} :: ListQueues)

instance Core.AWSPager ListQueues where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listQueuesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listQueuesResponse_queueSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listQueues_nextToken
          Lens..~ rs
          Lens.^? listQueuesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListQueues where
  type AWSResponse ListQueues = ListQueuesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQueuesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "QueueSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListQueues

instance Prelude.NFData ListQueues

instance Core.ToHeaders ListQueues where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListQueues where
  toPath ListQueues' {..} =
    Prelude.mconcat
      ["/queues-summary/", Core.toBS instanceId]

instance Core.ToQuery ListQueues where
  toQuery ListQueues' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "queueTypes"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> queueTypes)
      ]

-- | /See:/ 'newListQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the queues.
    queueSummaryList :: Prelude.Maybe [QueueSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListQueuesResponse
newListQueuesResponse pHttpStatus_ =
  ListQueuesResponse'
    { nextToken = Prelude.Nothing,
      queueSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listQueuesResponse_nextToken :: Lens.Lens' ListQueuesResponse (Prelude.Maybe Prelude.Text)
listQueuesResponse_nextToken = Lens.lens (\ListQueuesResponse' {nextToken} -> nextToken) (\s@ListQueuesResponse' {} a -> s {nextToken = a} :: ListQueuesResponse)

-- | Information about the queues.
listQueuesResponse_queueSummaryList :: Lens.Lens' ListQueuesResponse (Prelude.Maybe [QueueSummary])
listQueuesResponse_queueSummaryList = Lens.lens (\ListQueuesResponse' {queueSummaryList} -> queueSummaryList) (\s@ListQueuesResponse' {} a -> s {queueSummaryList = a} :: ListQueuesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listQueuesResponse_httpStatus :: Lens.Lens' ListQueuesResponse Prelude.Int
listQueuesResponse_httpStatus = Lens.lens (\ListQueuesResponse' {httpStatus} -> httpStatus) (\s@ListQueuesResponse' {} a -> s {httpStatus = a} :: ListQueuesResponse)

instance Prelude.NFData ListQueuesResponse
