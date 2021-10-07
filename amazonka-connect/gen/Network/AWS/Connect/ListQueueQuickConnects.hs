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
-- Module      : Network.AWS.Connect.ListQueueQuickConnects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Lists the quick connects associated with a queue.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListQueueQuickConnects
  ( -- * Creating a Request
    ListQueueQuickConnects (..),
    newListQueueQuickConnects,

    -- * Request Lenses
    listQueueQuickConnects_nextToken,
    listQueueQuickConnects_maxResults,
    listQueueQuickConnects_instanceId,
    listQueueQuickConnects_queueId,

    -- * Destructuring the Response
    ListQueueQuickConnectsResponse (..),
    newListQueueQuickConnectsResponse,

    -- * Response Lenses
    listQueueQuickConnectsResponse_nextToken,
    listQueueQuickConnectsResponse_quickConnectSummaryList,
    listQueueQuickConnectsResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListQueueQuickConnects' smart constructor.
data ListQueueQuickConnects = ListQueueQuickConnects'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueueQuickConnects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQueueQuickConnects_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listQueueQuickConnects_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listQueueQuickConnects_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'queueId', 'listQueueQuickConnects_queueId' - The identifier for the queue.
newListQueueQuickConnects ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'queueId'
  Prelude.Text ->
  ListQueueQuickConnects
newListQueueQuickConnects pInstanceId_ pQueueId_ =
  ListQueueQuickConnects'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_,
      queueId = pQueueId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listQueueQuickConnects_nextToken :: Lens.Lens' ListQueueQuickConnects (Prelude.Maybe Prelude.Text)
listQueueQuickConnects_nextToken = Lens.lens (\ListQueueQuickConnects' {nextToken} -> nextToken) (\s@ListQueueQuickConnects' {} a -> s {nextToken = a} :: ListQueueQuickConnects)

-- | The maximum number of results to return per page.
listQueueQuickConnects_maxResults :: Lens.Lens' ListQueueQuickConnects (Prelude.Maybe Prelude.Natural)
listQueueQuickConnects_maxResults = Lens.lens (\ListQueueQuickConnects' {maxResults} -> maxResults) (\s@ListQueueQuickConnects' {} a -> s {maxResults = a} :: ListQueueQuickConnects)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listQueueQuickConnects_instanceId :: Lens.Lens' ListQueueQuickConnects Prelude.Text
listQueueQuickConnects_instanceId = Lens.lens (\ListQueueQuickConnects' {instanceId} -> instanceId) (\s@ListQueueQuickConnects' {} a -> s {instanceId = a} :: ListQueueQuickConnects)

-- | The identifier for the queue.
listQueueQuickConnects_queueId :: Lens.Lens' ListQueueQuickConnects Prelude.Text
listQueueQuickConnects_queueId = Lens.lens (\ListQueueQuickConnects' {queueId} -> queueId) (\s@ListQueueQuickConnects' {} a -> s {queueId = a} :: ListQueueQuickConnects)

instance Core.AWSPager ListQueueQuickConnects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listQueueQuickConnectsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listQueueQuickConnectsResponse_quickConnectSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listQueueQuickConnects_nextToken
          Lens..~ rs
          Lens.^? listQueueQuickConnectsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListQueueQuickConnects where
  type
    AWSResponse ListQueueQuickConnects =
      ListQueueQuickConnectsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQueueQuickConnectsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "QuickConnectSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListQueueQuickConnects

instance Prelude.NFData ListQueueQuickConnects

instance Core.ToHeaders ListQueueQuickConnects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListQueueQuickConnects where
  toPath ListQueueQuickConnects' {..} =
    Prelude.mconcat
      [ "/queues/",
        Core.toBS instanceId,
        "/",
        Core.toBS queueId,
        "/quick-connects"
      ]

instance Core.ToQuery ListQueueQuickConnects where
  toQuery ListQueueQuickConnects' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListQueueQuickConnectsResponse' smart constructor.
data ListQueueQuickConnectsResponse = ListQueueQuickConnectsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the quick connects.
    quickConnectSummaryList :: Prelude.Maybe [QuickConnectSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueueQuickConnectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQueueQuickConnectsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'quickConnectSummaryList', 'listQueueQuickConnectsResponse_quickConnectSummaryList' - Information about the quick connects.
--
-- 'httpStatus', 'listQueueQuickConnectsResponse_httpStatus' - The response's http status code.
newListQueueQuickConnectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListQueueQuickConnectsResponse
newListQueueQuickConnectsResponse pHttpStatus_ =
  ListQueueQuickConnectsResponse'
    { nextToken =
        Prelude.Nothing,
      quickConnectSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listQueueQuickConnectsResponse_nextToken :: Lens.Lens' ListQueueQuickConnectsResponse (Prelude.Maybe Prelude.Text)
listQueueQuickConnectsResponse_nextToken = Lens.lens (\ListQueueQuickConnectsResponse' {nextToken} -> nextToken) (\s@ListQueueQuickConnectsResponse' {} a -> s {nextToken = a} :: ListQueueQuickConnectsResponse)

-- | Information about the quick connects.
listQueueQuickConnectsResponse_quickConnectSummaryList :: Lens.Lens' ListQueueQuickConnectsResponse (Prelude.Maybe [QuickConnectSummary])
listQueueQuickConnectsResponse_quickConnectSummaryList = Lens.lens (\ListQueueQuickConnectsResponse' {quickConnectSummaryList} -> quickConnectSummaryList) (\s@ListQueueQuickConnectsResponse' {} a -> s {quickConnectSummaryList = a} :: ListQueueQuickConnectsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listQueueQuickConnectsResponse_httpStatus :: Lens.Lens' ListQueueQuickConnectsResponse Prelude.Int
listQueueQuickConnectsResponse_httpStatus = Lens.lens (\ListQueueQuickConnectsResponse' {httpStatus} -> httpStatus) (\s@ListQueueQuickConnectsResponse' {} a -> s {httpStatus = a} :: ListQueueQuickConnectsResponse)

instance
  Prelude.NFData
    ListQueueQuickConnectsResponse
