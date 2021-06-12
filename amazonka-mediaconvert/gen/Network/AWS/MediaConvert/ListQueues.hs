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
-- Module      : Network.AWS.MediaConvert.ListQueues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your queues. This will return
-- the queues themselves, not just a list of them. To retrieve the next
-- twenty queues, use the nextToken string returned with the array.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListQueues
  ( -- * Creating a Request
    ListQueues (..),
    newListQueues,

    -- * Request Lenses
    listQueues_nextToken,
    listQueues_listBy,
    listQueues_maxResults,
    listQueues_order,

    -- * Destructuring the Response
    ListQueuesResponse (..),
    newListQueuesResponse,

    -- * Response Lenses
    listQueuesResponse_nextToken,
    listQueuesResponse_queues,
    listQueuesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListQueues' smart constructor.
data ListQueues = ListQueues'
  { -- | Use this string, provided with the response to a previous request, to
    -- request the next batch of queues.
    nextToken :: Core.Maybe Core.Text,
    -- | Optional. When you request a list of queues, you can choose to list them
    -- alphabetically by NAME or chronologically by CREATION_DATE. If you
    -- don\'t specify, the service will list them by creation date.
    listBy :: Core.Maybe QueueListBy,
    -- | Optional. Number of queues, up to twenty, that will be returned at one
    -- time.
    maxResults :: Core.Maybe Core.Natural,
    -- | Optional. When you request lists of resources, you can specify whether
    -- they are sorted in ASCENDING or DESCENDING order. Default varies by
    -- resource.
    order :: Core.Maybe Order
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
-- 'nextToken', 'listQueues_nextToken' - Use this string, provided with the response to a previous request, to
-- request the next batch of queues.
--
-- 'listBy', 'listQueues_listBy' - Optional. When you request a list of queues, you can choose to list them
-- alphabetically by NAME or chronologically by CREATION_DATE. If you
-- don\'t specify, the service will list them by creation date.
--
-- 'maxResults', 'listQueues_maxResults' - Optional. Number of queues, up to twenty, that will be returned at one
-- time.
--
-- 'order', 'listQueues_order' - Optional. When you request lists of resources, you can specify whether
-- they are sorted in ASCENDING or DESCENDING order. Default varies by
-- resource.
newListQueues ::
  ListQueues
newListQueues =
  ListQueues'
    { nextToken = Core.Nothing,
      listBy = Core.Nothing,
      maxResults = Core.Nothing,
      order = Core.Nothing
    }

-- | Use this string, provided with the response to a previous request, to
-- request the next batch of queues.
listQueues_nextToken :: Lens.Lens' ListQueues (Core.Maybe Core.Text)
listQueues_nextToken = Lens.lens (\ListQueues' {nextToken} -> nextToken) (\s@ListQueues' {} a -> s {nextToken = a} :: ListQueues)

-- | Optional. When you request a list of queues, you can choose to list them
-- alphabetically by NAME or chronologically by CREATION_DATE. If you
-- don\'t specify, the service will list them by creation date.
listQueues_listBy :: Lens.Lens' ListQueues (Core.Maybe QueueListBy)
listQueues_listBy = Lens.lens (\ListQueues' {listBy} -> listBy) (\s@ListQueues' {} a -> s {listBy = a} :: ListQueues)

-- | Optional. Number of queues, up to twenty, that will be returned at one
-- time.
listQueues_maxResults :: Lens.Lens' ListQueues (Core.Maybe Core.Natural)
listQueues_maxResults = Lens.lens (\ListQueues' {maxResults} -> maxResults) (\s@ListQueues' {} a -> s {maxResults = a} :: ListQueues)

-- | Optional. When you request lists of resources, you can specify whether
-- they are sorted in ASCENDING or DESCENDING order. Default varies by
-- resource.
listQueues_order :: Lens.Lens' ListQueues (Core.Maybe Order)
listQueues_order = Lens.lens (\ListQueues' {order} -> order) (\s@ListQueues' {} a -> s {order = a} :: ListQueues)

instance Core.AWSPager ListQueues where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listQueuesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listQueuesResponse_queues Core.. Lens._Just
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
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "queues" Core..!@ Core.mempty)
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
  toPath = Core.const "/2017-08-29/queues"

instance Core.ToQuery ListQueues where
  toQuery ListQueues' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "listBy" Core.=: listBy,
        "maxResults" Core.=: maxResults,
        "order" Core.=: order
      ]

-- | /See:/ 'newListQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { -- | Use this string to request the next batch of queues.
    nextToken :: Core.Maybe Core.Text,
    -- | List of queues.
    queues :: Core.Maybe [Queue],
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
-- 'nextToken', 'listQueuesResponse_nextToken' - Use this string to request the next batch of queues.
--
-- 'queues', 'listQueuesResponse_queues' - List of queues.
--
-- 'httpStatus', 'listQueuesResponse_httpStatus' - The response's http status code.
newListQueuesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListQueuesResponse
newListQueuesResponse pHttpStatus_ =
  ListQueuesResponse'
    { nextToken = Core.Nothing,
      queues = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Use this string to request the next batch of queues.
listQueuesResponse_nextToken :: Lens.Lens' ListQueuesResponse (Core.Maybe Core.Text)
listQueuesResponse_nextToken = Lens.lens (\ListQueuesResponse' {nextToken} -> nextToken) (\s@ListQueuesResponse' {} a -> s {nextToken = a} :: ListQueuesResponse)

-- | List of queues.
listQueuesResponse_queues :: Lens.Lens' ListQueuesResponse (Core.Maybe [Queue])
listQueuesResponse_queues = Lens.lens (\ListQueuesResponse' {queues} -> queues) (\s@ListQueuesResponse' {} a -> s {queues = a} :: ListQueuesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listQueuesResponse_httpStatus :: Lens.Lens' ListQueuesResponse Core.Int
listQueuesResponse_httpStatus = Lens.lens (\ListQueuesResponse' {httpStatus} -> httpStatus) (\s@ListQueuesResponse' {} a -> s {httpStatus = a} :: ListQueuesResponse)

instance Core.NFData ListQueuesResponse
