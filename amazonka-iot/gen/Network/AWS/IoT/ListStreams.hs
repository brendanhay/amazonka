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
-- Module      : Network.AWS.IoT.ListStreams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the streams in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListStreams
  ( -- * Creating a Request
    ListStreams (..),
    newListStreams,

    -- * Request Lenses
    listStreams_nextToken,
    listStreams_maxResults,
    listStreams_ascendingOrder,

    -- * Destructuring the Response
    ListStreamsResponse (..),
    newListStreamsResponse,

    -- * Response Lenses
    listStreamsResponse_streams,
    listStreamsResponse_nextToken,
    listStreamsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListStreams' smart constructor.
data ListStreams = ListStreams'
  { -- | A token used to get the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at a time.
    maxResults :: Core.Maybe Core.Natural,
    -- | Set to true to return the list of streams in ascending order.
    ascendingOrder :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStreams_nextToken' - A token used to get the next set of results.
--
-- 'maxResults', 'listStreams_maxResults' - The maximum number of results to return at a time.
--
-- 'ascendingOrder', 'listStreams_ascendingOrder' - Set to true to return the list of streams in ascending order.
newListStreams ::
  ListStreams
newListStreams =
  ListStreams'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      ascendingOrder = Core.Nothing
    }

-- | A token used to get the next set of results.
listStreams_nextToken :: Lens.Lens' ListStreams (Core.Maybe Core.Text)
listStreams_nextToken = Lens.lens (\ListStreams' {nextToken} -> nextToken) (\s@ListStreams' {} a -> s {nextToken = a} :: ListStreams)

-- | The maximum number of results to return at a time.
listStreams_maxResults :: Lens.Lens' ListStreams (Core.Maybe Core.Natural)
listStreams_maxResults = Lens.lens (\ListStreams' {maxResults} -> maxResults) (\s@ListStreams' {} a -> s {maxResults = a} :: ListStreams)

-- | Set to true to return the list of streams in ascending order.
listStreams_ascendingOrder :: Lens.Lens' ListStreams (Core.Maybe Core.Bool)
listStreams_ascendingOrder = Lens.lens (\ListStreams' {ascendingOrder} -> ascendingOrder) (\s@ListStreams' {} a -> s {ascendingOrder = a} :: ListStreams)

instance Core.AWSPager ListStreams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStreamsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listStreamsResponse_streams Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listStreams_nextToken
          Lens..~ rs
          Lens.^? listStreamsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListStreams where
  type AWSResponse ListStreams = ListStreamsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Core.<$> (x Core..?> "streams" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListStreams

instance Core.NFData ListStreams

instance Core.ToHeaders ListStreams where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListStreams where
  toPath = Core.const "/streams"

instance Core.ToQuery ListStreams where
  toQuery ListStreams' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "isAscendingOrder" Core.=: ascendingOrder
      ]

-- | /See:/ 'newListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { -- | A list of streams.
    streams :: Core.Maybe [StreamSummary],
    -- | A token used to get the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streams', 'listStreamsResponse_streams' - A list of streams.
--
-- 'nextToken', 'listStreamsResponse_nextToken' - A token used to get the next set of results.
--
-- 'httpStatus', 'listStreamsResponse_httpStatus' - The response's http status code.
newListStreamsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListStreamsResponse
newListStreamsResponse pHttpStatus_ =
  ListStreamsResponse'
    { streams = Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of streams.
listStreamsResponse_streams :: Lens.Lens' ListStreamsResponse (Core.Maybe [StreamSummary])
listStreamsResponse_streams = Lens.lens (\ListStreamsResponse' {streams} -> streams) (\s@ListStreamsResponse' {} a -> s {streams = a} :: ListStreamsResponse) Core.. Lens.mapping Lens._Coerce

-- | A token used to get the next set of results.
listStreamsResponse_nextToken :: Lens.Lens' ListStreamsResponse (Core.Maybe Core.Text)
listStreamsResponse_nextToken = Lens.lens (\ListStreamsResponse' {nextToken} -> nextToken) (\s@ListStreamsResponse' {} a -> s {nextToken = a} :: ListStreamsResponse)

-- | The response's http status code.
listStreamsResponse_httpStatus :: Lens.Lens' ListStreamsResponse Core.Int
listStreamsResponse_httpStatus = Lens.lens (\ListStreamsResponse' {httpStatus} -> httpStatus) (\s@ListStreamsResponse' {} a -> s {httpStatus = a} :: ListStreamsResponse)

instance Core.NFData ListStreamsResponse
