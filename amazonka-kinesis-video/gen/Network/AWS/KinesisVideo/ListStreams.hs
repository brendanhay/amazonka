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
-- Module      : Network.AWS.KinesisVideo.ListStreams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @StreamInfo@ objects. Each object describes a
-- stream. To retrieve only streams that satisfy a specific condition, you
-- can specify a @StreamNameCondition@.
--
-- This operation returns paginated results.
module Network.AWS.KinesisVideo.ListStreams
  ( -- * Creating a Request
    ListStreams (..),
    newListStreams,

    -- * Request Lenses
    listStreams_nextToken,
    listStreams_maxResults,
    listStreams_streamNameCondition,

    -- * Destructuring the Response
    ListStreamsResponse (..),
    newListStreamsResponse,

    -- * Response Lenses
    listStreamsResponse_nextToken,
    listStreamsResponse_streamInfoList,
    listStreamsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListStreams' smart constructor.
data ListStreams = ListStreams'
  { -- | If you specify this parameter, when the result of a @ListStreams@
    -- operation is truncated, the call returns the @NextToken@ in the
    -- response. To get another batch of streams, provide this token in your
    -- next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of streams to return in the response. The default is
    -- 10,000.
    maxResults :: Core.Maybe Core.Natural,
    -- | Optional: Returns only streams that satisfy a specific condition.
    -- Currently, you can specify only the prefix of a stream name as a
    -- condition.
    streamNameCondition :: Core.Maybe StreamNameCondition
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
-- 'nextToken', 'listStreams_nextToken' - If you specify this parameter, when the result of a @ListStreams@
-- operation is truncated, the call returns the @NextToken@ in the
-- response. To get another batch of streams, provide this token in your
-- next request.
--
-- 'maxResults', 'listStreams_maxResults' - The maximum number of streams to return in the response. The default is
-- 10,000.
--
-- 'streamNameCondition', 'listStreams_streamNameCondition' - Optional: Returns only streams that satisfy a specific condition.
-- Currently, you can specify only the prefix of a stream name as a
-- condition.
newListStreams ::
  ListStreams
newListStreams =
  ListStreams'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      streamNameCondition = Core.Nothing
    }

-- | If you specify this parameter, when the result of a @ListStreams@
-- operation is truncated, the call returns the @NextToken@ in the
-- response. To get another batch of streams, provide this token in your
-- next request.
listStreams_nextToken :: Lens.Lens' ListStreams (Core.Maybe Core.Text)
listStreams_nextToken = Lens.lens (\ListStreams' {nextToken} -> nextToken) (\s@ListStreams' {} a -> s {nextToken = a} :: ListStreams)

-- | The maximum number of streams to return in the response. The default is
-- 10,000.
listStreams_maxResults :: Lens.Lens' ListStreams (Core.Maybe Core.Natural)
listStreams_maxResults = Lens.lens (\ListStreams' {maxResults} -> maxResults) (\s@ListStreams' {} a -> s {maxResults = a} :: ListStreams)

-- | Optional: Returns only streams that satisfy a specific condition.
-- Currently, you can specify only the prefix of a stream name as a
-- condition.
listStreams_streamNameCondition :: Lens.Lens' ListStreams (Core.Maybe StreamNameCondition)
listStreams_streamNameCondition = Lens.lens (\ListStreams' {streamNameCondition} -> streamNameCondition) (\s@ListStreams' {} a -> s {streamNameCondition = a} :: ListStreams)

instance Core.AWSPager ListStreams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStreamsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listStreamsResponse_streamInfoList Core.. Lens._Just
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "StreamInfoList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListStreams

instance Core.NFData ListStreams

instance Core.ToHeaders ListStreams where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON ListStreams where
  toJSON ListStreams' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("StreamNameCondition" Core..=)
              Core.<$> streamNameCondition
          ]
      )

instance Core.ToPath ListStreams where
  toPath = Core.const "/listStreams"

instance Core.ToQuery ListStreams where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { -- | If the response is truncated, the call returns this element with a
    -- token. To get the next batch of streams, use this token in your next
    -- request.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @StreamInfo@ objects.
    streamInfoList :: Core.Maybe [StreamInfo],
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
-- 'nextToken', 'listStreamsResponse_nextToken' - If the response is truncated, the call returns this element with a
-- token. To get the next batch of streams, use this token in your next
-- request.
--
-- 'streamInfoList', 'listStreamsResponse_streamInfoList' - An array of @StreamInfo@ objects.
--
-- 'httpStatus', 'listStreamsResponse_httpStatus' - The response's http status code.
newListStreamsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListStreamsResponse
newListStreamsResponse pHttpStatus_ =
  ListStreamsResponse'
    { nextToken = Core.Nothing,
      streamInfoList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, the call returns this element with a
-- token. To get the next batch of streams, use this token in your next
-- request.
listStreamsResponse_nextToken :: Lens.Lens' ListStreamsResponse (Core.Maybe Core.Text)
listStreamsResponse_nextToken = Lens.lens (\ListStreamsResponse' {nextToken} -> nextToken) (\s@ListStreamsResponse' {} a -> s {nextToken = a} :: ListStreamsResponse)

-- | An array of @StreamInfo@ objects.
listStreamsResponse_streamInfoList :: Lens.Lens' ListStreamsResponse (Core.Maybe [StreamInfo])
listStreamsResponse_streamInfoList = Lens.lens (\ListStreamsResponse' {streamInfoList} -> streamInfoList) (\s@ListStreamsResponse' {} a -> s {streamInfoList = a} :: ListStreamsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listStreamsResponse_httpStatus :: Lens.Lens' ListStreamsResponse Core.Int
listStreamsResponse_httpStatus = Lens.lens (\ListStreamsResponse' {httpStatus} -> httpStatus) (\s@ListStreamsResponse' {} a -> s {httpStatus = a} :: ListStreamsResponse)

instance Core.NFData ListStreamsResponse
