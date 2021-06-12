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
-- Module      : Network.AWS.KinesisVideo.ListSignalingChannels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ChannelInfo@ objects. Each object describes a
-- signaling channel. To retrieve only those channels that satisfy a
-- specific condition, you can specify a @ChannelNameCondition@.
--
-- This operation returns paginated results.
module Network.AWS.KinesisVideo.ListSignalingChannels
  ( -- * Creating a Request
    ListSignalingChannels (..),
    newListSignalingChannels,

    -- * Request Lenses
    listSignalingChannels_channelNameCondition,
    listSignalingChannels_nextToken,
    listSignalingChannels_maxResults,

    -- * Destructuring the Response
    ListSignalingChannelsResponse (..),
    newListSignalingChannelsResponse,

    -- * Response Lenses
    listSignalingChannelsResponse_nextToken,
    listSignalingChannelsResponse_channelInfoList,
    listSignalingChannelsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSignalingChannels' smart constructor.
data ListSignalingChannels = ListSignalingChannels'
  { -- | Optional: Returns only the channels that satisfy a specific condition.
    channelNameCondition :: Core.Maybe ChannelNameCondition,
    -- | If you specify this parameter, when the result of a
    -- @ListSignalingChannels@ operation is truncated, the call returns the
    -- @NextToken@ in the response. To get another batch of channels, provide
    -- this token in your next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of channels to return in the response. The default is
    -- 500.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSignalingChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelNameCondition', 'listSignalingChannels_channelNameCondition' - Optional: Returns only the channels that satisfy a specific condition.
--
-- 'nextToken', 'listSignalingChannels_nextToken' - If you specify this parameter, when the result of a
-- @ListSignalingChannels@ operation is truncated, the call returns the
-- @NextToken@ in the response. To get another batch of channels, provide
-- this token in your next request.
--
-- 'maxResults', 'listSignalingChannels_maxResults' - The maximum number of channels to return in the response. The default is
-- 500.
newListSignalingChannels ::
  ListSignalingChannels
newListSignalingChannels =
  ListSignalingChannels'
    { channelNameCondition =
        Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Optional: Returns only the channels that satisfy a specific condition.
listSignalingChannels_channelNameCondition :: Lens.Lens' ListSignalingChannels (Core.Maybe ChannelNameCondition)
listSignalingChannels_channelNameCondition = Lens.lens (\ListSignalingChannels' {channelNameCondition} -> channelNameCondition) (\s@ListSignalingChannels' {} a -> s {channelNameCondition = a} :: ListSignalingChannels)

-- | If you specify this parameter, when the result of a
-- @ListSignalingChannels@ operation is truncated, the call returns the
-- @NextToken@ in the response. To get another batch of channels, provide
-- this token in your next request.
listSignalingChannels_nextToken :: Lens.Lens' ListSignalingChannels (Core.Maybe Core.Text)
listSignalingChannels_nextToken = Lens.lens (\ListSignalingChannels' {nextToken} -> nextToken) (\s@ListSignalingChannels' {} a -> s {nextToken = a} :: ListSignalingChannels)

-- | The maximum number of channels to return in the response. The default is
-- 500.
listSignalingChannels_maxResults :: Lens.Lens' ListSignalingChannels (Core.Maybe Core.Natural)
listSignalingChannels_maxResults = Lens.lens (\ListSignalingChannels' {maxResults} -> maxResults) (\s@ListSignalingChannels' {} a -> s {maxResults = a} :: ListSignalingChannels)

instance Core.AWSPager ListSignalingChannels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSignalingChannelsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSignalingChannelsResponse_channelInfoList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSignalingChannels_nextToken
          Lens..~ rs
          Lens.^? listSignalingChannelsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListSignalingChannels where
  type
    AWSResponse ListSignalingChannels =
      ListSignalingChannelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSignalingChannelsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ChannelInfoList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSignalingChannels

instance Core.NFData ListSignalingChannels

instance Core.ToHeaders ListSignalingChannels where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON ListSignalingChannels where
  toJSON ListSignalingChannels' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ChannelNameCondition" Core..=)
              Core.<$> channelNameCondition,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListSignalingChannels where
  toPath = Core.const "/listSignalingChannels"

instance Core.ToQuery ListSignalingChannels where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListSignalingChannelsResponse' smart constructor.
data ListSignalingChannelsResponse = ListSignalingChannelsResponse'
  { -- | If the response is truncated, the call returns this element with a
    -- token. To get the next batch of streams, use this token in your next
    -- request.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @ChannelInfo@ objects.
    channelInfoList :: Core.Maybe [ChannelInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSignalingChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSignalingChannelsResponse_nextToken' - If the response is truncated, the call returns this element with a
-- token. To get the next batch of streams, use this token in your next
-- request.
--
-- 'channelInfoList', 'listSignalingChannelsResponse_channelInfoList' - An array of @ChannelInfo@ objects.
--
-- 'httpStatus', 'listSignalingChannelsResponse_httpStatus' - The response's http status code.
newListSignalingChannelsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSignalingChannelsResponse
newListSignalingChannelsResponse pHttpStatus_ =
  ListSignalingChannelsResponse'
    { nextToken =
        Core.Nothing,
      channelInfoList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, the call returns this element with a
-- token. To get the next batch of streams, use this token in your next
-- request.
listSignalingChannelsResponse_nextToken :: Lens.Lens' ListSignalingChannelsResponse (Core.Maybe Core.Text)
listSignalingChannelsResponse_nextToken = Lens.lens (\ListSignalingChannelsResponse' {nextToken} -> nextToken) (\s@ListSignalingChannelsResponse' {} a -> s {nextToken = a} :: ListSignalingChannelsResponse)

-- | An array of @ChannelInfo@ objects.
listSignalingChannelsResponse_channelInfoList :: Lens.Lens' ListSignalingChannelsResponse (Core.Maybe [ChannelInfo])
listSignalingChannelsResponse_channelInfoList = Lens.lens (\ListSignalingChannelsResponse' {channelInfoList} -> channelInfoList) (\s@ListSignalingChannelsResponse' {} a -> s {channelInfoList = a} :: ListSignalingChannelsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSignalingChannelsResponse_httpStatus :: Lens.Lens' ListSignalingChannelsResponse Core.Int
listSignalingChannelsResponse_httpStatus = Lens.lens (\ListSignalingChannelsResponse' {httpStatus} -> httpStatus) (\s@ListSignalingChannelsResponse' {} a -> s {httpStatus = a} :: ListSignalingChannelsResponse)

instance Core.NFData ListSignalingChannelsResponse
