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

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSignalingChannels' smart constructor.
data ListSignalingChannels = ListSignalingChannels'
  { -- | Optional: Returns only the channels that satisfy a specific condition.
    channelNameCondition :: Prelude.Maybe ChannelNameCondition,
    -- | If you specify this parameter, when the result of a
    -- @ListSignalingChannels@ operation is truncated, the call returns the
    -- @NextToken@ in the response. To get another batch of channels, provide
    -- this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of channels to return in the response. The default is
    -- 500.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Optional: Returns only the channels that satisfy a specific condition.
listSignalingChannels_channelNameCondition :: Lens.Lens' ListSignalingChannels (Prelude.Maybe ChannelNameCondition)
listSignalingChannels_channelNameCondition = Lens.lens (\ListSignalingChannels' {channelNameCondition} -> channelNameCondition) (\s@ListSignalingChannels' {} a -> s {channelNameCondition = a} :: ListSignalingChannels)

-- | If you specify this parameter, when the result of a
-- @ListSignalingChannels@ operation is truncated, the call returns the
-- @NextToken@ in the response. To get another batch of channels, provide
-- this token in your next request.
listSignalingChannels_nextToken :: Lens.Lens' ListSignalingChannels (Prelude.Maybe Prelude.Text)
listSignalingChannels_nextToken = Lens.lens (\ListSignalingChannels' {nextToken} -> nextToken) (\s@ListSignalingChannels' {} a -> s {nextToken = a} :: ListSignalingChannels)

-- | The maximum number of channels to return in the response. The default is
-- 500.
listSignalingChannels_maxResults :: Lens.Lens' ListSignalingChannels (Prelude.Maybe Prelude.Natural)
listSignalingChannels_maxResults = Lens.lens (\ListSignalingChannels' {maxResults} -> maxResults) (\s@ListSignalingChannels' {} a -> s {maxResults = a} :: ListSignalingChannels)

instance Pager.AWSPager ListSignalingChannels where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listSignalingChannelsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listSignalingChannelsResponse_channelInfoList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listSignalingChannels_nextToken
          Lens..~ rs
          Lens.^? listSignalingChannelsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListSignalingChannels where
  type
    Rs ListSignalingChannels =
      ListSignalingChannelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSignalingChannelsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ChannelInfoList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSignalingChannels

instance Prelude.NFData ListSignalingChannels

instance Prelude.ToHeaders ListSignalingChannels where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON ListSignalingChannels where
  toJSON ListSignalingChannels' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ChannelNameCondition" Prelude..=)
              Prelude.<$> channelNameCondition,
            ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListSignalingChannels where
  toPath = Prelude.const "/listSignalingChannels"

instance Prelude.ToQuery ListSignalingChannels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSignalingChannelsResponse' smart constructor.
data ListSignalingChannelsResponse = ListSignalingChannelsResponse'
  { -- | If the response is truncated, the call returns this element with a
    -- token. To get the next batch of streams, use this token in your next
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @ChannelInfo@ objects.
    channelInfoList :: Prelude.Maybe [ChannelInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListSignalingChannelsResponse
newListSignalingChannelsResponse pHttpStatus_ =
  ListSignalingChannelsResponse'
    { nextToken =
        Prelude.Nothing,
      channelInfoList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, the call returns this element with a
-- token. To get the next batch of streams, use this token in your next
-- request.
listSignalingChannelsResponse_nextToken :: Lens.Lens' ListSignalingChannelsResponse (Prelude.Maybe Prelude.Text)
listSignalingChannelsResponse_nextToken = Lens.lens (\ListSignalingChannelsResponse' {nextToken} -> nextToken) (\s@ListSignalingChannelsResponse' {} a -> s {nextToken = a} :: ListSignalingChannelsResponse)

-- | An array of @ChannelInfo@ objects.
listSignalingChannelsResponse_channelInfoList :: Lens.Lens' ListSignalingChannelsResponse (Prelude.Maybe [ChannelInfo])
listSignalingChannelsResponse_channelInfoList = Lens.lens (\ListSignalingChannelsResponse' {channelInfoList} -> channelInfoList) (\s@ListSignalingChannelsResponse' {} a -> s {channelInfoList = a} :: ListSignalingChannelsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listSignalingChannelsResponse_httpStatus :: Lens.Lens' ListSignalingChannelsResponse Prelude.Int
listSignalingChannelsResponse_httpStatus = Lens.lens (\ListSignalingChannelsResponse' {httpStatus} -> httpStatus) (\s@ListSignalingChannelsResponse' {} a -> s {httpStatus = a} :: ListSignalingChannelsResponse)

instance Prelude.NFData ListSignalingChannelsResponse
