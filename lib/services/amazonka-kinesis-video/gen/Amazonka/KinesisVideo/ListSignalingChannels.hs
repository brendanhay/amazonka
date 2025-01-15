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
-- Module      : Amazonka.KinesisVideo.ListSignalingChannels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ChannelInfo@ objects. Each object describes a
-- signaling channel. To retrieve only those channels that satisfy a
-- specific condition, you can specify a @ChannelNameCondition@.
--
-- This operation returns paginated results.
module Amazonka.KinesisVideo.ListSignalingChannels
  ( -- * Creating a Request
    ListSignalingChannels (..),
    newListSignalingChannels,

    -- * Request Lenses
    listSignalingChannels_channelNameCondition,
    listSignalingChannels_maxResults,
    listSignalingChannels_nextToken,

    -- * Destructuring the Response
    ListSignalingChannelsResponse (..),
    newListSignalingChannelsResponse,

    -- * Response Lenses
    listSignalingChannelsResponse_channelInfoList,
    listSignalingChannelsResponse_nextToken,
    listSignalingChannelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSignalingChannels' smart constructor.
data ListSignalingChannels = ListSignalingChannels'
  { -- | Optional: Returns only the channels that satisfy a specific condition.
    channelNameCondition :: Prelude.Maybe ChannelNameCondition,
    -- | The maximum number of channels to return in the response. The default is
    -- 500.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If you specify this parameter, when the result of a
    -- @ListSignalingChannels@ operation is truncated, the call returns the
    -- @NextToken@ in the response. To get another batch of channels, provide
    -- this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'maxResults', 'listSignalingChannels_maxResults' - The maximum number of channels to return in the response. The default is
-- 500.
--
-- 'nextToken', 'listSignalingChannels_nextToken' - If you specify this parameter, when the result of a
-- @ListSignalingChannels@ operation is truncated, the call returns the
-- @NextToken@ in the response. To get another batch of channels, provide
-- this token in your next request.
newListSignalingChannels ::
  ListSignalingChannels
newListSignalingChannels =
  ListSignalingChannels'
    { channelNameCondition =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Optional: Returns only the channels that satisfy a specific condition.
listSignalingChannels_channelNameCondition :: Lens.Lens' ListSignalingChannels (Prelude.Maybe ChannelNameCondition)
listSignalingChannels_channelNameCondition = Lens.lens (\ListSignalingChannels' {channelNameCondition} -> channelNameCondition) (\s@ListSignalingChannels' {} a -> s {channelNameCondition = a} :: ListSignalingChannels)

-- | The maximum number of channels to return in the response. The default is
-- 500.
listSignalingChannels_maxResults :: Lens.Lens' ListSignalingChannels (Prelude.Maybe Prelude.Natural)
listSignalingChannels_maxResults = Lens.lens (\ListSignalingChannels' {maxResults} -> maxResults) (\s@ListSignalingChannels' {} a -> s {maxResults = a} :: ListSignalingChannels)

-- | If you specify this parameter, when the result of a
-- @ListSignalingChannels@ operation is truncated, the call returns the
-- @NextToken@ in the response. To get another batch of channels, provide
-- this token in your next request.
listSignalingChannels_nextToken :: Lens.Lens' ListSignalingChannels (Prelude.Maybe Prelude.Text)
listSignalingChannels_nextToken = Lens.lens (\ListSignalingChannels' {nextToken} -> nextToken) (\s@ListSignalingChannels' {} a -> s {nextToken = a} :: ListSignalingChannels)

instance Core.AWSPager ListSignalingChannels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSignalingChannelsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSignalingChannelsResponse_channelInfoList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listSignalingChannels_nextToken
              Lens..~ rs
              Lens.^? listSignalingChannelsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListSignalingChannels where
  type
    AWSResponse ListSignalingChannels =
      ListSignalingChannelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSignalingChannelsResponse'
            Prelude.<$> ( x
                            Data..?> "ChannelInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSignalingChannels where
  hashWithSalt _salt ListSignalingChannels' {..} =
    _salt
      `Prelude.hashWithSalt` channelNameCondition
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSignalingChannels where
  rnf ListSignalingChannels' {..} =
    Prelude.rnf channelNameCondition `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListSignalingChannels where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListSignalingChannels where
  toJSON ListSignalingChannels' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChannelNameCondition" Data..=)
              Prelude.<$> channelNameCondition,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListSignalingChannels where
  toPath = Prelude.const "/listSignalingChannels"

instance Data.ToQuery ListSignalingChannels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSignalingChannelsResponse' smart constructor.
data ListSignalingChannelsResponse = ListSignalingChannelsResponse'
  { -- | An array of @ChannelInfo@ objects.
    channelInfoList :: Prelude.Maybe [ChannelInfo],
    -- | If the response is truncated, the call returns this element with a
    -- token. To get the next batch of streams, use this token in your next
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSignalingChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelInfoList', 'listSignalingChannelsResponse_channelInfoList' - An array of @ChannelInfo@ objects.
--
-- 'nextToken', 'listSignalingChannelsResponse_nextToken' - If the response is truncated, the call returns this element with a
-- token. To get the next batch of streams, use this token in your next
-- request.
--
-- 'httpStatus', 'listSignalingChannelsResponse_httpStatus' - The response's http status code.
newListSignalingChannelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSignalingChannelsResponse
newListSignalingChannelsResponse pHttpStatus_ =
  ListSignalingChannelsResponse'
    { channelInfoList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @ChannelInfo@ objects.
listSignalingChannelsResponse_channelInfoList :: Lens.Lens' ListSignalingChannelsResponse (Prelude.Maybe [ChannelInfo])
listSignalingChannelsResponse_channelInfoList = Lens.lens (\ListSignalingChannelsResponse' {channelInfoList} -> channelInfoList) (\s@ListSignalingChannelsResponse' {} a -> s {channelInfoList = a} :: ListSignalingChannelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, the call returns this element with a
-- token. To get the next batch of streams, use this token in your next
-- request.
listSignalingChannelsResponse_nextToken :: Lens.Lens' ListSignalingChannelsResponse (Prelude.Maybe Prelude.Text)
listSignalingChannelsResponse_nextToken = Lens.lens (\ListSignalingChannelsResponse' {nextToken} -> nextToken) (\s@ListSignalingChannelsResponse' {} a -> s {nextToken = a} :: ListSignalingChannelsResponse)

-- | The response's http status code.
listSignalingChannelsResponse_httpStatus :: Lens.Lens' ListSignalingChannelsResponse Prelude.Int
listSignalingChannelsResponse_httpStatus = Lens.lens (\ListSignalingChannelsResponse' {httpStatus} -> httpStatus) (\s@ListSignalingChannelsResponse' {} a -> s {httpStatus = a} :: ListSignalingChannelsResponse)

instance Prelude.NFData ListSignalingChannelsResponse where
  rnf ListSignalingChannelsResponse' {..} =
    Prelude.rnf channelInfoList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
