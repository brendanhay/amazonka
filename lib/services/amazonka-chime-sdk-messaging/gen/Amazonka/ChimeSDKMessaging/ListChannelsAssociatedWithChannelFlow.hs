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
-- Module      : Amazonka.ChimeSDKMessaging.ListChannelsAssociatedWithChannelFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all channels associated with a specified channel flow. You can
-- associate a channel flow with multiple channels, but you can only
-- associate a channel with one channel flow. This is a developer API.
module Amazonka.ChimeSDKMessaging.ListChannelsAssociatedWithChannelFlow
  ( -- * Creating a Request
    ListChannelsAssociatedWithChannelFlow (..),
    newListChannelsAssociatedWithChannelFlow,

    -- * Request Lenses
    listChannelsAssociatedWithChannelFlow_maxResults,
    listChannelsAssociatedWithChannelFlow_nextToken,
    listChannelsAssociatedWithChannelFlow_channelFlowArn,

    -- * Destructuring the Response
    ListChannelsAssociatedWithChannelFlowResponse (..),
    newListChannelsAssociatedWithChannelFlowResponse,

    -- * Response Lenses
    listChannelsAssociatedWithChannelFlowResponse_channels,
    listChannelsAssociatedWithChannelFlowResponse_nextToken,
    listChannelsAssociatedWithChannelFlowResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChannelsAssociatedWithChannelFlow' smart constructor.
data ListChannelsAssociatedWithChannelFlow = ListChannelsAssociatedWithChannelFlow'
  { -- | The maximum number of channels that you want to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token passed by previous API calls until all requested channels are
    -- returned.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the channel flow.
    channelFlowArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelsAssociatedWithChannelFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listChannelsAssociatedWithChannelFlow_maxResults' - The maximum number of channels that you want to return.
--
-- 'nextToken', 'listChannelsAssociatedWithChannelFlow_nextToken' - The token passed by previous API calls until all requested channels are
-- returned.
--
-- 'channelFlowArn', 'listChannelsAssociatedWithChannelFlow_channelFlowArn' - The ARN of the channel flow.
newListChannelsAssociatedWithChannelFlow ::
  -- | 'channelFlowArn'
  Prelude.Text ->
  ListChannelsAssociatedWithChannelFlow
newListChannelsAssociatedWithChannelFlow
  pChannelFlowArn_ =
    ListChannelsAssociatedWithChannelFlow'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        channelFlowArn = pChannelFlowArn_
      }

-- | The maximum number of channels that you want to return.
listChannelsAssociatedWithChannelFlow_maxResults :: Lens.Lens' ListChannelsAssociatedWithChannelFlow (Prelude.Maybe Prelude.Natural)
listChannelsAssociatedWithChannelFlow_maxResults = Lens.lens (\ListChannelsAssociatedWithChannelFlow' {maxResults} -> maxResults) (\s@ListChannelsAssociatedWithChannelFlow' {} a -> s {maxResults = a} :: ListChannelsAssociatedWithChannelFlow)

-- | The token passed by previous API calls until all requested channels are
-- returned.
listChannelsAssociatedWithChannelFlow_nextToken :: Lens.Lens' ListChannelsAssociatedWithChannelFlow (Prelude.Maybe Prelude.Text)
listChannelsAssociatedWithChannelFlow_nextToken = Lens.lens (\ListChannelsAssociatedWithChannelFlow' {nextToken} -> nextToken) (\s@ListChannelsAssociatedWithChannelFlow' {} a -> s {nextToken = a} :: ListChannelsAssociatedWithChannelFlow) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the channel flow.
listChannelsAssociatedWithChannelFlow_channelFlowArn :: Lens.Lens' ListChannelsAssociatedWithChannelFlow Prelude.Text
listChannelsAssociatedWithChannelFlow_channelFlowArn = Lens.lens (\ListChannelsAssociatedWithChannelFlow' {channelFlowArn} -> channelFlowArn) (\s@ListChannelsAssociatedWithChannelFlow' {} a -> s {channelFlowArn = a} :: ListChannelsAssociatedWithChannelFlow)

instance
  Core.AWSRequest
    ListChannelsAssociatedWithChannelFlow
  where
  type
    AWSResponse
      ListChannelsAssociatedWithChannelFlow =
      ListChannelsAssociatedWithChannelFlowResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelsAssociatedWithChannelFlowResponse'
            Prelude.<$> (x Data..?> "Channels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListChannelsAssociatedWithChannelFlow
  where
  hashWithSalt
    _salt
    ListChannelsAssociatedWithChannelFlow' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` channelFlowArn

instance
  Prelude.NFData
    ListChannelsAssociatedWithChannelFlow
  where
  rnf ListChannelsAssociatedWithChannelFlow' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf channelFlowArn

instance
  Data.ToHeaders
    ListChannelsAssociatedWithChannelFlow
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListChannelsAssociatedWithChannelFlow
  where
  toPath = Prelude.const "/channels"

instance
  Data.ToQuery
    ListChannelsAssociatedWithChannelFlow
  where
  toQuery ListChannelsAssociatedWithChannelFlow' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken,
        "channel-flow-arn" Data.=: channelFlowArn,
        "scope=channel-flow-associations"
      ]

-- | /See:/ 'newListChannelsAssociatedWithChannelFlowResponse' smart constructor.
data ListChannelsAssociatedWithChannelFlowResponse = ListChannelsAssociatedWithChannelFlowResponse'
  { -- | The information about each channel.
    channels :: Prelude.Maybe [ChannelAssociatedWithFlowSummary],
    -- | The token passed by previous API calls until all requested channels are
    -- returned.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelsAssociatedWithChannelFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channels', 'listChannelsAssociatedWithChannelFlowResponse_channels' - The information about each channel.
--
-- 'nextToken', 'listChannelsAssociatedWithChannelFlowResponse_nextToken' - The token passed by previous API calls until all requested channels are
-- returned.
--
-- 'httpStatus', 'listChannelsAssociatedWithChannelFlowResponse_httpStatus' - The response's http status code.
newListChannelsAssociatedWithChannelFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChannelsAssociatedWithChannelFlowResponse
newListChannelsAssociatedWithChannelFlowResponse
  pHttpStatus_ =
    ListChannelsAssociatedWithChannelFlowResponse'
      { channels =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The information about each channel.
listChannelsAssociatedWithChannelFlowResponse_channels :: Lens.Lens' ListChannelsAssociatedWithChannelFlowResponse (Prelude.Maybe [ChannelAssociatedWithFlowSummary])
listChannelsAssociatedWithChannelFlowResponse_channels = Lens.lens (\ListChannelsAssociatedWithChannelFlowResponse' {channels} -> channels) (\s@ListChannelsAssociatedWithChannelFlowResponse' {} a -> s {channels = a} :: ListChannelsAssociatedWithChannelFlowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token passed by previous API calls until all requested channels are
-- returned.
listChannelsAssociatedWithChannelFlowResponse_nextToken :: Lens.Lens' ListChannelsAssociatedWithChannelFlowResponse (Prelude.Maybe Prelude.Text)
listChannelsAssociatedWithChannelFlowResponse_nextToken = Lens.lens (\ListChannelsAssociatedWithChannelFlowResponse' {nextToken} -> nextToken) (\s@ListChannelsAssociatedWithChannelFlowResponse' {} a -> s {nextToken = a} :: ListChannelsAssociatedWithChannelFlowResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
listChannelsAssociatedWithChannelFlowResponse_httpStatus :: Lens.Lens' ListChannelsAssociatedWithChannelFlowResponse Prelude.Int
listChannelsAssociatedWithChannelFlowResponse_httpStatus = Lens.lens (\ListChannelsAssociatedWithChannelFlowResponse' {httpStatus} -> httpStatus) (\s@ListChannelsAssociatedWithChannelFlowResponse' {} a -> s {httpStatus = a} :: ListChannelsAssociatedWithChannelFlowResponse)

instance
  Prelude.NFData
    ListChannelsAssociatedWithChannelFlowResponse
  where
  rnf
    ListChannelsAssociatedWithChannelFlowResponse' {..} =
      Prelude.rnf channels
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
