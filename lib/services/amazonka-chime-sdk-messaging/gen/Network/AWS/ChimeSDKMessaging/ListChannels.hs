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
-- Module      : Network.AWS.ChimeSDKMessaging.ListChannels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Channels created under a single Chime App as a paginated list.
-- You can specify filters to narrow results.
--
-- __Functionality & restrictions__
--
-- -   Use privacy = @PUBLIC@ to retrieve all public channels in the
--     account.
--
-- -   Only an @AppInstanceAdmin@ can set privacy = @PRIVATE@ to list the
--     private channels in an account.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Network.AWS.ChimeSDKMessaging.ListChannels
  ( -- * Creating a Request
    ListChannels (..),
    newListChannels,

    -- * Request Lenses
    listChannels_privacy,
    listChannels_nextToken,
    listChannels_maxResults,
    listChannels_appInstanceArn,
    listChannels_chimeBearer,

    -- * Destructuring the Response
    ListChannelsResponse (..),
    newListChannelsResponse,

    -- * Response Lenses
    listChannelsResponse_channels,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,
  )
where

import Network.AWS.ChimeSDKMessaging.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListChannels' smart constructor.
data ListChannels = ListChannels'
  { -- | The privacy setting. @PUBLIC@ retrieves all the public channels.
    -- @PRIVATE@ retrieves private channels. Only an @AppInstanceAdmin@ can
    -- retrieve private channels.
    privacy :: Prelude.Maybe ChannelPrivacy,
    -- | The token passed by previous API calls until all requested channels are
    -- returned.
    nextToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maximum number of channels that you want to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privacy', 'listChannels_privacy' - The privacy setting. @PUBLIC@ retrieves all the public channels.
-- @PRIVATE@ retrieves private channels. Only an @AppInstanceAdmin@ can
-- retrieve private channels.
--
-- 'nextToken', 'listChannels_nextToken' - The token passed by previous API calls until all requested channels are
-- returned.
--
-- 'maxResults', 'listChannels_maxResults' - The maximum number of channels that you want to return.
--
-- 'appInstanceArn', 'listChannels_appInstanceArn' - The ARN of the @AppInstance@.
--
-- 'chimeBearer', 'listChannels_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
newListChannels ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  ListChannels
newListChannels pAppInstanceArn_ pChimeBearer_ =
  ListChannels'
    { privacy = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      appInstanceArn = pAppInstanceArn_,
      chimeBearer = pChimeBearer_
    }

-- | The privacy setting. @PUBLIC@ retrieves all the public channels.
-- @PRIVATE@ retrieves private channels. Only an @AppInstanceAdmin@ can
-- retrieve private channels.
listChannels_privacy :: Lens.Lens' ListChannels (Prelude.Maybe ChannelPrivacy)
listChannels_privacy = Lens.lens (\ListChannels' {privacy} -> privacy) (\s@ListChannels' {} a -> s {privacy = a} :: ListChannels)

-- | The token passed by previous API calls until all requested channels are
-- returned.
listChannels_nextToken :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Text)
listChannels_nextToken = Lens.lens (\ListChannels' {nextToken} -> nextToken) (\s@ListChannels' {} a -> s {nextToken = a} :: ListChannels) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of channels that you want to return.
listChannels_maxResults :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Natural)
listChannels_maxResults = Lens.lens (\ListChannels' {maxResults} -> maxResults) (\s@ListChannels' {} a -> s {maxResults = a} :: ListChannels)

-- | The ARN of the @AppInstance@.
listChannels_appInstanceArn :: Lens.Lens' ListChannels Prelude.Text
listChannels_appInstanceArn = Lens.lens (\ListChannels' {appInstanceArn} -> appInstanceArn) (\s@ListChannels' {} a -> s {appInstanceArn = a} :: ListChannels)

-- | The @AppInstanceUserArn@ of the user that makes the API call.
listChannels_chimeBearer :: Lens.Lens' ListChannels Prelude.Text
listChannels_chimeBearer = Lens.lens (\ListChannels' {chimeBearer} -> chimeBearer) (\s@ListChannels' {} a -> s {chimeBearer = a} :: ListChannels)

instance Core.AWSRequest ListChannels where
  type AWSResponse ListChannels = ListChannelsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelsResponse'
            Prelude.<$> (x Core..?> "Channels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChannels

instance Prelude.NFData ListChannels

instance Core.ToHeaders ListChannels where
  toHeaders ListChannels' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Core.=# chimeBearer]

instance Core.ToPath ListChannels where
  toPath = Prelude.const "/channels"

instance Core.ToQuery ListChannels where
  toQuery ListChannels' {..} =
    Prelude.mconcat
      [ "privacy" Core.=: privacy,
        "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults,
        "app-instance-arn" Core.=: appInstanceArn
      ]

-- | /See:/ 'newListChannelsResponse' smart constructor.
data ListChannelsResponse = ListChannelsResponse'
  { -- | The information about each channel.
    channels :: Prelude.Maybe [ChannelSummary],
    -- | The token returned from previous API requests until the number of
    -- channels is reached.
    nextToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channels', 'listChannelsResponse_channels' - The information about each channel.
--
-- 'nextToken', 'listChannelsResponse_nextToken' - The token returned from previous API requests until the number of
-- channels is reached.
--
-- 'httpStatus', 'listChannelsResponse_httpStatus' - The response's http status code.
newListChannelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChannelsResponse
newListChannelsResponse pHttpStatus_ =
  ListChannelsResponse'
    { channels = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The information about each channel.
listChannelsResponse_channels :: Lens.Lens' ListChannelsResponse (Prelude.Maybe [ChannelSummary])
listChannelsResponse_channels = Lens.lens (\ListChannelsResponse' {channels} -> channels) (\s@ListChannelsResponse' {} a -> s {channels = a} :: ListChannelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token returned from previous API requests until the number of
-- channels is reached.
listChannelsResponse_nextToken :: Lens.Lens' ListChannelsResponse (Prelude.Maybe Prelude.Text)
listChannelsResponse_nextToken = Lens.lens (\ListChannelsResponse' {nextToken} -> nextToken) (\s@ListChannelsResponse' {} a -> s {nextToken = a} :: ListChannelsResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
listChannelsResponse_httpStatus :: Lens.Lens' ListChannelsResponse Prelude.Int
listChannelsResponse_httpStatus = Lens.lens (\ListChannelsResponse' {httpStatus} -> httpStatus) (\s@ListChannelsResponse' {} a -> s {httpStatus = a} :: ListChannelsResponse)

instance Prelude.NFData ListChannelsResponse
