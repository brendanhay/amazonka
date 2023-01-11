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
-- Module      : Amazonka.Chime.ListChannels
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Chime.ListChannels
  ( -- * Creating a Request
    ListChannels (..),
    newListChannels,

    -- * Request Lenses
    listChannels_chimeBearer,
    listChannels_maxResults,
    listChannels_nextToken,
    listChannels_privacy,
    listChannels_appInstanceArn,

    -- * Destructuring the Response
    ListChannelsResponse (..),
    newListChannelsResponse,

    -- * Response Lenses
    listChannelsResponse_channels,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChannels' smart constructor.
data ListChannels = ListChannels'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of channels that you want to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token passed by previous API calls until all requested channels are
    -- returned.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The privacy setting. @PUBLIC@ retrieves all the public channels.
    -- @PRIVATE@ retrieves private channels. Only an @AppInstanceAdmin@ can
    -- retrieve private channels.
    privacy :: Prelude.Maybe ChannelPrivacy,
    -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Text
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
-- 'chimeBearer', 'listChannels_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'maxResults', 'listChannels_maxResults' - The maximum number of channels that you want to return.
--
-- 'nextToken', 'listChannels_nextToken' - The token passed by previous API calls until all requested channels are
-- returned.
--
-- 'privacy', 'listChannels_privacy' - The privacy setting. @PUBLIC@ retrieves all the public channels.
-- @PRIVATE@ retrieves private channels. Only an @AppInstanceAdmin@ can
-- retrieve private channels.
--
-- 'appInstanceArn', 'listChannels_appInstanceArn' - The ARN of the @AppInstance@.
newListChannels ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  ListChannels
newListChannels pAppInstanceArn_ =
  ListChannels'
    { chimeBearer = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      privacy = Prelude.Nothing,
      appInstanceArn = pAppInstanceArn_
    }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
listChannels_chimeBearer :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Text)
listChannels_chimeBearer = Lens.lens (\ListChannels' {chimeBearer} -> chimeBearer) (\s@ListChannels' {} a -> s {chimeBearer = a} :: ListChannels)

-- | The maximum number of channels that you want to return.
listChannels_maxResults :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Natural)
listChannels_maxResults = Lens.lens (\ListChannels' {maxResults} -> maxResults) (\s@ListChannels' {} a -> s {maxResults = a} :: ListChannels)

-- | The token passed by previous API calls until all requested channels are
-- returned.
listChannels_nextToken :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Text)
listChannels_nextToken = Lens.lens (\ListChannels' {nextToken} -> nextToken) (\s@ListChannels' {} a -> s {nextToken = a} :: ListChannels) Prelude.. Lens.mapping Data._Sensitive

-- | The privacy setting. @PUBLIC@ retrieves all the public channels.
-- @PRIVATE@ retrieves private channels. Only an @AppInstanceAdmin@ can
-- retrieve private channels.
listChannels_privacy :: Lens.Lens' ListChannels (Prelude.Maybe ChannelPrivacy)
listChannels_privacy = Lens.lens (\ListChannels' {privacy} -> privacy) (\s@ListChannels' {} a -> s {privacy = a} :: ListChannels)

-- | The ARN of the @AppInstance@.
listChannels_appInstanceArn :: Lens.Lens' ListChannels Prelude.Text
listChannels_appInstanceArn = Lens.lens (\ListChannels' {appInstanceArn} -> appInstanceArn) (\s@ListChannels' {} a -> s {appInstanceArn = a} :: ListChannels)

instance Core.AWSRequest ListChannels where
  type AWSResponse ListChannels = ListChannelsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelsResponse'
            Prelude.<$> (x Data..?> "Channels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChannels where
  hashWithSalt _salt ListChannels' {..} =
    _salt `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` privacy
      `Prelude.hashWithSalt` appInstanceArn

instance Prelude.NFData ListChannels where
  rnf ListChannels' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf privacy
      `Prelude.seq` Prelude.rnf appInstanceArn

instance Data.ToHeaders ListChannels where
  toHeaders ListChannels' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath ListChannels where
  toPath = Prelude.const "/channels"

instance Data.ToQuery ListChannels where
  toQuery ListChannels' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken,
        "privacy" Data.=: privacy,
        "app-instance-arn" Data.=: appInstanceArn
      ]

-- | /See:/ 'newListChannelsResponse' smart constructor.
data ListChannelsResponse = ListChannelsResponse'
  { -- | The information about each channel.
    channels :: Prelude.Maybe [ChannelSummary],
    -- | The token returned from previous API requests until the number of
    -- channels is reached.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
listChannelsResponse_nextToken = Lens.lens (\ListChannelsResponse' {nextToken} -> nextToken) (\s@ListChannelsResponse' {} a -> s {nextToken = a} :: ListChannelsResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
listChannelsResponse_httpStatus :: Lens.Lens' ListChannelsResponse Prelude.Int
listChannelsResponse_httpStatus = Lens.lens (\ListChannelsResponse' {httpStatus} -> httpStatus) (\s@ListChannelsResponse' {} a -> s {httpStatus = a} :: ListChannelsResponse)

instance Prelude.NFData ListChannelsResponse where
  rnf ListChannelsResponse' {..} =
    Prelude.rnf channels
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
