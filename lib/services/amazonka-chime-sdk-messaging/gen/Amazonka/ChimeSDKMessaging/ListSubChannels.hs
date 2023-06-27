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
-- Module      : Amazonka.ChimeSDKMessaging.ListSubChannels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the SubChannels in an elastic channel when given a channel ID.
-- Available only to the app instance admins and channel moderators of
-- elastic channels.
module Amazonka.ChimeSDKMessaging.ListSubChannels
  ( -- * Creating a Request
    ListSubChannels (..),
    newListSubChannels,

    -- * Request Lenses
    listSubChannels_maxResults,
    listSubChannels_nextToken,
    listSubChannels_channelArn,
    listSubChannels_chimeBearer,

    -- * Destructuring the Response
    ListSubChannelsResponse (..),
    newListSubChannelsResponse,

    -- * Response Lenses
    listSubChannelsResponse_channelArn,
    listSubChannelsResponse_nextToken,
    listSubChannelsResponse_subChannels,
    listSubChannelsResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSubChannels' smart constructor.
data ListSubChannels = ListSubChannels'
  { -- | The maximum number of sub-channels that you want to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token passed by previous API calls until all requested sub-channels
    -- are returned.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of elastic channel.
    channelArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the user making the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSubChannels_maxResults' - The maximum number of sub-channels that you want to return.
--
-- 'nextToken', 'listSubChannels_nextToken' - The token passed by previous API calls until all requested sub-channels
-- are returned.
--
-- 'channelArn', 'listSubChannels_channelArn' - The ARN of elastic channel.
--
-- 'chimeBearer', 'listSubChannels_chimeBearer' - The @AppInstanceUserArn@ of the user making the API call.
newListSubChannels ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  ListSubChannels
newListSubChannels pChannelArn_ pChimeBearer_ =
  ListSubChannels'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      channelArn = pChannelArn_,
      chimeBearer = pChimeBearer_
    }

-- | The maximum number of sub-channels that you want to return.
listSubChannels_maxResults :: Lens.Lens' ListSubChannels (Prelude.Maybe Prelude.Natural)
listSubChannels_maxResults = Lens.lens (\ListSubChannels' {maxResults} -> maxResults) (\s@ListSubChannels' {} a -> s {maxResults = a} :: ListSubChannels)

-- | The token passed by previous API calls until all requested sub-channels
-- are returned.
listSubChannels_nextToken :: Lens.Lens' ListSubChannels (Prelude.Maybe Prelude.Text)
listSubChannels_nextToken = Lens.lens (\ListSubChannels' {nextToken} -> nextToken) (\s@ListSubChannels' {} a -> s {nextToken = a} :: ListSubChannels) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of elastic channel.
listSubChannels_channelArn :: Lens.Lens' ListSubChannels Prelude.Text
listSubChannels_channelArn = Lens.lens (\ListSubChannels' {channelArn} -> channelArn) (\s@ListSubChannels' {} a -> s {channelArn = a} :: ListSubChannels)

-- | The @AppInstanceUserArn@ of the user making the API call.
listSubChannels_chimeBearer :: Lens.Lens' ListSubChannels Prelude.Text
listSubChannels_chimeBearer = Lens.lens (\ListSubChannels' {chimeBearer} -> chimeBearer) (\s@ListSubChannels' {} a -> s {chimeBearer = a} :: ListSubChannels)

instance Core.AWSRequest ListSubChannels where
  type
    AWSResponse ListSubChannels =
      ListSubChannelsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubChannelsResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SubChannels" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSubChannels where
  hashWithSalt _salt ListSubChannels' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData ListSubChannels where
  rnf ListSubChannels' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf chimeBearer

instance Data.ToHeaders ListSubChannels where
  toHeaders ListSubChannels' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath ListSubChannels where
  toPath ListSubChannels' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelArn, "/subchannels"]

instance Data.ToQuery ListSubChannels where
  toQuery ListSubChannels' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListSubChannelsResponse' smart constructor.
data ListSubChannelsResponse = ListSubChannelsResponse'
  { -- | The ARN of elastic channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The token passed by previous API calls until all requested sub-channels
    -- are returned.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The information about each sub-channel.
    subChannels :: Prelude.Maybe [SubChannelSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'listSubChannelsResponse_channelArn' - The ARN of elastic channel.
--
-- 'nextToken', 'listSubChannelsResponse_nextToken' - The token passed by previous API calls until all requested sub-channels
-- are returned.
--
-- 'subChannels', 'listSubChannelsResponse_subChannels' - The information about each sub-channel.
--
-- 'httpStatus', 'listSubChannelsResponse_httpStatus' - The response's http status code.
newListSubChannelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSubChannelsResponse
newListSubChannelsResponse pHttpStatus_ =
  ListSubChannelsResponse'
    { channelArn =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      subChannels = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of elastic channel.
listSubChannelsResponse_channelArn :: Lens.Lens' ListSubChannelsResponse (Prelude.Maybe Prelude.Text)
listSubChannelsResponse_channelArn = Lens.lens (\ListSubChannelsResponse' {channelArn} -> channelArn) (\s@ListSubChannelsResponse' {} a -> s {channelArn = a} :: ListSubChannelsResponse)

-- | The token passed by previous API calls until all requested sub-channels
-- are returned.
listSubChannelsResponse_nextToken :: Lens.Lens' ListSubChannelsResponse (Prelude.Maybe Prelude.Text)
listSubChannelsResponse_nextToken = Lens.lens (\ListSubChannelsResponse' {nextToken} -> nextToken) (\s@ListSubChannelsResponse' {} a -> s {nextToken = a} :: ListSubChannelsResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The information about each sub-channel.
listSubChannelsResponse_subChannels :: Lens.Lens' ListSubChannelsResponse (Prelude.Maybe [SubChannelSummary])
listSubChannelsResponse_subChannels = Lens.lens (\ListSubChannelsResponse' {subChannels} -> subChannels) (\s@ListSubChannelsResponse' {} a -> s {subChannels = a} :: ListSubChannelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSubChannelsResponse_httpStatus :: Lens.Lens' ListSubChannelsResponse Prelude.Int
listSubChannelsResponse_httpStatus = Lens.lens (\ListSubChannelsResponse' {httpStatus} -> httpStatus) (\s@ListSubChannelsResponse' {} a -> s {httpStatus = a} :: ListSubChannelsResponse)

instance Prelude.NFData ListSubChannelsResponse where
  rnf ListSubChannelsResponse' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subChannels
      `Prelude.seq` Prelude.rnf httpStatus
