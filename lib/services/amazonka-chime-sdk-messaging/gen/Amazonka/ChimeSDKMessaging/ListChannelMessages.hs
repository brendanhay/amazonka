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
-- Module      : Amazonka.ChimeSDKMessaging.ListChannelMessages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all the messages in a channel. Returns a paginated list of
-- @ChannelMessages@. By default, sorted by creation timestamp in
-- descending order.
--
-- Redacted messages appear in the results as empty, since they are only
-- redacted, not deleted. Deleted messages do not appear in the results.
-- This action always returns the latest version of an edited message.
--
-- Also, the x-amz-chime-bearer request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.ChimeSDKMessaging.ListChannelMessages
  ( -- * Creating a Request
    ListChannelMessages (..),
    newListChannelMessages,

    -- * Request Lenses
    listChannelMessages_sortOrder,
    listChannelMessages_nextToken,
    listChannelMessages_subChannelId,
    listChannelMessages_notBefore,
    listChannelMessages_maxResults,
    listChannelMessages_notAfter,
    listChannelMessages_channelArn,
    listChannelMessages_chimeBearer,

    -- * Destructuring the Response
    ListChannelMessagesResponse (..),
    newListChannelMessagesResponse,

    -- * Response Lenses
    listChannelMessagesResponse_nextToken,
    listChannelMessagesResponse_subChannelId,
    listChannelMessagesResponse_channelArn,
    listChannelMessagesResponse_channelMessages,
    listChannelMessagesResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChannelMessages' smart constructor.
data ListChannelMessages = ListChannelMessages'
  { -- | The order in which you want messages sorted. Default is Descending,
    -- based on time created.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The token passed by previous API calls until all requested messages are
    -- returned.
    nextToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the SubChannel in the request.
    --
    -- Only required when listing the messages in a SubChannel that the user
    -- belongs to.
    subChannelId :: Prelude.Maybe Prelude.Text,
    -- | The initial or starting time stamp for your requested messages.
    notBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of messages that you want returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The final or ending time stamp for your requested messages.
    notAfter :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelMessages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listChannelMessages_sortOrder' - The order in which you want messages sorted. Default is Descending,
-- based on time created.
--
-- 'nextToken', 'listChannelMessages_nextToken' - The token passed by previous API calls until all requested messages are
-- returned.
--
-- 'subChannelId', 'listChannelMessages_subChannelId' - The ID of the SubChannel in the request.
--
-- Only required when listing the messages in a SubChannel that the user
-- belongs to.
--
-- 'notBefore', 'listChannelMessages_notBefore' - The initial or starting time stamp for your requested messages.
--
-- 'maxResults', 'listChannelMessages_maxResults' - The maximum number of messages that you want returned.
--
-- 'notAfter', 'listChannelMessages_notAfter' - The final or ending time stamp for your requested messages.
--
-- 'channelArn', 'listChannelMessages_channelArn' - The ARN of the channel.
--
-- 'chimeBearer', 'listChannelMessages_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
newListChannelMessages ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  ListChannelMessages
newListChannelMessages pChannelArn_ pChimeBearer_ =
  ListChannelMessages'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      subChannelId = Prelude.Nothing,
      notBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      notAfter = Prelude.Nothing,
      channelArn = pChannelArn_,
      chimeBearer = pChimeBearer_
    }

-- | The order in which you want messages sorted. Default is Descending,
-- based on time created.
listChannelMessages_sortOrder :: Lens.Lens' ListChannelMessages (Prelude.Maybe SortOrder)
listChannelMessages_sortOrder = Lens.lens (\ListChannelMessages' {sortOrder} -> sortOrder) (\s@ListChannelMessages' {} a -> s {sortOrder = a} :: ListChannelMessages)

-- | The token passed by previous API calls until all requested messages are
-- returned.
listChannelMessages_nextToken :: Lens.Lens' ListChannelMessages (Prelude.Maybe Prelude.Text)
listChannelMessages_nextToken = Lens.lens (\ListChannelMessages' {nextToken} -> nextToken) (\s@ListChannelMessages' {} a -> s {nextToken = a} :: ListChannelMessages) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the SubChannel in the request.
--
-- Only required when listing the messages in a SubChannel that the user
-- belongs to.
listChannelMessages_subChannelId :: Lens.Lens' ListChannelMessages (Prelude.Maybe Prelude.Text)
listChannelMessages_subChannelId = Lens.lens (\ListChannelMessages' {subChannelId} -> subChannelId) (\s@ListChannelMessages' {} a -> s {subChannelId = a} :: ListChannelMessages)

-- | The initial or starting time stamp for your requested messages.
listChannelMessages_notBefore :: Lens.Lens' ListChannelMessages (Prelude.Maybe Prelude.UTCTime)
listChannelMessages_notBefore = Lens.lens (\ListChannelMessages' {notBefore} -> notBefore) (\s@ListChannelMessages' {} a -> s {notBefore = a} :: ListChannelMessages) Prelude.. Lens.mapping Core._Time

-- | The maximum number of messages that you want returned.
listChannelMessages_maxResults :: Lens.Lens' ListChannelMessages (Prelude.Maybe Prelude.Natural)
listChannelMessages_maxResults = Lens.lens (\ListChannelMessages' {maxResults} -> maxResults) (\s@ListChannelMessages' {} a -> s {maxResults = a} :: ListChannelMessages)

-- | The final or ending time stamp for your requested messages.
listChannelMessages_notAfter :: Lens.Lens' ListChannelMessages (Prelude.Maybe Prelude.UTCTime)
listChannelMessages_notAfter = Lens.lens (\ListChannelMessages' {notAfter} -> notAfter) (\s@ListChannelMessages' {} a -> s {notAfter = a} :: ListChannelMessages) Prelude.. Lens.mapping Core._Time

-- | The ARN of the channel.
listChannelMessages_channelArn :: Lens.Lens' ListChannelMessages Prelude.Text
listChannelMessages_channelArn = Lens.lens (\ListChannelMessages' {channelArn} -> channelArn) (\s@ListChannelMessages' {} a -> s {channelArn = a} :: ListChannelMessages)

-- | The @AppInstanceUserArn@ of the user that makes the API call.
listChannelMessages_chimeBearer :: Lens.Lens' ListChannelMessages Prelude.Text
listChannelMessages_chimeBearer = Lens.lens (\ListChannelMessages' {chimeBearer} -> chimeBearer) (\s@ListChannelMessages' {} a -> s {chimeBearer = a} :: ListChannelMessages)

instance Core.AWSRequest ListChannelMessages where
  type
    AWSResponse ListChannelMessages =
      ListChannelMessagesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelMessagesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "SubChannelId")
            Prelude.<*> (x Core..?> "ChannelArn")
            Prelude.<*> ( x Core..?> "ChannelMessages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChannelMessages where
  hashWithSalt _salt ListChannelMessages' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` subChannelId
      `Prelude.hashWithSalt` notBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` notAfter
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData ListChannelMessages where
  rnf ListChannelMessages' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subChannelId
      `Prelude.seq` Prelude.rnf notBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf notAfter
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf chimeBearer

instance Core.ToHeaders ListChannelMessages where
  toHeaders ListChannelMessages' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Core.=# chimeBearer]

instance Core.ToPath ListChannelMessages where
  toPath ListChannelMessages' {..} =
    Prelude.mconcat
      ["/channels/", Core.toBS channelArn, "/messages"]

instance Core.ToQuery ListChannelMessages where
  toQuery ListChannelMessages' {..} =
    Prelude.mconcat
      [ "sort-order" Core.=: sortOrder,
        "next-token" Core.=: nextToken,
        "sub-channel-id" Core.=: subChannelId,
        "not-before" Core.=: notBefore,
        "max-results" Core.=: maxResults,
        "not-after" Core.=: notAfter
      ]

-- | /See:/ 'newListChannelMessagesResponse' smart constructor.
data ListChannelMessagesResponse = ListChannelMessagesResponse'
  { -- | The token passed by previous API calls until all requested messages are
    -- returned.
    nextToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the SubChannel in the response.
    subChannelId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel containing the requested messages.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The information about, and content of, each requested message.
    channelMessages :: Prelude.Maybe [ChannelMessageSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelMessagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChannelMessagesResponse_nextToken' - The token passed by previous API calls until all requested messages are
-- returned.
--
-- 'subChannelId', 'listChannelMessagesResponse_subChannelId' - The ID of the SubChannel in the response.
--
-- 'channelArn', 'listChannelMessagesResponse_channelArn' - The ARN of the channel containing the requested messages.
--
-- 'channelMessages', 'listChannelMessagesResponse_channelMessages' - The information about, and content of, each requested message.
--
-- 'httpStatus', 'listChannelMessagesResponse_httpStatus' - The response's http status code.
newListChannelMessagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChannelMessagesResponse
newListChannelMessagesResponse pHttpStatus_ =
  ListChannelMessagesResponse'
    { nextToken =
        Prelude.Nothing,
      subChannelId = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      channelMessages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token passed by previous API calls until all requested messages are
-- returned.
listChannelMessagesResponse_nextToken :: Lens.Lens' ListChannelMessagesResponse (Prelude.Maybe Prelude.Text)
listChannelMessagesResponse_nextToken = Lens.lens (\ListChannelMessagesResponse' {nextToken} -> nextToken) (\s@ListChannelMessagesResponse' {} a -> s {nextToken = a} :: ListChannelMessagesResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the SubChannel in the response.
listChannelMessagesResponse_subChannelId :: Lens.Lens' ListChannelMessagesResponse (Prelude.Maybe Prelude.Text)
listChannelMessagesResponse_subChannelId = Lens.lens (\ListChannelMessagesResponse' {subChannelId} -> subChannelId) (\s@ListChannelMessagesResponse' {} a -> s {subChannelId = a} :: ListChannelMessagesResponse)

-- | The ARN of the channel containing the requested messages.
listChannelMessagesResponse_channelArn :: Lens.Lens' ListChannelMessagesResponse (Prelude.Maybe Prelude.Text)
listChannelMessagesResponse_channelArn = Lens.lens (\ListChannelMessagesResponse' {channelArn} -> channelArn) (\s@ListChannelMessagesResponse' {} a -> s {channelArn = a} :: ListChannelMessagesResponse)

-- | The information about, and content of, each requested message.
listChannelMessagesResponse_channelMessages :: Lens.Lens' ListChannelMessagesResponse (Prelude.Maybe [ChannelMessageSummary])
listChannelMessagesResponse_channelMessages = Lens.lens (\ListChannelMessagesResponse' {channelMessages} -> channelMessages) (\s@ListChannelMessagesResponse' {} a -> s {channelMessages = a} :: ListChannelMessagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listChannelMessagesResponse_httpStatus :: Lens.Lens' ListChannelMessagesResponse Prelude.Int
listChannelMessagesResponse_httpStatus = Lens.lens (\ListChannelMessagesResponse' {httpStatus} -> httpStatus) (\s@ListChannelMessagesResponse' {} a -> s {httpStatus = a} :: ListChannelMessagesResponse)

instance Prelude.NFData ListChannelMessagesResponse where
  rnf ListChannelMessagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subChannelId
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf channelMessages
      `Prelude.seq` Prelude.rnf httpStatus
