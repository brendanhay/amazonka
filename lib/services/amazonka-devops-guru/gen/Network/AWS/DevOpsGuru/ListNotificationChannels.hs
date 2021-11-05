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
-- Module      : Network.AWS.DevOpsGuru.ListNotificationChannels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of notification channels configured for DevOps Guru. Each
-- notification channel is used to notify you when DevOps Guru generates an
-- insight that contains information about how to improve your operations.
-- The one supported notification channel is Amazon Simple Notification
-- Service (Amazon SNS).
--
-- This operation returns paginated results.
module Network.AWS.DevOpsGuru.ListNotificationChannels
  ( -- * Creating a Request
    ListNotificationChannels (..),
    newListNotificationChannels,

    -- * Request Lenses
    listNotificationChannels_nextToken,

    -- * Destructuring the Response
    ListNotificationChannelsResponse (..),
    newListNotificationChannelsResponse,

    -- * Response Lenses
    listNotificationChannelsResponse_channels,
    listNotificationChannelsResponse_nextToken,
    listNotificationChannelsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListNotificationChannels' smart constructor.
data ListNotificationChannels = ListNotificationChannels'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotificationChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNotificationChannels_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
newListNotificationChannels ::
  ListNotificationChannels
newListNotificationChannels =
  ListNotificationChannels'
    { nextToken =
        Prelude.Nothing
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listNotificationChannels_nextToken :: Lens.Lens' ListNotificationChannels (Prelude.Maybe Prelude.Text)
listNotificationChannels_nextToken = Lens.lens (\ListNotificationChannels' {nextToken} -> nextToken) (\s@ListNotificationChannels' {} a -> s {nextToken = a} :: ListNotificationChannels)

instance Core.AWSPager ListNotificationChannels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNotificationChannelsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listNotificationChannelsResponse_channels
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listNotificationChannels_nextToken
          Lens..~ rs
          Lens.^? listNotificationChannelsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListNotificationChannels where
  type
    AWSResponse ListNotificationChannels =
      ListNotificationChannelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNotificationChannelsResponse'
            Prelude.<$> (x Core..?> "Channels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNotificationChannels

instance Prelude.NFData ListNotificationChannels

instance Core.ToHeaders ListNotificationChannels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListNotificationChannels where
  toJSON ListNotificationChannels' {..} =
    Core.object
      ( Prelude.catMaybes
          [("NextToken" Core..=) Prelude.<$> nextToken]
      )

instance Core.ToPath ListNotificationChannels where
  toPath = Prelude.const "/channels"

instance Core.ToQuery ListNotificationChannels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNotificationChannelsResponse' smart constructor.
data ListNotificationChannelsResponse = ListNotificationChannelsResponse'
  { -- | An array that contains the requested notification channels.
    channels :: Prelude.Maybe [NotificationChannel],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotificationChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channels', 'listNotificationChannelsResponse_channels' - An array that contains the requested notification channels.
--
-- 'nextToken', 'listNotificationChannelsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'listNotificationChannelsResponse_httpStatus' - The response's http status code.
newListNotificationChannelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNotificationChannelsResponse
newListNotificationChannelsResponse pHttpStatus_ =
  ListNotificationChannelsResponse'
    { channels =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array that contains the requested notification channels.
listNotificationChannelsResponse_channels :: Lens.Lens' ListNotificationChannelsResponse (Prelude.Maybe [NotificationChannel])
listNotificationChannelsResponse_channels = Lens.lens (\ListNotificationChannelsResponse' {channels} -> channels) (\s@ListNotificationChannelsResponse' {} a -> s {channels = a} :: ListNotificationChannelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listNotificationChannelsResponse_nextToken :: Lens.Lens' ListNotificationChannelsResponse (Prelude.Maybe Prelude.Text)
listNotificationChannelsResponse_nextToken = Lens.lens (\ListNotificationChannelsResponse' {nextToken} -> nextToken) (\s@ListNotificationChannelsResponse' {} a -> s {nextToken = a} :: ListNotificationChannelsResponse)

-- | The response's http status code.
listNotificationChannelsResponse_httpStatus :: Lens.Lens' ListNotificationChannelsResponse Prelude.Int
listNotificationChannelsResponse_httpStatus = Lens.lens (\ListNotificationChannelsResponse' {httpStatus} -> httpStatus) (\s@ListNotificationChannelsResponse' {} a -> s {httpStatus = a} :: ListNotificationChannelsResponse)

instance
  Prelude.NFData
    ListNotificationChannelsResponse
