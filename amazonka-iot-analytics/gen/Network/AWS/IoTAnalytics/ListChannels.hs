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
-- Module      : Network.AWS.IoTAnalytics.ListChannels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of channels.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListChannels
  ( -- * Creating a Request
    ListChannels (..),
    newListChannels,

    -- * Request Lenses
    listChannels_nextToken,
    listChannels_maxResults,

    -- * Destructuring the Response
    ListChannelsResponse (..),
    newListChannelsResponse,

    -- * Response Lenses
    listChannelsResponse_nextToken,
    listChannelsResponse_channelSummaries,
    listChannelsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListChannels' smart constructor.
data ListChannels = ListChannels'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in this request.
    --
    -- The default value is 100.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChannels_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listChannels_maxResults' - The maximum number of results to return in this request.
--
-- The default value is 100.
newListChannels ::
  ListChannels
newListChannels =
  ListChannels'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results.
listChannels_nextToken :: Lens.Lens' ListChannels (Core.Maybe Core.Text)
listChannels_nextToken = Lens.lens (\ListChannels' {nextToken} -> nextToken) (\s@ListChannels' {} a -> s {nextToken = a} :: ListChannels)

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
listChannels_maxResults :: Lens.Lens' ListChannels (Core.Maybe Core.Natural)
listChannels_maxResults = Lens.lens (\ListChannels' {maxResults} -> maxResults) (\s@ListChannels' {} a -> s {maxResults = a} :: ListChannels)

instance Core.AWSPager ListChannels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listChannelsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listChannelsResponse_channelSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listChannels_nextToken
          Lens..~ rs
          Lens.^? listChannelsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListChannels where
  type AWSResponse ListChannels = ListChannelsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "channelSummaries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListChannels

instance Core.NFData ListChannels

instance Core.ToHeaders ListChannels where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListChannels where
  toPath = Core.const "/channels"

instance Core.ToQuery ListChannels where
  toQuery ListChannels' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListChannelsResponse' smart constructor.
data ListChannelsResponse = ListChannelsResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @ChannelSummary@ objects.
    channelSummaries :: Core.Maybe [ChannelSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChannelsResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'channelSummaries', 'listChannelsResponse_channelSummaries' - A list of @ChannelSummary@ objects.
--
-- 'httpStatus', 'listChannelsResponse_httpStatus' - The response's http status code.
newListChannelsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListChannelsResponse
newListChannelsResponse pHttpStatus_ =
  ListChannelsResponse'
    { nextToken = Core.Nothing,
      channelSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listChannelsResponse_nextToken :: Lens.Lens' ListChannelsResponse (Core.Maybe Core.Text)
listChannelsResponse_nextToken = Lens.lens (\ListChannelsResponse' {nextToken} -> nextToken) (\s@ListChannelsResponse' {} a -> s {nextToken = a} :: ListChannelsResponse)

-- | A list of @ChannelSummary@ objects.
listChannelsResponse_channelSummaries :: Lens.Lens' ListChannelsResponse (Core.Maybe [ChannelSummary])
listChannelsResponse_channelSummaries = Lens.lens (\ListChannelsResponse' {channelSummaries} -> channelSummaries) (\s@ListChannelsResponse' {} a -> s {channelSummaries = a} :: ListChannelsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listChannelsResponse_httpStatus :: Lens.Lens' ListChannelsResponse Core.Int
listChannelsResponse_httpStatus = Lens.lens (\ListChannelsResponse' {httpStatus} -> httpStatus) (\s@ListChannelsResponse' {} a -> s {httpStatus = a} :: ListChannelsResponse)

instance Core.NFData ListChannelsResponse
