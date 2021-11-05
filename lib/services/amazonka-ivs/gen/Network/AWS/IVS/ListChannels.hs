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
-- Module      : Network.AWS.IVS.ListChannels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summary information about all channels in your account, in the
-- Amazon Web Services region where the API request is processed. This list
-- can be filtered to match a specified name or recording-configuration
-- ARN. Filters are mutually exclusive and cannot be used together. If you
-- try to use both filters, you will get an error (409 ConflictException).
--
-- This operation returns paginated results.
module Network.AWS.IVS.ListChannels
  ( -- * Creating a Request
    ListChannels (..),
    newListChannels,

    -- * Request Lenses
    listChannels_filterByName,
    listChannels_nextToken,
    listChannels_filterByRecordingConfigurationArn,
    listChannels_maxResults,

    -- * Destructuring the Response
    ListChannelsResponse (..),
    newListChannelsResponse,

    -- * Response Lenses
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,
    listChannelsResponse_channels,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IVS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListChannels' smart constructor.
data ListChannels = ListChannels'
  { -- | Filters the channel list to match the specified name.
    filterByName :: Prelude.Maybe Prelude.Text,
    -- | The first channel to retrieve. This is used for pagination; see the
    -- @nextToken@ response field.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the channel list to match the specified recording-configuration
    -- ARN.
    filterByRecordingConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of channels to return. Default: 50.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterByName', 'listChannels_filterByName' - Filters the channel list to match the specified name.
--
-- 'nextToken', 'listChannels_nextToken' - The first channel to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
--
-- 'filterByRecordingConfigurationArn', 'listChannels_filterByRecordingConfigurationArn' - Filters the channel list to match the specified recording-configuration
-- ARN.
--
-- 'maxResults', 'listChannels_maxResults' - Maximum number of channels to return. Default: 50.
newListChannels ::
  ListChannels
newListChannels =
  ListChannels'
    { filterByName = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filterByRecordingConfigurationArn = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Filters the channel list to match the specified name.
listChannels_filterByName :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Text)
listChannels_filterByName = Lens.lens (\ListChannels' {filterByName} -> filterByName) (\s@ListChannels' {} a -> s {filterByName = a} :: ListChannels)

-- | The first channel to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
listChannels_nextToken :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Text)
listChannels_nextToken = Lens.lens (\ListChannels' {nextToken} -> nextToken) (\s@ListChannels' {} a -> s {nextToken = a} :: ListChannels)

-- | Filters the channel list to match the specified recording-configuration
-- ARN.
listChannels_filterByRecordingConfigurationArn :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Text)
listChannels_filterByRecordingConfigurationArn = Lens.lens (\ListChannels' {filterByRecordingConfigurationArn} -> filterByRecordingConfigurationArn) (\s@ListChannels' {} a -> s {filterByRecordingConfigurationArn = a} :: ListChannels)

-- | Maximum number of channels to return. Default: 50.
listChannels_maxResults :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Natural)
listChannels_maxResults = Lens.lens (\ListChannels' {maxResults} -> maxResults) (\s@ListChannels' {} a -> s {maxResults = a} :: ListChannels)

instance Core.AWSPager ListChannels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listChannelsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listChannelsResponse_channels) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listChannels_nextToken
          Lens..~ rs
          Lens.^? listChannelsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListChannels where
  type AWSResponse ListChannels = ListChannelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "channels" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListChannels

instance Prelude.NFData ListChannels

instance Core.ToHeaders ListChannels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListChannels where
  toJSON ListChannels' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filterByName" Core..=) Prelude.<$> filterByName,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filterByRecordingConfigurationArn" Core..=)
              Prelude.<$> filterByRecordingConfigurationArn,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListChannels where
  toPath = Prelude.const "/ListChannels"

instance Core.ToQuery ListChannels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListChannelsResponse' smart constructor.
data ListChannelsResponse = ListChannelsResponse'
  { -- | If there are more channels than @maxResults@, use @nextToken@ in the
    -- request to get the next set.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of the matching channels.
    channels :: [ChannelSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChannelsResponse_nextToken' - If there are more channels than @maxResults@, use @nextToken@ in the
-- request to get the next set.
--
-- 'httpStatus', 'listChannelsResponse_httpStatus' - The response's http status code.
--
-- 'channels', 'listChannelsResponse_channels' - List of the matching channels.
newListChannelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChannelsResponse
newListChannelsResponse pHttpStatus_ =
  ListChannelsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      channels = Prelude.mempty
    }

-- | If there are more channels than @maxResults@, use @nextToken@ in the
-- request to get the next set.
listChannelsResponse_nextToken :: Lens.Lens' ListChannelsResponse (Prelude.Maybe Prelude.Text)
listChannelsResponse_nextToken = Lens.lens (\ListChannelsResponse' {nextToken} -> nextToken) (\s@ListChannelsResponse' {} a -> s {nextToken = a} :: ListChannelsResponse)

-- | The response's http status code.
listChannelsResponse_httpStatus :: Lens.Lens' ListChannelsResponse Prelude.Int
listChannelsResponse_httpStatus = Lens.lens (\ListChannelsResponse' {httpStatus} -> httpStatus) (\s@ListChannelsResponse' {} a -> s {httpStatus = a} :: ListChannelsResponse)

-- | List of the matching channels.
listChannelsResponse_channels :: Lens.Lens' ListChannelsResponse [ChannelSummary]
listChannelsResponse_channels = Lens.lens (\ListChannelsResponse' {channels} -> channels) (\s@ListChannelsResponse' {} a -> s {channels = a} :: ListChannelsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListChannelsResponse
