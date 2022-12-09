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
-- Module      : Amazonka.IVS.ListChannels
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.IVS.ListChannels
  ( -- * Creating a Request
    ListChannels (..),
    newListChannels,

    -- * Request Lenses
    listChannels_filterByName,
    listChannels_filterByRecordingConfigurationArn,
    listChannels_maxResults,
    listChannels_nextToken,

    -- * Destructuring the Response
    ListChannelsResponse (..),
    newListChannelsResponse,

    -- * Response Lenses
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,
    listChannelsResponse_channels,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChannels' smart constructor.
data ListChannels = ListChannels'
  { -- | Filters the channel list to match the specified name.
    filterByName :: Prelude.Maybe Prelude.Text,
    -- | Filters the channel list to match the specified recording-configuration
    -- ARN.
    filterByRecordingConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of channels to return. Default: 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The first channel to retrieve. This is used for pagination; see the
    -- @nextToken@ response field.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filterByRecordingConfigurationArn', 'listChannels_filterByRecordingConfigurationArn' - Filters the channel list to match the specified recording-configuration
-- ARN.
--
-- 'maxResults', 'listChannels_maxResults' - Maximum number of channels to return. Default: 100.
--
-- 'nextToken', 'listChannels_nextToken' - The first channel to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
newListChannels ::
  ListChannels
newListChannels =
  ListChannels'
    { filterByName = Prelude.Nothing,
      filterByRecordingConfigurationArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the channel list to match the specified name.
listChannels_filterByName :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Text)
listChannels_filterByName = Lens.lens (\ListChannels' {filterByName} -> filterByName) (\s@ListChannels' {} a -> s {filterByName = a} :: ListChannels)

-- | Filters the channel list to match the specified recording-configuration
-- ARN.
listChannels_filterByRecordingConfigurationArn :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Text)
listChannels_filterByRecordingConfigurationArn = Lens.lens (\ListChannels' {filterByRecordingConfigurationArn} -> filterByRecordingConfigurationArn) (\s@ListChannels' {} a -> s {filterByRecordingConfigurationArn = a} :: ListChannels)

-- | Maximum number of channels to return. Default: 100.
listChannels_maxResults :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Natural)
listChannels_maxResults = Lens.lens (\ListChannels' {maxResults} -> maxResults) (\s@ListChannels' {} a -> s {maxResults = a} :: ListChannels)

-- | The first channel to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
listChannels_nextToken :: Lens.Lens' ListChannels (Prelude.Maybe Prelude.Text)
listChannels_nextToken = Lens.lens (\ListChannels' {nextToken} -> nextToken) (\s@ListChannels' {} a -> s {nextToken = a} :: ListChannels)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "channels" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListChannels where
  hashWithSalt _salt ListChannels' {..} =
    _salt `Prelude.hashWithSalt` filterByName
      `Prelude.hashWithSalt` filterByRecordingConfigurationArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListChannels where
  rnf ListChannels' {..} =
    Prelude.rnf filterByName
      `Prelude.seq` Prelude.rnf filterByRecordingConfigurationArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListChannels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListChannels where
  toJSON ListChannels' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterByName" Data..=) Prelude.<$> filterByName,
            ("filterByRecordingConfigurationArn" Data..=)
              Prelude.<$> filterByRecordingConfigurationArn,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListChannels where
  toPath = Prelude.const "/ListChannels"

instance Data.ToQuery ListChannels where
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

instance Prelude.NFData ListChannelsResponse where
  rnf ListChannelsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf channels
