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
-- Module      : Amazonka.MediaLive.ListOfferings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List offerings available for purchase.
--
-- This operation returns paginated results.
module Amazonka.MediaLive.ListOfferings
  ( -- * Creating a Request
    ListOfferings (..),
    newListOfferings,

    -- * Request Lenses
    listOfferings_resourceType,
    listOfferings_nextToken,
    listOfferings_channelConfiguration,
    listOfferings_channelClass,
    listOfferings_codec,
    listOfferings_duration,
    listOfferings_maxResults,
    listOfferings_maximumFramerate,
    listOfferings_videoQuality,
    listOfferings_maximumBitrate,
    listOfferings_specialFeature,
    listOfferings_resolution,

    -- * Destructuring the Response
    ListOfferingsResponse (..),
    newListOfferingsResponse,

    -- * Response Lenses
    listOfferingsResponse_nextToken,
    listOfferingsResponse_offerings,
    listOfferingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for ListOfferingsRequest
--
-- /See:/ 'newListOfferings' smart constructor.
data ListOfferings = ListOfferings'
  { -- | Filter by resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or
    -- \'CHANNEL\'
    resourceType :: Prelude.Maybe Prelude.Text,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter to offerings that match the configuration of an existing channel,
    -- e.g. \'2345678\' (a channel ID)
    channelConfiguration :: Prelude.Maybe Prelude.Text,
    -- | Filter by channel class, \'STANDARD\' or \'SINGLE_PIPELINE\'
    channelClass :: Prelude.Maybe Prelude.Text,
    -- | Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
    codec :: Prelude.Maybe Prelude.Text,
    -- | Filter by offering duration, e.g. \'12\'
    duration :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
    maximumFramerate :: Prelude.Maybe Prelude.Text,
    -- | Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
    videoQuality :: Prelude.Maybe Prelude.Text,
    -- | Filter by bitrate, \'MAX_10_MBPS\', \'MAX_20_MBPS\', or \'MAX_50_MBPS\'
    maximumBitrate :: Prelude.Maybe Prelude.Text,
    -- | Filter by special feature, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
    specialFeature :: Prelude.Maybe Prelude.Text,
    -- | Filter by resolution, \'SD\', \'HD\', \'FHD\', or \'UHD\'
    resolution :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'listOfferings_resourceType' - Filter by resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or
-- \'CHANNEL\'
--
-- 'nextToken', 'listOfferings_nextToken' - Undocumented member.
--
-- 'channelConfiguration', 'listOfferings_channelConfiguration' - Filter to offerings that match the configuration of an existing channel,
-- e.g. \'2345678\' (a channel ID)
--
-- 'channelClass', 'listOfferings_channelClass' - Filter by channel class, \'STANDARD\' or \'SINGLE_PIPELINE\'
--
-- 'codec', 'listOfferings_codec' - Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
--
-- 'duration', 'listOfferings_duration' - Filter by offering duration, e.g. \'12\'
--
-- 'maxResults', 'listOfferings_maxResults' - Undocumented member.
--
-- 'maximumFramerate', 'listOfferings_maximumFramerate' - Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
--
-- 'videoQuality', 'listOfferings_videoQuality' - Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
--
-- 'maximumBitrate', 'listOfferings_maximumBitrate' - Filter by bitrate, \'MAX_10_MBPS\', \'MAX_20_MBPS\', or \'MAX_50_MBPS\'
--
-- 'specialFeature', 'listOfferings_specialFeature' - Filter by special feature, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
--
-- 'resolution', 'listOfferings_resolution' - Filter by resolution, \'SD\', \'HD\', \'FHD\', or \'UHD\'
newListOfferings ::
  ListOfferings
newListOfferings =
  ListOfferings'
    { resourceType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      channelConfiguration = Prelude.Nothing,
      channelClass = Prelude.Nothing,
      codec = Prelude.Nothing,
      duration = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      maximumFramerate = Prelude.Nothing,
      videoQuality = Prelude.Nothing,
      maximumBitrate = Prelude.Nothing,
      specialFeature = Prelude.Nothing,
      resolution = Prelude.Nothing
    }

-- | Filter by resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or
-- \'CHANNEL\'
listOfferings_resourceType :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_resourceType = Lens.lens (\ListOfferings' {resourceType} -> resourceType) (\s@ListOfferings' {} a -> s {resourceType = a} :: ListOfferings)

-- | Undocumented member.
listOfferings_nextToken :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_nextToken = Lens.lens (\ListOfferings' {nextToken} -> nextToken) (\s@ListOfferings' {} a -> s {nextToken = a} :: ListOfferings)

-- | Filter to offerings that match the configuration of an existing channel,
-- e.g. \'2345678\' (a channel ID)
listOfferings_channelConfiguration :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_channelConfiguration = Lens.lens (\ListOfferings' {channelConfiguration} -> channelConfiguration) (\s@ListOfferings' {} a -> s {channelConfiguration = a} :: ListOfferings)

-- | Filter by channel class, \'STANDARD\' or \'SINGLE_PIPELINE\'
listOfferings_channelClass :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_channelClass = Lens.lens (\ListOfferings' {channelClass} -> channelClass) (\s@ListOfferings' {} a -> s {channelClass = a} :: ListOfferings)

-- | Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
listOfferings_codec :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_codec = Lens.lens (\ListOfferings' {codec} -> codec) (\s@ListOfferings' {} a -> s {codec = a} :: ListOfferings)

-- | Filter by offering duration, e.g. \'12\'
listOfferings_duration :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_duration = Lens.lens (\ListOfferings' {duration} -> duration) (\s@ListOfferings' {} a -> s {duration = a} :: ListOfferings)

-- | Undocumented member.
listOfferings_maxResults :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Natural)
listOfferings_maxResults = Lens.lens (\ListOfferings' {maxResults} -> maxResults) (\s@ListOfferings' {} a -> s {maxResults = a} :: ListOfferings)

-- | Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
listOfferings_maximumFramerate :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_maximumFramerate = Lens.lens (\ListOfferings' {maximumFramerate} -> maximumFramerate) (\s@ListOfferings' {} a -> s {maximumFramerate = a} :: ListOfferings)

-- | Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
listOfferings_videoQuality :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_videoQuality = Lens.lens (\ListOfferings' {videoQuality} -> videoQuality) (\s@ListOfferings' {} a -> s {videoQuality = a} :: ListOfferings)

-- | Filter by bitrate, \'MAX_10_MBPS\', \'MAX_20_MBPS\', or \'MAX_50_MBPS\'
listOfferings_maximumBitrate :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_maximumBitrate = Lens.lens (\ListOfferings' {maximumBitrate} -> maximumBitrate) (\s@ListOfferings' {} a -> s {maximumBitrate = a} :: ListOfferings)

-- | Filter by special feature, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
listOfferings_specialFeature :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_specialFeature = Lens.lens (\ListOfferings' {specialFeature} -> specialFeature) (\s@ListOfferings' {} a -> s {specialFeature = a} :: ListOfferings)

-- | Filter by resolution, \'SD\', \'HD\', \'FHD\', or \'UHD\'
listOfferings_resolution :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_resolution = Lens.lens (\ListOfferings' {resolution} -> resolution) (\s@ListOfferings' {} a -> s {resolution = a} :: ListOfferings)

instance Core.AWSPager ListOfferings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOfferingsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOfferingsResponse_offerings Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOfferings_nextToken
          Lens..~ rs
          Lens.^? listOfferingsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListOfferings where
  type
    AWSResponse ListOfferings =
      ListOfferingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOfferingsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "offerings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOfferings where
  hashWithSalt _salt ListOfferings' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` channelConfiguration
      `Prelude.hashWithSalt` channelClass
      `Prelude.hashWithSalt` codec
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` maximumFramerate
      `Prelude.hashWithSalt` videoQuality
      `Prelude.hashWithSalt` maximumBitrate
      `Prelude.hashWithSalt` specialFeature
      `Prelude.hashWithSalt` resolution

instance Prelude.NFData ListOfferings where
  rnf ListOfferings' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf channelConfiguration
      `Prelude.seq` Prelude.rnf channelClass
      `Prelude.seq` Prelude.rnf codec
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf maximumFramerate
      `Prelude.seq` Prelude.rnf videoQuality
      `Prelude.seq` Prelude.rnf maximumBitrate
      `Prelude.seq` Prelude.rnf specialFeature
      `Prelude.seq` Prelude.rnf resolution

instance Core.ToHeaders ListOfferings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListOfferings where
  toPath = Prelude.const "/prod/offerings"

instance Core.ToQuery ListOfferings where
  toQuery ListOfferings' {..} =
    Prelude.mconcat
      [ "resourceType" Core.=: resourceType,
        "nextToken" Core.=: nextToken,
        "channelConfiguration" Core.=: channelConfiguration,
        "channelClass" Core.=: channelClass,
        "codec" Core.=: codec,
        "duration" Core.=: duration,
        "maxResults" Core.=: maxResults,
        "maximumFramerate" Core.=: maximumFramerate,
        "videoQuality" Core.=: videoQuality,
        "maximumBitrate" Core.=: maximumBitrate,
        "specialFeature" Core.=: specialFeature,
        "resolution" Core.=: resolution
      ]

-- | Placeholder documentation for ListOfferingsResponse
--
-- /See:/ 'newListOfferingsResponse' smart constructor.
data ListOfferingsResponse = ListOfferingsResponse'
  { -- | Token to retrieve the next page of results
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of offerings
    offerings :: Prelude.Maybe [Offering],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOfferingsResponse_nextToken' - Token to retrieve the next page of results
--
-- 'offerings', 'listOfferingsResponse_offerings' - List of offerings
--
-- 'httpStatus', 'listOfferingsResponse_httpStatus' - The response's http status code.
newListOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOfferingsResponse
newListOfferingsResponse pHttpStatus_ =
  ListOfferingsResponse'
    { nextToken = Prelude.Nothing,
      offerings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token to retrieve the next page of results
listOfferingsResponse_nextToken :: Lens.Lens' ListOfferingsResponse (Prelude.Maybe Prelude.Text)
listOfferingsResponse_nextToken = Lens.lens (\ListOfferingsResponse' {nextToken} -> nextToken) (\s@ListOfferingsResponse' {} a -> s {nextToken = a} :: ListOfferingsResponse)

-- | List of offerings
listOfferingsResponse_offerings :: Lens.Lens' ListOfferingsResponse (Prelude.Maybe [Offering])
listOfferingsResponse_offerings = Lens.lens (\ListOfferingsResponse' {offerings} -> offerings) (\s@ListOfferingsResponse' {} a -> s {offerings = a} :: ListOfferingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOfferingsResponse_httpStatus :: Lens.Lens' ListOfferingsResponse Prelude.Int
listOfferingsResponse_httpStatus = Lens.lens (\ListOfferingsResponse' {httpStatus} -> httpStatus) (\s@ListOfferingsResponse' {} a -> s {httpStatus = a} :: ListOfferingsResponse)

instance Prelude.NFData ListOfferingsResponse where
  rnf ListOfferingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf offerings
      `Prelude.seq` Prelude.rnf httpStatus
