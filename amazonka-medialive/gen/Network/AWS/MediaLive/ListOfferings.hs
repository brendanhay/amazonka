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
-- Module      : Network.AWS.MediaLive.ListOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List offerings available for purchase.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListOfferings
  ( -- * Creating a Request
    ListOfferings (..),
    newListOfferings,

    -- * Request Lenses
    listOfferings_maximumFramerate,
    listOfferings_nextToken,
    listOfferings_videoQuality,
    listOfferings_duration,
    listOfferings_maxResults,
    listOfferings_codec,
    listOfferings_channelConfiguration,
    listOfferings_maximumBitrate,
    listOfferings_specialFeature,
    listOfferings_channelClass,
    listOfferings_resourceType,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListOfferingsRequest
--
-- /See:/ 'newListOfferings' smart constructor.
data ListOfferings = ListOfferings'
  { -- | Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
    maximumFramerate :: Core.Maybe Core.Text,
    nextToken :: Core.Maybe Core.Text,
    -- | Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
    videoQuality :: Core.Maybe Core.Text,
    -- | Filter by offering duration, e.g. \'12\'
    duration :: Core.Maybe Core.Text,
    maxResults :: Core.Maybe Core.Natural,
    -- | Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
    codec :: Core.Maybe Core.Text,
    -- | Filter to offerings that match the configuration of an existing channel,
    -- e.g. \'2345678\' (a channel ID)
    channelConfiguration :: Core.Maybe Core.Text,
    -- | Filter by bitrate, \'MAX_10_MBPS\', \'MAX_20_MBPS\', or \'MAX_50_MBPS\'
    maximumBitrate :: Core.Maybe Core.Text,
    -- | Filter by special feature, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
    specialFeature :: Core.Maybe Core.Text,
    -- | Filter by channel class, \'STANDARD\' or \'SINGLE_PIPELINE\'
    channelClass :: Core.Maybe Core.Text,
    -- | Filter by resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or
    -- \'CHANNEL\'
    resourceType :: Core.Maybe Core.Text,
    -- | Filter by resolution, \'SD\', \'HD\', \'FHD\', or \'UHD\'
    resolution :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumFramerate', 'listOfferings_maximumFramerate' - Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
--
-- 'nextToken', 'listOfferings_nextToken' - Undocumented member.
--
-- 'videoQuality', 'listOfferings_videoQuality' - Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
--
-- 'duration', 'listOfferings_duration' - Filter by offering duration, e.g. \'12\'
--
-- 'maxResults', 'listOfferings_maxResults' - Undocumented member.
--
-- 'codec', 'listOfferings_codec' - Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
--
-- 'channelConfiguration', 'listOfferings_channelConfiguration' - Filter to offerings that match the configuration of an existing channel,
-- e.g. \'2345678\' (a channel ID)
--
-- 'maximumBitrate', 'listOfferings_maximumBitrate' - Filter by bitrate, \'MAX_10_MBPS\', \'MAX_20_MBPS\', or \'MAX_50_MBPS\'
--
-- 'specialFeature', 'listOfferings_specialFeature' - Filter by special feature, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
--
-- 'channelClass', 'listOfferings_channelClass' - Filter by channel class, \'STANDARD\' or \'SINGLE_PIPELINE\'
--
-- 'resourceType', 'listOfferings_resourceType' - Filter by resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or
-- \'CHANNEL\'
--
-- 'resolution', 'listOfferings_resolution' - Filter by resolution, \'SD\', \'HD\', \'FHD\', or \'UHD\'
newListOfferings ::
  ListOfferings
newListOfferings =
  ListOfferings'
    { maximumFramerate = Core.Nothing,
      nextToken = Core.Nothing,
      videoQuality = Core.Nothing,
      duration = Core.Nothing,
      maxResults = Core.Nothing,
      codec = Core.Nothing,
      channelConfiguration = Core.Nothing,
      maximumBitrate = Core.Nothing,
      specialFeature = Core.Nothing,
      channelClass = Core.Nothing,
      resourceType = Core.Nothing,
      resolution = Core.Nothing
    }

-- | Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
listOfferings_maximumFramerate :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_maximumFramerate = Lens.lens (\ListOfferings' {maximumFramerate} -> maximumFramerate) (\s@ListOfferings' {} a -> s {maximumFramerate = a} :: ListOfferings)

-- | Undocumented member.
listOfferings_nextToken :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_nextToken = Lens.lens (\ListOfferings' {nextToken} -> nextToken) (\s@ListOfferings' {} a -> s {nextToken = a} :: ListOfferings)

-- | Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
listOfferings_videoQuality :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_videoQuality = Lens.lens (\ListOfferings' {videoQuality} -> videoQuality) (\s@ListOfferings' {} a -> s {videoQuality = a} :: ListOfferings)

-- | Filter by offering duration, e.g. \'12\'
listOfferings_duration :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_duration = Lens.lens (\ListOfferings' {duration} -> duration) (\s@ListOfferings' {} a -> s {duration = a} :: ListOfferings)

-- | Undocumented member.
listOfferings_maxResults :: Lens.Lens' ListOfferings (Core.Maybe Core.Natural)
listOfferings_maxResults = Lens.lens (\ListOfferings' {maxResults} -> maxResults) (\s@ListOfferings' {} a -> s {maxResults = a} :: ListOfferings)

-- | Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
listOfferings_codec :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_codec = Lens.lens (\ListOfferings' {codec} -> codec) (\s@ListOfferings' {} a -> s {codec = a} :: ListOfferings)

-- | Filter to offerings that match the configuration of an existing channel,
-- e.g. \'2345678\' (a channel ID)
listOfferings_channelConfiguration :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_channelConfiguration = Lens.lens (\ListOfferings' {channelConfiguration} -> channelConfiguration) (\s@ListOfferings' {} a -> s {channelConfiguration = a} :: ListOfferings)

-- | Filter by bitrate, \'MAX_10_MBPS\', \'MAX_20_MBPS\', or \'MAX_50_MBPS\'
listOfferings_maximumBitrate :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_maximumBitrate = Lens.lens (\ListOfferings' {maximumBitrate} -> maximumBitrate) (\s@ListOfferings' {} a -> s {maximumBitrate = a} :: ListOfferings)

-- | Filter by special feature, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
listOfferings_specialFeature :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_specialFeature = Lens.lens (\ListOfferings' {specialFeature} -> specialFeature) (\s@ListOfferings' {} a -> s {specialFeature = a} :: ListOfferings)

-- | Filter by channel class, \'STANDARD\' or \'SINGLE_PIPELINE\'
listOfferings_channelClass :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_channelClass = Lens.lens (\ListOfferings' {channelClass} -> channelClass) (\s@ListOfferings' {} a -> s {channelClass = a} :: ListOfferings)

-- | Filter by resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or
-- \'CHANNEL\'
listOfferings_resourceType :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_resourceType = Lens.lens (\ListOfferings' {resourceType} -> resourceType) (\s@ListOfferings' {} a -> s {resourceType = a} :: ListOfferings)

-- | Filter by resolution, \'SD\', \'HD\', \'FHD\', or \'UHD\'
listOfferings_resolution :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_resolution = Lens.lens (\ListOfferings' {resolution} -> resolution) (\s@ListOfferings' {} a -> s {resolution = a} :: ListOfferings)

instance Core.AWSPager ListOfferings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOfferingsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOfferingsResponse_offerings Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOfferings_nextToken
          Lens..~ rs
          Lens.^? listOfferingsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListOfferings where
  type
    AWSResponse ListOfferings =
      ListOfferingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOfferingsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "offerings" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOfferings

instance Core.NFData ListOfferings

instance Core.ToHeaders ListOfferings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListOfferings where
  toPath = Core.const "/prod/offerings"

instance Core.ToQuery ListOfferings where
  toQuery ListOfferings' {..} =
    Core.mconcat
      [ "maximumFramerate" Core.=: maximumFramerate,
        "nextToken" Core.=: nextToken,
        "videoQuality" Core.=: videoQuality,
        "duration" Core.=: duration,
        "maxResults" Core.=: maxResults,
        "codec" Core.=: codec,
        "channelConfiguration" Core.=: channelConfiguration,
        "maximumBitrate" Core.=: maximumBitrate,
        "specialFeature" Core.=: specialFeature,
        "channelClass" Core.=: channelClass,
        "resourceType" Core.=: resourceType,
        "resolution" Core.=: resolution
      ]

-- | Placeholder documentation for ListOfferingsResponse
--
-- /See:/ 'newListOfferingsResponse' smart constructor.
data ListOfferingsResponse = ListOfferingsResponse'
  { -- | Token to retrieve the next page of results
    nextToken :: Core.Maybe Core.Text,
    -- | List of offerings
    offerings :: Core.Maybe [Offering],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListOfferingsResponse
newListOfferingsResponse pHttpStatus_ =
  ListOfferingsResponse'
    { nextToken = Core.Nothing,
      offerings = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token to retrieve the next page of results
listOfferingsResponse_nextToken :: Lens.Lens' ListOfferingsResponse (Core.Maybe Core.Text)
listOfferingsResponse_nextToken = Lens.lens (\ListOfferingsResponse' {nextToken} -> nextToken) (\s@ListOfferingsResponse' {} a -> s {nextToken = a} :: ListOfferingsResponse)

-- | List of offerings
listOfferingsResponse_offerings :: Lens.Lens' ListOfferingsResponse (Core.Maybe [Offering])
listOfferingsResponse_offerings = Lens.lens (\ListOfferingsResponse' {offerings} -> offerings) (\s@ListOfferingsResponse' {} a -> s {offerings = a} :: ListOfferingsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOfferingsResponse_httpStatus :: Lens.Lens' ListOfferingsResponse Core.Int
listOfferingsResponse_httpStatus = Lens.lens (\ListOfferingsResponse' {httpStatus} -> httpStatus) (\s@ListOfferingsResponse' {} a -> s {httpStatus = a} :: ListOfferingsResponse)

instance Core.NFData ListOfferingsResponse
