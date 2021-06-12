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
-- Module      : Network.AWS.MediaLive.ListReservations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List purchased reservations.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListReservations
  ( -- * Creating a Request
    ListReservations (..),
    newListReservations,

    -- * Request Lenses
    listReservations_maximumFramerate,
    listReservations_nextToken,
    listReservations_videoQuality,
    listReservations_maxResults,
    listReservations_codec,
    listReservations_maximumBitrate,
    listReservations_specialFeature,
    listReservations_channelClass,
    listReservations_resourceType,
    listReservations_resolution,

    -- * Destructuring the Response
    ListReservationsResponse (..),
    newListReservationsResponse,

    -- * Response Lenses
    listReservationsResponse_nextToken,
    listReservationsResponse_reservations,
    listReservationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListReservationsRequest
--
-- /See:/ 'newListReservations' smart constructor.
data ListReservations = ListReservations'
  { -- | Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
    maximumFramerate :: Core.Maybe Core.Text,
    nextToken :: Core.Maybe Core.Text,
    -- | Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
    videoQuality :: Core.Maybe Core.Text,
    maxResults :: Core.Maybe Core.Natural,
    -- | Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
    codec :: Core.Maybe Core.Text,
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
-- Create a value of 'ListReservations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumFramerate', 'listReservations_maximumFramerate' - Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
--
-- 'nextToken', 'listReservations_nextToken' - Undocumented member.
--
-- 'videoQuality', 'listReservations_videoQuality' - Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
--
-- 'maxResults', 'listReservations_maxResults' - Undocumented member.
--
-- 'codec', 'listReservations_codec' - Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
--
-- 'maximumBitrate', 'listReservations_maximumBitrate' - Filter by bitrate, \'MAX_10_MBPS\', \'MAX_20_MBPS\', or \'MAX_50_MBPS\'
--
-- 'specialFeature', 'listReservations_specialFeature' - Filter by special feature, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
--
-- 'channelClass', 'listReservations_channelClass' - Filter by channel class, \'STANDARD\' or \'SINGLE_PIPELINE\'
--
-- 'resourceType', 'listReservations_resourceType' - Filter by resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or
-- \'CHANNEL\'
--
-- 'resolution', 'listReservations_resolution' - Filter by resolution, \'SD\', \'HD\', \'FHD\', or \'UHD\'
newListReservations ::
  ListReservations
newListReservations =
  ListReservations'
    { maximumFramerate = Core.Nothing,
      nextToken = Core.Nothing,
      videoQuality = Core.Nothing,
      maxResults = Core.Nothing,
      codec = Core.Nothing,
      maximumBitrate = Core.Nothing,
      specialFeature = Core.Nothing,
      channelClass = Core.Nothing,
      resourceType = Core.Nothing,
      resolution = Core.Nothing
    }

-- | Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
listReservations_maximumFramerate :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
listReservations_maximumFramerate = Lens.lens (\ListReservations' {maximumFramerate} -> maximumFramerate) (\s@ListReservations' {} a -> s {maximumFramerate = a} :: ListReservations)

-- | Undocumented member.
listReservations_nextToken :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
listReservations_nextToken = Lens.lens (\ListReservations' {nextToken} -> nextToken) (\s@ListReservations' {} a -> s {nextToken = a} :: ListReservations)

-- | Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
listReservations_videoQuality :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
listReservations_videoQuality = Lens.lens (\ListReservations' {videoQuality} -> videoQuality) (\s@ListReservations' {} a -> s {videoQuality = a} :: ListReservations)

-- | Undocumented member.
listReservations_maxResults :: Lens.Lens' ListReservations (Core.Maybe Core.Natural)
listReservations_maxResults = Lens.lens (\ListReservations' {maxResults} -> maxResults) (\s@ListReservations' {} a -> s {maxResults = a} :: ListReservations)

-- | Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
listReservations_codec :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
listReservations_codec = Lens.lens (\ListReservations' {codec} -> codec) (\s@ListReservations' {} a -> s {codec = a} :: ListReservations)

-- | Filter by bitrate, \'MAX_10_MBPS\', \'MAX_20_MBPS\', or \'MAX_50_MBPS\'
listReservations_maximumBitrate :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
listReservations_maximumBitrate = Lens.lens (\ListReservations' {maximumBitrate} -> maximumBitrate) (\s@ListReservations' {} a -> s {maximumBitrate = a} :: ListReservations)

-- | Filter by special feature, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
listReservations_specialFeature :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
listReservations_specialFeature = Lens.lens (\ListReservations' {specialFeature} -> specialFeature) (\s@ListReservations' {} a -> s {specialFeature = a} :: ListReservations)

-- | Filter by channel class, \'STANDARD\' or \'SINGLE_PIPELINE\'
listReservations_channelClass :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
listReservations_channelClass = Lens.lens (\ListReservations' {channelClass} -> channelClass) (\s@ListReservations' {} a -> s {channelClass = a} :: ListReservations)

-- | Filter by resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or
-- \'CHANNEL\'
listReservations_resourceType :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
listReservations_resourceType = Lens.lens (\ListReservations' {resourceType} -> resourceType) (\s@ListReservations' {} a -> s {resourceType = a} :: ListReservations)

-- | Filter by resolution, \'SD\', \'HD\', \'FHD\', or \'UHD\'
listReservations_resolution :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
listReservations_resolution = Lens.lens (\ListReservations' {resolution} -> resolution) (\s@ListReservations' {} a -> s {resolution = a} :: ListReservations)

instance Core.AWSPager ListReservations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReservationsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listReservationsResponse_reservations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listReservations_nextToken
          Lens..~ rs
          Lens.^? listReservationsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListReservations where
  type
    AWSResponse ListReservations =
      ListReservationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReservationsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "reservations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListReservations

instance Core.NFData ListReservations

instance Core.ToHeaders ListReservations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListReservations where
  toPath = Core.const "/prod/reservations"

instance Core.ToQuery ListReservations where
  toQuery ListReservations' {..} =
    Core.mconcat
      [ "maximumFramerate" Core.=: maximumFramerate,
        "nextToken" Core.=: nextToken,
        "videoQuality" Core.=: videoQuality,
        "maxResults" Core.=: maxResults,
        "codec" Core.=: codec,
        "maximumBitrate" Core.=: maximumBitrate,
        "specialFeature" Core.=: specialFeature,
        "channelClass" Core.=: channelClass,
        "resourceType" Core.=: resourceType,
        "resolution" Core.=: resolution
      ]

-- | Placeholder documentation for ListReservationsResponse
--
-- /See:/ 'newListReservationsResponse' smart constructor.
data ListReservationsResponse = ListReservationsResponse'
  { -- | Token to retrieve the next page of results
    nextToken :: Core.Maybe Core.Text,
    -- | List of reservations
    reservations :: Core.Maybe [Reservation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReservationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReservationsResponse_nextToken' - Token to retrieve the next page of results
--
-- 'reservations', 'listReservationsResponse_reservations' - List of reservations
--
-- 'httpStatus', 'listReservationsResponse_httpStatus' - The response's http status code.
newListReservationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListReservationsResponse
newListReservationsResponse pHttpStatus_ =
  ListReservationsResponse'
    { nextToken = Core.Nothing,
      reservations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token to retrieve the next page of results
listReservationsResponse_nextToken :: Lens.Lens' ListReservationsResponse (Core.Maybe Core.Text)
listReservationsResponse_nextToken = Lens.lens (\ListReservationsResponse' {nextToken} -> nextToken) (\s@ListReservationsResponse' {} a -> s {nextToken = a} :: ListReservationsResponse)

-- | List of reservations
listReservationsResponse_reservations :: Lens.Lens' ListReservationsResponse (Core.Maybe [Reservation])
listReservationsResponse_reservations = Lens.lens (\ListReservationsResponse' {reservations} -> reservations) (\s@ListReservationsResponse' {} a -> s {reservations = a} :: ListReservationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listReservationsResponse_httpStatus :: Lens.Lens' ListReservationsResponse Core.Int
listReservationsResponse_httpStatus = Lens.lens (\ListReservationsResponse' {httpStatus} -> httpStatus) (\s@ListReservationsResponse' {} a -> s {httpStatus = a} :: ListReservationsResponse)

instance Core.NFData ListReservationsResponse
