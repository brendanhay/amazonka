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
-- Module      : Amazonka.MediaLive.ListReservations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List purchased reservations.
--
-- This operation returns paginated results.
module Amazonka.MediaLive.ListReservations
  ( -- * Creating a Request
    ListReservations (..),
    newListReservations,

    -- * Request Lenses
    listReservations_videoQuality,
    listReservations_maximumFramerate,
    listReservations_resourceType,
    listReservations_resolution,
    listReservations_codec,
    listReservations_nextToken,
    listReservations_specialFeature,
    listReservations_channelClass,
    listReservations_maximumBitrate,
    listReservations_maxResults,

    -- * Destructuring the Response
    ListReservationsResponse (..),
    newListReservationsResponse,

    -- * Response Lenses
    listReservationsResponse_nextToken,
    listReservationsResponse_reservations,
    listReservationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for ListReservationsRequest
--
-- /See:/ 'newListReservations' smart constructor.
data ListReservations = ListReservations'
  { -- | Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
    videoQuality :: Prelude.Maybe Prelude.Text,
    -- | Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
    maximumFramerate :: Prelude.Maybe Prelude.Text,
    -- | Filter by resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or
    -- \'CHANNEL\'
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Filter by resolution, \'SD\', \'HD\', \'FHD\', or \'UHD\'
    resolution :: Prelude.Maybe Prelude.Text,
    -- | Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
    codec :: Prelude.Maybe Prelude.Text,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter by special feature, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
    specialFeature :: Prelude.Maybe Prelude.Text,
    -- | Filter by channel class, \'STANDARD\' or \'SINGLE_PIPELINE\'
    channelClass :: Prelude.Maybe Prelude.Text,
    -- | Filter by bitrate, \'MAX_10_MBPS\', \'MAX_20_MBPS\', or \'MAX_50_MBPS\'
    maximumBitrate :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReservations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'videoQuality', 'listReservations_videoQuality' - Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
--
-- 'maximumFramerate', 'listReservations_maximumFramerate' - Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
--
-- 'resourceType', 'listReservations_resourceType' - Filter by resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or
-- \'CHANNEL\'
--
-- 'resolution', 'listReservations_resolution' - Filter by resolution, \'SD\', \'HD\', \'FHD\', or \'UHD\'
--
-- 'codec', 'listReservations_codec' - Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
--
-- 'nextToken', 'listReservations_nextToken' - Undocumented member.
--
-- 'specialFeature', 'listReservations_specialFeature' - Filter by special feature, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
--
-- 'channelClass', 'listReservations_channelClass' - Filter by channel class, \'STANDARD\' or \'SINGLE_PIPELINE\'
--
-- 'maximumBitrate', 'listReservations_maximumBitrate' - Filter by bitrate, \'MAX_10_MBPS\', \'MAX_20_MBPS\', or \'MAX_50_MBPS\'
--
-- 'maxResults', 'listReservations_maxResults' - Undocumented member.
newListReservations ::
  ListReservations
newListReservations =
  ListReservations'
    { videoQuality = Prelude.Nothing,
      maximumFramerate = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resolution = Prelude.Nothing,
      codec = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      specialFeature = Prelude.Nothing,
      channelClass = Prelude.Nothing,
      maximumBitrate = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Filter by video quality, \'STANDARD\', \'ENHANCED\', or \'PREMIUM\'
listReservations_videoQuality :: Lens.Lens' ListReservations (Prelude.Maybe Prelude.Text)
listReservations_videoQuality = Lens.lens (\ListReservations' {videoQuality} -> videoQuality) (\s@ListReservations' {} a -> s {videoQuality = a} :: ListReservations)

-- | Filter by framerate, \'MAX_30_FPS\' or \'MAX_60_FPS\'
listReservations_maximumFramerate :: Lens.Lens' ListReservations (Prelude.Maybe Prelude.Text)
listReservations_maximumFramerate = Lens.lens (\ListReservations' {maximumFramerate} -> maximumFramerate) (\s@ListReservations' {} a -> s {maximumFramerate = a} :: ListReservations)

-- | Filter by resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or
-- \'CHANNEL\'
listReservations_resourceType :: Lens.Lens' ListReservations (Prelude.Maybe Prelude.Text)
listReservations_resourceType = Lens.lens (\ListReservations' {resourceType} -> resourceType) (\s@ListReservations' {} a -> s {resourceType = a} :: ListReservations)

-- | Filter by resolution, \'SD\', \'HD\', \'FHD\', or \'UHD\'
listReservations_resolution :: Lens.Lens' ListReservations (Prelude.Maybe Prelude.Text)
listReservations_resolution = Lens.lens (\ListReservations' {resolution} -> resolution) (\s@ListReservations' {} a -> s {resolution = a} :: ListReservations)

-- | Filter by codec, \'AVC\', \'HEVC\', \'MPEG2\', \'AUDIO\', or \'LINK\'
listReservations_codec :: Lens.Lens' ListReservations (Prelude.Maybe Prelude.Text)
listReservations_codec = Lens.lens (\ListReservations' {codec} -> codec) (\s@ListReservations' {} a -> s {codec = a} :: ListReservations)

-- | Undocumented member.
listReservations_nextToken :: Lens.Lens' ListReservations (Prelude.Maybe Prelude.Text)
listReservations_nextToken = Lens.lens (\ListReservations' {nextToken} -> nextToken) (\s@ListReservations' {} a -> s {nextToken = a} :: ListReservations)

-- | Filter by special feature, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
listReservations_specialFeature :: Lens.Lens' ListReservations (Prelude.Maybe Prelude.Text)
listReservations_specialFeature = Lens.lens (\ListReservations' {specialFeature} -> specialFeature) (\s@ListReservations' {} a -> s {specialFeature = a} :: ListReservations)

-- | Filter by channel class, \'STANDARD\' or \'SINGLE_PIPELINE\'
listReservations_channelClass :: Lens.Lens' ListReservations (Prelude.Maybe Prelude.Text)
listReservations_channelClass = Lens.lens (\ListReservations' {channelClass} -> channelClass) (\s@ListReservations' {} a -> s {channelClass = a} :: ListReservations)

-- | Filter by bitrate, \'MAX_10_MBPS\', \'MAX_20_MBPS\', or \'MAX_50_MBPS\'
listReservations_maximumBitrate :: Lens.Lens' ListReservations (Prelude.Maybe Prelude.Text)
listReservations_maximumBitrate = Lens.lens (\ListReservations' {maximumBitrate} -> maximumBitrate) (\s@ListReservations' {} a -> s {maximumBitrate = a} :: ListReservations)

-- | Undocumented member.
listReservations_maxResults :: Lens.Lens' ListReservations (Prelude.Maybe Prelude.Natural)
listReservations_maxResults = Lens.lens (\ListReservations' {maxResults} -> maxResults) (\s@ListReservations' {} a -> s {maxResults = a} :: ListReservations)

instance Core.AWSPager ListReservations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReservationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReservationsResponse_reservations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listReservations_nextToken
          Lens..~ rs
          Lens.^? listReservationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListReservations where
  type
    AWSResponse ListReservations =
      ListReservationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReservationsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "reservations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReservations

instance Prelude.NFData ListReservations

instance Core.ToHeaders ListReservations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListReservations where
  toPath = Prelude.const "/prod/reservations"

instance Core.ToQuery ListReservations where
  toQuery ListReservations' {..} =
    Prelude.mconcat
      [ "videoQuality" Core.=: videoQuality,
        "maximumFramerate" Core.=: maximumFramerate,
        "resourceType" Core.=: resourceType,
        "resolution" Core.=: resolution,
        "codec" Core.=: codec,
        "nextToken" Core.=: nextToken,
        "specialFeature" Core.=: specialFeature,
        "channelClass" Core.=: channelClass,
        "maximumBitrate" Core.=: maximumBitrate,
        "maxResults" Core.=: maxResults
      ]

-- | Placeholder documentation for ListReservationsResponse
--
-- /See:/ 'newListReservationsResponse' smart constructor.
data ListReservationsResponse = ListReservationsResponse'
  { -- | Token to retrieve the next page of results
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of reservations
    reservations :: Prelude.Maybe [Reservation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListReservationsResponse
newListReservationsResponse pHttpStatus_ =
  ListReservationsResponse'
    { nextToken =
        Prelude.Nothing,
      reservations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token to retrieve the next page of results
listReservationsResponse_nextToken :: Lens.Lens' ListReservationsResponse (Prelude.Maybe Prelude.Text)
listReservationsResponse_nextToken = Lens.lens (\ListReservationsResponse' {nextToken} -> nextToken) (\s@ListReservationsResponse' {} a -> s {nextToken = a} :: ListReservationsResponse)

-- | List of reservations
listReservationsResponse_reservations :: Lens.Lens' ListReservationsResponse (Prelude.Maybe [Reservation])
listReservationsResponse_reservations = Lens.lens (\ListReservationsResponse' {reservations} -> reservations) (\s@ListReservationsResponse' {} a -> s {reservations = a} :: ListReservationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReservationsResponse_httpStatus :: Lens.Lens' ListReservationsResponse Prelude.Int
listReservationsResponse_httpStatus = Lens.lens (\ListReservationsResponse' {httpStatus} -> httpStatus) (\s@ListReservationsResponse' {} a -> s {httpStatus = a} :: ListReservationsResponse)

instance Prelude.NFData ListReservationsResponse
