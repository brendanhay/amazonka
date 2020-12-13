{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListReservations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List purchased reservations.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListReservations
  ( -- * Creating a request
    ListReservations (..),
    mkListReservations,

    -- ** Request lenses
    lrVideoQuality,
    lrMaximumFramerate,
    lrResourceType,
    lrResolution,
    lrCodec,
    lrNextToken,
    lrSpecialFeature,
    lrChannelClass,
    lrMaximumBitrate,
    lrMaxResults,

    -- * Destructuring the response
    ListReservationsResponse (..),
    mkListReservationsResponse,

    -- ** Response lenses
    lrrsNextToken,
    lrrsReservations,
    lrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for ListReservationsRequest
--
-- /See:/ 'mkListReservations' smart constructor.
data ListReservations = ListReservations'
  { -- | Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
    videoQuality :: Lude.Maybe Lude.Text,
    -- | Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
    maximumFramerate :: Lude.Maybe Lude.Text,
    -- | Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
    resourceType :: Lude.Maybe Lude.Text,
    -- | Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
    resolution :: Lude.Maybe Lude.Text,
    -- | Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
    codec :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    -- | Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
    specialFeature :: Lude.Maybe Lude.Text,
    -- | Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
    channelClass :: Lude.Maybe Lude.Text,
    -- | Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
    maximumBitrate :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReservations' with the minimum fields required to make a request.
--
-- * 'videoQuality' - Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
-- * 'maximumFramerate' - Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
-- * 'resourceType' - Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
-- * 'resolution' - Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
-- * 'codec' - Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
-- * 'nextToken' -
-- * 'specialFeature' - Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
-- * 'channelClass' - Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
-- * 'maximumBitrate' - Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
-- * 'maxResults' -
mkListReservations ::
  ListReservations
mkListReservations =
  ListReservations'
    { videoQuality = Lude.Nothing,
      maximumFramerate = Lude.Nothing,
      resourceType = Lude.Nothing,
      resolution = Lude.Nothing,
      codec = Lude.Nothing,
      nextToken = Lude.Nothing,
      specialFeature = Lude.Nothing,
      channelClass = Lude.Nothing,
      maximumBitrate = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
--
-- /Note:/ Consider using 'videoQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrVideoQuality :: Lens.Lens' ListReservations (Lude.Maybe Lude.Text)
lrVideoQuality = Lens.lens (videoQuality :: ListReservations -> Lude.Maybe Lude.Text) (\s a -> s {videoQuality = a} :: ListReservations)
{-# DEPRECATED lrVideoQuality "Use generic-lens or generic-optics with 'videoQuality' instead." #-}

-- | Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
--
-- /Note:/ Consider using 'maximumFramerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaximumFramerate :: Lens.Lens' ListReservations (Lude.Maybe Lude.Text)
lrMaximumFramerate = Lens.lens (maximumFramerate :: ListReservations -> Lude.Maybe Lude.Text) (\s a -> s {maximumFramerate = a} :: ListReservations)
{-# DEPRECATED lrMaximumFramerate "Use generic-lens or generic-optics with 'maximumFramerate' instead." #-}

-- | Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrResourceType :: Lens.Lens' ListReservations (Lude.Maybe Lude.Text)
lrResourceType = Lens.lens (resourceType :: ListReservations -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ListReservations)
{-# DEPRECATED lrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrResolution :: Lens.Lens' ListReservations (Lude.Maybe Lude.Text)
lrResolution = Lens.lens (resolution :: ListReservations -> Lude.Maybe Lude.Text) (\s a -> s {resolution = a} :: ListReservations)
{-# DEPRECATED lrResolution "Use generic-lens or generic-optics with 'resolution' instead." #-}

-- | Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrCodec :: Lens.Lens' ListReservations (Lude.Maybe Lude.Text)
lrCodec = Lens.lens (codec :: ListReservations -> Lude.Maybe Lude.Text) (\s a -> s {codec = a} :: ListReservations)
{-# DEPRECATED lrCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListReservations (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListReservations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReservations)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
--
-- /Note:/ Consider using 'specialFeature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrSpecialFeature :: Lens.Lens' ListReservations (Lude.Maybe Lude.Text)
lrSpecialFeature = Lens.lens (specialFeature :: ListReservations -> Lude.Maybe Lude.Text) (\s a -> s {specialFeature = a} :: ListReservations)
{-# DEPRECATED lrSpecialFeature "Use generic-lens or generic-optics with 'specialFeature' instead." #-}

-- | Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrChannelClass :: Lens.Lens' ListReservations (Lude.Maybe Lude.Text)
lrChannelClass = Lens.lens (channelClass :: ListReservations -> Lude.Maybe Lude.Text) (\s a -> s {channelClass = a} :: ListReservations)
{-# DEPRECATED lrChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
--
-- /Note:/ Consider using 'maximumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaximumBitrate :: Lens.Lens' ListReservations (Lude.Maybe Lude.Text)
lrMaximumBitrate = Lens.lens (maximumBitrate :: ListReservations -> Lude.Maybe Lude.Text) (\s a -> s {maximumBitrate = a} :: ListReservations)
{-# DEPRECATED lrMaximumBitrate "Use generic-lens or generic-optics with 'maximumBitrate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListReservations (Lude.Maybe Lude.Natural)
lrMaxResults = Lens.lens (maxResults :: ListReservations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListReservations)
{-# DEPRECATED lrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListReservations where
  page rq rs
    | Page.stop (rs Lens.^. lrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrrsReservations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrNextToken Lens..~ rs Lens.^. lrrsNextToken

instance Lude.AWSRequest ListReservations where
  type Rs ListReservations = ListReservationsResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListReservationsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "reservations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListReservations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListReservations where
  toPath = Lude.const "/prod/reservations"

instance Lude.ToQuery ListReservations where
  toQuery ListReservations' {..} =
    Lude.mconcat
      [ "videoQuality" Lude.=: videoQuality,
        "maximumFramerate" Lude.=: maximumFramerate,
        "resourceType" Lude.=: resourceType,
        "resolution" Lude.=: resolution,
        "codec" Lude.=: codec,
        "nextToken" Lude.=: nextToken,
        "specialFeature" Lude.=: specialFeature,
        "channelClass" Lude.=: channelClass,
        "maximumBitrate" Lude.=: maximumBitrate,
        "maxResults" Lude.=: maxResults
      ]

-- | Placeholder documentation for ListReservationsResponse
--
-- /See:/ 'mkListReservationsResponse' smart constructor.
data ListReservationsResponse = ListReservationsResponse'
  { -- | Token to retrieve the next page of results
    nextToken :: Lude.Maybe Lude.Text,
    -- | List of reservations
    reservations :: Lude.Maybe [Reservation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReservationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token to retrieve the next page of results
-- * 'reservations' - List of reservations
-- * 'responseStatus' - The response status code.
mkListReservationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListReservationsResponse
mkListReservationsResponse pResponseStatus_ =
  ListReservationsResponse'
    { nextToken = Lude.Nothing,
      reservations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token to retrieve the next page of results
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListReservationsResponse (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListReservationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReservationsResponse)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of reservations
--
-- /Note:/ Consider using 'reservations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsReservations :: Lens.Lens' ListReservationsResponse (Lude.Maybe [Reservation])
lrrsReservations = Lens.lens (reservations :: ListReservationsResponse -> Lude.Maybe [Reservation]) (\s a -> s {reservations = a} :: ListReservationsResponse)
{-# DEPRECATED lrrsReservations "Use generic-lens or generic-optics with 'reservations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListReservationsResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListReservationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListReservationsResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
