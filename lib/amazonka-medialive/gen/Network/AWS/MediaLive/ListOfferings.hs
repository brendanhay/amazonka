{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List offerings available for purchase.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListOfferings
  ( -- * Creating a request
    ListOfferings (..),
    mkListOfferings,

    -- ** Request lenses
    loVideoQuality,
    loMaximumFramerate,
    loResourceType,
    loChannelConfiguration,
    loResolution,
    loCodec,
    loNextToken,
    loSpecialFeature,
    loChannelClass,
    loMaximumBitrate,
    loDuration,
    loMaxResults,

    -- * Destructuring the response
    ListOfferingsResponse (..),
    mkListOfferingsResponse,

    -- ** Response lenses
    lorsNextToken,
    lorsOfferings,
    lorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for ListOfferingsRequest
--
-- /See:/ 'mkListOfferings' smart constructor.
data ListOfferings = ListOfferings'
  { videoQuality ::
      Lude.Maybe Lude.Text,
    maximumFramerate :: Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe Lude.Text,
    channelConfiguration :: Lude.Maybe Lude.Text,
    resolution :: Lude.Maybe Lude.Text,
    codec :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    specialFeature :: Lude.Maybe Lude.Text,
    channelClass :: Lude.Maybe Lude.Text,
    maximumBitrate :: Lude.Maybe Lude.Text,
    duration :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOfferings' with the minimum fields required to make a request.
--
-- * 'channelClass' - Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
-- * 'channelConfiguration' - Filter to offerings that match the configuration of an existing channel, e.g. '2345678' (a channel ID)
-- * 'codec' - Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
-- * 'duration' - Filter by offering duration, e.g. '12'
-- * 'maxResults' - Undocumented field.
-- * 'maximumBitrate' - Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
-- * 'maximumFramerate' - Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
-- * 'nextToken' - Undocumented field.
-- * 'resolution' - Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
-- * 'resourceType' - Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
-- * 'specialFeature' - Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
-- * 'videoQuality' - Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
mkListOfferings ::
  ListOfferings
mkListOfferings =
  ListOfferings'
    { videoQuality = Lude.Nothing,
      maximumFramerate = Lude.Nothing,
      resourceType = Lude.Nothing,
      channelConfiguration = Lude.Nothing,
      resolution = Lude.Nothing,
      codec = Lude.Nothing,
      nextToken = Lude.Nothing,
      specialFeature = Lude.Nothing,
      channelClass = Lude.Nothing,
      maximumBitrate = Lude.Nothing,
      duration = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
--
-- /Note:/ Consider using 'videoQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loVideoQuality :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loVideoQuality = Lens.lens (videoQuality :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {videoQuality = a} :: ListOfferings)
{-# DEPRECATED loVideoQuality "Use generic-lens or generic-optics with 'videoQuality' instead." #-}

-- | Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
--
-- /Note:/ Consider using 'maximumFramerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaximumFramerate :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loMaximumFramerate = Lens.lens (maximumFramerate :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {maximumFramerate = a} :: ListOfferings)
{-# DEPRECATED loMaximumFramerate "Use generic-lens or generic-optics with 'maximumFramerate' instead." #-}

-- | Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loResourceType :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loResourceType = Lens.lens (resourceType :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ListOfferings)
{-# DEPRECATED loResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Filter to offerings that match the configuration of an existing channel, e.g. '2345678' (a channel ID)
--
-- /Note:/ Consider using 'channelConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loChannelConfiguration :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loChannelConfiguration = Lens.lens (channelConfiguration :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {channelConfiguration = a} :: ListOfferings)
{-# DEPRECATED loChannelConfiguration "Use generic-lens or generic-optics with 'channelConfiguration' instead." #-}

-- | Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loResolution :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loResolution = Lens.lens (resolution :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {resolution = a} :: ListOfferings)
{-# DEPRECATED loResolution "Use generic-lens or generic-optics with 'resolution' instead." #-}

-- | Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loCodec :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loCodec = Lens.lens (codec :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {codec = a} :: ListOfferings)
{-# DEPRECATED loCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loNextToken :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loNextToken = Lens.lens (nextToken :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOfferings)
{-# DEPRECATED loNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
--
-- /Note:/ Consider using 'specialFeature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSpecialFeature :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loSpecialFeature = Lens.lens (specialFeature :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {specialFeature = a} :: ListOfferings)
{-# DEPRECATED loSpecialFeature "Use generic-lens or generic-optics with 'specialFeature' instead." #-}

-- | Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loChannelClass :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loChannelClass = Lens.lens (channelClass :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {channelClass = a} :: ListOfferings)
{-# DEPRECATED loChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
--
-- /Note:/ Consider using 'maximumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaximumBitrate :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loMaximumBitrate = Lens.lens (maximumBitrate :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {maximumBitrate = a} :: ListOfferings)
{-# DEPRECATED loMaximumBitrate "Use generic-lens or generic-optics with 'maximumBitrate' instead." #-}

-- | Filter by offering duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loDuration :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loDuration = Lens.lens (duration :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {duration = a} :: ListOfferings)
{-# DEPRECATED loDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaxResults :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Natural)
loMaxResults = Lens.lens (maxResults :: ListOfferings -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListOfferings)
{-# DEPRECATED loMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListOfferings where
  page rq rs
    | Page.stop (rs Lens.^. lorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lorsOfferings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loNextToken Lens..~ rs Lens.^. lorsNextToken

instance Lude.AWSRequest ListOfferings where
  type Rs ListOfferings = ListOfferingsResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOfferingsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "offerings" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOfferings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListOfferings where
  toPath = Lude.const "/prod/offerings"

instance Lude.ToQuery ListOfferings where
  toQuery ListOfferings' {..} =
    Lude.mconcat
      [ "videoQuality" Lude.=: videoQuality,
        "maximumFramerate" Lude.=: maximumFramerate,
        "resourceType" Lude.=: resourceType,
        "channelConfiguration" Lude.=: channelConfiguration,
        "resolution" Lude.=: resolution,
        "codec" Lude.=: codec,
        "nextToken" Lude.=: nextToken,
        "specialFeature" Lude.=: specialFeature,
        "channelClass" Lude.=: channelClass,
        "maximumBitrate" Lude.=: maximumBitrate,
        "duration" Lude.=: duration,
        "maxResults" Lude.=: maxResults
      ]

-- | Placeholder documentation for ListOfferingsResponse
--
-- /See:/ 'mkListOfferingsResponse' smart constructor.
data ListOfferingsResponse = ListOfferingsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    offerings :: Lude.Maybe [Offering],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOfferingsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token to retrieve the next page of results
-- * 'offerings' - List of offerings
-- * 'responseStatus' - The response status code.
mkListOfferingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOfferingsResponse
mkListOfferingsResponse pResponseStatus_ =
  ListOfferingsResponse'
    { nextToken = Lude.Nothing,
      offerings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token to retrieve the next page of results
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsNextToken :: Lens.Lens' ListOfferingsResponse (Lude.Maybe Lude.Text)
lorsNextToken = Lens.lens (nextToken :: ListOfferingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOfferingsResponse)
{-# DEPRECATED lorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of offerings
--
-- /Note:/ Consider using 'offerings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsOfferings :: Lens.Lens' ListOfferingsResponse (Lude.Maybe [Offering])
lorsOfferings = Lens.lens (offerings :: ListOfferingsResponse -> Lude.Maybe [Offering]) (\s a -> s {offerings = a} :: ListOfferingsResponse)
{-# DEPRECATED lorsOfferings "Use generic-lens or generic-optics with 'offerings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsResponseStatus :: Lens.Lens' ListOfferingsResponse Lude.Int
lorsResponseStatus = Lens.lens (responseStatus :: ListOfferingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOfferingsResponse)
{-# DEPRECATED lorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
