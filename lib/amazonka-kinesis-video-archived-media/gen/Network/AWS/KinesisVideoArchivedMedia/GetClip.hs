{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.GetClip
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads an MP4 file (clip) containing the archived, on-demand media from the specified video stream over the specified time range.
--
-- Both the StreamName and the StreamARN parameters are optional, but you must specify either the StreamName or the StreamARN when invoking this API operation.
-- As a prerequsite to using GetCLip API, you must obtain an endpoint using @GetDataEndpoint@ , specifying GET_CLIP forthe @APIName@ parameter.
-- An Amazon Kinesis video stream has the following requirements for providing data through MP4:
--
--     * The media must contain h.264 or h.265 encoded video and, optionally, AAC or G.711 encoded audio. Specifically, the codec ID of track 1 should be @V_MPEG/ISO/AVC@ (for h.264) or V_MPEGH/ISO/HEVC (for H.265). Optionally, the codec ID of track 2 should be @A_AAC@ (for AAC) or A_MS/ACM (for G.711).
--
--
--     * Data retention must be greater than 0.
--
--
--     * The video track of each fragment must contain codec private data in the Advanced Video Coding (AVC) for H.264 format and HEVC for H.265 format. For more information, see <https://www.iso.org/standard/55980.html MPEG-4 specification ISO/IEC 14496-15> . For information about adapting stream data to a given format, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/producer-reference-nal.html NAL Adaptation Flags> .
--
--
--     * The audio track (if present) of each fragment must contain codec private data in the AAC format (<https://www.iso.org/standard/43345.html AAC specification ISO/IEC 13818-7> ) or the <http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html MS Wave format> .
--
--
-- You can monitor the amount of outgoing data by monitoring the @GetClip.OutgoingBytes@ Amazon CloudWatch metric. For information about using CloudWatch to monitor Kinesis Video Streams, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/monitoring.html Monitoring Kinesis Video Streams> . For pricing information, see <https://aws.amazon.com/kinesis/video-streams/pricing/ Amazon Kinesis Video Streams Pricing> and <https://aws.amazon.com/pricing/ AWS Pricing> . Charges for outgoing AWS data apply.
module Network.AWS.KinesisVideoArchivedMedia.GetClip
  ( -- * Creating a request
    GetClip (..),
    mkGetClip,

    -- ** Request lenses
    gcClipFragmentSelector,
    gcStreamARN,
    gcStreamName,

    -- * Destructuring the response
    GetClipResponse (..),
    mkGetClipResponse,

    -- ** Response lenses
    gcrsPayload,
    gcrsContentType,
    gcrsResponseStatus,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetClip' smart constructor.
data GetClip = GetClip'
  { -- | The time range of the requested clip and the source of the timestamps.
    clipFragmentSelector :: ClipFragmentSelector,
    -- | The Amazon Resource Name (ARN) of the stream for which to retrieve the media clip.
    --
    -- You must specify either the StreamName or the StreamARN.
    streamARN :: Lude.Maybe Lude.Text,
    -- | The name of the stream for which to retrieve the media clip.
    --
    -- You must specify either the StreamName or the StreamARN.
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetClip' with the minimum fields required to make a request.
--
-- * 'clipFragmentSelector' - The time range of the requested clip and the source of the timestamps.
-- * 'streamARN' - The Amazon Resource Name (ARN) of the stream for which to retrieve the media clip.
--
-- You must specify either the StreamName or the StreamARN.
-- * 'streamName' - The name of the stream for which to retrieve the media clip.
--
-- You must specify either the StreamName or the StreamARN.
mkGetClip ::
  -- | 'clipFragmentSelector'
  ClipFragmentSelector ->
  GetClip
mkGetClip pClipFragmentSelector_ =
  GetClip'
    { clipFragmentSelector = pClipFragmentSelector_,
      streamARN = Lude.Nothing,
      streamName = Lude.Nothing
    }

-- | The time range of the requested clip and the source of the timestamps.
--
-- /Note:/ Consider using 'clipFragmentSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcClipFragmentSelector :: Lens.Lens' GetClip ClipFragmentSelector
gcClipFragmentSelector = Lens.lens (clipFragmentSelector :: GetClip -> ClipFragmentSelector) (\s a -> s {clipFragmentSelector = a} :: GetClip)
{-# DEPRECATED gcClipFragmentSelector "Use generic-lens or generic-optics with 'clipFragmentSelector' instead." #-}

-- | The Amazon Resource Name (ARN) of the stream for which to retrieve the media clip.
--
-- You must specify either the StreamName or the StreamARN.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStreamARN :: Lens.Lens' GetClip (Lude.Maybe Lude.Text)
gcStreamARN = Lens.lens (streamARN :: GetClip -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: GetClip)
{-# DEPRECATED gcStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream for which to retrieve the media clip.
--
-- You must specify either the StreamName or the StreamARN.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStreamName :: Lens.Lens' GetClip (Lude.Maybe Lude.Text)
gcStreamName = Lens.lens (streamName :: GetClip -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: GetClip)
{-# DEPRECATED gcStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest GetClip where
  type Rs GetClip = GetClipResponse
  request = Req.postJSON kinesisVideoArchivedMediaService
  response =
    Res.receiveBody
      ( \s h x ->
          GetClipResponse'
            Lude.<$> (Lude.pure x)
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetClip where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetClip where
  toJSON GetClip' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClipFragmentSelector" Lude..= clipFragmentSelector),
            ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("StreamName" Lude..=) Lude.<$> streamName
          ]
      )

instance Lude.ToPath GetClip where
  toPath = Lude.const "/getClip"

instance Lude.ToQuery GetClip where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetClipResponse' smart constructor.
data GetClipResponse = GetClipResponse'
  { -- | Traditional MP4 file that contains the media clip from the specified video stream. The output will contain the first 100 MB or the first 200 fragments from the specified start timestamp. For more information, see <Kinesis Video Streams Limits Kinesis Video Streams Limits> .
    payload :: Lude.RsBody,
    -- | The content type of the media in the requested clip.
    contentType :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'GetClipResponse' with the minimum fields required to make a request.
--
-- * 'payload' - Traditional MP4 file that contains the media clip from the specified video stream. The output will contain the first 100 MB or the first 200 fragments from the specified start timestamp. For more information, see <Kinesis Video Streams Limits Kinesis Video Streams Limits> .
-- * 'contentType' - The content type of the media in the requested clip.
-- * 'responseStatus' - The response status code.
mkGetClipResponse ::
  -- | 'payload'
  Lude.RsBody ->
  -- | 'responseStatus'
  Lude.Int ->
  GetClipResponse
mkGetClipResponse pPayload_ pResponseStatus_ =
  GetClipResponse'
    { payload = pPayload_,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Traditional MP4 file that contains the media clip from the specified video stream. The output will contain the first 100 MB or the first 200 fragments from the specified start timestamp. For more information, see <Kinesis Video Streams Limits Kinesis Video Streams Limits> .
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsPayload :: Lens.Lens' GetClipResponse Lude.RsBody
gcrsPayload = Lens.lens (payload :: GetClipResponse -> Lude.RsBody) (\s a -> s {payload = a} :: GetClipResponse)
{-# DEPRECATED gcrsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The content type of the media in the requested clip.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsContentType :: Lens.Lens' GetClipResponse (Lude.Maybe Lude.Text)
gcrsContentType = Lens.lens (contentType :: GetClipResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: GetClipResponse)
{-# DEPRECATED gcrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetClipResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetClipResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetClipResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
