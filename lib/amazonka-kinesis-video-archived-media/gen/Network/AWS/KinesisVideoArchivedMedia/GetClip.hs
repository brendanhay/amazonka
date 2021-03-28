{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetClip (..)
    , mkGetClip
    -- ** Request lenses
    , gcClipFragmentSelector
    , gcStreamARN
    , gcStreamName

    -- * Destructuring the response
    , GetClipResponse (..)
    , mkGetClipResponse
    -- ** Response lenses
    , gcrrsContentType
    , gcrrsPayload
    , gcrrsResponseStatus
    ) where

import qualified Network.AWS.KinesisVideoArchivedMedia.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetClip' smart constructor.
data GetClip = GetClip'
  { clipFragmentSelector :: Types.ClipFragmentSelector
    -- ^ The time range of the requested clip and the source of the timestamps.
  , streamARN :: Core.Maybe Types.ResourceARN
    -- ^ The Amazon Resource Name (ARN) of the stream for which to retrieve the media clip. 
--
-- You must specify either the StreamName or the StreamARN. 
  , streamName :: Core.Maybe Types.StreamName
    -- ^ The name of the stream for which to retrieve the media clip. 
--
-- You must specify either the StreamName or the StreamARN. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetClip' value with any optional fields omitted.
mkGetClip
    :: Types.ClipFragmentSelector -- ^ 'clipFragmentSelector'
    -> GetClip
mkGetClip clipFragmentSelector
  = GetClip'{clipFragmentSelector, streamARN = Core.Nothing,
             streamName = Core.Nothing}

-- | The time range of the requested clip and the source of the timestamps.
--
-- /Note:/ Consider using 'clipFragmentSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcClipFragmentSelector :: Lens.Lens' GetClip Types.ClipFragmentSelector
gcClipFragmentSelector = Lens.field @"clipFragmentSelector"
{-# INLINEABLE gcClipFragmentSelector #-}
{-# DEPRECATED clipFragmentSelector "Use generic-lens or generic-optics with 'clipFragmentSelector' instead"  #-}

-- | The Amazon Resource Name (ARN) of the stream for which to retrieve the media clip. 
--
-- You must specify either the StreamName or the StreamARN. 
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStreamARN :: Lens.Lens' GetClip (Core.Maybe Types.ResourceARN)
gcStreamARN = Lens.field @"streamARN"
{-# INLINEABLE gcStreamARN #-}
{-# DEPRECATED streamARN "Use generic-lens or generic-optics with 'streamARN' instead"  #-}

-- | The name of the stream for which to retrieve the media clip. 
--
-- You must specify either the StreamName or the StreamARN. 
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStreamName :: Lens.Lens' GetClip (Core.Maybe Types.StreamName)
gcStreamName = Lens.field @"streamName"
{-# INLINEABLE gcStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

instance Core.ToQuery GetClip where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetClip where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetClip where
        toJSON GetClip{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClipFragmentSelector" Core..= clipFragmentSelector),
                  ("StreamARN" Core..=) Core.<$> streamARN,
                  ("StreamName" Core..=) Core.<$> streamName])

instance Core.AWSRequest GetClip where
        type Rs GetClip = GetClipResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/getClip",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveBody
              (\ s h x ->
                 GetClipResponse' Core.<$>
                   (Core.parseHeaderMaybe "Content-Type" h) Core.<*> Core.pure x
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetClipResponse' smart constructor.
data GetClipResponse = GetClipResponse'
  { contentType :: Core.Maybe Types.ContentType
    -- ^ The content type of the media in the requested clip.
  , payload :: Core.RsBody
    -- ^ Traditional MP4 file that contains the media clip from the specified video stream. The output will contain the first 100 MB or the first 200 fragments from the specified start timestamp. For more information, see <Kinesis Video Streams Limits Kinesis Video Streams Limits> . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'GetClipResponse' value with any optional fields omitted.
mkGetClipResponse
    :: Core.RsBody -- ^ 'payload'
    -> Core.Int -- ^ 'responseStatus'
    -> GetClipResponse
mkGetClipResponse payload responseStatus
  = GetClipResponse'{contentType = Core.Nothing, payload,
                     responseStatus}

-- | The content type of the media in the requested clip.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsContentType :: Lens.Lens' GetClipResponse (Core.Maybe Types.ContentType)
gcrrsContentType = Lens.field @"contentType"
{-# INLINEABLE gcrrsContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | Traditional MP4 file that contains the media clip from the specified video stream. The output will contain the first 100 MB or the first 200 fragments from the specified start timestamp. For more information, see <Kinesis Video Streams Limits Kinesis Video Streams Limits> . 
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsPayload :: Lens.Lens' GetClipResponse Core.RsBody
gcrrsPayload = Lens.field @"payload"
{-# INLINEABLE gcrrsPayload #-}
{-# DEPRECATED payload "Use generic-lens or generic-optics with 'payload' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetClipResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
