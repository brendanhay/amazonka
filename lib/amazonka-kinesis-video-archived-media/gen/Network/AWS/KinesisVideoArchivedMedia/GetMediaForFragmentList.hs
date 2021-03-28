{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.GetMediaForFragmentList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets media for a list of fragments (specified by fragment number) from the archived data in an Amazon Kinesis video stream.
--
-- The following limits apply when using the @GetMediaForFragmentList@ API:
--
--     * A client can call @GetMediaForFragmentList@ up to five times per second per stream. 
--
--
--     * Kinesis Video Streams sends media data at a rate of up to 25 megabytes per second (or 200 megabits per second) during a @GetMediaForFragmentList@ session. 
--
--
-- /Important:/ If an error is thrown after invoking a Kinesis Video Streams archived media API, in addition to the HTTP status code and the response body, it includes the following pieces of information: 
--
--     * @x-amz-ErrorType@ HTTP header – contains a more specific error type in addition to what the HTTP status code provides. 
--
--
--     * @x-amz-RequestId@ HTTP header – if you want to report an issue to AWS, the support team can better diagnose the problem if given the Request Id.
--
--
-- Both the HTTP status code and the ErrorType header can be utilized to make programmatic decisions about whether errors are retry-able and under what conditions, as well as provide information on what actions the client programmer might need to take in order to successfully try again.
-- For more information, see the __Errors__ section at the bottom of this topic, as well as <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/CommonErrors.html Common Errors> . 
module Network.AWS.KinesisVideoArchivedMedia.GetMediaForFragmentList
    (
    -- * Creating a request
      GetMediaForFragmentList (..)
    , mkGetMediaForFragmentList
    -- ** Request lenses
    , gmfflStreamName
    , gmfflFragments

    -- * Destructuring the response
    , GetMediaForFragmentListResponse (..)
    , mkGetMediaForFragmentListResponse
    -- ** Response lenses
    , gmfflrrsContentType
    , gmfflrrsPayload
    , gmfflrrsResponseStatus
    ) where

import qualified Network.AWS.KinesisVideoArchivedMedia.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMediaForFragmentList' smart constructor.
data GetMediaForFragmentList = GetMediaForFragmentList'
  { streamName :: Types.StreamName
    -- ^ The name of the stream from which to retrieve fragment media.
  , fragments :: Core.NonEmpty Types.FragmentNumberString
    -- ^ A list of the numbers of fragments for which to retrieve media. You retrieve these values with 'ListFragments' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMediaForFragmentList' value with any optional fields omitted.
mkGetMediaForFragmentList
    :: Types.StreamName -- ^ 'streamName'
    -> Core.NonEmpty Types.FragmentNumberString -- ^ 'fragments'
    -> GetMediaForFragmentList
mkGetMediaForFragmentList streamName fragments
  = GetMediaForFragmentList'{streamName, fragments}

-- | The name of the stream from which to retrieve fragment media.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfflStreamName :: Lens.Lens' GetMediaForFragmentList Types.StreamName
gmfflStreamName = Lens.field @"streamName"
{-# INLINEABLE gmfflStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | A list of the numbers of fragments for which to retrieve media. You retrieve these values with 'ListFragments' .
--
-- /Note:/ Consider using 'fragments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfflFragments :: Lens.Lens' GetMediaForFragmentList (Core.NonEmpty Types.FragmentNumberString)
gmfflFragments = Lens.field @"fragments"
{-# INLINEABLE gmfflFragments #-}
{-# DEPRECATED fragments "Use generic-lens or generic-optics with 'fragments' instead"  #-}

instance Core.ToQuery GetMediaForFragmentList where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMediaForFragmentList where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetMediaForFragmentList where
        toJSON GetMediaForFragmentList{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StreamName" Core..= streamName),
                  Core.Just ("Fragments" Core..= fragments)])

instance Core.AWSRequest GetMediaForFragmentList where
        type Rs GetMediaForFragmentList = GetMediaForFragmentListResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/getMediaForFragmentList",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveBody
              (\ s h x ->
                 GetMediaForFragmentListResponse' Core.<$>
                   (Core.parseHeaderMaybe "Content-Type" h) Core.<*> Core.pure x
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMediaForFragmentListResponse' smart constructor.
data GetMediaForFragmentListResponse = GetMediaForFragmentListResponse'
  { contentType :: Core.Maybe Types.ContentType
    -- ^ The content type of the requested media.
  , payload :: Core.RsBody
    -- ^ The payload that Kinesis Video Streams returns is a sequence of chunks from the specified stream. For information about the chunks, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_dataplane_PutMedia.html PutMedia> . The chunks that Kinesis Video Streams returns in the @GetMediaForFragmentList@ call also include the following additional Matroska (MKV) tags: 
--
--
--     * AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the chunk.
--
--
--     * AWS_KINESISVIDEO_SERVER_SIDE_TIMESTAMP - Server-side timestamp of the fragment.
--
--
--     * AWS_KINESISVIDEO_PRODUCER_SIDE_TIMESTAMP - Producer-side timestamp of the fragment.
--
--
-- The following tags will be included if an exception occurs:
--
--     * AWS_KINESISVIDEO_FRAGMENT_NUMBER - The number of the fragment that threw the exception
--
--
--     * AWS_KINESISVIDEO_EXCEPTION_ERROR_CODE - The integer code of the exception
--
--
--     * AWS_KINESISVIDEO_EXCEPTION_MESSAGE - A text description of the exception
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'GetMediaForFragmentListResponse' value with any optional fields omitted.
mkGetMediaForFragmentListResponse
    :: Core.RsBody -- ^ 'payload'
    -> Core.Int -- ^ 'responseStatus'
    -> GetMediaForFragmentListResponse
mkGetMediaForFragmentListResponse payload responseStatus
  = GetMediaForFragmentListResponse'{contentType = Core.Nothing,
                                     payload, responseStatus}

-- | The content type of the requested media.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfflrrsContentType :: Lens.Lens' GetMediaForFragmentListResponse (Core.Maybe Types.ContentType)
gmfflrrsContentType = Lens.field @"contentType"
{-# INLINEABLE gmfflrrsContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | The payload that Kinesis Video Streams returns is a sequence of chunks from the specified stream. For information about the chunks, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_dataplane_PutMedia.html PutMedia> . The chunks that Kinesis Video Streams returns in the @GetMediaForFragmentList@ call also include the following additional Matroska (MKV) tags: 
--
--
--     * AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the chunk.
--
--
--     * AWS_KINESISVIDEO_SERVER_SIDE_TIMESTAMP - Server-side timestamp of the fragment.
--
--
--     * AWS_KINESISVIDEO_PRODUCER_SIDE_TIMESTAMP - Producer-side timestamp of the fragment.
--
--
-- The following tags will be included if an exception occurs:
--
--     * AWS_KINESISVIDEO_FRAGMENT_NUMBER - The number of the fragment that threw the exception
--
--
--     * AWS_KINESISVIDEO_EXCEPTION_ERROR_CODE - The integer code of the exception
--
--
--     * AWS_KINESISVIDEO_EXCEPTION_MESSAGE - A text description of the exception
--
--
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfflrrsPayload :: Lens.Lens' GetMediaForFragmentListResponse Core.RsBody
gmfflrrsPayload = Lens.field @"payload"
{-# INLINEABLE gmfflrrsPayload #-}
{-# DEPRECATED payload "Use generic-lens or generic-optics with 'payload' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfflrrsResponseStatus :: Lens.Lens' GetMediaForFragmentListResponse Core.Int
gmfflrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmfflrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
