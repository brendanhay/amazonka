{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetMediaForFragmentList (..),
    mkGetMediaForFragmentList,

    -- ** Request lenses
    gmfflFragments,
    gmfflStreamName,

    -- * Destructuring the response
    GetMediaForFragmentListResponse (..),
    mkGetMediaForFragmentListResponse,

    -- ** Response lenses
    gmfflrsPayload,
    gmfflrsContentType,
    gmfflrsResponseStatus,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMediaForFragmentList' smart constructor.
data GetMediaForFragmentList = GetMediaForFragmentList'
  { -- | A list of the numbers of fragments for which to retrieve media. You retrieve these values with 'ListFragments' .
    fragments :: Lude.NonEmpty Lude.Text,
    -- | The name of the stream from which to retrieve fragment media.
    streamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMediaForFragmentList' with the minimum fields required to make a request.
--
-- * 'fragments' - A list of the numbers of fragments for which to retrieve media. You retrieve these values with 'ListFragments' .
-- * 'streamName' - The name of the stream from which to retrieve fragment media.
mkGetMediaForFragmentList ::
  -- | 'fragments'
  Lude.NonEmpty Lude.Text ->
  -- | 'streamName'
  Lude.Text ->
  GetMediaForFragmentList
mkGetMediaForFragmentList pFragments_ pStreamName_ =
  GetMediaForFragmentList'
    { fragments = pFragments_,
      streamName = pStreamName_
    }

-- | A list of the numbers of fragments for which to retrieve media. You retrieve these values with 'ListFragments' .
--
-- /Note:/ Consider using 'fragments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfflFragments :: Lens.Lens' GetMediaForFragmentList (Lude.NonEmpty Lude.Text)
gmfflFragments = Lens.lens (fragments :: GetMediaForFragmentList -> Lude.NonEmpty Lude.Text) (\s a -> s {fragments = a} :: GetMediaForFragmentList)
{-# DEPRECATED gmfflFragments "Use generic-lens or generic-optics with 'fragments' instead." #-}

-- | The name of the stream from which to retrieve fragment media.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfflStreamName :: Lens.Lens' GetMediaForFragmentList Lude.Text
gmfflStreamName = Lens.lens (streamName :: GetMediaForFragmentList -> Lude.Text) (\s a -> s {streamName = a} :: GetMediaForFragmentList)
{-# DEPRECATED gmfflStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest GetMediaForFragmentList where
  type Rs GetMediaForFragmentList = GetMediaForFragmentListResponse
  request = Req.postJSON kinesisVideoArchivedMediaService
  response =
    Res.receiveBody
      ( \s h x ->
          GetMediaForFragmentListResponse'
            Lude.<$> (Lude.pure x)
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMediaForFragmentList where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetMediaForFragmentList where
  toJSON GetMediaForFragmentList' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Fragments" Lude..= fragments),
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath GetMediaForFragmentList where
  toPath = Lude.const "/getMediaForFragmentList"

instance Lude.ToQuery GetMediaForFragmentList where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMediaForFragmentListResponse' smart constructor.
data GetMediaForFragmentListResponse = GetMediaForFragmentListResponse'
  { -- | The payload that Kinesis Video Streams returns is a sequence of chunks from the specified stream. For information about the chunks, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_dataplane_PutMedia.html PutMedia> . The chunks that Kinesis Video Streams returns in the @GetMediaForFragmentList@ call also include the following additional Matroska (MKV) tags:
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
    payload :: Lude.RsBody,
    -- | The content type of the requested media.
    contentType :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'GetMediaForFragmentListResponse' with the minimum fields required to make a request.
--
-- * 'payload' - The payload that Kinesis Video Streams returns is a sequence of chunks from the specified stream. For information about the chunks, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_dataplane_PutMedia.html PutMedia> . The chunks that Kinesis Video Streams returns in the @GetMediaForFragmentList@ call also include the following additional Matroska (MKV) tags:
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
-- * 'contentType' - The content type of the requested media.
-- * 'responseStatus' - The response status code.
mkGetMediaForFragmentListResponse ::
  -- | 'payload'
  Lude.RsBody ->
  -- | 'responseStatus'
  Lude.Int ->
  GetMediaForFragmentListResponse
mkGetMediaForFragmentListResponse pPayload_ pResponseStatus_ =
  GetMediaForFragmentListResponse'
    { payload = pPayload_,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

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
gmfflrsPayload :: Lens.Lens' GetMediaForFragmentListResponse Lude.RsBody
gmfflrsPayload = Lens.lens (payload :: GetMediaForFragmentListResponse -> Lude.RsBody) (\s a -> s {payload = a} :: GetMediaForFragmentListResponse)
{-# DEPRECATED gmfflrsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The content type of the requested media.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfflrsContentType :: Lens.Lens' GetMediaForFragmentListResponse (Lude.Maybe Lude.Text)
gmfflrsContentType = Lens.lens (contentType :: GetMediaForFragmentListResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: GetMediaForFragmentListResponse)
{-# DEPRECATED gmfflrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmfflrsResponseStatus :: Lens.Lens' GetMediaForFragmentListResponse Lude.Int
gmfflrsResponseStatus = Lens.lens (responseStatus :: GetMediaForFragmentListResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMediaForFragmentListResponse)
{-# DEPRECATED gmfflrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
