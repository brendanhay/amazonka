{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.GetSegmentDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the segment detection results of a Amazon Rekognition Video analysis started by 'StartSegmentDetection' .
--
-- Segment detection with Amazon Rekognition Video is an asynchronous operation. You start segment detection by calling 'StartSegmentDetection' which returns a job identifier (@JobId@ ). When the segment detection operation finishes, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartSegmentDetection@ . To get the results of the segment detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . if so, call @GetSegmentDetection@ and pass the job identifier (@JobId@ ) from the initial call of @StartSegmentDetection@ .
-- @GetSegmentDetection@ returns detected segments in an array (@Segments@ ) of 'SegmentDetection' objects. @Segments@ is sorted by the segment types specified in the @SegmentTypes@ input parameter of @StartSegmentDetection@ . Each element of the array includes the detected segment, the precentage confidence in the acuracy of the detected segment, the type of the segment, and the frame in which the segment was detected.
-- Use @SelectedSegmentTypes@ to find out the type of segment detection requested in the call to @StartSegmentDetection@ .
-- Use the @MaxResults@ parameter to limit the number of segment detections returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetSegmentDetection@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetSegmentDetection@ .
-- For more information, see Detecting Video Segments in Stored Video in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.GetSegmentDetection
  ( -- * Creating a request
    GetSegmentDetection (..),
    mkGetSegmentDetection,

    -- ** Request lenses
    gsdJobId,
    gsdMaxResults,
    gsdNextToken,

    -- * Destructuring the response
    GetSegmentDetectionResponse (..),
    mkGetSegmentDetectionResponse,

    -- ** Response lenses
    gsdrrsAudioMetadata,
    gsdrrsJobStatus,
    gsdrrsNextToken,
    gsdrrsSegments,
    gsdrrsSelectedSegmentTypes,
    gsdrrsStatusMessage,
    gsdrrsVideoMetadata,
    gsdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSegmentDetection' smart constructor.
data GetSegmentDetection = GetSegmentDetection'
  { -- | Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartSegmentDetection@ .
    jobId :: Types.JobId,
    -- | Maximum number of results to return per paginated call. The largest value you can specify is 1000.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentDetection' value with any optional fields omitted.
mkGetSegmentDetection ::
  -- | 'jobId'
  Types.JobId ->
  GetSegmentDetection
mkGetSegmentDetection jobId =
  GetSegmentDetection'
    { jobId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartSegmentDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdJobId :: Lens.Lens' GetSegmentDetection Types.JobId
gsdJobId = Lens.field @"jobId"
{-# DEPRECATED gsdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdMaxResults :: Lens.Lens' GetSegmentDetection (Core.Maybe Core.Natural)
gsdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gsdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdNextToken :: Lens.Lens' GetSegmentDetection (Core.Maybe Types.PaginationToken)
gsdNextToken = Lens.field @"nextToken"
{-# DEPRECATED gsdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetSegmentDetection where
  toJSON GetSegmentDetection {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobId" Core..= jobId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetSegmentDetection where
  type Rs GetSegmentDetection = GetSegmentDetectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "RekognitionService.GetSegmentDetection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentDetectionResponse'
            Core.<$> (x Core..:? "AudioMetadata")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Segments")
            Core.<*> (x Core..:? "SelectedSegmentTypes")
            Core.<*> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "VideoMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSegmentDetectionResponse' smart constructor.
data GetSegmentDetectionResponse = GetSegmentDetectionResponse'
  { -- | An array of objects. There can be multiple audio streams. Each @AudioMetadata@ object contains metadata for a single audio stream. Audio information in an @AudioMetadata@ objects includes the audio codec, the number of audio channels, the duration of the audio stream, and the sample rate. Audio metadata is returned in each page of information returned by @GetSegmentDetection@ .
    audioMetadata :: Core.Maybe [Types.AudioMetadata],
    -- | Current status of the segment detection job.
    jobStatus :: Core.Maybe Types.VideoJobStatus,
    -- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | An array of segments detected in a video. The array is sorted by the segment types (TECHNICAL_CUE or SHOT) specified in the @SegmentTypes@ input parameter of @StartSegmentDetection@ . Within each segment type the array is sorted by timestamp values.
    segments :: Core.Maybe [Types.SegmentDetection],
    -- | An array containing the segment types requested in the call to @StartSegmentDetection@ .
    selectedSegmentTypes :: Core.Maybe [Types.SegmentTypeInfo],
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Core.Maybe Types.StatusMessage,
    -- | Currently, Amazon Rekognition Video returns a single object in the @VideoMetadata@ array. The object contains information about the video stream in the input file that Amazon Rekognition Video chose to analyze. The @VideoMetadata@ object includes the video codec, video format and other information. Video metadata is returned in each page of information returned by @GetSegmentDetection@ .
    videoMetadata :: Core.Maybe [Types.VideoMetadata],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentDetectionResponse' value with any optional fields omitted.
mkGetSegmentDetectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSegmentDetectionResponse
mkGetSegmentDetectionResponse responseStatus =
  GetSegmentDetectionResponse'
    { audioMetadata = Core.Nothing,
      jobStatus = Core.Nothing,
      nextToken = Core.Nothing,
      segments = Core.Nothing,
      selectedSegmentTypes = Core.Nothing,
      statusMessage = Core.Nothing,
      videoMetadata = Core.Nothing,
      responseStatus
    }

-- | An array of objects. There can be multiple audio streams. Each @AudioMetadata@ object contains metadata for a single audio stream. Audio information in an @AudioMetadata@ objects includes the audio codec, the number of audio channels, the duration of the audio stream, and the sample rate. Audio metadata is returned in each page of information returned by @GetSegmentDetection@ .
--
-- /Note:/ Consider using 'audioMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsAudioMetadata :: Lens.Lens' GetSegmentDetectionResponse (Core.Maybe [Types.AudioMetadata])
gsdrrsAudioMetadata = Lens.field @"audioMetadata"
{-# DEPRECATED gsdrrsAudioMetadata "Use generic-lens or generic-optics with 'audioMetadata' instead." #-}

-- | Current status of the segment detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsJobStatus :: Lens.Lens' GetSegmentDetectionResponse (Core.Maybe Types.VideoJobStatus)
gsdrrsJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED gsdrrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsNextToken :: Lens.Lens' GetSegmentDetectionResponse (Core.Maybe Types.PaginationToken)
gsdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gsdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of segments detected in a video. The array is sorted by the segment types (TECHNICAL_CUE or SHOT) specified in the @SegmentTypes@ input parameter of @StartSegmentDetection@ . Within each segment type the array is sorted by timestamp values.
--
-- /Note:/ Consider using 'segments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsSegments :: Lens.Lens' GetSegmentDetectionResponse (Core.Maybe [Types.SegmentDetection])
gsdrrsSegments = Lens.field @"segments"
{-# DEPRECATED gsdrrsSegments "Use generic-lens or generic-optics with 'segments' instead." #-}

-- | An array containing the segment types requested in the call to @StartSegmentDetection@ .
--
-- /Note:/ Consider using 'selectedSegmentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsSelectedSegmentTypes :: Lens.Lens' GetSegmentDetectionResponse (Core.Maybe [Types.SegmentTypeInfo])
gsdrrsSelectedSegmentTypes = Lens.field @"selectedSegmentTypes"
{-# DEPRECATED gsdrrsSelectedSegmentTypes "Use generic-lens or generic-optics with 'selectedSegmentTypes' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsStatusMessage :: Lens.Lens' GetSegmentDetectionResponse (Core.Maybe Types.StatusMessage)
gsdrrsStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED gsdrrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Currently, Amazon Rekognition Video returns a single object in the @VideoMetadata@ array. The object contains information about the video stream in the input file that Amazon Rekognition Video chose to analyze. The @VideoMetadata@ object includes the video codec, video format and other information. Video metadata is returned in each page of information returned by @GetSegmentDetection@ .
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsVideoMetadata :: Lens.Lens' GetSegmentDetectionResponse (Core.Maybe [Types.VideoMetadata])
gsdrrsVideoMetadata = Lens.field @"videoMetadata"
{-# DEPRECATED gsdrrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsResponseStatus :: Lens.Lens' GetSegmentDetectionResponse Core.Int
gsdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
