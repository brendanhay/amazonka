{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.GetFaceSearch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the face search results for Amazon Rekognition Video face search started by 'StartFaceSearch' . The search returns faces in a collection that match the faces of persons detected in a video. It also includes the time(s) that faces are matched in the video.
--
-- Face search in a video is an asynchronous operation. You start face search by calling to 'StartFaceSearch' which returns a job identifier (@JobId@ ). When the search operation finishes, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartFaceSearch@ . To get the search results, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call @GetFaceSearch@ and pass the job identifier (@JobId@ ) from the initial call to @StartFaceSearch@ .
-- For more information, see Searching Faces in a Collection in the Amazon Rekognition Developer Guide.
-- The search results are retured in an array, @Persons@ , of 'PersonMatch' objects. Each@PersonMatch@ element contains details about the matching faces in the input collection, person information (facial attributes, bounding boxes, and person identifer) for the matched person, and the time the person was matched in the video.
-- By default, the @Persons@ array is sorted by the time, in milliseconds from the start of the video, persons are matched. You can also sort by persons by specifying @INDEX@ for the @SORTBY@ input parameter.
module Network.AWS.Rekognition.GetFaceSearch
  ( -- * Creating a request
    GetFaceSearch (..),
    mkGetFaceSearch,

    -- ** Request lenses
    gfsJobId,
    gfsMaxResults,
    gfsNextToken,
    gfsSortBy,

    -- * Destructuring the response
    GetFaceSearchResponse (..),
    mkGetFaceSearchResponse,

    -- ** Response lenses
    gfsrrsJobStatus,
    gfsrrsNextToken,
    gfsrrsPersons,
    gfsrrsStatusMessage,
    gfsrrsVideoMetadata,
    gfsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFaceSearch' smart constructor.
data GetFaceSearch = GetFaceSearch'
  { -- | The job identifer for the search request. You get the job identifier from an initial call to @StartFaceSearch@ .
    jobId :: Types.JobId,
    -- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous response was incomplete (because there is more search results to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of search results.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | Sort to use for grouping faces in the response. Use @TIMESTAMP@ to group faces by the time that they are recognized. Use @INDEX@ to sort by recognized faces.
    sortBy :: Core.Maybe Types.FaceSearchSortBy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFaceSearch' value with any optional fields omitted.
mkGetFaceSearch ::
  -- | 'jobId'
  Types.JobId ->
  GetFaceSearch
mkGetFaceSearch jobId =
  GetFaceSearch'
    { jobId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | The job identifer for the search request. You get the job identifier from an initial call to @StartFaceSearch@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsJobId :: Lens.Lens' GetFaceSearch Types.JobId
gfsJobId = Lens.field @"jobId"
{-# DEPRECATED gfsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsMaxResults :: Lens.Lens' GetFaceSearch (Core.Maybe Core.Natural)
gfsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gfsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous response was incomplete (because there is more search results to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of search results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsNextToken :: Lens.Lens' GetFaceSearch (Core.Maybe Types.PaginationToken)
gfsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gfsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Sort to use for grouping faces in the response. Use @TIMESTAMP@ to group faces by the time that they are recognized. Use @INDEX@ to sort by recognized faces.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsSortBy :: Lens.Lens' GetFaceSearch (Core.Maybe Types.FaceSearchSortBy)
gfsSortBy = Lens.field @"sortBy"
{-# DEPRECATED gfsSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Core.FromJSON GetFaceSearch where
  toJSON GetFaceSearch {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobId" Core..= jobId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.AWSRequest GetFaceSearch where
  type Rs GetFaceSearch = GetFaceSearchResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "RekognitionService.GetFaceSearch")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFaceSearchResponse'
            Core.<$> (x Core..:? "JobStatus")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Persons")
            Core.<*> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "VideoMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFaceSearchResponse' smart constructor.
data GetFaceSearchResponse = GetFaceSearchResponse'
  { -- | The current status of the face search job.
    jobStatus :: Core.Maybe Types.VideoJobStatus,
    -- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of search results.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | An array of persons, 'PersonMatch' , in the video whose face(s) match the face(s) in an Amazon Rekognition collection. It also includes time information for when persons are matched in the video. You specify the input collection in an initial call to @StartFaceSearch@ . Each @Persons@ element includes a time the person was matched, face match details (@FaceMatches@ ) for matching faces in the collection, and person information (@Person@ ) for the matched person.
    persons :: Core.Maybe [Types.PersonMatch],
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Core.Maybe Types.StatusMessage,
    -- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
    videoMetadata :: Core.Maybe Types.VideoMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFaceSearchResponse' value with any optional fields omitted.
mkGetFaceSearchResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFaceSearchResponse
mkGetFaceSearchResponse responseStatus =
  GetFaceSearchResponse'
    { jobStatus = Core.Nothing,
      nextToken = Core.Nothing,
      persons = Core.Nothing,
      statusMessage = Core.Nothing,
      videoMetadata = Core.Nothing,
      responseStatus
    }

-- | The current status of the face search job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrrsJobStatus :: Lens.Lens' GetFaceSearchResponse (Core.Maybe Types.VideoJobStatus)
gfsrrsJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED gfsrrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of search results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrrsNextToken :: Lens.Lens' GetFaceSearchResponse (Core.Maybe Types.PaginationToken)
gfsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gfsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of persons, 'PersonMatch' , in the video whose face(s) match the face(s) in an Amazon Rekognition collection. It also includes time information for when persons are matched in the video. You specify the input collection in an initial call to @StartFaceSearch@ . Each @Persons@ element includes a time the person was matched, face match details (@FaceMatches@ ) for matching faces in the collection, and person information (@Person@ ) for the matched person.
--
-- /Note:/ Consider using 'persons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrrsPersons :: Lens.Lens' GetFaceSearchResponse (Core.Maybe [Types.PersonMatch])
gfsrrsPersons = Lens.field @"persons"
{-# DEPRECATED gfsrrsPersons "Use generic-lens or generic-optics with 'persons' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrrsStatusMessage :: Lens.Lens' GetFaceSearchResponse (Core.Maybe Types.StatusMessage)
gfsrrsStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED gfsrrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrrsVideoMetadata :: Lens.Lens' GetFaceSearchResponse (Core.Maybe Types.VideoMetadata)
gfsrrsVideoMetadata = Lens.field @"videoMetadata"
{-# DEPRECATED gfsrrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrrsResponseStatus :: Lens.Lens' GetFaceSearchResponse Core.Int
gfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
