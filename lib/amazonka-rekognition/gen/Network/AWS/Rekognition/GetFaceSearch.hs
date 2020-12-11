{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gfsNextToken,
    gfsMaxResults,
    gfsSortBy,
    gfsJobId,

    -- * Destructuring the response
    GetFaceSearchResponse (..),
    mkGetFaceSearchResponse,

    -- ** Response lenses
    gfsrsNextToken,
    gfsrsVideoMetadata,
    gfsrsStatusMessage,
    gfsrsJobStatus,
    gfsrsPersons,
    gfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFaceSearch' smart constructor.
data GetFaceSearch = GetFaceSearch'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe FaceSearchSortBy,
    jobId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFaceSearch' with the minimum fields required to make a request.
--
-- * 'jobId' - The job identifer for the search request. You get the job identifier from an initial call to @StartFaceSearch@ .
-- * 'maxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
-- * 'nextToken' - If the previous response was incomplete (because there is more search results to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of search results.
-- * 'sortBy' - Sort to use for grouping faces in the response. Use @TIMESTAMP@ to group faces by the time that they are recognized. Use @INDEX@ to sort by recognized faces.
mkGetFaceSearch ::
  -- | 'jobId'
  Lude.Text ->
  GetFaceSearch
mkGetFaceSearch pJobId_ =
  GetFaceSearch'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing,
      jobId = pJobId_
    }

-- | If the previous response was incomplete (because there is more search results to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of search results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsNextToken :: Lens.Lens' GetFaceSearch (Lude.Maybe Lude.Text)
gfsNextToken = Lens.lens (nextToken :: GetFaceSearch -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetFaceSearch)
{-# DEPRECATED gfsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsMaxResults :: Lens.Lens' GetFaceSearch (Lude.Maybe Lude.Natural)
gfsMaxResults = Lens.lens (maxResults :: GetFaceSearch -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetFaceSearch)
{-# DEPRECATED gfsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Sort to use for grouping faces in the response. Use @TIMESTAMP@ to group faces by the time that they are recognized. Use @INDEX@ to sort by recognized faces.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsSortBy :: Lens.Lens' GetFaceSearch (Lude.Maybe FaceSearchSortBy)
gfsSortBy = Lens.lens (sortBy :: GetFaceSearch -> Lude.Maybe FaceSearchSortBy) (\s a -> s {sortBy = a} :: GetFaceSearch)
{-# DEPRECATED gfsSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The job identifer for the search request. You get the job identifier from an initial call to @StartFaceSearch@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsJobId :: Lens.Lens' GetFaceSearch Lude.Text
gfsJobId = Lens.lens (jobId :: GetFaceSearch -> Lude.Text) (\s a -> s {jobId = a} :: GetFaceSearch)
{-# DEPRECATED gfsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetFaceSearch where
  type Rs GetFaceSearch = GetFaceSearchResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFaceSearchResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "VideoMetadata")
            Lude.<*> (x Lude..?> "StatusMessage")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (x Lude..?> "Persons" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFaceSearch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.GetFaceSearch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetFaceSearch where
  toJSON GetFaceSearch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy,
            Lude.Just ("JobId" Lude..= jobId)
          ]
      )

instance Lude.ToPath GetFaceSearch where
  toPath = Lude.const "/"

instance Lude.ToQuery GetFaceSearch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFaceSearchResponse' smart constructor.
data GetFaceSearchResponse = GetFaceSearchResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    videoMetadata :: Lude.Maybe VideoMetadata,
    statusMessage :: Lude.Maybe Lude.Text,
    jobStatus :: Lude.Maybe VideoJobStatus,
    persons :: Lude.Maybe [PersonMatch],
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

-- | Creates a value of 'GetFaceSearchResponse' with the minimum fields required to make a request.
--
-- * 'jobStatus' - The current status of the face search job.
-- * 'nextToken' - If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of search results.
-- * 'persons' - An array of persons, 'PersonMatch' , in the video whose face(s) match the face(s) in an Amazon Rekognition collection. It also includes time information for when persons are matched in the video. You specify the input collection in an initial call to @StartFaceSearch@ . Each @Persons@ element includes a time the person was matched, face match details (@FaceMatches@ ) for matching faces in the collection, and person information (@Person@ ) for the matched person.
-- * 'responseStatus' - The response status code.
-- * 'statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
-- * 'videoMetadata' - Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
mkGetFaceSearchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFaceSearchResponse
mkGetFaceSearchResponse pResponseStatus_ =
  GetFaceSearchResponse'
    { nextToken = Lude.Nothing,
      videoMetadata = Lude.Nothing,
      statusMessage = Lude.Nothing,
      jobStatus = Lude.Nothing,
      persons = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of search results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrsNextToken :: Lens.Lens' GetFaceSearchResponse (Lude.Maybe Lude.Text)
gfsrsNextToken = Lens.lens (nextToken :: GetFaceSearchResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetFaceSearchResponse)
{-# DEPRECATED gfsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrsVideoMetadata :: Lens.Lens' GetFaceSearchResponse (Lude.Maybe VideoMetadata)
gfsrsVideoMetadata = Lens.lens (videoMetadata :: GetFaceSearchResponse -> Lude.Maybe VideoMetadata) (\s a -> s {videoMetadata = a} :: GetFaceSearchResponse)
{-# DEPRECATED gfsrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrsStatusMessage :: Lens.Lens' GetFaceSearchResponse (Lude.Maybe Lude.Text)
gfsrsStatusMessage = Lens.lens (statusMessage :: GetFaceSearchResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: GetFaceSearchResponse)
{-# DEPRECATED gfsrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The current status of the face search job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrsJobStatus :: Lens.Lens' GetFaceSearchResponse (Lude.Maybe VideoJobStatus)
gfsrsJobStatus = Lens.lens (jobStatus :: GetFaceSearchResponse -> Lude.Maybe VideoJobStatus) (\s a -> s {jobStatus = a} :: GetFaceSearchResponse)
{-# DEPRECATED gfsrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | An array of persons, 'PersonMatch' , in the video whose face(s) match the face(s) in an Amazon Rekognition collection. It also includes time information for when persons are matched in the video. You specify the input collection in an initial call to @StartFaceSearch@ . Each @Persons@ element includes a time the person was matched, face match details (@FaceMatches@ ) for matching faces in the collection, and person information (@Person@ ) for the matched person.
--
-- /Note:/ Consider using 'persons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrsPersons :: Lens.Lens' GetFaceSearchResponse (Lude.Maybe [PersonMatch])
gfsrsPersons = Lens.lens (persons :: GetFaceSearchResponse -> Lude.Maybe [PersonMatch]) (\s a -> s {persons = a} :: GetFaceSearchResponse)
{-# DEPRECATED gfsrsPersons "Use generic-lens or generic-optics with 'persons' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrsResponseStatus :: Lens.Lens' GetFaceSearchResponse Lude.Int
gfsrsResponseStatus = Lens.lens (responseStatus :: GetFaceSearchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFaceSearchResponse)
{-# DEPRECATED gfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
