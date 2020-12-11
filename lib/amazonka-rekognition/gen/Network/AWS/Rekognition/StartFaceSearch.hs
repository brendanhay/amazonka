{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartFaceSearch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous search for faces in a collection that match the faces of persons detected in a stored video.
--
-- The video must be stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartFaceSearch@ returns a job identifier (@JobId@ ) which you use to get the search results once the search has completed. When searching is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ . To get the search results, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetFaceSearch' and pass the job identifier (@JobId@ ) from the initial call to @StartFaceSearch@ . For more information, see 'procedure-person-search-videos' .
module Network.AWS.Rekognition.StartFaceSearch
  ( -- * Creating a request
    StartFaceSearch (..),
    mkStartFaceSearch,

    -- ** Request lenses
    sfsFaceMatchThreshold,
    sfsJobTag,
    sfsNotificationChannel,
    sfsClientRequestToken,
    sfsVideo,
    sfsCollectionId,

    -- * Destructuring the response
    StartFaceSearchResponse (..),
    mkStartFaceSearchResponse,

    -- ** Response lenses
    sfsrsJobId,
    sfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartFaceSearch' smart constructor.
data StartFaceSearch = StartFaceSearch'
  { faceMatchThreshold ::
      Lude.Maybe Lude.Double,
    jobTag :: Lude.Maybe Lude.Text,
    notificationChannel :: Lude.Maybe NotificationChannel,
    clientRequestToken :: Lude.Maybe Lude.Text,
    video :: Video,
    collectionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartFaceSearch' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartFaceSearch@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
-- * 'collectionId' - ID of the collection that contains the faces you want to search for.
-- * 'faceMatchThreshold' - The minimum confidence in the person match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
-- * 'jobTag' - An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
-- * 'notificationChannel' - The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the search.
-- * 'video' - The video you want to search. The video must be stored in an Amazon S3 bucket.
mkStartFaceSearch ::
  -- | 'video'
  Video ->
  -- | 'collectionId'
  Lude.Text ->
  StartFaceSearch
mkStartFaceSearch pVideo_ pCollectionId_ =
  StartFaceSearch'
    { faceMatchThreshold = Lude.Nothing,
      jobTag = Lude.Nothing,
      notificationChannel = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      video = pVideo_,
      collectionId = pCollectionId_
    }

-- | The minimum confidence in the person match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
--
-- /Note:/ Consider using 'faceMatchThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsFaceMatchThreshold :: Lens.Lens' StartFaceSearch (Lude.Maybe Lude.Double)
sfsFaceMatchThreshold = Lens.lens (faceMatchThreshold :: StartFaceSearch -> Lude.Maybe Lude.Double) (\s a -> s {faceMatchThreshold = a} :: StartFaceSearch)
{-# DEPRECATED sfsFaceMatchThreshold "Use generic-lens or generic-optics with 'faceMatchThreshold' instead." #-}

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsJobTag :: Lens.Lens' StartFaceSearch (Lude.Maybe Lude.Text)
sfsJobTag = Lens.lens (jobTag :: StartFaceSearch -> Lude.Maybe Lude.Text) (\s a -> s {jobTag = a} :: StartFaceSearch)
{-# DEPRECATED sfsJobTag "Use generic-lens or generic-optics with 'jobTag' instead." #-}

-- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the search.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsNotificationChannel :: Lens.Lens' StartFaceSearch (Lude.Maybe NotificationChannel)
sfsNotificationChannel = Lens.lens (notificationChannel :: StartFaceSearch -> Lude.Maybe NotificationChannel) (\s a -> s {notificationChannel = a} :: StartFaceSearch)
{-# DEPRECATED sfsNotificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead." #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartFaceSearch@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsClientRequestToken :: Lens.Lens' StartFaceSearch (Lude.Maybe Lude.Text)
sfsClientRequestToken = Lens.lens (clientRequestToken :: StartFaceSearch -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartFaceSearch)
{-# DEPRECATED sfsClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The video you want to search. The video must be stored in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsVideo :: Lens.Lens' StartFaceSearch Video
sfsVideo = Lens.lens (video :: StartFaceSearch -> Video) (\s a -> s {video = a} :: StartFaceSearch)
{-# DEPRECATED sfsVideo "Use generic-lens or generic-optics with 'video' instead." #-}

-- | ID of the collection that contains the faces you want to search for.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsCollectionId :: Lens.Lens' StartFaceSearch Lude.Text
sfsCollectionId = Lens.lens (collectionId :: StartFaceSearch -> Lude.Text) (\s a -> s {collectionId = a} :: StartFaceSearch)
{-# DEPRECATED sfsCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

instance Lude.AWSRequest StartFaceSearch where
  type Rs StartFaceSearch = StartFaceSearchResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartFaceSearchResponse'
            Lude.<$> (x Lude..?> "JobId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartFaceSearch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.StartFaceSearch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartFaceSearch where
  toJSON StartFaceSearch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FaceMatchThreshold" Lude..=) Lude.<$> faceMatchThreshold,
            ("JobTag" Lude..=) Lude.<$> jobTag,
            ("NotificationChannel" Lude..=) Lude.<$> notificationChannel,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("Video" Lude..= video),
            Lude.Just ("CollectionId" Lude..= collectionId)
          ]
      )

instance Lude.ToPath StartFaceSearch where
  toPath = Lude.const "/"

instance Lude.ToQuery StartFaceSearch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartFaceSearchResponse' smart constructor.
data StartFaceSearchResponse = StartFaceSearchResponse'
  { jobId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'StartFaceSearchResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier for the search job. Use @JobId@ to identify the job in a subsequent call to @GetFaceSearch@ .
-- * 'responseStatus' - The response status code.
mkStartFaceSearchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartFaceSearchResponse
mkStartFaceSearchResponse pResponseStatus_ =
  StartFaceSearchResponse'
    { jobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for the search job. Use @JobId@ to identify the job in a subsequent call to @GetFaceSearch@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsrsJobId :: Lens.Lens' StartFaceSearchResponse (Lude.Maybe Lude.Text)
sfsrsJobId = Lens.lens (jobId :: StartFaceSearchResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartFaceSearchResponse)
{-# DEPRECATED sfsrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsrsResponseStatus :: Lens.Lens' StartFaceSearchResponse Lude.Int
sfsrsResponseStatus = Lens.lens (responseStatus :: StartFaceSearchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartFaceSearchResponse)
{-# DEPRECATED sfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
