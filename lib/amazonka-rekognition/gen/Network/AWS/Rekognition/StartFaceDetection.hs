{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartFaceDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of faces in a stored video.
--
-- Amazon Rekognition Video can detect faces in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartFaceDetection@ returns a job identifier (@JobId@ ) that you use to get the results of the operation. When face detection is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ . To get the results of the face detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetFaceDetection' and pass the job identifier (@JobId@ ) from the initial call to @StartFaceDetection@ .
-- For more information, see Detecting Faces in a Stored Video in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.StartFaceDetection
  ( -- * Creating a request
    StartFaceDetection (..),
    mkStartFaceDetection,

    -- ** Request lenses
    sfdJobTag,
    sfdNotificationChannel,
    sfdVideo,
    sfdClientRequestToken,
    sfdFaceAttributes,

    -- * Destructuring the response
    StartFaceDetectionResponse (..),
    mkStartFaceDetectionResponse,

    -- ** Response lenses
    sfdrsJobId,
    sfdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartFaceDetection' smart constructor.
data StartFaceDetection = StartFaceDetection'
  { -- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
    jobTag :: Lude.Maybe Lude.Text,
    -- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the face detection operation.
    notificationChannel :: Lude.Maybe NotificationChannel,
    -- | The video in which you want to detect faces. The video must be stored in an Amazon S3 bucket.
    video :: Video,
    -- | Idempotent token used to identify the start request. If you use the same token with multiple @StartFaceDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | The face attributes you want returned.
    --
    -- @DEFAULT@ - The following subset of facial attributes are returned: BoundingBox, Confidence, Pose, Quality and Landmarks.
    -- @ALL@ - All facial attributes are returned.
    faceAttributes :: Lude.Maybe FaceAttributes
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartFaceDetection' with the minimum fields required to make a request.
--
-- * 'jobTag' - An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
-- * 'notificationChannel' - The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the face detection operation.
-- * 'video' - The video in which you want to detect faces. The video must be stored in an Amazon S3 bucket.
-- * 'clientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartFaceDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
-- * 'faceAttributes' - The face attributes you want returned.
--
-- @DEFAULT@ - The following subset of facial attributes are returned: BoundingBox, Confidence, Pose, Quality and Landmarks.
-- @ALL@ - All facial attributes are returned.
mkStartFaceDetection ::
  -- | 'video'
  Video ->
  StartFaceDetection
mkStartFaceDetection pVideo_ =
  StartFaceDetection'
    { jobTag = Lude.Nothing,
      notificationChannel = Lude.Nothing,
      video = pVideo_,
      clientRequestToken = Lude.Nothing,
      faceAttributes = Lude.Nothing
    }

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfdJobTag :: Lens.Lens' StartFaceDetection (Lude.Maybe Lude.Text)
sfdJobTag = Lens.lens (jobTag :: StartFaceDetection -> Lude.Maybe Lude.Text) (\s a -> s {jobTag = a} :: StartFaceDetection)
{-# DEPRECATED sfdJobTag "Use generic-lens or generic-optics with 'jobTag' instead." #-}

-- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the face detection operation.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfdNotificationChannel :: Lens.Lens' StartFaceDetection (Lude.Maybe NotificationChannel)
sfdNotificationChannel = Lens.lens (notificationChannel :: StartFaceDetection -> Lude.Maybe NotificationChannel) (\s a -> s {notificationChannel = a} :: StartFaceDetection)
{-# DEPRECATED sfdNotificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead." #-}

-- | The video in which you want to detect faces. The video must be stored in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfdVideo :: Lens.Lens' StartFaceDetection Video
sfdVideo = Lens.lens (video :: StartFaceDetection -> Video) (\s a -> s {video = a} :: StartFaceDetection)
{-# DEPRECATED sfdVideo "Use generic-lens or generic-optics with 'video' instead." #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartFaceDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfdClientRequestToken :: Lens.Lens' StartFaceDetection (Lude.Maybe Lude.Text)
sfdClientRequestToken = Lens.lens (clientRequestToken :: StartFaceDetection -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartFaceDetection)
{-# DEPRECATED sfdClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The face attributes you want returned.
--
-- @DEFAULT@ - The following subset of facial attributes are returned: BoundingBox, Confidence, Pose, Quality and Landmarks.
-- @ALL@ - All facial attributes are returned.
--
-- /Note:/ Consider using 'faceAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfdFaceAttributes :: Lens.Lens' StartFaceDetection (Lude.Maybe FaceAttributes)
sfdFaceAttributes = Lens.lens (faceAttributes :: StartFaceDetection -> Lude.Maybe FaceAttributes) (\s a -> s {faceAttributes = a} :: StartFaceDetection)
{-# DEPRECATED sfdFaceAttributes "Use generic-lens or generic-optics with 'faceAttributes' instead." #-}

instance Lude.AWSRequest StartFaceDetection where
  type Rs StartFaceDetection = StartFaceDetectionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartFaceDetectionResponse'
            Lude.<$> (x Lude..?> "JobId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartFaceDetection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.StartFaceDetection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartFaceDetection where
  toJSON StartFaceDetection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobTag" Lude..=) Lude.<$> jobTag,
            ("NotificationChannel" Lude..=) Lude.<$> notificationChannel,
            Lude.Just ("Video" Lude..= video),
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("FaceAttributes" Lude..=) Lude.<$> faceAttributes
          ]
      )

instance Lude.ToPath StartFaceDetection where
  toPath = Lude.const "/"

instance Lude.ToQuery StartFaceDetection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartFaceDetectionResponse' smart constructor.
data StartFaceDetectionResponse = StartFaceDetectionResponse'
  { -- | The identifier for the face detection job. Use @JobId@ to identify the job in a subsequent call to @GetFaceDetection@ .
    jobId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartFaceDetectionResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier for the face detection job. Use @JobId@ to identify the job in a subsequent call to @GetFaceDetection@ .
-- * 'responseStatus' - The response status code.
mkStartFaceDetectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartFaceDetectionResponse
mkStartFaceDetectionResponse pResponseStatus_ =
  StartFaceDetectionResponse'
    { jobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for the face detection job. Use @JobId@ to identify the job in a subsequent call to @GetFaceDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfdrsJobId :: Lens.Lens' StartFaceDetectionResponse (Lude.Maybe Lude.Text)
sfdrsJobId = Lens.lens (jobId :: StartFaceDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartFaceDetectionResponse)
{-# DEPRECATED sfdrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfdrsResponseStatus :: Lens.Lens' StartFaceDetectionResponse Lude.Int
sfdrsResponseStatus = Lens.lens (responseStatus :: StartFaceDetectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartFaceDetectionResponse)
{-# DEPRECATED sfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
