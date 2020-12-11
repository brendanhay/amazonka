{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartPersonTracking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous tracking of a person's path in a stored video.
--
-- Amazon Rekognition Video can track the path of people in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartPersonTracking@ returns a job identifier (@JobId@ ) which you use to get the results of the operation. When label detection is finished, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
-- To get the results of the person detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetPersonTracking' and pass the job identifier (@JobId@ ) from the initial call to @StartPersonTracking@ .
module Network.AWS.Rekognition.StartPersonTracking
  ( -- * Creating a request
    StartPersonTracking (..),
    mkStartPersonTracking,

    -- ** Request lenses
    sptJobTag,
    sptNotificationChannel,
    sptClientRequestToken,
    sptVideo,

    -- * Destructuring the response
    StartPersonTrackingResponse (..),
    mkStartPersonTrackingResponse,

    -- ** Response lenses
    sptrsJobId,
    sptrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartPersonTracking' smart constructor.
data StartPersonTracking = StartPersonTracking'
  { jobTag ::
      Lude.Maybe Lude.Text,
    notificationChannel ::
      Lude.Maybe NotificationChannel,
    clientRequestToken :: Lude.Maybe Lude.Text,
    video :: Video
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartPersonTracking' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartPersonTracking@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
-- * 'jobTag' - An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
-- * 'notificationChannel' - The Amazon SNS topic ARN you want Amazon Rekognition Video to publish the completion status of the people detection operation to.
-- * 'video' - The video in which you want to detect people. The video must be stored in an Amazon S3 bucket.
mkStartPersonTracking ::
  -- | 'video'
  Video ->
  StartPersonTracking
mkStartPersonTracking pVideo_ =
  StartPersonTracking'
    { jobTag = Lude.Nothing,
      notificationChannel = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      video = pVideo_
    }

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptJobTag :: Lens.Lens' StartPersonTracking (Lude.Maybe Lude.Text)
sptJobTag = Lens.lens (jobTag :: StartPersonTracking -> Lude.Maybe Lude.Text) (\s a -> s {jobTag = a} :: StartPersonTracking)
{-# DEPRECATED sptJobTag "Use generic-lens or generic-optics with 'jobTag' instead." #-}

-- | The Amazon SNS topic ARN you want Amazon Rekognition Video to publish the completion status of the people detection operation to.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptNotificationChannel :: Lens.Lens' StartPersonTracking (Lude.Maybe NotificationChannel)
sptNotificationChannel = Lens.lens (notificationChannel :: StartPersonTracking -> Lude.Maybe NotificationChannel) (\s a -> s {notificationChannel = a} :: StartPersonTracking)
{-# DEPRECATED sptNotificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead." #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartPersonTracking@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptClientRequestToken :: Lens.Lens' StartPersonTracking (Lude.Maybe Lude.Text)
sptClientRequestToken = Lens.lens (clientRequestToken :: StartPersonTracking -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartPersonTracking)
{-# DEPRECATED sptClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The video in which you want to detect people. The video must be stored in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptVideo :: Lens.Lens' StartPersonTracking Video
sptVideo = Lens.lens (video :: StartPersonTracking -> Video) (\s a -> s {video = a} :: StartPersonTracking)
{-# DEPRECATED sptVideo "Use generic-lens or generic-optics with 'video' instead." #-}

instance Lude.AWSRequest StartPersonTracking where
  type Rs StartPersonTracking = StartPersonTrackingResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartPersonTrackingResponse'
            Lude.<$> (x Lude..?> "JobId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartPersonTracking where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.StartPersonTracking" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartPersonTracking where
  toJSON StartPersonTracking' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobTag" Lude..=) Lude.<$> jobTag,
            ("NotificationChannel" Lude..=) Lude.<$> notificationChannel,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("Video" Lude..= video)
          ]
      )

instance Lude.ToPath StartPersonTracking where
  toPath = Lude.const "/"

instance Lude.ToQuery StartPersonTracking where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartPersonTrackingResponse' smart constructor.
data StartPersonTrackingResponse = StartPersonTrackingResponse'
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

-- | Creates a value of 'StartPersonTrackingResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier for the person detection job. Use @JobId@ to identify the job in a subsequent call to @GetPersonTracking@ .
-- * 'responseStatus' - The response status code.
mkStartPersonTrackingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartPersonTrackingResponse
mkStartPersonTrackingResponse pResponseStatus_ =
  StartPersonTrackingResponse'
    { jobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for the person detection job. Use @JobId@ to identify the job in a subsequent call to @GetPersonTracking@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptrsJobId :: Lens.Lens' StartPersonTrackingResponse (Lude.Maybe Lude.Text)
sptrsJobId = Lens.lens (jobId :: StartPersonTrackingResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartPersonTrackingResponse)
{-# DEPRECATED sptrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptrsResponseStatus :: Lens.Lens' StartPersonTrackingResponse Lude.Int
sptrsResponseStatus = Lens.lens (responseStatus :: StartPersonTrackingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartPersonTrackingResponse)
{-# DEPRECATED sptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
