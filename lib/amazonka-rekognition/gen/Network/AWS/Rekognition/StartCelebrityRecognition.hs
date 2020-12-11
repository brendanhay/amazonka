{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartCelebrityRecognition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous recognition of celebrities in a stored video.
--
-- Amazon Rekognition Video can detect celebrities in a video must be stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartCelebrityRecognition@ returns a job identifier (@JobId@ ) which you use to get the results of the analysis. When celebrity recognition analysis is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ . To get the results of the celebrity recognition analysis, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetCelebrityRecognition' and pass the job identifier (@JobId@ ) from the initial call to @StartCelebrityRecognition@ .
-- For more information, see Recognizing Celebrities in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.StartCelebrityRecognition
  ( -- * Creating a request
    StartCelebrityRecognition (..),
    mkStartCelebrityRecognition,

    -- ** Request lenses
    scrJobTag,
    scrNotificationChannel,
    scrClientRequestToken,
    scrVideo,

    -- * Destructuring the response
    StartCelebrityRecognitionResponse (..),
    mkStartCelebrityRecognitionResponse,

    -- ** Response lenses
    scrrsJobId,
    scrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartCelebrityRecognition' smart constructor.
data StartCelebrityRecognition = StartCelebrityRecognition'
  { jobTag ::
      Lude.Maybe Lude.Text,
    notificationChannel ::
      Lude.Maybe NotificationChannel,
    clientRequestToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'StartCelebrityRecognition' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartCelebrityRecognition@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
-- * 'jobTag' - An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
-- * 'notificationChannel' - The Amazon SNS topic ARN that you want Amazon Rekognition Video to publish the completion status of the celebrity recognition analysis to.
-- * 'video' - The video in which you want to recognize celebrities. The video must be stored in an Amazon S3 bucket.
mkStartCelebrityRecognition ::
  -- | 'video'
  Video ->
  StartCelebrityRecognition
mkStartCelebrityRecognition pVideo_ =
  StartCelebrityRecognition'
    { jobTag = Lude.Nothing,
      notificationChannel = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      video = pVideo_
    }

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrJobTag :: Lens.Lens' StartCelebrityRecognition (Lude.Maybe Lude.Text)
scrJobTag = Lens.lens (jobTag :: StartCelebrityRecognition -> Lude.Maybe Lude.Text) (\s a -> s {jobTag = a} :: StartCelebrityRecognition)
{-# DEPRECATED scrJobTag "Use generic-lens or generic-optics with 'jobTag' instead." #-}

-- | The Amazon SNS topic ARN that you want Amazon Rekognition Video to publish the completion status of the celebrity recognition analysis to.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrNotificationChannel :: Lens.Lens' StartCelebrityRecognition (Lude.Maybe NotificationChannel)
scrNotificationChannel = Lens.lens (notificationChannel :: StartCelebrityRecognition -> Lude.Maybe NotificationChannel) (\s a -> s {notificationChannel = a} :: StartCelebrityRecognition)
{-# DEPRECATED scrNotificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead." #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartCelebrityRecognition@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrClientRequestToken :: Lens.Lens' StartCelebrityRecognition (Lude.Maybe Lude.Text)
scrClientRequestToken = Lens.lens (clientRequestToken :: StartCelebrityRecognition -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartCelebrityRecognition)
{-# DEPRECATED scrClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The video in which you want to recognize celebrities. The video must be stored in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrVideo :: Lens.Lens' StartCelebrityRecognition Video
scrVideo = Lens.lens (video :: StartCelebrityRecognition -> Video) (\s a -> s {video = a} :: StartCelebrityRecognition)
{-# DEPRECATED scrVideo "Use generic-lens or generic-optics with 'video' instead." #-}

instance Lude.AWSRequest StartCelebrityRecognition where
  type
    Rs StartCelebrityRecognition =
      StartCelebrityRecognitionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartCelebrityRecognitionResponse'
            Lude.<$> (x Lude..?> "JobId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartCelebrityRecognition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "RekognitionService.StartCelebrityRecognition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartCelebrityRecognition where
  toJSON StartCelebrityRecognition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobTag" Lude..=) Lude.<$> jobTag,
            ("NotificationChannel" Lude..=) Lude.<$> notificationChannel,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("Video" Lude..= video)
          ]
      )

instance Lude.ToPath StartCelebrityRecognition where
  toPath = Lude.const "/"

instance Lude.ToQuery StartCelebrityRecognition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartCelebrityRecognitionResponse' smart constructor.
data StartCelebrityRecognitionResponse = StartCelebrityRecognitionResponse'
  { jobId ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartCelebrityRecognitionResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier for the celebrity recognition analysis job. Use @JobId@ to identify the job in a subsequent call to @GetCelebrityRecognition@ .
-- * 'responseStatus' - The response status code.
mkStartCelebrityRecognitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartCelebrityRecognitionResponse
mkStartCelebrityRecognitionResponse pResponseStatus_ =
  StartCelebrityRecognitionResponse'
    { jobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for the celebrity recognition analysis job. Use @JobId@ to identify the job in a subsequent call to @GetCelebrityRecognition@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsJobId :: Lens.Lens' StartCelebrityRecognitionResponse (Lude.Maybe Lude.Text)
scrrsJobId = Lens.lens (jobId :: StartCelebrityRecognitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartCelebrityRecognitionResponse)
{-# DEPRECATED scrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsResponseStatus :: Lens.Lens' StartCelebrityRecognitionResponse Lude.Int
scrrsResponseStatus = Lens.lens (responseStatus :: StartCelebrityRecognitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartCelebrityRecognitionResponse)
{-# DEPRECATED scrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
