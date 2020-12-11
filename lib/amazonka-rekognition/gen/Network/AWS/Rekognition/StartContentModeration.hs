{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartContentModeration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of unsafe content in a stored video.
--
-- Amazon Rekognition Video can moderate content in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartContentModeration@ returns a job identifier (@JobId@ ) which you use to get the results of the analysis. When unsafe content analysis is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
-- To get the results of the unsafe content analysis, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetContentModeration' and pass the job identifier (@JobId@ ) from the initial call to @StartContentModeration@ .
-- For more information, see Detecting Unsafe Content in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.StartContentModeration
  ( -- * Creating a request
    StartContentModeration (..),
    mkStartContentModeration,

    -- ** Request lenses
    scmJobTag,
    scmNotificationChannel,
    scmClientRequestToken,
    scmMinConfidence,
    scmVideo,

    -- * Destructuring the response
    StartContentModerationResponse (..),
    mkStartContentModerationResponse,

    -- ** Response lenses
    scmrsJobId,
    scmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartContentModeration' smart constructor.
data StartContentModeration = StartContentModeration'
  { jobTag ::
      Lude.Maybe Lude.Text,
    notificationChannel ::
      Lude.Maybe NotificationChannel,
    clientRequestToken :: Lude.Maybe Lude.Text,
    minConfidence :: Lude.Maybe Lude.Double,
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

-- | Creates a value of 'StartContentModeration' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartContentModeration@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
-- * 'jobTag' - An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
-- * 'minConfidence' - Specifies the minimum confidence that Amazon Rekognition must have in order to return a moderated content label. Confidence represents how certain Amazon Rekognition is that the moderated content is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition doesn't return any moderated content labels with a confidence level lower than this specified value. If you don't specify @MinConfidence@ , @GetContentModeration@ returns labels with confidence values greater than or equal to 50 percent.
-- * 'notificationChannel' - The Amazon SNS topic ARN that you want Amazon Rekognition Video to publish the completion status of the unsafe content analysis to.
-- * 'video' - The video in which you want to detect unsafe content. The video must be stored in an Amazon S3 bucket.
mkStartContentModeration ::
  -- | 'video'
  Video ->
  StartContentModeration
mkStartContentModeration pVideo_ =
  StartContentModeration'
    { jobTag = Lude.Nothing,
      notificationChannel = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      minConfidence = Lude.Nothing,
      video = pVideo_
    }

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmJobTag :: Lens.Lens' StartContentModeration (Lude.Maybe Lude.Text)
scmJobTag = Lens.lens (jobTag :: StartContentModeration -> Lude.Maybe Lude.Text) (\s a -> s {jobTag = a} :: StartContentModeration)
{-# DEPRECATED scmJobTag "Use generic-lens or generic-optics with 'jobTag' instead." #-}

-- | The Amazon SNS topic ARN that you want Amazon Rekognition Video to publish the completion status of the unsafe content analysis to.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmNotificationChannel :: Lens.Lens' StartContentModeration (Lude.Maybe NotificationChannel)
scmNotificationChannel = Lens.lens (notificationChannel :: StartContentModeration -> Lude.Maybe NotificationChannel) (\s a -> s {notificationChannel = a} :: StartContentModeration)
{-# DEPRECATED scmNotificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead." #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartContentModeration@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmClientRequestToken :: Lens.Lens' StartContentModeration (Lude.Maybe Lude.Text)
scmClientRequestToken = Lens.lens (clientRequestToken :: StartContentModeration -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartContentModeration)
{-# DEPRECATED scmClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Specifies the minimum confidence that Amazon Rekognition must have in order to return a moderated content label. Confidence represents how certain Amazon Rekognition is that the moderated content is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition doesn't return any moderated content labels with a confidence level lower than this specified value. If you don't specify @MinConfidence@ , @GetContentModeration@ returns labels with confidence values greater than or equal to 50 percent.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmMinConfidence :: Lens.Lens' StartContentModeration (Lude.Maybe Lude.Double)
scmMinConfidence = Lens.lens (minConfidence :: StartContentModeration -> Lude.Maybe Lude.Double) (\s a -> s {minConfidence = a} :: StartContentModeration)
{-# DEPRECATED scmMinConfidence "Use generic-lens or generic-optics with 'minConfidence' instead." #-}

-- | The video in which you want to detect unsafe content. The video must be stored in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmVideo :: Lens.Lens' StartContentModeration Video
scmVideo = Lens.lens (video :: StartContentModeration -> Video) (\s a -> s {video = a} :: StartContentModeration)
{-# DEPRECATED scmVideo "Use generic-lens or generic-optics with 'video' instead." #-}

instance Lude.AWSRequest StartContentModeration where
  type Rs StartContentModeration = StartContentModerationResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartContentModerationResponse'
            Lude.<$> (x Lude..?> "JobId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartContentModeration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.StartContentModeration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartContentModeration where
  toJSON StartContentModeration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobTag" Lude..=) Lude.<$> jobTag,
            ("NotificationChannel" Lude..=) Lude.<$> notificationChannel,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("MinConfidence" Lude..=) Lude.<$> minConfidence,
            Lude.Just ("Video" Lude..= video)
          ]
      )

instance Lude.ToPath StartContentModeration where
  toPath = Lude.const "/"

instance Lude.ToQuery StartContentModeration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartContentModerationResponse' smart constructor.
data StartContentModerationResponse = StartContentModerationResponse'
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

-- | Creates a value of 'StartContentModerationResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier for the unsafe content analysis job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
-- * 'responseStatus' - The response status code.
mkStartContentModerationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartContentModerationResponse
mkStartContentModerationResponse pResponseStatus_ =
  StartContentModerationResponse'
    { jobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for the unsafe content analysis job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmrsJobId :: Lens.Lens' StartContentModerationResponse (Lude.Maybe Lude.Text)
scmrsJobId = Lens.lens (jobId :: StartContentModerationResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartContentModerationResponse)
{-# DEPRECATED scmrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmrsResponseStatus :: Lens.Lens' StartContentModerationResponse Lude.Int
scmrsResponseStatus = Lens.lens (responseStatus :: StartContentModerationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartContentModerationResponse)
{-# DEPRECATED scmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
