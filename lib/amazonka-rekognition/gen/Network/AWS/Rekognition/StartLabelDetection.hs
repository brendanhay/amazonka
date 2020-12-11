{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartLabelDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of labels in a stored video.
--
-- Amazon Rekognition Video can detect labels in a video. Labels are instances of real-world entities. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; concepts like landscape, evening, and nature; and activities like a person getting out of a car or a person skiing.
-- The video must be stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartLabelDetection@ returns a job identifier (@JobId@ ) which you use to get the results of the operation. When label detection is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
-- To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetLabelDetection' and pass the job identifier (@JobId@ ) from the initial call to @StartLabelDetection@ .
module Network.AWS.Rekognition.StartLabelDetection
  ( -- * Creating a request
    StartLabelDetection (..),
    mkStartLabelDetection,

    -- ** Request lenses
    sldJobTag,
    sldNotificationChannel,
    sldClientRequestToken,
    sldMinConfidence,
    sldVideo,

    -- * Destructuring the response
    StartLabelDetectionResponse (..),
    mkStartLabelDetectionResponse,

    -- ** Response lenses
    sldrsJobId,
    sldrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartLabelDetection' smart constructor.
data StartLabelDetection = StartLabelDetection'
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

-- | Creates a value of 'StartLabelDetection' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartLabelDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
-- * 'jobTag' - An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
-- * 'minConfidence' - Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected label. Confidence represents how certain Amazon Rekognition is that a label is correctly identified.0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any labels with a confidence level lower than this specified value.
--
-- If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
-- * 'notificationChannel' - The Amazon SNS topic ARN you want Amazon Rekognition Video to publish the completion status of the label detection operation to.
-- * 'video' - The video in which you want to detect labels. The video must be stored in an Amazon S3 bucket.
mkStartLabelDetection ::
  -- | 'video'
  Video ->
  StartLabelDetection
mkStartLabelDetection pVideo_ =
  StartLabelDetection'
    { jobTag = Lude.Nothing,
      notificationChannel = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      minConfidence = Lude.Nothing,
      video = pVideo_
    }

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldJobTag :: Lens.Lens' StartLabelDetection (Lude.Maybe Lude.Text)
sldJobTag = Lens.lens (jobTag :: StartLabelDetection -> Lude.Maybe Lude.Text) (\s a -> s {jobTag = a} :: StartLabelDetection)
{-# DEPRECATED sldJobTag "Use generic-lens or generic-optics with 'jobTag' instead." #-}

-- | The Amazon SNS topic ARN you want Amazon Rekognition Video to publish the completion status of the label detection operation to.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldNotificationChannel :: Lens.Lens' StartLabelDetection (Lude.Maybe NotificationChannel)
sldNotificationChannel = Lens.lens (notificationChannel :: StartLabelDetection -> Lude.Maybe NotificationChannel) (\s a -> s {notificationChannel = a} :: StartLabelDetection)
{-# DEPRECATED sldNotificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead." #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartLabelDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldClientRequestToken :: Lens.Lens' StartLabelDetection (Lude.Maybe Lude.Text)
sldClientRequestToken = Lens.lens (clientRequestToken :: StartLabelDetection -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartLabelDetection)
{-# DEPRECATED sldClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Specifies the minimum confidence that Amazon Rekognition Video must have in order to return a detected label. Confidence represents how certain Amazon Rekognition is that a label is correctly identified.0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition Video doesn't return any labels with a confidence level lower than this specified value.
--
-- If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
--
-- /Note:/ Consider using 'minConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldMinConfidence :: Lens.Lens' StartLabelDetection (Lude.Maybe Lude.Double)
sldMinConfidence = Lens.lens (minConfidence :: StartLabelDetection -> Lude.Maybe Lude.Double) (\s a -> s {minConfidence = a} :: StartLabelDetection)
{-# DEPRECATED sldMinConfidence "Use generic-lens or generic-optics with 'minConfidence' instead." #-}

-- | The video in which you want to detect labels. The video must be stored in an Amazon S3 bucket.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldVideo :: Lens.Lens' StartLabelDetection Video
sldVideo = Lens.lens (video :: StartLabelDetection -> Video) (\s a -> s {video = a} :: StartLabelDetection)
{-# DEPRECATED sldVideo "Use generic-lens or generic-optics with 'video' instead." #-}

instance Lude.AWSRequest StartLabelDetection where
  type Rs StartLabelDetection = StartLabelDetectionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartLabelDetectionResponse'
            Lude.<$> (x Lude..?> "JobId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartLabelDetection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.StartLabelDetection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartLabelDetection where
  toJSON StartLabelDetection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobTag" Lude..=) Lude.<$> jobTag,
            ("NotificationChannel" Lude..=) Lude.<$> notificationChannel,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("MinConfidence" Lude..=) Lude.<$> minConfidence,
            Lude.Just ("Video" Lude..= video)
          ]
      )

instance Lude.ToPath StartLabelDetection where
  toPath = Lude.const "/"

instance Lude.ToQuery StartLabelDetection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartLabelDetectionResponse' smart constructor.
data StartLabelDetectionResponse = StartLabelDetectionResponse'
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

-- | Creates a value of 'StartLabelDetectionResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier for the label detection job. Use @JobId@ to identify the job in a subsequent call to @GetLabelDetection@ .
-- * 'responseStatus' - The response status code.
mkStartLabelDetectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartLabelDetectionResponse
mkStartLabelDetectionResponse pResponseStatus_ =
  StartLabelDetectionResponse'
    { jobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for the label detection job. Use @JobId@ to identify the job in a subsequent call to @GetLabelDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldrsJobId :: Lens.Lens' StartLabelDetectionResponse (Lude.Maybe Lude.Text)
sldrsJobId = Lens.lens (jobId :: StartLabelDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartLabelDetectionResponse)
{-# DEPRECATED sldrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sldrsResponseStatus :: Lens.Lens' StartLabelDetectionResponse Lude.Int
sldrsResponseStatus = Lens.lens (responseStatus :: StartLabelDetectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartLabelDetectionResponse)
{-# DEPRECATED sldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
