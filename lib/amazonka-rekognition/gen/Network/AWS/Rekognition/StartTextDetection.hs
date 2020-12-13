{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartTextDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of text in a stored video.
--
-- Amazon Rekognition Video can detect text in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartTextDetection@ returns a job identifier (@JobId@ ) which you use to get the results of the operation. When text detection is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
-- To get the results of the text detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . if so, call 'GetTextDetection' and pass the job identifier (@JobId@ ) from the initial call to @StartTextDetection@ .
module Network.AWS.Rekognition.StartTextDetection
  ( -- * Creating a request
    StartTextDetection (..),
    mkStartTextDetection,

    -- ** Request lenses
    stdJobTag,
    stdFilters,
    stdNotificationChannel,
    stdVideo,
    stdClientRequestToken,

    -- * Destructuring the response
    StartTextDetectionResponse (..),
    mkStartTextDetectionResponse,

    -- ** Response lenses
    stdrsJobId,
    stdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartTextDetection' smart constructor.
data StartTextDetection = StartTextDetection'
  { -- | An identifier returned in the completion status published by your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
    jobTag :: Lude.Maybe Lude.Text,
    -- | Optional parameters that let you set criteria the text must meet to be included in your response.
    filters :: Lude.Maybe StartTextDetectionFilters,
    notificationChannel :: Lude.Maybe NotificationChannel,
    video :: Video,
    -- | Idempotent token used to identify the start request. If you use the same token with multiple @StartTextDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidentaly started more than once.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTextDetection' with the minimum fields required to make a request.
--
-- * 'jobTag' - An identifier returned in the completion status published by your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
-- * 'filters' - Optional parameters that let you set criteria the text must meet to be included in your response.
-- * 'notificationChannel' -
-- * 'video' -
-- * 'clientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartTextDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidentaly started more than once.
mkStartTextDetection ::
  -- | 'video'
  Video ->
  StartTextDetection
mkStartTextDetection pVideo_ =
  StartTextDetection'
    { jobTag = Lude.Nothing,
      filters = Lude.Nothing,
      notificationChannel = Lude.Nothing,
      video = pVideo_,
      clientRequestToken = Lude.Nothing
    }

-- | An identifier returned in the completion status published by your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdJobTag :: Lens.Lens' StartTextDetection (Lude.Maybe Lude.Text)
stdJobTag = Lens.lens (jobTag :: StartTextDetection -> Lude.Maybe Lude.Text) (\s a -> s {jobTag = a} :: StartTextDetection)
{-# DEPRECATED stdJobTag "Use generic-lens or generic-optics with 'jobTag' instead." #-}

-- | Optional parameters that let you set criteria the text must meet to be included in your response.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdFilters :: Lens.Lens' StartTextDetection (Lude.Maybe StartTextDetectionFilters)
stdFilters = Lens.lens (filters :: StartTextDetection -> Lude.Maybe StartTextDetectionFilters) (\s a -> s {filters = a} :: StartTextDetection)
{-# DEPRECATED stdFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdNotificationChannel :: Lens.Lens' StartTextDetection (Lude.Maybe NotificationChannel)
stdNotificationChannel = Lens.lens (notificationChannel :: StartTextDetection -> Lude.Maybe NotificationChannel) (\s a -> s {notificationChannel = a} :: StartTextDetection)
{-# DEPRECATED stdNotificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdVideo :: Lens.Lens' StartTextDetection Video
stdVideo = Lens.lens (video :: StartTextDetection -> Video) (\s a -> s {video = a} :: StartTextDetection)
{-# DEPRECATED stdVideo "Use generic-lens or generic-optics with 'video' instead." #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartTextDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidentaly started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdClientRequestToken :: Lens.Lens' StartTextDetection (Lude.Maybe Lude.Text)
stdClientRequestToken = Lens.lens (clientRequestToken :: StartTextDetection -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartTextDetection)
{-# DEPRECATED stdClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest StartTextDetection where
  type Rs StartTextDetection = StartTextDetectionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartTextDetectionResponse'
            Lude.<$> (x Lude..?> "JobId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartTextDetection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.StartTextDetection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartTextDetection where
  toJSON StartTextDetection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobTag" Lude..=) Lude.<$> jobTag,
            ("Filters" Lude..=) Lude.<$> filters,
            ("NotificationChannel" Lude..=) Lude.<$> notificationChannel,
            Lude.Just ("Video" Lude..= video),
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken
          ]
      )

instance Lude.ToPath StartTextDetection where
  toPath = Lude.const "/"

instance Lude.ToQuery StartTextDetection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartTextDetectionResponse' smart constructor.
data StartTextDetectionResponse = StartTextDetectionResponse'
  { -- | Identifier for the text detection job. Use @JobId@ to identify the job in a subsequent call to @GetTextDetection@ .
    jobId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTextDetectionResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - Identifier for the text detection job. Use @JobId@ to identify the job in a subsequent call to @GetTextDetection@ .
-- * 'responseStatus' - The response status code.
mkStartTextDetectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartTextDetectionResponse
mkStartTextDetectionResponse pResponseStatus_ =
  StartTextDetectionResponse'
    { jobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifier for the text detection job. Use @JobId@ to identify the job in a subsequent call to @GetTextDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdrsJobId :: Lens.Lens' StartTextDetectionResponse (Lude.Maybe Lude.Text)
stdrsJobId = Lens.lens (jobId :: StartTextDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartTextDetectionResponse)
{-# DEPRECATED stdrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdrsResponseStatus :: Lens.Lens' StartTextDetectionResponse Lude.Int
stdrsResponseStatus = Lens.lens (responseStatus :: StartTextDetectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartTextDetectionResponse)
{-# DEPRECATED stdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
