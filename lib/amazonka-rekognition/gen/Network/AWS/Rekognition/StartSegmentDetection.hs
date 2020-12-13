{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartSegmentDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of segment detection in a stored video.
--
-- Amazon Rekognition Video can detect segments in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartSegmentDetection@ returns a job identifier (@JobId@ ) which you use to get the results of the operation. When segment detection is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
-- You can use the @Filters@ ('StartSegmentDetectionFilters' ) input parameter to specify the minimum detection confidence returned in the response. Within @Filters@ , use @ShotFilter@ ('StartShotDetectionFilter' ) to filter detected shots. Use @TechnicalCueFilter@ ('StartTechnicalCueDetectionFilter' ) to filter technical cues.
-- To get the results of the segment detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . if so, call 'GetSegmentDetection' and pass the job identifier (@JobId@ ) from the initial call to @StartSegmentDetection@ .
-- For more information, see Detecting Video Segments in Stored Video in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.StartSegmentDetection
  ( -- * Creating a request
    StartSegmentDetection (..),
    mkStartSegmentDetection,

    -- ** Request lenses
    ssdJobTag,
    ssdSegmentTypes,
    ssdFilters,
    ssdNotificationChannel,
    ssdVideo,
    ssdClientRequestToken,

    -- * Destructuring the response
    StartSegmentDetectionResponse (..),
    mkStartSegmentDetectionResponse,

    -- ** Response lenses
    ssdrsJobId,
    ssdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartSegmentDetection' smart constructor.
data StartSegmentDetection = StartSegmentDetection'
  { -- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
    jobTag :: Lude.Maybe Lude.Text,
    -- | An array of segment types to detect in the video. Valid values are TECHNICAL_CUE and SHOT.
    segmentTypes :: Lude.NonEmpty SegmentType,
    -- | Filters for technical cue or shot detection.
    filters :: Lude.Maybe StartSegmentDetectionFilters,
    -- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the segment detection operation.
    notificationChannel :: Lude.Maybe NotificationChannel,
    video :: Video,
    -- | Idempotent token used to identify the start request. If you use the same token with multiple @StartSegmentDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSegmentDetection' with the minimum fields required to make a request.
--
-- * 'jobTag' - An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
-- * 'segmentTypes' - An array of segment types to detect in the video. Valid values are TECHNICAL_CUE and SHOT.
-- * 'filters' - Filters for technical cue or shot detection.
-- * 'notificationChannel' - The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the segment detection operation.
-- * 'video' -
-- * 'clientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartSegmentDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
mkStartSegmentDetection ::
  -- | 'segmentTypes'
  Lude.NonEmpty SegmentType ->
  -- | 'video'
  Video ->
  StartSegmentDetection
mkStartSegmentDetection pSegmentTypes_ pVideo_ =
  StartSegmentDetection'
    { jobTag = Lude.Nothing,
      segmentTypes = pSegmentTypes_,
      filters = Lude.Nothing,
      notificationChannel = Lude.Nothing,
      video = pVideo_,
      clientRequestToken = Lude.Nothing
    }

-- | An identifier you specify that's returned in the completion notification that's published to your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- /Note:/ Consider using 'jobTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdJobTag :: Lens.Lens' StartSegmentDetection (Lude.Maybe Lude.Text)
ssdJobTag = Lens.lens (jobTag :: StartSegmentDetection -> Lude.Maybe Lude.Text) (\s a -> s {jobTag = a} :: StartSegmentDetection)
{-# DEPRECATED ssdJobTag "Use generic-lens or generic-optics with 'jobTag' instead." #-}

-- | An array of segment types to detect in the video. Valid values are TECHNICAL_CUE and SHOT.
--
-- /Note:/ Consider using 'segmentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSegmentTypes :: Lens.Lens' StartSegmentDetection (Lude.NonEmpty SegmentType)
ssdSegmentTypes = Lens.lens (segmentTypes :: StartSegmentDetection -> Lude.NonEmpty SegmentType) (\s a -> s {segmentTypes = a} :: StartSegmentDetection)
{-# DEPRECATED ssdSegmentTypes "Use generic-lens or generic-optics with 'segmentTypes' instead." #-}

-- | Filters for technical cue or shot detection.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdFilters :: Lens.Lens' StartSegmentDetection (Lude.Maybe StartSegmentDetectionFilters)
ssdFilters = Lens.lens (filters :: StartSegmentDetection -> Lude.Maybe StartSegmentDetectionFilters) (\s a -> s {filters = a} :: StartSegmentDetection)
{-# DEPRECATED ssdFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition Video to publish the completion status of the segment detection operation.
--
-- /Note:/ Consider using 'notificationChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdNotificationChannel :: Lens.Lens' StartSegmentDetection (Lude.Maybe NotificationChannel)
ssdNotificationChannel = Lens.lens (notificationChannel :: StartSegmentDetection -> Lude.Maybe NotificationChannel) (\s a -> s {notificationChannel = a} :: StartSegmentDetection)
{-# DEPRECATED ssdNotificationChannel "Use generic-lens or generic-optics with 'notificationChannel' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdVideo :: Lens.Lens' StartSegmentDetection Video
ssdVideo = Lens.lens (video :: StartSegmentDetection -> Video) (\s a -> s {video = a} :: StartSegmentDetection)
{-# DEPRECATED ssdVideo "Use generic-lens or generic-optics with 'video' instead." #-}

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartSegmentDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdClientRequestToken :: Lens.Lens' StartSegmentDetection (Lude.Maybe Lude.Text)
ssdClientRequestToken = Lens.lens (clientRequestToken :: StartSegmentDetection -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartSegmentDetection)
{-# DEPRECATED ssdClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest StartSegmentDetection where
  type Rs StartSegmentDetection = StartSegmentDetectionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartSegmentDetectionResponse'
            Lude.<$> (x Lude..?> "JobId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartSegmentDetection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.StartSegmentDetection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartSegmentDetection where
  toJSON StartSegmentDetection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobTag" Lude..=) Lude.<$> jobTag,
            Lude.Just ("SegmentTypes" Lude..= segmentTypes),
            ("Filters" Lude..=) Lude.<$> filters,
            ("NotificationChannel" Lude..=) Lude.<$> notificationChannel,
            Lude.Just ("Video" Lude..= video),
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken
          ]
      )

instance Lude.ToPath StartSegmentDetection where
  toPath = Lude.const "/"

instance Lude.ToQuery StartSegmentDetection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartSegmentDetectionResponse' smart constructor.
data StartSegmentDetectionResponse = StartSegmentDetectionResponse'
  { -- | Unique identifier for the segment detection job. The @JobId@ is returned from @StartSegmentDetection@ .
    jobId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSegmentDetectionResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - Unique identifier for the segment detection job. The @JobId@ is returned from @StartSegmentDetection@ .
-- * 'responseStatus' - The response status code.
mkStartSegmentDetectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartSegmentDetectionResponse
mkStartSegmentDetectionResponse pResponseStatus_ =
  StartSegmentDetectionResponse'
    { jobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Unique identifier for the segment detection job. The @JobId@ is returned from @StartSegmentDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdrsJobId :: Lens.Lens' StartSegmentDetectionResponse (Lude.Maybe Lude.Text)
ssdrsJobId = Lens.lens (jobId :: StartSegmentDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartSegmentDetectionResponse)
{-# DEPRECATED ssdrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdrsResponseStatus :: Lens.Lens' StartSegmentDetectionResponse Lude.Int
ssdrsResponseStatus = Lens.lens (responseStatus :: StartSegmentDetectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartSegmentDetectionResponse)
{-# DEPRECATED ssdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
