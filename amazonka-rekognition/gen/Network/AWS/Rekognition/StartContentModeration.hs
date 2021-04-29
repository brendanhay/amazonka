{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartContentModeration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of unsafe content in a stored video.
--
-- Amazon Rekognition Video can moderate content in a video stored in an
-- Amazon S3 bucket. Use Video to specify the bucket name and the filename
-- of the video. @StartContentModeration@ returns a job identifier
-- (@JobId@) which you use to get the results of the analysis. When unsafe
-- content analysis is finished, Amazon Rekognition Video publishes a
-- completion status to the Amazon Simple Notification Service topic that
-- you specify in @NotificationChannel@.
--
-- To get the results of the unsafe content analysis, first check that the
-- status value published to the Amazon SNS topic is @SUCCEEDED@. If so,
-- call GetContentModeration and pass the job identifier (@JobId@) from the
-- initial call to @StartContentModeration@.
--
-- For more information, see Detecting Unsafe Content in the Amazon
-- Rekognition Developer Guide.
module Network.AWS.Rekognition.StartContentModeration
  ( -- * Creating a Request
    StartContentModeration (..),
    newStartContentModeration,

    -- * Request Lenses
    startContentModeration_notificationChannel,
    startContentModeration_minConfidence,
    startContentModeration_clientRequestToken,
    startContentModeration_jobTag,
    startContentModeration_video,

    -- * Destructuring the Response
    StartContentModerationResponse (..),
    newStartContentModerationResponse,

    -- * Response Lenses
    startContentModerationResponse_jobId,
    startContentModerationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartContentModeration' smart constructor.
data StartContentModeration = StartContentModeration'
  { -- | The Amazon SNS topic ARN that you want Amazon Rekognition Video to
    -- publish the completion status of the unsafe content analysis to.
    notificationChannel :: Prelude.Maybe NotificationChannel,
    -- | Specifies the minimum confidence that Amazon Rekognition must have in
    -- order to return a moderated content label. Confidence represents how
    -- certain Amazon Rekognition is that the moderated content is correctly
    -- identified. 0 is the lowest confidence. 100 is the highest confidence.
    -- Amazon Rekognition doesn\'t return any moderated content labels with a
    -- confidence level lower than this specified value. If you don\'t specify
    -- @MinConfidence@, @GetContentModeration@ returns labels with confidence
    -- values greater than or equal to 50 percent.
    minConfidence :: Prelude.Maybe Prelude.Double,
    -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartContentModeration@ requests, the same @JobId@
    -- is returned. Use @ClientRequestToken@ to prevent the same job from being
    -- accidently started more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | The video in which you want to detect unsafe content. The video must be
    -- stored in an Amazon S3 bucket.
    video :: Video
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartContentModeration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationChannel', 'startContentModeration_notificationChannel' - The Amazon SNS topic ARN that you want Amazon Rekognition Video to
-- publish the completion status of the unsafe content analysis to.
--
-- 'minConfidence', 'startContentModeration_minConfidence' - Specifies the minimum confidence that Amazon Rekognition must have in
-- order to return a moderated content label. Confidence represents how
-- certain Amazon Rekognition is that the moderated content is correctly
-- identified. 0 is the lowest confidence. 100 is the highest confidence.
-- Amazon Rekognition doesn\'t return any moderated content labels with a
-- confidence level lower than this specified value. If you don\'t specify
-- @MinConfidence@, @GetContentModeration@ returns labels with confidence
-- values greater than or equal to 50 percent.
--
-- 'clientRequestToken', 'startContentModeration_clientRequestToken' - Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartContentModeration@ requests, the same @JobId@
-- is returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
--
-- 'jobTag', 'startContentModeration_jobTag' - An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
--
-- 'video', 'startContentModeration_video' - The video in which you want to detect unsafe content. The video must be
-- stored in an Amazon S3 bucket.
newStartContentModeration ::
  -- | 'video'
  Video ->
  StartContentModeration
newStartContentModeration pVideo_ =
  StartContentModeration'
    { notificationChannel =
        Prelude.Nothing,
      minConfidence = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      jobTag = Prelude.Nothing,
      video = pVideo_
    }

-- | The Amazon SNS topic ARN that you want Amazon Rekognition Video to
-- publish the completion status of the unsafe content analysis to.
startContentModeration_notificationChannel :: Lens.Lens' StartContentModeration (Prelude.Maybe NotificationChannel)
startContentModeration_notificationChannel = Lens.lens (\StartContentModeration' {notificationChannel} -> notificationChannel) (\s@StartContentModeration' {} a -> s {notificationChannel = a} :: StartContentModeration)

-- | Specifies the minimum confidence that Amazon Rekognition must have in
-- order to return a moderated content label. Confidence represents how
-- certain Amazon Rekognition is that the moderated content is correctly
-- identified. 0 is the lowest confidence. 100 is the highest confidence.
-- Amazon Rekognition doesn\'t return any moderated content labels with a
-- confidence level lower than this specified value. If you don\'t specify
-- @MinConfidence@, @GetContentModeration@ returns labels with confidence
-- values greater than or equal to 50 percent.
startContentModeration_minConfidence :: Lens.Lens' StartContentModeration (Prelude.Maybe Prelude.Double)
startContentModeration_minConfidence = Lens.lens (\StartContentModeration' {minConfidence} -> minConfidence) (\s@StartContentModeration' {} a -> s {minConfidence = a} :: StartContentModeration)

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartContentModeration@ requests, the same @JobId@
-- is returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
startContentModeration_clientRequestToken :: Lens.Lens' StartContentModeration (Prelude.Maybe Prelude.Text)
startContentModeration_clientRequestToken = Lens.lens (\StartContentModeration' {clientRequestToken} -> clientRequestToken) (\s@StartContentModeration' {} a -> s {clientRequestToken = a} :: StartContentModeration)

-- | An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
startContentModeration_jobTag :: Lens.Lens' StartContentModeration (Prelude.Maybe Prelude.Text)
startContentModeration_jobTag = Lens.lens (\StartContentModeration' {jobTag} -> jobTag) (\s@StartContentModeration' {} a -> s {jobTag = a} :: StartContentModeration)

-- | The video in which you want to detect unsafe content. The video must be
-- stored in an Amazon S3 bucket.
startContentModeration_video :: Lens.Lens' StartContentModeration Video
startContentModeration_video = Lens.lens (\StartContentModeration' {video} -> video) (\s@StartContentModeration' {} a -> s {video = a} :: StartContentModeration)

instance Prelude.AWSRequest StartContentModeration where
  type
    Rs StartContentModeration =
      StartContentModerationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartContentModerationResponse'
            Prelude.<$> (x Prelude..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartContentModeration

instance Prelude.NFData StartContentModeration

instance Prelude.ToHeaders StartContentModeration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.StartContentModeration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartContentModeration where
  toJSON StartContentModeration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NotificationChannel" Prelude..=)
              Prelude.<$> notificationChannel,
            ("MinConfidence" Prelude..=)
              Prelude.<$> minConfidence,
            ("ClientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            ("JobTag" Prelude..=) Prelude.<$> jobTag,
            Prelude.Just ("Video" Prelude..= video)
          ]
      )

instance Prelude.ToPath StartContentModeration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartContentModeration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartContentModerationResponse' smart constructor.
data StartContentModerationResponse = StartContentModerationResponse'
  { -- | The identifier for the unsafe content analysis job. Use @JobId@ to
    -- identify the job in a subsequent call to @GetContentModeration@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartContentModerationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startContentModerationResponse_jobId' - The identifier for the unsafe content analysis job. Use @JobId@ to
-- identify the job in a subsequent call to @GetContentModeration@.
--
-- 'httpStatus', 'startContentModerationResponse_httpStatus' - The response's http status code.
newStartContentModerationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartContentModerationResponse
newStartContentModerationResponse pHttpStatus_ =
  StartContentModerationResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the unsafe content analysis job. Use @JobId@ to
-- identify the job in a subsequent call to @GetContentModeration@.
startContentModerationResponse_jobId :: Lens.Lens' StartContentModerationResponse (Prelude.Maybe Prelude.Text)
startContentModerationResponse_jobId = Lens.lens (\StartContentModerationResponse' {jobId} -> jobId) (\s@StartContentModerationResponse' {} a -> s {jobId = a} :: StartContentModerationResponse)

-- | The response's http status code.
startContentModerationResponse_httpStatus :: Lens.Lens' StartContentModerationResponse Prelude.Int
startContentModerationResponse_httpStatus = Lens.lens (\StartContentModerationResponse' {httpStatus} -> httpStatus) (\s@StartContentModerationResponse' {} a -> s {httpStatus = a} :: StartContentModerationResponse)

instance
  Prelude.NFData
    StartContentModerationResponse
