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
-- Module      : Network.AWS.Rekognition.StartPersonTracking
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous tracking of a person\'s path in a stored video.
--
-- Amazon Rekognition Video can track the path of people in a video stored
-- in an Amazon S3 bucket. Use Video to specify the bucket name and the
-- filename of the video. @StartPersonTracking@ returns a job identifier
-- (@JobId@) which you use to get the results of the operation. When label
-- detection is finished, Amazon Rekognition publishes a completion status
-- to the Amazon Simple Notification Service topic that you specify in
-- @NotificationChannel@.
--
-- To get the results of the person detection operation, first check that
-- the status value published to the Amazon SNS topic is @SUCCEEDED@. If
-- so, call GetPersonTracking and pass the job identifier (@JobId@) from
-- the initial call to @StartPersonTracking@.
module Network.AWS.Rekognition.StartPersonTracking
  ( -- * Creating a Request
    StartPersonTracking (..),
    newStartPersonTracking,

    -- * Request Lenses
    startPersonTracking_notificationChannel,
    startPersonTracking_clientRequestToken,
    startPersonTracking_jobTag,
    startPersonTracking_video,

    -- * Destructuring the Response
    StartPersonTrackingResponse (..),
    newStartPersonTrackingResponse,

    -- * Response Lenses
    startPersonTrackingResponse_jobId,
    startPersonTrackingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartPersonTracking' smart constructor.
data StartPersonTracking = StartPersonTracking'
  { -- | The Amazon SNS topic ARN you want Amazon Rekognition Video to publish
    -- the completion status of the people detection operation to.
    notificationChannel :: Core.Maybe NotificationChannel,
    -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartPersonTracking@ requests, the same @JobId@ is
    -- returned. Use @ClientRequestToken@ to prevent the same job from being
    -- accidently started more than once.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Core.Maybe Core.Text,
    -- | The video in which you want to detect people. The video must be stored
    -- in an Amazon S3 bucket.
    video :: Video
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartPersonTracking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationChannel', 'startPersonTracking_notificationChannel' - The Amazon SNS topic ARN you want Amazon Rekognition Video to publish
-- the completion status of the people detection operation to.
--
-- 'clientRequestToken', 'startPersonTracking_clientRequestToken' - Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartPersonTracking@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
--
-- 'jobTag', 'startPersonTracking_jobTag' - An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
--
-- 'video', 'startPersonTracking_video' - The video in which you want to detect people. The video must be stored
-- in an Amazon S3 bucket.
newStartPersonTracking ::
  -- | 'video'
  Video ->
  StartPersonTracking
newStartPersonTracking pVideo_ =
  StartPersonTracking'
    { notificationChannel =
        Core.Nothing,
      clientRequestToken = Core.Nothing,
      jobTag = Core.Nothing,
      video = pVideo_
    }

-- | The Amazon SNS topic ARN you want Amazon Rekognition Video to publish
-- the completion status of the people detection operation to.
startPersonTracking_notificationChannel :: Lens.Lens' StartPersonTracking (Core.Maybe NotificationChannel)
startPersonTracking_notificationChannel = Lens.lens (\StartPersonTracking' {notificationChannel} -> notificationChannel) (\s@StartPersonTracking' {} a -> s {notificationChannel = a} :: StartPersonTracking)

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartPersonTracking@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
startPersonTracking_clientRequestToken :: Lens.Lens' StartPersonTracking (Core.Maybe Core.Text)
startPersonTracking_clientRequestToken = Lens.lens (\StartPersonTracking' {clientRequestToken} -> clientRequestToken) (\s@StartPersonTracking' {} a -> s {clientRequestToken = a} :: StartPersonTracking)

-- | An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
startPersonTracking_jobTag :: Lens.Lens' StartPersonTracking (Core.Maybe Core.Text)
startPersonTracking_jobTag = Lens.lens (\StartPersonTracking' {jobTag} -> jobTag) (\s@StartPersonTracking' {} a -> s {jobTag = a} :: StartPersonTracking)

-- | The video in which you want to detect people. The video must be stored
-- in an Amazon S3 bucket.
startPersonTracking_video :: Lens.Lens' StartPersonTracking Video
startPersonTracking_video = Lens.lens (\StartPersonTracking' {video} -> video) (\s@StartPersonTracking' {} a -> s {video = a} :: StartPersonTracking)

instance Core.AWSRequest StartPersonTracking where
  type
    AWSResponse StartPersonTracking =
      StartPersonTrackingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPersonTrackingResponse'
            Core.<$> (x Core..?> "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartPersonTracking

instance Core.NFData StartPersonTracking

instance Core.ToHeaders StartPersonTracking where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.StartPersonTracking" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartPersonTracking where
  toJSON StartPersonTracking' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotificationChannel" Core..=)
              Core.<$> notificationChannel,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            ("JobTag" Core..=) Core.<$> jobTag,
            Core.Just ("Video" Core..= video)
          ]
      )

instance Core.ToPath StartPersonTracking where
  toPath = Core.const "/"

instance Core.ToQuery StartPersonTracking where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartPersonTrackingResponse' smart constructor.
data StartPersonTrackingResponse = StartPersonTrackingResponse'
  { -- | The identifier for the person detection job. Use @JobId@ to identify the
    -- job in a subsequent call to @GetPersonTracking@.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartPersonTrackingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startPersonTrackingResponse_jobId' - The identifier for the person detection job. Use @JobId@ to identify the
-- job in a subsequent call to @GetPersonTracking@.
--
-- 'httpStatus', 'startPersonTrackingResponse_httpStatus' - The response's http status code.
newStartPersonTrackingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartPersonTrackingResponse
newStartPersonTrackingResponse pHttpStatus_ =
  StartPersonTrackingResponse'
    { jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the person detection job. Use @JobId@ to identify the
-- job in a subsequent call to @GetPersonTracking@.
startPersonTrackingResponse_jobId :: Lens.Lens' StartPersonTrackingResponse (Core.Maybe Core.Text)
startPersonTrackingResponse_jobId = Lens.lens (\StartPersonTrackingResponse' {jobId} -> jobId) (\s@StartPersonTrackingResponse' {} a -> s {jobId = a} :: StartPersonTrackingResponse)

-- | The response's http status code.
startPersonTrackingResponse_httpStatus :: Lens.Lens' StartPersonTrackingResponse Core.Int
startPersonTrackingResponse_httpStatus = Lens.lens (\StartPersonTrackingResponse' {httpStatus} -> httpStatus) (\s@StartPersonTrackingResponse' {} a -> s {httpStatus = a} :: StartPersonTrackingResponse)

instance Core.NFData StartPersonTrackingResponse
