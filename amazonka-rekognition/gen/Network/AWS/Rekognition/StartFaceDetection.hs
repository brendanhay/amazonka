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
-- Module      : Network.AWS.Rekognition.StartFaceDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of faces in a stored video.
--
-- Amazon Rekognition Video can detect faces in a video stored in an Amazon
-- S3 bucket. Use Video to specify the bucket name and the filename of the
-- video. @StartFaceDetection@ returns a job identifier (@JobId@) that you
-- use to get the results of the operation. When face detection is
-- finished, Amazon Rekognition Video publishes a completion status to the
-- Amazon Simple Notification Service topic that you specify in
-- @NotificationChannel@. To get the results of the face detection
-- operation, first check that the status value published to the Amazon SNS
-- topic is @SUCCEEDED@. If so, call GetFaceDetection and pass the job
-- identifier (@JobId@) from the initial call to @StartFaceDetection@.
--
-- For more information, see Detecting Faces in a Stored Video in the
-- Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.StartFaceDetection
  ( -- * Creating a Request
    StartFaceDetection (..),
    newStartFaceDetection,

    -- * Request Lenses
    startFaceDetection_notificationChannel,
    startFaceDetection_faceAttributes,
    startFaceDetection_clientRequestToken,
    startFaceDetection_jobTag,
    startFaceDetection_video,

    -- * Destructuring the Response
    StartFaceDetectionResponse (..),
    newStartFaceDetectionResponse,

    -- * Response Lenses
    startFaceDetectionResponse_jobId,
    startFaceDetectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartFaceDetection' smart constructor.
data StartFaceDetection = StartFaceDetection'
  { -- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
    -- Video to publish the completion status of the face detection operation.
    notificationChannel :: Prelude.Maybe NotificationChannel,
    -- | The face attributes you want returned.
    --
    -- @DEFAULT@ - The following subset of facial attributes are returned:
    -- BoundingBox, Confidence, Pose, Quality and Landmarks.
    --
    -- @ALL@ - All facial attributes are returned.
    faceAttributes :: Prelude.Maybe FaceAttributes,
    -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartFaceDetection@ requests, the same @JobId@ is
    -- returned. Use @ClientRequestToken@ to prevent the same job from being
    -- accidently started more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | The video in which you want to detect faces. The video must be stored in
    -- an Amazon S3 bucket.
    video :: Video
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartFaceDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationChannel', 'startFaceDetection_notificationChannel' - The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the face detection operation.
--
-- 'faceAttributes', 'startFaceDetection_faceAttributes' - The face attributes you want returned.
--
-- @DEFAULT@ - The following subset of facial attributes are returned:
-- BoundingBox, Confidence, Pose, Quality and Landmarks.
--
-- @ALL@ - All facial attributes are returned.
--
-- 'clientRequestToken', 'startFaceDetection_clientRequestToken' - Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartFaceDetection@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
--
-- 'jobTag', 'startFaceDetection_jobTag' - An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
--
-- 'video', 'startFaceDetection_video' - The video in which you want to detect faces. The video must be stored in
-- an Amazon S3 bucket.
newStartFaceDetection ::
  -- | 'video'
  Video ->
  StartFaceDetection
newStartFaceDetection pVideo_ =
  StartFaceDetection'
    { notificationChannel =
        Prelude.Nothing,
      faceAttributes = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      jobTag = Prelude.Nothing,
      video = pVideo_
    }

-- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the face detection operation.
startFaceDetection_notificationChannel :: Lens.Lens' StartFaceDetection (Prelude.Maybe NotificationChannel)
startFaceDetection_notificationChannel = Lens.lens (\StartFaceDetection' {notificationChannel} -> notificationChannel) (\s@StartFaceDetection' {} a -> s {notificationChannel = a} :: StartFaceDetection)

-- | The face attributes you want returned.
--
-- @DEFAULT@ - The following subset of facial attributes are returned:
-- BoundingBox, Confidence, Pose, Quality and Landmarks.
--
-- @ALL@ - All facial attributes are returned.
startFaceDetection_faceAttributes :: Lens.Lens' StartFaceDetection (Prelude.Maybe FaceAttributes)
startFaceDetection_faceAttributes = Lens.lens (\StartFaceDetection' {faceAttributes} -> faceAttributes) (\s@StartFaceDetection' {} a -> s {faceAttributes = a} :: StartFaceDetection)

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartFaceDetection@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
startFaceDetection_clientRequestToken :: Lens.Lens' StartFaceDetection (Prelude.Maybe Prelude.Text)
startFaceDetection_clientRequestToken = Lens.lens (\StartFaceDetection' {clientRequestToken} -> clientRequestToken) (\s@StartFaceDetection' {} a -> s {clientRequestToken = a} :: StartFaceDetection)

-- | An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
startFaceDetection_jobTag :: Lens.Lens' StartFaceDetection (Prelude.Maybe Prelude.Text)
startFaceDetection_jobTag = Lens.lens (\StartFaceDetection' {jobTag} -> jobTag) (\s@StartFaceDetection' {} a -> s {jobTag = a} :: StartFaceDetection)

-- | The video in which you want to detect faces. The video must be stored in
-- an Amazon S3 bucket.
startFaceDetection_video :: Lens.Lens' StartFaceDetection Video
startFaceDetection_video = Lens.lens (\StartFaceDetection' {video} -> video) (\s@StartFaceDetection' {} a -> s {video = a} :: StartFaceDetection)

instance Prelude.AWSRequest StartFaceDetection where
  type
    Rs StartFaceDetection =
      StartFaceDetectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFaceDetectionResponse'
            Prelude.<$> (x Prelude..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFaceDetection

instance Prelude.NFData StartFaceDetection

instance Prelude.ToHeaders StartFaceDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.StartFaceDetection" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartFaceDetection where
  toJSON StartFaceDetection' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NotificationChannel" Prelude..=)
              Prelude.<$> notificationChannel,
            ("FaceAttributes" Prelude..=)
              Prelude.<$> faceAttributes,
            ("ClientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            ("JobTag" Prelude..=) Prelude.<$> jobTag,
            Prelude.Just ("Video" Prelude..= video)
          ]
      )

instance Prelude.ToPath StartFaceDetection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartFaceDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFaceDetectionResponse' smart constructor.
data StartFaceDetectionResponse = StartFaceDetectionResponse'
  { -- | The identifier for the face detection job. Use @JobId@ to identify the
    -- job in a subsequent call to @GetFaceDetection@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartFaceDetectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startFaceDetectionResponse_jobId' - The identifier for the face detection job. Use @JobId@ to identify the
-- job in a subsequent call to @GetFaceDetection@.
--
-- 'httpStatus', 'startFaceDetectionResponse_httpStatus' - The response's http status code.
newStartFaceDetectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartFaceDetectionResponse
newStartFaceDetectionResponse pHttpStatus_ =
  StartFaceDetectionResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the face detection job. Use @JobId@ to identify the
-- job in a subsequent call to @GetFaceDetection@.
startFaceDetectionResponse_jobId :: Lens.Lens' StartFaceDetectionResponse (Prelude.Maybe Prelude.Text)
startFaceDetectionResponse_jobId = Lens.lens (\StartFaceDetectionResponse' {jobId} -> jobId) (\s@StartFaceDetectionResponse' {} a -> s {jobId = a} :: StartFaceDetectionResponse)

-- | The response's http status code.
startFaceDetectionResponse_httpStatus :: Lens.Lens' StartFaceDetectionResponse Prelude.Int
startFaceDetectionResponse_httpStatus = Lens.lens (\StartFaceDetectionResponse' {httpStatus} -> httpStatus) (\s@StartFaceDetectionResponse' {} a -> s {httpStatus = a} :: StartFaceDetectionResponse)

instance Prelude.NFData StartFaceDetectionResponse
