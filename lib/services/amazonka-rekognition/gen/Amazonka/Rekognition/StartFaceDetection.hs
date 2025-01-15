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
-- Module      : Amazonka.Rekognition.StartFaceDetection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- For more information, see Detecting faces in a stored video in the
-- Amazon Rekognition Developer Guide.
module Amazonka.Rekognition.StartFaceDetection
  ( -- * Creating a Request
    StartFaceDetection (..),
    newStartFaceDetection,

    -- * Request Lenses
    startFaceDetection_clientRequestToken,
    startFaceDetection_faceAttributes,
    startFaceDetection_jobTag,
    startFaceDetection_notificationChannel,
    startFaceDetection_video,

    -- * Destructuring the Response
    StartFaceDetectionResponse (..),
    newStartFaceDetectionResponse,

    -- * Response Lenses
    startFaceDetectionResponse_jobId,
    startFaceDetectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartFaceDetection' smart constructor.
data StartFaceDetection = StartFaceDetection'
  { -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartFaceDetection@ requests, the same @JobId@ is
    -- returned. Use @ClientRequestToken@ to prevent the same job from being
    -- accidently started more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The face attributes you want returned.
    --
    -- @DEFAULT@ - The following subset of facial attributes are returned:
    -- BoundingBox, Confidence, Pose, Quality and Landmarks.
    --
    -- @ALL@ - All facial attributes are returned.
    faceAttributes :: Prelude.Maybe FaceAttributes,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
    -- Video to publish the completion status of the face detection operation.
    -- The Amazon SNS topic must have a topic name that begins with
    -- /AmazonRekognition/ if you are using the AmazonRekognitionServiceRole
    -- permissions policy.
    notificationChannel :: Prelude.Maybe NotificationChannel,
    -- | The video in which you want to detect faces. The video must be stored in
    -- an Amazon S3 bucket.
    video :: Video
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFaceDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startFaceDetection_clientRequestToken' - Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartFaceDetection@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
--
-- 'faceAttributes', 'startFaceDetection_faceAttributes' - The face attributes you want returned.
--
-- @DEFAULT@ - The following subset of facial attributes are returned:
-- BoundingBox, Confidence, Pose, Quality and Landmarks.
--
-- @ALL@ - All facial attributes are returned.
--
-- 'jobTag', 'startFaceDetection_jobTag' - An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
--
-- 'notificationChannel', 'startFaceDetection_notificationChannel' - The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the face detection operation.
-- The Amazon SNS topic must have a topic name that begins with
-- /AmazonRekognition/ if you are using the AmazonRekognitionServiceRole
-- permissions policy.
--
-- 'video', 'startFaceDetection_video' - The video in which you want to detect faces. The video must be stored in
-- an Amazon S3 bucket.
newStartFaceDetection ::
  -- | 'video'
  Video ->
  StartFaceDetection
newStartFaceDetection pVideo_ =
  StartFaceDetection'
    { clientRequestToken =
        Prelude.Nothing,
      faceAttributes = Prelude.Nothing,
      jobTag = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      video = pVideo_
    }

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartFaceDetection@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
startFaceDetection_clientRequestToken :: Lens.Lens' StartFaceDetection (Prelude.Maybe Prelude.Text)
startFaceDetection_clientRequestToken = Lens.lens (\StartFaceDetection' {clientRequestToken} -> clientRequestToken) (\s@StartFaceDetection' {} a -> s {clientRequestToken = a} :: StartFaceDetection)

-- | The face attributes you want returned.
--
-- @DEFAULT@ - The following subset of facial attributes are returned:
-- BoundingBox, Confidence, Pose, Quality and Landmarks.
--
-- @ALL@ - All facial attributes are returned.
startFaceDetection_faceAttributes :: Lens.Lens' StartFaceDetection (Prelude.Maybe FaceAttributes)
startFaceDetection_faceAttributes = Lens.lens (\StartFaceDetection' {faceAttributes} -> faceAttributes) (\s@StartFaceDetection' {} a -> s {faceAttributes = a} :: StartFaceDetection)

-- | An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
startFaceDetection_jobTag :: Lens.Lens' StartFaceDetection (Prelude.Maybe Prelude.Text)
startFaceDetection_jobTag = Lens.lens (\StartFaceDetection' {jobTag} -> jobTag) (\s@StartFaceDetection' {} a -> s {jobTag = a} :: StartFaceDetection)

-- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the face detection operation.
-- The Amazon SNS topic must have a topic name that begins with
-- /AmazonRekognition/ if you are using the AmazonRekognitionServiceRole
-- permissions policy.
startFaceDetection_notificationChannel :: Lens.Lens' StartFaceDetection (Prelude.Maybe NotificationChannel)
startFaceDetection_notificationChannel = Lens.lens (\StartFaceDetection' {notificationChannel} -> notificationChannel) (\s@StartFaceDetection' {} a -> s {notificationChannel = a} :: StartFaceDetection)

-- | The video in which you want to detect faces. The video must be stored in
-- an Amazon S3 bucket.
startFaceDetection_video :: Lens.Lens' StartFaceDetection Video
startFaceDetection_video = Lens.lens (\StartFaceDetection' {video} -> video) (\s@StartFaceDetection' {} a -> s {video = a} :: StartFaceDetection)

instance Core.AWSRequest StartFaceDetection where
  type
    AWSResponse StartFaceDetection =
      StartFaceDetectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFaceDetectionResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFaceDetection where
  hashWithSalt _salt StartFaceDetection' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` faceAttributes
      `Prelude.hashWithSalt` jobTag
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` video

instance Prelude.NFData StartFaceDetection where
  rnf StartFaceDetection' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf faceAttributes `Prelude.seq`
        Prelude.rnf jobTag `Prelude.seq`
          Prelude.rnf notificationChannel `Prelude.seq`
            Prelude.rnf video

instance Data.ToHeaders StartFaceDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.StartFaceDetection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartFaceDetection where
  toJSON StartFaceDetection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("FaceAttributes" Data..=)
              Prelude.<$> faceAttributes,
            ("JobTag" Data..=) Prelude.<$> jobTag,
            ("NotificationChannel" Data..=)
              Prelude.<$> notificationChannel,
            Prelude.Just ("Video" Data..= video)
          ]
      )

instance Data.ToPath StartFaceDetection where
  toPath = Prelude.const "/"

instance Data.ToQuery StartFaceDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFaceDetectionResponse' smart constructor.
data StartFaceDetectionResponse = StartFaceDetectionResponse'
  { -- | The identifier for the face detection job. Use @JobId@ to identify the
    -- job in a subsequent call to @GetFaceDetection@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData StartFaceDetectionResponse where
  rnf StartFaceDetectionResponse' {..} =
    Prelude.rnf jobId `Prelude.seq`
      Prelude.rnf httpStatus
