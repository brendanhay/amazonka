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
-- Module      : Amazonka.Rekognition.StartLabelDetection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of labels in a stored video.
--
-- Amazon Rekognition Video can detect labels in a video. Labels are
-- instances of real-world entities. This includes objects like flower,
-- tree, and table; events like wedding, graduation, and birthday party;
-- concepts like landscape, evening, and nature; and activities like a
-- person getting out of a car or a person skiing.
--
-- The video must be stored in an Amazon S3 bucket. Use Video to specify
-- the bucket name and the filename of the video. @StartLabelDetection@
-- returns a job identifier (@JobId@) which you use to get the results of
-- the operation. When label detection is finished, Amazon Rekognition
-- Video publishes a completion status to the Amazon Simple Notification
-- Service topic that you specify in @NotificationChannel@.
--
-- To get the results of the label detection operation, first check that
-- the status value published to the Amazon SNS topic is @SUCCEEDED@. If
-- so, call GetLabelDetection and pass the job identifier (@JobId@) from
-- the initial call to @StartLabelDetection@.
module Amazonka.Rekognition.StartLabelDetection
  ( -- * Creating a Request
    StartLabelDetection (..),
    newStartLabelDetection,

    -- * Request Lenses
    startLabelDetection_clientRequestToken,
    startLabelDetection_minConfidence,
    startLabelDetection_jobTag,
    startLabelDetection_notificationChannel,
    startLabelDetection_video,

    -- * Destructuring the Response
    StartLabelDetectionResponse (..),
    newStartLabelDetectionResponse,

    -- * Response Lenses
    startLabelDetectionResponse_jobId,
    startLabelDetectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartLabelDetection' smart constructor.
data StartLabelDetection = StartLabelDetection'
  { -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartLabelDetection@ requests, the same @JobId@ is
    -- returned. Use @ClientRequestToken@ to prevent the same job from being
    -- accidently started more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the minimum confidence that Amazon Rekognition Video must have
    -- in order to return a detected label. Confidence represents how certain
    -- Amazon Rekognition is that a label is correctly identified.0 is the
    -- lowest confidence. 100 is the highest confidence. Amazon Rekognition
    -- Video doesn\'t return any labels with a confidence level lower than this
    -- specified value.
    --
    -- If you don\'t specify @MinConfidence@, the operation returns labels with
    -- confidence values greater than or equal to 50 percent.
    minConfidence :: Prelude.Maybe Prelude.Double,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SNS topic ARN you want Amazon Rekognition Video to publish
    -- the completion status of the label detection operation to. The Amazon
    -- SNS topic must have a topic name that begins with /AmazonRekognition/ if
    -- you are using the AmazonRekognitionServiceRole permissions policy.
    notificationChannel :: Prelude.Maybe NotificationChannel,
    -- | The video in which you want to detect labels. The video must be stored
    -- in an Amazon S3 bucket.
    video :: Video
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartLabelDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startLabelDetection_clientRequestToken' - Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartLabelDetection@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
--
-- 'minConfidence', 'startLabelDetection_minConfidence' - Specifies the minimum confidence that Amazon Rekognition Video must have
-- in order to return a detected label. Confidence represents how certain
-- Amazon Rekognition is that a label is correctly identified.0 is the
-- lowest confidence. 100 is the highest confidence. Amazon Rekognition
-- Video doesn\'t return any labels with a confidence level lower than this
-- specified value.
--
-- If you don\'t specify @MinConfidence@, the operation returns labels with
-- confidence values greater than or equal to 50 percent.
--
-- 'jobTag', 'startLabelDetection_jobTag' - An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
--
-- 'notificationChannel', 'startLabelDetection_notificationChannel' - The Amazon SNS topic ARN you want Amazon Rekognition Video to publish
-- the completion status of the label detection operation to. The Amazon
-- SNS topic must have a topic name that begins with /AmazonRekognition/ if
-- you are using the AmazonRekognitionServiceRole permissions policy.
--
-- 'video', 'startLabelDetection_video' - The video in which you want to detect labels. The video must be stored
-- in an Amazon S3 bucket.
newStartLabelDetection ::
  -- | 'video'
  Video ->
  StartLabelDetection
newStartLabelDetection pVideo_ =
  StartLabelDetection'
    { clientRequestToken =
        Prelude.Nothing,
      minConfidence = Prelude.Nothing,
      jobTag = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      video = pVideo_
    }

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartLabelDetection@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
startLabelDetection_clientRequestToken :: Lens.Lens' StartLabelDetection (Prelude.Maybe Prelude.Text)
startLabelDetection_clientRequestToken = Lens.lens (\StartLabelDetection' {clientRequestToken} -> clientRequestToken) (\s@StartLabelDetection' {} a -> s {clientRequestToken = a} :: StartLabelDetection)

-- | Specifies the minimum confidence that Amazon Rekognition Video must have
-- in order to return a detected label. Confidence represents how certain
-- Amazon Rekognition is that a label is correctly identified.0 is the
-- lowest confidence. 100 is the highest confidence. Amazon Rekognition
-- Video doesn\'t return any labels with a confidence level lower than this
-- specified value.
--
-- If you don\'t specify @MinConfidence@, the operation returns labels with
-- confidence values greater than or equal to 50 percent.
startLabelDetection_minConfidence :: Lens.Lens' StartLabelDetection (Prelude.Maybe Prelude.Double)
startLabelDetection_minConfidence = Lens.lens (\StartLabelDetection' {minConfidence} -> minConfidence) (\s@StartLabelDetection' {} a -> s {minConfidence = a} :: StartLabelDetection)

-- | An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
startLabelDetection_jobTag :: Lens.Lens' StartLabelDetection (Prelude.Maybe Prelude.Text)
startLabelDetection_jobTag = Lens.lens (\StartLabelDetection' {jobTag} -> jobTag) (\s@StartLabelDetection' {} a -> s {jobTag = a} :: StartLabelDetection)

-- | The Amazon SNS topic ARN you want Amazon Rekognition Video to publish
-- the completion status of the label detection operation to. The Amazon
-- SNS topic must have a topic name that begins with /AmazonRekognition/ if
-- you are using the AmazonRekognitionServiceRole permissions policy.
startLabelDetection_notificationChannel :: Lens.Lens' StartLabelDetection (Prelude.Maybe NotificationChannel)
startLabelDetection_notificationChannel = Lens.lens (\StartLabelDetection' {notificationChannel} -> notificationChannel) (\s@StartLabelDetection' {} a -> s {notificationChannel = a} :: StartLabelDetection)

-- | The video in which you want to detect labels. The video must be stored
-- in an Amazon S3 bucket.
startLabelDetection_video :: Lens.Lens' StartLabelDetection Video
startLabelDetection_video = Lens.lens (\StartLabelDetection' {video} -> video) (\s@StartLabelDetection' {} a -> s {video = a} :: StartLabelDetection)

instance Core.AWSRequest StartLabelDetection where
  type
    AWSResponse StartLabelDetection =
      StartLabelDetectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartLabelDetectionResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartLabelDetection where
  hashWithSalt _salt StartLabelDetection' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` minConfidence
      `Prelude.hashWithSalt` jobTag
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` video

instance Prelude.NFData StartLabelDetection where
  rnf StartLabelDetection' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf minConfidence
      `Prelude.seq` Prelude.rnf jobTag
      `Prelude.seq` Prelude.rnf notificationChannel
      `Prelude.seq` Prelude.rnf video

instance Data.ToHeaders StartLabelDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.StartLabelDetection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartLabelDetection where
  toJSON StartLabelDetection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("MinConfidence" Data..=) Prelude.<$> minConfidence,
            ("JobTag" Data..=) Prelude.<$> jobTag,
            ("NotificationChannel" Data..=)
              Prelude.<$> notificationChannel,
            Prelude.Just ("Video" Data..= video)
          ]
      )

instance Data.ToPath StartLabelDetection where
  toPath = Prelude.const "/"

instance Data.ToQuery StartLabelDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartLabelDetectionResponse' smart constructor.
data StartLabelDetectionResponse = StartLabelDetectionResponse'
  { -- | The identifier for the label detection job. Use @JobId@ to identify the
    -- job in a subsequent call to @GetLabelDetection@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartLabelDetectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startLabelDetectionResponse_jobId' - The identifier for the label detection job. Use @JobId@ to identify the
-- job in a subsequent call to @GetLabelDetection@.
--
-- 'httpStatus', 'startLabelDetectionResponse_httpStatus' - The response's http status code.
newStartLabelDetectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartLabelDetectionResponse
newStartLabelDetectionResponse pHttpStatus_ =
  StartLabelDetectionResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the label detection job. Use @JobId@ to identify the
-- job in a subsequent call to @GetLabelDetection@.
startLabelDetectionResponse_jobId :: Lens.Lens' StartLabelDetectionResponse (Prelude.Maybe Prelude.Text)
startLabelDetectionResponse_jobId = Lens.lens (\StartLabelDetectionResponse' {jobId} -> jobId) (\s@StartLabelDetectionResponse' {} a -> s {jobId = a} :: StartLabelDetectionResponse)

-- | The response's http status code.
startLabelDetectionResponse_httpStatus :: Lens.Lens' StartLabelDetectionResponse Prelude.Int
startLabelDetectionResponse_httpStatus = Lens.lens (\StartLabelDetectionResponse' {httpStatus} -> httpStatus) (\s@StartLabelDetectionResponse' {} a -> s {httpStatus = a} :: StartLabelDetectionResponse)

instance Prelude.NFData StartLabelDetectionResponse where
  rnf StartLabelDetectionResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
