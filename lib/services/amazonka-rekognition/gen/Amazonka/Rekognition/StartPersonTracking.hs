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
-- Module      : Amazonka.Rekognition.StartPersonTracking
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.Rekognition.StartPersonTracking
  ( -- * Creating a Request
    StartPersonTracking (..),
    newStartPersonTracking,

    -- * Request Lenses
    startPersonTracking_clientRequestToken,
    startPersonTracking_jobTag,
    startPersonTracking_notificationChannel,
    startPersonTracking_video,

    -- * Destructuring the Response
    StartPersonTrackingResponse (..),
    newStartPersonTrackingResponse,

    -- * Response Lenses
    startPersonTrackingResponse_jobId,
    startPersonTrackingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartPersonTracking' smart constructor.
data StartPersonTracking = StartPersonTracking'
  { -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartPersonTracking@ requests, the same @JobId@ is
    -- returned. Use @ClientRequestToken@ to prevent the same job from being
    -- accidently started more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SNS topic ARN you want Amazon Rekognition Video to publish
    -- the completion status of the people detection operation to. The Amazon
    -- SNS topic must have a topic name that begins with /AmazonRekognition/ if
    -- you are using the AmazonRekognitionServiceRole permissions policy.
    notificationChannel :: Prelude.Maybe NotificationChannel,
    -- | The video in which you want to detect people. The video must be stored
    -- in an Amazon S3 bucket.
    video :: Video
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPersonTracking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'notificationChannel', 'startPersonTracking_notificationChannel' - The Amazon SNS topic ARN you want Amazon Rekognition Video to publish
-- the completion status of the people detection operation to. The Amazon
-- SNS topic must have a topic name that begins with /AmazonRekognition/ if
-- you are using the AmazonRekognitionServiceRole permissions policy.
--
-- 'video', 'startPersonTracking_video' - The video in which you want to detect people. The video must be stored
-- in an Amazon S3 bucket.
newStartPersonTracking ::
  -- | 'video'
  Video ->
  StartPersonTracking
newStartPersonTracking pVideo_ =
  StartPersonTracking'
    { clientRequestToken =
        Prelude.Nothing,
      jobTag = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      video = pVideo_
    }

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartPersonTracking@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
startPersonTracking_clientRequestToken :: Lens.Lens' StartPersonTracking (Prelude.Maybe Prelude.Text)
startPersonTracking_clientRequestToken = Lens.lens (\StartPersonTracking' {clientRequestToken} -> clientRequestToken) (\s@StartPersonTracking' {} a -> s {clientRequestToken = a} :: StartPersonTracking)

-- | An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
startPersonTracking_jobTag :: Lens.Lens' StartPersonTracking (Prelude.Maybe Prelude.Text)
startPersonTracking_jobTag = Lens.lens (\StartPersonTracking' {jobTag} -> jobTag) (\s@StartPersonTracking' {} a -> s {jobTag = a} :: StartPersonTracking)

-- | The Amazon SNS topic ARN you want Amazon Rekognition Video to publish
-- the completion status of the people detection operation to. The Amazon
-- SNS topic must have a topic name that begins with /AmazonRekognition/ if
-- you are using the AmazonRekognitionServiceRole permissions policy.
startPersonTracking_notificationChannel :: Lens.Lens' StartPersonTracking (Prelude.Maybe NotificationChannel)
startPersonTracking_notificationChannel = Lens.lens (\StartPersonTracking' {notificationChannel} -> notificationChannel) (\s@StartPersonTracking' {} a -> s {notificationChannel = a} :: StartPersonTracking)

-- | The video in which you want to detect people. The video must be stored
-- in an Amazon S3 bucket.
startPersonTracking_video :: Lens.Lens' StartPersonTracking Video
startPersonTracking_video = Lens.lens (\StartPersonTracking' {video} -> video) (\s@StartPersonTracking' {} a -> s {video = a} :: StartPersonTracking)

instance Core.AWSRequest StartPersonTracking where
  type
    AWSResponse StartPersonTracking =
      StartPersonTrackingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPersonTrackingResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartPersonTracking where
  hashWithSalt _salt StartPersonTracking' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobTag
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` video

instance Prelude.NFData StartPersonTracking where
  rnf StartPersonTracking' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf jobTag `Prelude.seq`
        Prelude.rnf notificationChannel `Prelude.seq`
          Prelude.rnf video

instance Data.ToHeaders StartPersonTracking where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.StartPersonTracking" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartPersonTracking where
  toJSON StartPersonTracking' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("JobTag" Data..=) Prelude.<$> jobTag,
            ("NotificationChannel" Data..=)
              Prelude.<$> notificationChannel,
            Prelude.Just ("Video" Data..= video)
          ]
      )

instance Data.ToPath StartPersonTracking where
  toPath = Prelude.const "/"

instance Data.ToQuery StartPersonTracking where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartPersonTrackingResponse' smart constructor.
data StartPersonTrackingResponse = StartPersonTrackingResponse'
  { -- | The identifier for the person detection job. Use @JobId@ to identify the
    -- job in a subsequent call to @GetPersonTracking@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartPersonTrackingResponse
newStartPersonTrackingResponse pHttpStatus_ =
  StartPersonTrackingResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the person detection job. Use @JobId@ to identify the
-- job in a subsequent call to @GetPersonTracking@.
startPersonTrackingResponse_jobId :: Lens.Lens' StartPersonTrackingResponse (Prelude.Maybe Prelude.Text)
startPersonTrackingResponse_jobId = Lens.lens (\StartPersonTrackingResponse' {jobId} -> jobId) (\s@StartPersonTrackingResponse' {} a -> s {jobId = a} :: StartPersonTrackingResponse)

-- | The response's http status code.
startPersonTrackingResponse_httpStatus :: Lens.Lens' StartPersonTrackingResponse Prelude.Int
startPersonTrackingResponse_httpStatus = Lens.lens (\StartPersonTrackingResponse' {httpStatus} -> httpStatus) (\s@StartPersonTrackingResponse' {} a -> s {httpStatus = a} :: StartPersonTrackingResponse)

instance Prelude.NFData StartPersonTrackingResponse where
  rnf StartPersonTrackingResponse' {..} =
    Prelude.rnf jobId `Prelude.seq`
      Prelude.rnf httpStatus
