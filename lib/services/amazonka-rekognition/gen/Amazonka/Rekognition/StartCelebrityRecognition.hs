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
-- Module      : Amazonka.Rekognition.StartCelebrityRecognition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous recognition of celebrities in a stored video.
--
-- Amazon Rekognition Video can detect celebrities in a video must be
-- stored in an Amazon S3 bucket. Use Video to specify the bucket name and
-- the filename of the video. @StartCelebrityRecognition@ returns a job
-- identifier (@JobId@) which you use to get the results of the analysis.
-- When celebrity recognition analysis is finished, Amazon Rekognition
-- Video publishes a completion status to the Amazon Simple Notification
-- Service topic that you specify in @NotificationChannel@. To get the
-- results of the celebrity recognition analysis, first check that the
-- status value published to the Amazon SNS topic is @SUCCEEDED@. If so,
-- call GetCelebrityRecognition and pass the job identifier (@JobId@) from
-- the initial call to @StartCelebrityRecognition@.
--
-- For more information, see Recognizing celebrities in the Amazon
-- Rekognition Developer Guide.
module Amazonka.Rekognition.StartCelebrityRecognition
  ( -- * Creating a Request
    StartCelebrityRecognition (..),
    newStartCelebrityRecognition,

    -- * Request Lenses
    startCelebrityRecognition_clientRequestToken,
    startCelebrityRecognition_jobTag,
    startCelebrityRecognition_notificationChannel,
    startCelebrityRecognition_video,

    -- * Destructuring the Response
    StartCelebrityRecognitionResponse (..),
    newStartCelebrityRecognitionResponse,

    -- * Response Lenses
    startCelebrityRecognitionResponse_jobId,
    startCelebrityRecognitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartCelebrityRecognition' smart constructor.
data StartCelebrityRecognition = StartCelebrityRecognition'
  { -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartCelebrityRecognition@ requests, the same
    -- @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job
    -- from being accidently started more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SNS topic ARN that you want Amazon Rekognition Video to
    -- publish the completion status of the celebrity recognition analysis to.
    -- The Amazon SNS topic must have a topic name that begins with
    -- /AmazonRekognition/ if you are using the AmazonRekognitionServiceRole
    -- permissions policy.
    notificationChannel :: Prelude.Maybe NotificationChannel,
    -- | The video in which you want to recognize celebrities. The video must be
    -- stored in an Amazon S3 bucket.
    video :: Video
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCelebrityRecognition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startCelebrityRecognition_clientRequestToken' - Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartCelebrityRecognition@ requests, the same
-- @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job
-- from being accidently started more than once.
--
-- 'jobTag', 'startCelebrityRecognition_jobTag' - An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
--
-- 'notificationChannel', 'startCelebrityRecognition_notificationChannel' - The Amazon SNS topic ARN that you want Amazon Rekognition Video to
-- publish the completion status of the celebrity recognition analysis to.
-- The Amazon SNS topic must have a topic name that begins with
-- /AmazonRekognition/ if you are using the AmazonRekognitionServiceRole
-- permissions policy.
--
-- 'video', 'startCelebrityRecognition_video' - The video in which you want to recognize celebrities. The video must be
-- stored in an Amazon S3 bucket.
newStartCelebrityRecognition ::
  -- | 'video'
  Video ->
  StartCelebrityRecognition
newStartCelebrityRecognition pVideo_ =
  StartCelebrityRecognition'
    { clientRequestToken =
        Prelude.Nothing,
      jobTag = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      video = pVideo_
    }

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartCelebrityRecognition@ requests, the same
-- @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job
-- from being accidently started more than once.
startCelebrityRecognition_clientRequestToken :: Lens.Lens' StartCelebrityRecognition (Prelude.Maybe Prelude.Text)
startCelebrityRecognition_clientRequestToken = Lens.lens (\StartCelebrityRecognition' {clientRequestToken} -> clientRequestToken) (\s@StartCelebrityRecognition' {} a -> s {clientRequestToken = a} :: StartCelebrityRecognition)

-- | An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
startCelebrityRecognition_jobTag :: Lens.Lens' StartCelebrityRecognition (Prelude.Maybe Prelude.Text)
startCelebrityRecognition_jobTag = Lens.lens (\StartCelebrityRecognition' {jobTag} -> jobTag) (\s@StartCelebrityRecognition' {} a -> s {jobTag = a} :: StartCelebrityRecognition)

-- | The Amazon SNS topic ARN that you want Amazon Rekognition Video to
-- publish the completion status of the celebrity recognition analysis to.
-- The Amazon SNS topic must have a topic name that begins with
-- /AmazonRekognition/ if you are using the AmazonRekognitionServiceRole
-- permissions policy.
startCelebrityRecognition_notificationChannel :: Lens.Lens' StartCelebrityRecognition (Prelude.Maybe NotificationChannel)
startCelebrityRecognition_notificationChannel = Lens.lens (\StartCelebrityRecognition' {notificationChannel} -> notificationChannel) (\s@StartCelebrityRecognition' {} a -> s {notificationChannel = a} :: StartCelebrityRecognition)

-- | The video in which you want to recognize celebrities. The video must be
-- stored in an Amazon S3 bucket.
startCelebrityRecognition_video :: Lens.Lens' StartCelebrityRecognition Video
startCelebrityRecognition_video = Lens.lens (\StartCelebrityRecognition' {video} -> video) (\s@StartCelebrityRecognition' {} a -> s {video = a} :: StartCelebrityRecognition)

instance Core.AWSRequest StartCelebrityRecognition where
  type
    AWSResponse StartCelebrityRecognition =
      StartCelebrityRecognitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartCelebrityRecognitionResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartCelebrityRecognition where
  hashWithSalt _salt StartCelebrityRecognition' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobTag
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` video

instance Prelude.NFData StartCelebrityRecognition where
  rnf StartCelebrityRecognition' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf jobTag
      `Prelude.seq` Prelude.rnf notificationChannel
      `Prelude.seq` Prelude.rnf video

instance Data.ToHeaders StartCelebrityRecognition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.StartCelebrityRecognition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartCelebrityRecognition where
  toJSON StartCelebrityRecognition' {..} =
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

instance Data.ToPath StartCelebrityRecognition where
  toPath = Prelude.const "/"

instance Data.ToQuery StartCelebrityRecognition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartCelebrityRecognitionResponse' smart constructor.
data StartCelebrityRecognitionResponse = StartCelebrityRecognitionResponse'
  { -- | The identifier for the celebrity recognition analysis job. Use @JobId@
    -- to identify the job in a subsequent call to @GetCelebrityRecognition@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCelebrityRecognitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startCelebrityRecognitionResponse_jobId' - The identifier for the celebrity recognition analysis job. Use @JobId@
-- to identify the job in a subsequent call to @GetCelebrityRecognition@.
--
-- 'httpStatus', 'startCelebrityRecognitionResponse_httpStatus' - The response's http status code.
newStartCelebrityRecognitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartCelebrityRecognitionResponse
newStartCelebrityRecognitionResponse pHttpStatus_ =
  StartCelebrityRecognitionResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the celebrity recognition analysis job. Use @JobId@
-- to identify the job in a subsequent call to @GetCelebrityRecognition@.
startCelebrityRecognitionResponse_jobId :: Lens.Lens' StartCelebrityRecognitionResponse (Prelude.Maybe Prelude.Text)
startCelebrityRecognitionResponse_jobId = Lens.lens (\StartCelebrityRecognitionResponse' {jobId} -> jobId) (\s@StartCelebrityRecognitionResponse' {} a -> s {jobId = a} :: StartCelebrityRecognitionResponse)

-- | The response's http status code.
startCelebrityRecognitionResponse_httpStatus :: Lens.Lens' StartCelebrityRecognitionResponse Prelude.Int
startCelebrityRecognitionResponse_httpStatus = Lens.lens (\StartCelebrityRecognitionResponse' {httpStatus} -> httpStatus) (\s@StartCelebrityRecognitionResponse' {} a -> s {httpStatus = a} :: StartCelebrityRecognitionResponse)

instance
  Prelude.NFData
    StartCelebrityRecognitionResponse
  where
  rnf StartCelebrityRecognitionResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
