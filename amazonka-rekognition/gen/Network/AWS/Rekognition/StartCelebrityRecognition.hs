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
-- Module      : Network.AWS.Rekognition.StartCelebrityRecognition
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- For more information, see Recognizing Celebrities in the Amazon
-- Rekognition Developer Guide.
module Network.AWS.Rekognition.StartCelebrityRecognition
  ( -- * Creating a Request
    StartCelebrityRecognition (..),
    newStartCelebrityRecognition,

    -- * Request Lenses
    startCelebrityRecognition_notificationChannel,
    startCelebrityRecognition_clientRequestToken,
    startCelebrityRecognition_jobTag,
    startCelebrityRecognition_video,

    -- * Destructuring the Response
    StartCelebrityRecognitionResponse (..),
    newStartCelebrityRecognitionResponse,

    -- * Response Lenses
    startCelebrityRecognitionResponse_jobId,
    startCelebrityRecognitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartCelebrityRecognition' smart constructor.
data StartCelebrityRecognition = StartCelebrityRecognition'
  { -- | The Amazon SNS topic ARN that you want Amazon Rekognition Video to
    -- publish the completion status of the celebrity recognition analysis to.
    notificationChannel :: Prelude.Maybe NotificationChannel,
    -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartCelebrityRecognition@ requests, the same
    -- @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job
    -- from being accidently started more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | The video in which you want to recognize celebrities. The video must be
    -- stored in an Amazon S3 bucket.
    video :: Video
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartCelebrityRecognition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationChannel', 'startCelebrityRecognition_notificationChannel' - The Amazon SNS topic ARN that you want Amazon Rekognition Video to
-- publish the completion status of the celebrity recognition analysis to.
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
-- 'video', 'startCelebrityRecognition_video' - The video in which you want to recognize celebrities. The video must be
-- stored in an Amazon S3 bucket.
newStartCelebrityRecognition ::
  -- | 'video'
  Video ->
  StartCelebrityRecognition
newStartCelebrityRecognition pVideo_ =
  StartCelebrityRecognition'
    { notificationChannel =
        Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      jobTag = Prelude.Nothing,
      video = pVideo_
    }

-- | The Amazon SNS topic ARN that you want Amazon Rekognition Video to
-- publish the completion status of the celebrity recognition analysis to.
startCelebrityRecognition_notificationChannel :: Lens.Lens' StartCelebrityRecognition (Prelude.Maybe NotificationChannel)
startCelebrityRecognition_notificationChannel = Lens.lens (\StartCelebrityRecognition' {notificationChannel} -> notificationChannel) (\s@StartCelebrityRecognition' {} a -> s {notificationChannel = a} :: StartCelebrityRecognition)

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

-- | The video in which you want to recognize celebrities. The video must be
-- stored in an Amazon S3 bucket.
startCelebrityRecognition_video :: Lens.Lens' StartCelebrityRecognition Video
startCelebrityRecognition_video = Lens.lens (\StartCelebrityRecognition' {video} -> video) (\s@StartCelebrityRecognition' {} a -> s {video = a} :: StartCelebrityRecognition)

instance Prelude.AWSRequest StartCelebrityRecognition where
  type
    Rs StartCelebrityRecognition =
      StartCelebrityRecognitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartCelebrityRecognitionResponse'
            Prelude.<$> (x Prelude..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartCelebrityRecognition

instance Prelude.NFData StartCelebrityRecognition

instance Prelude.ToHeaders StartCelebrityRecognition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.StartCelebrityRecognition" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartCelebrityRecognition where
  toJSON StartCelebrityRecognition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NotificationChannel" Prelude..=)
              Prelude.<$> notificationChannel,
            ("ClientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            ("JobTag" Prelude..=) Prelude.<$> jobTag,
            Prelude.Just ("Video" Prelude..= video)
          ]
      )

instance Prelude.ToPath StartCelebrityRecognition where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartCelebrityRecognition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartCelebrityRecognitionResponse' smart constructor.
data StartCelebrityRecognitionResponse = StartCelebrityRecognitionResponse'
  { -- | The identifier for the celebrity recognition analysis job. Use @JobId@
    -- to identify the job in a subsequent call to @GetCelebrityRecognition@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
