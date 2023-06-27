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
-- Module      : Amazonka.Rekognition.StartTextDetection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of text in a stored video.
--
-- Amazon Rekognition Video can detect text in a video stored in an Amazon
-- S3 bucket. Use Video to specify the bucket name and the filename of the
-- video. @StartTextDetection@ returns a job identifier (@JobId@) which you
-- use to get the results of the operation. When text detection is
-- finished, Amazon Rekognition Video publishes a completion status to the
-- Amazon Simple Notification Service topic that you specify in
-- @NotificationChannel@.
--
-- To get the results of the text detection operation, first check that the
-- status value published to the Amazon SNS topic is @SUCCEEDED@. if so,
-- call GetTextDetection and pass the job identifier (@JobId@) from the
-- initial call to @StartTextDetection@.
module Amazonka.Rekognition.StartTextDetection
  ( -- * Creating a Request
    StartTextDetection (..),
    newStartTextDetection,

    -- * Request Lenses
    startTextDetection_clientRequestToken,
    startTextDetection_filters,
    startTextDetection_jobTag,
    startTextDetection_notificationChannel,
    startTextDetection_video,

    -- * Destructuring the Response
    StartTextDetectionResponse (..),
    newStartTextDetectionResponse,

    -- * Response Lenses
    startTextDetectionResponse_jobId,
    startTextDetectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartTextDetection' smart constructor.
data StartTextDetection = StartTextDetection'
  { -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartTextDetection@ requests, the same @JobId@ is
    -- returned. Use @ClientRequestToken@ to prevent the same job from being
    -- accidentaly started more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Optional parameters that let you set criteria the text must meet to be
    -- included in your response.
    filters :: Prelude.Maybe StartTextDetectionFilters,
    -- | An identifier returned in the completion status published by your Amazon
    -- Simple Notification Service topic. For example, you can use @JobTag@ to
    -- group related jobs and identify them in the completion notification.
    jobTag :: Prelude.Maybe Prelude.Text,
    notificationChannel :: Prelude.Maybe NotificationChannel,
    video :: Video
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTextDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startTextDetection_clientRequestToken' - Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartTextDetection@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidentaly started more than once.
--
-- 'filters', 'startTextDetection_filters' - Optional parameters that let you set criteria the text must meet to be
-- included in your response.
--
-- 'jobTag', 'startTextDetection_jobTag' - An identifier returned in the completion status published by your Amazon
-- Simple Notification Service topic. For example, you can use @JobTag@ to
-- group related jobs and identify them in the completion notification.
--
-- 'notificationChannel', 'startTextDetection_notificationChannel' - Undocumented member.
--
-- 'video', 'startTextDetection_video' - Undocumented member.
newStartTextDetection ::
  -- | 'video'
  Video ->
  StartTextDetection
newStartTextDetection pVideo_ =
  StartTextDetection'
    { clientRequestToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      jobTag = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      video = pVideo_
    }

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartTextDetection@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidentaly started more than once.
startTextDetection_clientRequestToken :: Lens.Lens' StartTextDetection (Prelude.Maybe Prelude.Text)
startTextDetection_clientRequestToken = Lens.lens (\StartTextDetection' {clientRequestToken} -> clientRequestToken) (\s@StartTextDetection' {} a -> s {clientRequestToken = a} :: StartTextDetection)

-- | Optional parameters that let you set criteria the text must meet to be
-- included in your response.
startTextDetection_filters :: Lens.Lens' StartTextDetection (Prelude.Maybe StartTextDetectionFilters)
startTextDetection_filters = Lens.lens (\StartTextDetection' {filters} -> filters) (\s@StartTextDetection' {} a -> s {filters = a} :: StartTextDetection)

-- | An identifier returned in the completion status published by your Amazon
-- Simple Notification Service topic. For example, you can use @JobTag@ to
-- group related jobs and identify them in the completion notification.
startTextDetection_jobTag :: Lens.Lens' StartTextDetection (Prelude.Maybe Prelude.Text)
startTextDetection_jobTag = Lens.lens (\StartTextDetection' {jobTag} -> jobTag) (\s@StartTextDetection' {} a -> s {jobTag = a} :: StartTextDetection)

-- | Undocumented member.
startTextDetection_notificationChannel :: Lens.Lens' StartTextDetection (Prelude.Maybe NotificationChannel)
startTextDetection_notificationChannel = Lens.lens (\StartTextDetection' {notificationChannel} -> notificationChannel) (\s@StartTextDetection' {} a -> s {notificationChannel = a} :: StartTextDetection)

-- | Undocumented member.
startTextDetection_video :: Lens.Lens' StartTextDetection Video
startTextDetection_video = Lens.lens (\StartTextDetection' {video} -> video) (\s@StartTextDetection' {} a -> s {video = a} :: StartTextDetection)

instance Core.AWSRequest StartTextDetection where
  type
    AWSResponse StartTextDetection =
      StartTextDetectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTextDetectionResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTextDetection where
  hashWithSalt _salt StartTextDetection' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` jobTag
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` video

instance Prelude.NFData StartTextDetection where
  rnf StartTextDetection' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf jobTag
      `Prelude.seq` Prelude.rnf notificationChannel
      `Prelude.seq` Prelude.rnf video

instance Data.ToHeaders StartTextDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.StartTextDetection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartTextDetection where
  toJSON StartTextDetection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("JobTag" Data..=) Prelude.<$> jobTag,
            ("NotificationChannel" Data..=)
              Prelude.<$> notificationChannel,
            Prelude.Just ("Video" Data..= video)
          ]
      )

instance Data.ToPath StartTextDetection where
  toPath = Prelude.const "/"

instance Data.ToQuery StartTextDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTextDetectionResponse' smart constructor.
data StartTextDetectionResponse = StartTextDetectionResponse'
  { -- | Identifier for the text detection job. Use @JobId@ to identify the job
    -- in a subsequent call to @GetTextDetection@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTextDetectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startTextDetectionResponse_jobId' - Identifier for the text detection job. Use @JobId@ to identify the job
-- in a subsequent call to @GetTextDetection@.
--
-- 'httpStatus', 'startTextDetectionResponse_httpStatus' - The response's http status code.
newStartTextDetectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartTextDetectionResponse
newStartTextDetectionResponse pHttpStatus_ =
  StartTextDetectionResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier for the text detection job. Use @JobId@ to identify the job
-- in a subsequent call to @GetTextDetection@.
startTextDetectionResponse_jobId :: Lens.Lens' StartTextDetectionResponse (Prelude.Maybe Prelude.Text)
startTextDetectionResponse_jobId = Lens.lens (\StartTextDetectionResponse' {jobId} -> jobId) (\s@StartTextDetectionResponse' {} a -> s {jobId = a} :: StartTextDetectionResponse)

-- | The response's http status code.
startTextDetectionResponse_httpStatus :: Lens.Lens' StartTextDetectionResponse Prelude.Int
startTextDetectionResponse_httpStatus = Lens.lens (\StartTextDetectionResponse' {httpStatus} -> httpStatus) (\s@StartTextDetectionResponse' {} a -> s {httpStatus = a} :: StartTextDetectionResponse)

instance Prelude.NFData StartTextDetectionResponse where
  rnf StartTextDetectionResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
