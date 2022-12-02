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
-- Module      : Amazonka.Rekognition.StartSegmentDetection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of segment detection in a stored video.
--
-- Amazon Rekognition Video can detect segments in a video stored in an
-- Amazon S3 bucket. Use Video to specify the bucket name and the filename
-- of the video. @StartSegmentDetection@ returns a job identifier (@JobId@)
-- which you use to get the results of the operation. When segment
-- detection is finished, Amazon Rekognition Video publishes a completion
-- status to the Amazon Simple Notification Service topic that you specify
-- in @NotificationChannel@.
--
-- You can use the @Filters@ (StartSegmentDetectionFilters) input parameter
-- to specify the minimum detection confidence returned in the response.
-- Within @Filters@, use @ShotFilter@ (StartShotDetectionFilter) to filter
-- detected shots. Use @TechnicalCueFilter@
-- (StartTechnicalCueDetectionFilter) to filter technical cues.
--
-- To get the results of the segment detection operation, first check that
-- the status value published to the Amazon SNS topic is @SUCCEEDED@. if
-- so, call GetSegmentDetection and pass the job identifier (@JobId@) from
-- the initial call to @StartSegmentDetection@.
--
-- For more information, see Detecting video segments in stored video in
-- the Amazon Rekognition Developer Guide.
module Amazonka.Rekognition.StartSegmentDetection
  ( -- * Creating a Request
    StartSegmentDetection (..),
    newStartSegmentDetection,

    -- * Request Lenses
    startSegmentDetection_clientRequestToken,
    startSegmentDetection_filters,
    startSegmentDetection_jobTag,
    startSegmentDetection_notificationChannel,
    startSegmentDetection_video,
    startSegmentDetection_segmentTypes,

    -- * Destructuring the Response
    StartSegmentDetectionResponse (..),
    newStartSegmentDetectionResponse,

    -- * Response Lenses
    startSegmentDetectionResponse_jobId,
    startSegmentDetectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSegmentDetection' smart constructor.
data StartSegmentDetection = StartSegmentDetection'
  { -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartSegmentDetection@ requests, the same @JobId@
    -- is returned. Use @ClientRequestToken@ to prevent the same job from being
    -- accidently started more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Filters for technical cue or shot detection.
    filters :: Prelude.Maybe StartSegmentDetectionFilters,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
    -- Video to publish the completion status of the segment detection
    -- operation. Note that the Amazon SNS topic must have a topic name that
    -- begins with /AmazonRekognition/ if you are using the
    -- AmazonRekognitionServiceRole permissions policy to access the topic.
    notificationChannel :: Prelude.Maybe NotificationChannel,
    video :: Video,
    -- | An array of segment types to detect in the video. Valid values are
    -- TECHNICAL_CUE and SHOT.
    segmentTypes :: Prelude.NonEmpty SegmentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSegmentDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startSegmentDetection_clientRequestToken' - Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartSegmentDetection@ requests, the same @JobId@
-- is returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
--
-- 'filters', 'startSegmentDetection_filters' - Filters for technical cue or shot detection.
--
-- 'jobTag', 'startSegmentDetection_jobTag' - An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
--
-- 'notificationChannel', 'startSegmentDetection_notificationChannel' - The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the segment detection
-- operation. Note that the Amazon SNS topic must have a topic name that
-- begins with /AmazonRekognition/ if you are using the
-- AmazonRekognitionServiceRole permissions policy to access the topic.
--
-- 'video', 'startSegmentDetection_video' - Undocumented member.
--
-- 'segmentTypes', 'startSegmentDetection_segmentTypes' - An array of segment types to detect in the video. Valid values are
-- TECHNICAL_CUE and SHOT.
newStartSegmentDetection ::
  -- | 'video'
  Video ->
  -- | 'segmentTypes'
  Prelude.NonEmpty SegmentType ->
  StartSegmentDetection
newStartSegmentDetection pVideo_ pSegmentTypes_ =
  StartSegmentDetection'
    { clientRequestToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      jobTag = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      video = pVideo_,
      segmentTypes = Lens.coerced Lens.# pSegmentTypes_
    }

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartSegmentDetection@ requests, the same @JobId@
-- is returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
startSegmentDetection_clientRequestToken :: Lens.Lens' StartSegmentDetection (Prelude.Maybe Prelude.Text)
startSegmentDetection_clientRequestToken = Lens.lens (\StartSegmentDetection' {clientRequestToken} -> clientRequestToken) (\s@StartSegmentDetection' {} a -> s {clientRequestToken = a} :: StartSegmentDetection)

-- | Filters for technical cue or shot detection.
startSegmentDetection_filters :: Lens.Lens' StartSegmentDetection (Prelude.Maybe StartSegmentDetectionFilters)
startSegmentDetection_filters = Lens.lens (\StartSegmentDetection' {filters} -> filters) (\s@StartSegmentDetection' {} a -> s {filters = a} :: StartSegmentDetection)

-- | An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
startSegmentDetection_jobTag :: Lens.Lens' StartSegmentDetection (Prelude.Maybe Prelude.Text)
startSegmentDetection_jobTag = Lens.lens (\StartSegmentDetection' {jobTag} -> jobTag) (\s@StartSegmentDetection' {} a -> s {jobTag = a} :: StartSegmentDetection)

-- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the segment detection
-- operation. Note that the Amazon SNS topic must have a topic name that
-- begins with /AmazonRekognition/ if you are using the
-- AmazonRekognitionServiceRole permissions policy to access the topic.
startSegmentDetection_notificationChannel :: Lens.Lens' StartSegmentDetection (Prelude.Maybe NotificationChannel)
startSegmentDetection_notificationChannel = Lens.lens (\StartSegmentDetection' {notificationChannel} -> notificationChannel) (\s@StartSegmentDetection' {} a -> s {notificationChannel = a} :: StartSegmentDetection)

-- | Undocumented member.
startSegmentDetection_video :: Lens.Lens' StartSegmentDetection Video
startSegmentDetection_video = Lens.lens (\StartSegmentDetection' {video} -> video) (\s@StartSegmentDetection' {} a -> s {video = a} :: StartSegmentDetection)

-- | An array of segment types to detect in the video. Valid values are
-- TECHNICAL_CUE and SHOT.
startSegmentDetection_segmentTypes :: Lens.Lens' StartSegmentDetection (Prelude.NonEmpty SegmentType)
startSegmentDetection_segmentTypes = Lens.lens (\StartSegmentDetection' {segmentTypes} -> segmentTypes) (\s@StartSegmentDetection' {} a -> s {segmentTypes = a} :: StartSegmentDetection) Prelude.. Lens.coerced

instance Core.AWSRequest StartSegmentDetection where
  type
    AWSResponse StartSegmentDetection =
      StartSegmentDetectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSegmentDetectionResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSegmentDetection where
  hashWithSalt _salt StartSegmentDetection' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` jobTag
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` video
      `Prelude.hashWithSalt` segmentTypes

instance Prelude.NFData StartSegmentDetection where
  rnf StartSegmentDetection' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf jobTag
      `Prelude.seq` Prelude.rnf notificationChannel
      `Prelude.seq` Prelude.rnf video
      `Prelude.seq` Prelude.rnf segmentTypes

instance Data.ToHeaders StartSegmentDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.StartSegmentDetection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSegmentDetection where
  toJSON StartSegmentDetection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("JobTag" Data..=) Prelude.<$> jobTag,
            ("NotificationChannel" Data..=)
              Prelude.<$> notificationChannel,
            Prelude.Just ("Video" Data..= video),
            Prelude.Just ("SegmentTypes" Data..= segmentTypes)
          ]
      )

instance Data.ToPath StartSegmentDetection where
  toPath = Prelude.const "/"

instance Data.ToQuery StartSegmentDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSegmentDetectionResponse' smart constructor.
data StartSegmentDetectionResponse = StartSegmentDetectionResponse'
  { -- | Unique identifier for the segment detection job. The @JobId@ is returned
    -- from @StartSegmentDetection@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSegmentDetectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startSegmentDetectionResponse_jobId' - Unique identifier for the segment detection job. The @JobId@ is returned
-- from @StartSegmentDetection@.
--
-- 'httpStatus', 'startSegmentDetectionResponse_httpStatus' - The response's http status code.
newStartSegmentDetectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSegmentDetectionResponse
newStartSegmentDetectionResponse pHttpStatus_ =
  StartSegmentDetectionResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique identifier for the segment detection job. The @JobId@ is returned
-- from @StartSegmentDetection@.
startSegmentDetectionResponse_jobId :: Lens.Lens' StartSegmentDetectionResponse (Prelude.Maybe Prelude.Text)
startSegmentDetectionResponse_jobId = Lens.lens (\StartSegmentDetectionResponse' {jobId} -> jobId) (\s@StartSegmentDetectionResponse' {} a -> s {jobId = a} :: StartSegmentDetectionResponse)

-- | The response's http status code.
startSegmentDetectionResponse_httpStatus :: Lens.Lens' StartSegmentDetectionResponse Prelude.Int
startSegmentDetectionResponse_httpStatus = Lens.lens (\StartSegmentDetectionResponse' {httpStatus} -> httpStatus) (\s@StartSegmentDetectionResponse' {} a -> s {httpStatus = a} :: StartSegmentDetectionResponse)

instance Prelude.NFData StartSegmentDetectionResponse where
  rnf StartSegmentDetectionResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
