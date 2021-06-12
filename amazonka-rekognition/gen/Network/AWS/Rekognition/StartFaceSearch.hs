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
-- Module      : Network.AWS.Rekognition.StartFaceSearch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous search for faces in a collection that match the
-- faces of persons detected in a stored video.
--
-- The video must be stored in an Amazon S3 bucket. Use Video to specify
-- the bucket name and the filename of the video. @StartFaceSearch@ returns
-- a job identifier (@JobId@) which you use to get the search results once
-- the search has completed. When searching is finished, Amazon Rekognition
-- Video publishes a completion status to the Amazon Simple Notification
-- Service topic that you specify in @NotificationChannel@. To get the
-- search results, first check that the status value published to the
-- Amazon SNS topic is @SUCCEEDED@. If so, call GetFaceSearch and pass the
-- job identifier (@JobId@) from the initial call to @StartFaceSearch@. For
-- more information, see procedure-person-search-videos.
module Network.AWS.Rekognition.StartFaceSearch
  ( -- * Creating a Request
    StartFaceSearch (..),
    newStartFaceSearch,

    -- * Request Lenses
    startFaceSearch_notificationChannel,
    startFaceSearch_clientRequestToken,
    startFaceSearch_jobTag,
    startFaceSearch_faceMatchThreshold,
    startFaceSearch_video,
    startFaceSearch_collectionId,

    -- * Destructuring the Response
    StartFaceSearchResponse (..),
    newStartFaceSearchResponse,

    -- * Response Lenses
    startFaceSearchResponse_jobId,
    startFaceSearchResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartFaceSearch' smart constructor.
data StartFaceSearch = StartFaceSearch'
  { -- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
    -- Video to publish the completion status of the search.
    notificationChannel :: Core.Maybe NotificationChannel,
    -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartFaceSearch@ requests, the same @JobId@ is
    -- returned. Use @ClientRequestToken@ to prevent the same job from being
    -- accidently started more than once.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Core.Maybe Core.Text,
    -- | The minimum confidence in the person match to return. For example,
    -- don\'t return any matches where confidence in matches is less than 70%.
    -- The default value is 80%.
    faceMatchThreshold :: Core.Maybe Core.Double,
    -- | The video you want to search. The video must be stored in an Amazon S3
    -- bucket.
    video :: Video,
    -- | ID of the collection that contains the faces you want to search for.
    collectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartFaceSearch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationChannel', 'startFaceSearch_notificationChannel' - The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the search.
--
-- 'clientRequestToken', 'startFaceSearch_clientRequestToken' - Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartFaceSearch@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
--
-- 'jobTag', 'startFaceSearch_jobTag' - An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
--
-- 'faceMatchThreshold', 'startFaceSearch_faceMatchThreshold' - The minimum confidence in the person match to return. For example,
-- don\'t return any matches where confidence in matches is less than 70%.
-- The default value is 80%.
--
-- 'video', 'startFaceSearch_video' - The video you want to search. The video must be stored in an Amazon S3
-- bucket.
--
-- 'collectionId', 'startFaceSearch_collectionId' - ID of the collection that contains the faces you want to search for.
newStartFaceSearch ::
  -- | 'video'
  Video ->
  -- | 'collectionId'
  Core.Text ->
  StartFaceSearch
newStartFaceSearch pVideo_ pCollectionId_ =
  StartFaceSearch'
    { notificationChannel =
        Core.Nothing,
      clientRequestToken = Core.Nothing,
      jobTag = Core.Nothing,
      faceMatchThreshold = Core.Nothing,
      video = pVideo_,
      collectionId = pCollectionId_
    }

-- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the search.
startFaceSearch_notificationChannel :: Lens.Lens' StartFaceSearch (Core.Maybe NotificationChannel)
startFaceSearch_notificationChannel = Lens.lens (\StartFaceSearch' {notificationChannel} -> notificationChannel) (\s@StartFaceSearch' {} a -> s {notificationChannel = a} :: StartFaceSearch)

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartFaceSearch@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
startFaceSearch_clientRequestToken :: Lens.Lens' StartFaceSearch (Core.Maybe Core.Text)
startFaceSearch_clientRequestToken = Lens.lens (\StartFaceSearch' {clientRequestToken} -> clientRequestToken) (\s@StartFaceSearch' {} a -> s {clientRequestToken = a} :: StartFaceSearch)

-- | An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
startFaceSearch_jobTag :: Lens.Lens' StartFaceSearch (Core.Maybe Core.Text)
startFaceSearch_jobTag = Lens.lens (\StartFaceSearch' {jobTag} -> jobTag) (\s@StartFaceSearch' {} a -> s {jobTag = a} :: StartFaceSearch)

-- | The minimum confidence in the person match to return. For example,
-- don\'t return any matches where confidence in matches is less than 70%.
-- The default value is 80%.
startFaceSearch_faceMatchThreshold :: Lens.Lens' StartFaceSearch (Core.Maybe Core.Double)
startFaceSearch_faceMatchThreshold = Lens.lens (\StartFaceSearch' {faceMatchThreshold} -> faceMatchThreshold) (\s@StartFaceSearch' {} a -> s {faceMatchThreshold = a} :: StartFaceSearch)

-- | The video you want to search. The video must be stored in an Amazon S3
-- bucket.
startFaceSearch_video :: Lens.Lens' StartFaceSearch Video
startFaceSearch_video = Lens.lens (\StartFaceSearch' {video} -> video) (\s@StartFaceSearch' {} a -> s {video = a} :: StartFaceSearch)

-- | ID of the collection that contains the faces you want to search for.
startFaceSearch_collectionId :: Lens.Lens' StartFaceSearch Core.Text
startFaceSearch_collectionId = Lens.lens (\StartFaceSearch' {collectionId} -> collectionId) (\s@StartFaceSearch' {} a -> s {collectionId = a} :: StartFaceSearch)

instance Core.AWSRequest StartFaceSearch where
  type
    AWSResponse StartFaceSearch =
      StartFaceSearchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFaceSearchResponse'
            Core.<$> (x Core..?> "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartFaceSearch

instance Core.NFData StartFaceSearch

instance Core.ToHeaders StartFaceSearch where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.StartFaceSearch" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartFaceSearch where
  toJSON StartFaceSearch' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotificationChannel" Core..=)
              Core.<$> notificationChannel,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            ("JobTag" Core..=) Core.<$> jobTag,
            ("FaceMatchThreshold" Core..=)
              Core.<$> faceMatchThreshold,
            Core.Just ("Video" Core..= video),
            Core.Just ("CollectionId" Core..= collectionId)
          ]
      )

instance Core.ToPath StartFaceSearch where
  toPath = Core.const "/"

instance Core.ToQuery StartFaceSearch where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartFaceSearchResponse' smart constructor.
data StartFaceSearchResponse = StartFaceSearchResponse'
  { -- | The identifier for the search job. Use @JobId@ to identify the job in a
    -- subsequent call to @GetFaceSearch@.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartFaceSearchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startFaceSearchResponse_jobId' - The identifier for the search job. Use @JobId@ to identify the job in a
-- subsequent call to @GetFaceSearch@.
--
-- 'httpStatus', 'startFaceSearchResponse_httpStatus' - The response's http status code.
newStartFaceSearchResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartFaceSearchResponse
newStartFaceSearchResponse pHttpStatus_ =
  StartFaceSearchResponse'
    { jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the search job. Use @JobId@ to identify the job in a
-- subsequent call to @GetFaceSearch@.
startFaceSearchResponse_jobId :: Lens.Lens' StartFaceSearchResponse (Core.Maybe Core.Text)
startFaceSearchResponse_jobId = Lens.lens (\StartFaceSearchResponse' {jobId} -> jobId) (\s@StartFaceSearchResponse' {} a -> s {jobId = a} :: StartFaceSearchResponse)

-- | The response's http status code.
startFaceSearchResponse_httpStatus :: Lens.Lens' StartFaceSearchResponse Core.Int
startFaceSearchResponse_httpStatus = Lens.lens (\StartFaceSearchResponse' {httpStatus} -> httpStatus) (\s@StartFaceSearchResponse' {} a -> s {httpStatus = a} :: StartFaceSearchResponse)

instance Core.NFData StartFaceSearchResponse
