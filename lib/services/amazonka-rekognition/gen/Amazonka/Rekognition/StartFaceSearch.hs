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
-- Module      : Amazonka.Rekognition.StartFaceSearch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- more information, see
-- <https://docs.aws.amazon.com/rekognition/latest/dg/procedure-person-search-videos.html Searching stored videos for faces>.
module Amazonka.Rekognition.StartFaceSearch
  ( -- * Creating a Request
    StartFaceSearch (..),
    newStartFaceSearch,

    -- * Request Lenses
    startFaceSearch_clientRequestToken,
    startFaceSearch_faceMatchThreshold,
    startFaceSearch_jobTag,
    startFaceSearch_notificationChannel,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartFaceSearch' smart constructor.
data StartFaceSearch = StartFaceSearch'
  { -- | Idempotent token used to identify the start request. If you use the same
    -- token with multiple @StartFaceSearch@ requests, the same @JobId@ is
    -- returned. Use @ClientRequestToken@ to prevent the same job from being
    -- accidently started more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The minimum confidence in the person match to return. For example,
    -- don\'t return any matches where confidence in matches is less than 70%.
    -- The default value is 80%.
    faceMatchThreshold :: Prelude.Maybe Prelude.Double,
    -- | An identifier you specify that\'s returned in the completion
    -- notification that\'s published to your Amazon Simple Notification
    -- Service topic. For example, you can use @JobTag@ to group related jobs
    -- and identify them in the completion notification.
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
    -- Video to publish the completion status of the search. The Amazon SNS
    -- topic must have a topic name that begins with /AmazonRekognition/ if you
    -- are using the AmazonRekognitionServiceRole permissions policy to access
    -- the topic.
    notificationChannel :: Prelude.Maybe NotificationChannel,
    -- | The video you want to search. The video must be stored in an Amazon S3
    -- bucket.
    video :: Video,
    -- | ID of the collection that contains the faces you want to search for.
    collectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFaceSearch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startFaceSearch_clientRequestToken' - Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartFaceSearch@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
--
-- 'faceMatchThreshold', 'startFaceSearch_faceMatchThreshold' - The minimum confidence in the person match to return. For example,
-- don\'t return any matches where confidence in matches is less than 70%.
-- The default value is 80%.
--
-- 'jobTag', 'startFaceSearch_jobTag' - An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
--
-- 'notificationChannel', 'startFaceSearch_notificationChannel' - The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the search. The Amazon SNS
-- topic must have a topic name that begins with /AmazonRekognition/ if you
-- are using the AmazonRekognitionServiceRole permissions policy to access
-- the topic.
--
-- 'video', 'startFaceSearch_video' - The video you want to search. The video must be stored in an Amazon S3
-- bucket.
--
-- 'collectionId', 'startFaceSearch_collectionId' - ID of the collection that contains the faces you want to search for.
newStartFaceSearch ::
  -- | 'video'
  Video ->
  -- | 'collectionId'
  Prelude.Text ->
  StartFaceSearch
newStartFaceSearch pVideo_ pCollectionId_ =
  StartFaceSearch'
    { clientRequestToken =
        Prelude.Nothing,
      faceMatchThreshold = Prelude.Nothing,
      jobTag = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      video = pVideo_,
      collectionId = pCollectionId_
    }

-- | Idempotent token used to identify the start request. If you use the same
-- token with multiple @StartFaceSearch@ requests, the same @JobId@ is
-- returned. Use @ClientRequestToken@ to prevent the same job from being
-- accidently started more than once.
startFaceSearch_clientRequestToken :: Lens.Lens' StartFaceSearch (Prelude.Maybe Prelude.Text)
startFaceSearch_clientRequestToken = Lens.lens (\StartFaceSearch' {clientRequestToken} -> clientRequestToken) (\s@StartFaceSearch' {} a -> s {clientRequestToken = a} :: StartFaceSearch)

-- | The minimum confidence in the person match to return. For example,
-- don\'t return any matches where confidence in matches is less than 70%.
-- The default value is 80%.
startFaceSearch_faceMatchThreshold :: Lens.Lens' StartFaceSearch (Prelude.Maybe Prelude.Double)
startFaceSearch_faceMatchThreshold = Lens.lens (\StartFaceSearch' {faceMatchThreshold} -> faceMatchThreshold) (\s@StartFaceSearch' {} a -> s {faceMatchThreshold = a} :: StartFaceSearch)

-- | An identifier you specify that\'s returned in the completion
-- notification that\'s published to your Amazon Simple Notification
-- Service topic. For example, you can use @JobTag@ to group related jobs
-- and identify them in the completion notification.
startFaceSearch_jobTag :: Lens.Lens' StartFaceSearch (Prelude.Maybe Prelude.Text)
startFaceSearch_jobTag = Lens.lens (\StartFaceSearch' {jobTag} -> jobTag) (\s@StartFaceSearch' {} a -> s {jobTag = a} :: StartFaceSearch)

-- | The ARN of the Amazon SNS topic to which you want Amazon Rekognition
-- Video to publish the completion status of the search. The Amazon SNS
-- topic must have a topic name that begins with /AmazonRekognition/ if you
-- are using the AmazonRekognitionServiceRole permissions policy to access
-- the topic.
startFaceSearch_notificationChannel :: Lens.Lens' StartFaceSearch (Prelude.Maybe NotificationChannel)
startFaceSearch_notificationChannel = Lens.lens (\StartFaceSearch' {notificationChannel} -> notificationChannel) (\s@StartFaceSearch' {} a -> s {notificationChannel = a} :: StartFaceSearch)

-- | The video you want to search. The video must be stored in an Amazon S3
-- bucket.
startFaceSearch_video :: Lens.Lens' StartFaceSearch Video
startFaceSearch_video = Lens.lens (\StartFaceSearch' {video} -> video) (\s@StartFaceSearch' {} a -> s {video = a} :: StartFaceSearch)

-- | ID of the collection that contains the faces you want to search for.
startFaceSearch_collectionId :: Lens.Lens' StartFaceSearch Prelude.Text
startFaceSearch_collectionId = Lens.lens (\StartFaceSearch' {collectionId} -> collectionId) (\s@StartFaceSearch' {} a -> s {collectionId = a} :: StartFaceSearch)

instance Core.AWSRequest StartFaceSearch where
  type
    AWSResponse StartFaceSearch =
      StartFaceSearchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFaceSearchResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFaceSearch where
  hashWithSalt _salt StartFaceSearch' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` faceMatchThreshold
      `Prelude.hashWithSalt` jobTag
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` video
      `Prelude.hashWithSalt` collectionId

instance Prelude.NFData StartFaceSearch where
  rnf StartFaceSearch' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf faceMatchThreshold `Prelude.seq`
        Prelude.rnf jobTag `Prelude.seq`
          Prelude.rnf notificationChannel `Prelude.seq`
            Prelude.rnf video `Prelude.seq`
              Prelude.rnf collectionId

instance Data.ToHeaders StartFaceSearch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.StartFaceSearch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartFaceSearch where
  toJSON StartFaceSearch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("FaceMatchThreshold" Data..=)
              Prelude.<$> faceMatchThreshold,
            ("JobTag" Data..=) Prelude.<$> jobTag,
            ("NotificationChannel" Data..=)
              Prelude.<$> notificationChannel,
            Prelude.Just ("Video" Data..= video),
            Prelude.Just ("CollectionId" Data..= collectionId)
          ]
      )

instance Data.ToPath StartFaceSearch where
  toPath = Prelude.const "/"

instance Data.ToQuery StartFaceSearch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFaceSearchResponse' smart constructor.
data StartFaceSearchResponse = StartFaceSearchResponse'
  { -- | The identifier for the search job. Use @JobId@ to identify the job in a
    -- subsequent call to @GetFaceSearch@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartFaceSearchResponse
newStartFaceSearchResponse pHttpStatus_ =
  StartFaceSearchResponse'
    { jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the search job. Use @JobId@ to identify the job in a
-- subsequent call to @GetFaceSearch@.
startFaceSearchResponse_jobId :: Lens.Lens' StartFaceSearchResponse (Prelude.Maybe Prelude.Text)
startFaceSearchResponse_jobId = Lens.lens (\StartFaceSearchResponse' {jobId} -> jobId) (\s@StartFaceSearchResponse' {} a -> s {jobId = a} :: StartFaceSearchResponse)

-- | The response's http status code.
startFaceSearchResponse_httpStatus :: Lens.Lens' StartFaceSearchResponse Prelude.Int
startFaceSearchResponse_httpStatus = Lens.lens (\StartFaceSearchResponse' {httpStatus} -> httpStatus) (\s@StartFaceSearchResponse' {} a -> s {httpStatus = a} :: StartFaceSearchResponse)

instance Prelude.NFData StartFaceSearchResponse where
  rnf StartFaceSearchResponse' {..} =
    Prelude.rnf jobId `Prelude.seq`
      Prelude.rnf httpStatus
