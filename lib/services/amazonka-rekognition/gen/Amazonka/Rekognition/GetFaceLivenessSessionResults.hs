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
-- Module      : Amazonka.Rekognition.GetFaceLivenessSessionResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the results of a specific Face Liveness session. It requires
-- the @sessionId@ as input, which was created using
-- @CreateFaceLivenessSession@. Returns the corresponding Face Liveness
-- confidence score, a reference image that includes a face bounding box,
-- and audit images that also contain face bounding boxes. The Face
-- Liveness confidence score ranges from 0 to 100. The reference image can
-- optionally be returned.
module Amazonka.Rekognition.GetFaceLivenessSessionResults
  ( -- * Creating a Request
    GetFaceLivenessSessionResults (..),
    newGetFaceLivenessSessionResults,

    -- * Request Lenses
    getFaceLivenessSessionResults_sessionId,

    -- * Destructuring the Response
    GetFaceLivenessSessionResultsResponse (..),
    newGetFaceLivenessSessionResultsResponse,

    -- * Response Lenses
    getFaceLivenessSessionResultsResponse_auditImages,
    getFaceLivenessSessionResultsResponse_confidence,
    getFaceLivenessSessionResultsResponse_referenceImage,
    getFaceLivenessSessionResultsResponse_httpStatus,
    getFaceLivenessSessionResultsResponse_sessionId,
    getFaceLivenessSessionResultsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFaceLivenessSessionResults' smart constructor.
data GetFaceLivenessSessionResults = GetFaceLivenessSessionResults'
  { -- | A unique 128-bit UUID. This is used to uniquely identify the session and
    -- also acts as an idempotency token for all operations associated with the
    -- session.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFaceLivenessSessionResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'getFaceLivenessSessionResults_sessionId' - A unique 128-bit UUID. This is used to uniquely identify the session and
-- also acts as an idempotency token for all operations associated with the
-- session.
newGetFaceLivenessSessionResults ::
  -- | 'sessionId'
  Prelude.Text ->
  GetFaceLivenessSessionResults
newGetFaceLivenessSessionResults pSessionId_ =
  GetFaceLivenessSessionResults'
    { sessionId =
        pSessionId_
    }

-- | A unique 128-bit UUID. This is used to uniquely identify the session and
-- also acts as an idempotency token for all operations associated with the
-- session.
getFaceLivenessSessionResults_sessionId :: Lens.Lens' GetFaceLivenessSessionResults Prelude.Text
getFaceLivenessSessionResults_sessionId = Lens.lens (\GetFaceLivenessSessionResults' {sessionId} -> sessionId) (\s@GetFaceLivenessSessionResults' {} a -> s {sessionId = a} :: GetFaceLivenessSessionResults)

instance
  Core.AWSRequest
    GetFaceLivenessSessionResults
  where
  type
    AWSResponse GetFaceLivenessSessionResults =
      GetFaceLivenessSessionResultsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFaceLivenessSessionResultsResponse'
            Prelude.<$> (x Data..?> "AuditImages" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Confidence")
            Prelude.<*> (x Data..?> "ReferenceImage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "SessionId")
            Prelude.<*> (x Data..:> "Status")
      )

instance
  Prelude.Hashable
    GetFaceLivenessSessionResults
  where
  hashWithSalt _salt GetFaceLivenessSessionResults' {..} =
    _salt `Prelude.hashWithSalt` sessionId

instance Prelude.NFData GetFaceLivenessSessionResults where
  rnf GetFaceLivenessSessionResults' {..} =
    Prelude.rnf sessionId

instance Data.ToHeaders GetFaceLivenessSessionResults where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.GetFaceLivenessSessionResults" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFaceLivenessSessionResults where
  toJSON GetFaceLivenessSessionResults' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SessionId" Data..= sessionId)]
      )

instance Data.ToPath GetFaceLivenessSessionResults where
  toPath = Prelude.const "/"

instance Data.ToQuery GetFaceLivenessSessionResults where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFaceLivenessSessionResultsResponse' smart constructor.
data GetFaceLivenessSessionResultsResponse = GetFaceLivenessSessionResultsResponse'
  { -- | A set of images from the Face Liveness video that can be used for audit
    -- purposes. It includes a bounding box of the face and the Base64-encoded
    -- bytes that return an image. If the CreateFaceLivenessSession request
    -- included an OutputConfig argument, the image will be uploaded to an
    -- S3Object specified in the output configuration.
    auditImages :: Prelude.Maybe [AuditImage],
    -- | Probabalistic confidence score for if the person in the given video was
    -- live, represented as a float value between 0 to 100.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | A high-quality image from the Face Liveness video that can be used for
    -- face comparison or search. It includes a bounding box of the face and
    -- the Base64-encoded bytes that return an image. If the
    -- CreateFaceLivenessSession request included an OutputConfig argument, the
    -- image will be uploaded to an S3Object specified in the output
    -- configuration. In case the reference image is not returned, it\'s
    -- recommended to retry the Liveness check.
    referenceImage :: Prelude.Maybe AuditImage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The sessionId for which this request was called.
    sessionId :: Prelude.Text,
    -- | Represents a status corresponding to the state of the session. Possible
    -- statuses are: CREATED, IN_PROGRESS, SUCCEEDED, FAILED, EXPIRED.
    status :: LivenessSessionStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFaceLivenessSessionResultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditImages', 'getFaceLivenessSessionResultsResponse_auditImages' - A set of images from the Face Liveness video that can be used for audit
-- purposes. It includes a bounding box of the face and the Base64-encoded
-- bytes that return an image. If the CreateFaceLivenessSession request
-- included an OutputConfig argument, the image will be uploaded to an
-- S3Object specified in the output configuration.
--
-- 'confidence', 'getFaceLivenessSessionResultsResponse_confidence' - Probabalistic confidence score for if the person in the given video was
-- live, represented as a float value between 0 to 100.
--
-- 'referenceImage', 'getFaceLivenessSessionResultsResponse_referenceImage' - A high-quality image from the Face Liveness video that can be used for
-- face comparison or search. It includes a bounding box of the face and
-- the Base64-encoded bytes that return an image. If the
-- CreateFaceLivenessSession request included an OutputConfig argument, the
-- image will be uploaded to an S3Object specified in the output
-- configuration. In case the reference image is not returned, it\'s
-- recommended to retry the Liveness check.
--
-- 'httpStatus', 'getFaceLivenessSessionResultsResponse_httpStatus' - The response's http status code.
--
-- 'sessionId', 'getFaceLivenessSessionResultsResponse_sessionId' - The sessionId for which this request was called.
--
-- 'status', 'getFaceLivenessSessionResultsResponse_status' - Represents a status corresponding to the state of the session. Possible
-- statuses are: CREATED, IN_PROGRESS, SUCCEEDED, FAILED, EXPIRED.
newGetFaceLivenessSessionResultsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'status'
  LivenessSessionStatus ->
  GetFaceLivenessSessionResultsResponse
newGetFaceLivenessSessionResultsResponse
  pHttpStatus_
  pSessionId_
  pStatus_ =
    GetFaceLivenessSessionResultsResponse'
      { auditImages =
          Prelude.Nothing,
        confidence = Prelude.Nothing,
        referenceImage = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        sessionId = pSessionId_,
        status = pStatus_
      }

-- | A set of images from the Face Liveness video that can be used for audit
-- purposes. It includes a bounding box of the face and the Base64-encoded
-- bytes that return an image. If the CreateFaceLivenessSession request
-- included an OutputConfig argument, the image will be uploaded to an
-- S3Object specified in the output configuration.
getFaceLivenessSessionResultsResponse_auditImages :: Lens.Lens' GetFaceLivenessSessionResultsResponse (Prelude.Maybe [AuditImage])
getFaceLivenessSessionResultsResponse_auditImages = Lens.lens (\GetFaceLivenessSessionResultsResponse' {auditImages} -> auditImages) (\s@GetFaceLivenessSessionResultsResponse' {} a -> s {auditImages = a} :: GetFaceLivenessSessionResultsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Probabalistic confidence score for if the person in the given video was
-- live, represented as a float value between 0 to 100.
getFaceLivenessSessionResultsResponse_confidence :: Lens.Lens' GetFaceLivenessSessionResultsResponse (Prelude.Maybe Prelude.Double)
getFaceLivenessSessionResultsResponse_confidence = Lens.lens (\GetFaceLivenessSessionResultsResponse' {confidence} -> confidence) (\s@GetFaceLivenessSessionResultsResponse' {} a -> s {confidence = a} :: GetFaceLivenessSessionResultsResponse)

-- | A high-quality image from the Face Liveness video that can be used for
-- face comparison or search. It includes a bounding box of the face and
-- the Base64-encoded bytes that return an image. If the
-- CreateFaceLivenessSession request included an OutputConfig argument, the
-- image will be uploaded to an S3Object specified in the output
-- configuration. In case the reference image is not returned, it\'s
-- recommended to retry the Liveness check.
getFaceLivenessSessionResultsResponse_referenceImage :: Lens.Lens' GetFaceLivenessSessionResultsResponse (Prelude.Maybe AuditImage)
getFaceLivenessSessionResultsResponse_referenceImage = Lens.lens (\GetFaceLivenessSessionResultsResponse' {referenceImage} -> referenceImage) (\s@GetFaceLivenessSessionResultsResponse' {} a -> s {referenceImage = a} :: GetFaceLivenessSessionResultsResponse)

-- | The response's http status code.
getFaceLivenessSessionResultsResponse_httpStatus :: Lens.Lens' GetFaceLivenessSessionResultsResponse Prelude.Int
getFaceLivenessSessionResultsResponse_httpStatus = Lens.lens (\GetFaceLivenessSessionResultsResponse' {httpStatus} -> httpStatus) (\s@GetFaceLivenessSessionResultsResponse' {} a -> s {httpStatus = a} :: GetFaceLivenessSessionResultsResponse)

-- | The sessionId for which this request was called.
getFaceLivenessSessionResultsResponse_sessionId :: Lens.Lens' GetFaceLivenessSessionResultsResponse Prelude.Text
getFaceLivenessSessionResultsResponse_sessionId = Lens.lens (\GetFaceLivenessSessionResultsResponse' {sessionId} -> sessionId) (\s@GetFaceLivenessSessionResultsResponse' {} a -> s {sessionId = a} :: GetFaceLivenessSessionResultsResponse)

-- | Represents a status corresponding to the state of the session. Possible
-- statuses are: CREATED, IN_PROGRESS, SUCCEEDED, FAILED, EXPIRED.
getFaceLivenessSessionResultsResponse_status :: Lens.Lens' GetFaceLivenessSessionResultsResponse LivenessSessionStatus
getFaceLivenessSessionResultsResponse_status = Lens.lens (\GetFaceLivenessSessionResultsResponse' {status} -> status) (\s@GetFaceLivenessSessionResultsResponse' {} a -> s {status = a} :: GetFaceLivenessSessionResultsResponse)

instance
  Prelude.NFData
    GetFaceLivenessSessionResultsResponse
  where
  rnf GetFaceLivenessSessionResultsResponse' {..} =
    Prelude.rnf auditImages
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf referenceImage
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf status
