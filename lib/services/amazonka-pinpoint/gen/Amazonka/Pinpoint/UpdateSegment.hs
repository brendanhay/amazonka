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
-- Module      : Amazonka.Pinpoint.UpdateSegment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new segment for an application or updates the configuration,
-- dimension, and other settings for an existing segment that\'s associated
-- with an application.
module Amazonka.Pinpoint.UpdateSegment
  ( -- * Creating a Request
    UpdateSegment (..),
    newUpdateSegment,

    -- * Request Lenses
    updateSegment_segmentId,
    updateSegment_applicationId,
    updateSegment_writeSegmentRequest,

    -- * Destructuring the Response
    UpdateSegmentResponse (..),
    newUpdateSegmentResponse,

    -- * Response Lenses
    updateSegmentResponse_httpStatus,
    updateSegmentResponse_segmentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSegment' smart constructor.
data UpdateSegment = UpdateSegment'
  { -- | The unique identifier for the segment.
    segmentId :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    writeSegmentRequest :: WriteSegmentRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentId', 'updateSegment_segmentId' - The unique identifier for the segment.
--
-- 'applicationId', 'updateSegment_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'writeSegmentRequest', 'updateSegment_writeSegmentRequest' - Undocumented member.
newUpdateSegment ::
  -- | 'segmentId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'writeSegmentRequest'
  WriteSegmentRequest ->
  UpdateSegment
newUpdateSegment
  pSegmentId_
  pApplicationId_
  pWriteSegmentRequest_ =
    UpdateSegment'
      { segmentId = pSegmentId_,
        applicationId = pApplicationId_,
        writeSegmentRequest = pWriteSegmentRequest_
      }

-- | The unique identifier for the segment.
updateSegment_segmentId :: Lens.Lens' UpdateSegment Prelude.Text
updateSegment_segmentId = Lens.lens (\UpdateSegment' {segmentId} -> segmentId) (\s@UpdateSegment' {} a -> s {segmentId = a} :: UpdateSegment)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateSegment_applicationId :: Lens.Lens' UpdateSegment Prelude.Text
updateSegment_applicationId = Lens.lens (\UpdateSegment' {applicationId} -> applicationId) (\s@UpdateSegment' {} a -> s {applicationId = a} :: UpdateSegment)

-- | Undocumented member.
updateSegment_writeSegmentRequest :: Lens.Lens' UpdateSegment WriteSegmentRequest
updateSegment_writeSegmentRequest = Lens.lens (\UpdateSegment' {writeSegmentRequest} -> writeSegmentRequest) (\s@UpdateSegment' {} a -> s {writeSegmentRequest = a} :: UpdateSegment)

instance Core.AWSRequest UpdateSegment where
  type
    AWSResponse UpdateSegment =
      UpdateSegmentResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSegmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateSegment where
  hashWithSalt _salt UpdateSegment' {..} =
    _salt `Prelude.hashWithSalt` segmentId
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` writeSegmentRequest

instance Prelude.NFData UpdateSegment where
  rnf UpdateSegment' {..} =
    Prelude.rnf segmentId
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf writeSegmentRequest

instance Core.ToHeaders UpdateSegment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSegment where
  toJSON UpdateSegment' {..} =
    Core.toJSON writeSegmentRequest

instance Core.ToPath UpdateSegment where
  toPath UpdateSegment' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/segments/",
        Core.toBS segmentId
      ]

instance Core.ToQuery UpdateSegment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSegmentResponse' smart constructor.
data UpdateSegmentResponse = UpdateSegmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    segmentResponse :: SegmentResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSegmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSegmentResponse_httpStatus' - The response's http status code.
--
-- 'segmentResponse', 'updateSegmentResponse_segmentResponse' - Undocumented member.
newUpdateSegmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'segmentResponse'
  SegmentResponse ->
  UpdateSegmentResponse
newUpdateSegmentResponse
  pHttpStatus_
  pSegmentResponse_ =
    UpdateSegmentResponse'
      { httpStatus = pHttpStatus_,
        segmentResponse = pSegmentResponse_
      }

-- | The response's http status code.
updateSegmentResponse_httpStatus :: Lens.Lens' UpdateSegmentResponse Prelude.Int
updateSegmentResponse_httpStatus = Lens.lens (\UpdateSegmentResponse' {httpStatus} -> httpStatus) (\s@UpdateSegmentResponse' {} a -> s {httpStatus = a} :: UpdateSegmentResponse)

-- | Undocumented member.
updateSegmentResponse_segmentResponse :: Lens.Lens' UpdateSegmentResponse SegmentResponse
updateSegmentResponse_segmentResponse = Lens.lens (\UpdateSegmentResponse' {segmentResponse} -> segmentResponse) (\s@UpdateSegmentResponse' {} a -> s {segmentResponse = a} :: UpdateSegmentResponse)

instance Prelude.NFData UpdateSegmentResponse where
  rnf UpdateSegmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf segmentResponse
