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
-- Module      : Network.AWS.Pinpoint.UpdateSegment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new segment for an application or updates the configuration,
-- dimension, and other settings for an existing segment that\'s associated
-- with an application.
module Network.AWS.Pinpoint.UpdateSegment
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSegment' smart constructor.
data UpdateSegment = UpdateSegment'
  { -- | The unique identifier for the segment.
    segmentId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    writeSegmentRequest :: WriteSegmentRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
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
updateSegment_segmentId :: Lens.Lens' UpdateSegment Core.Text
updateSegment_segmentId = Lens.lens (\UpdateSegment' {segmentId} -> segmentId) (\s@UpdateSegment' {} a -> s {segmentId = a} :: UpdateSegment)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateSegment_applicationId :: Lens.Lens' UpdateSegment Core.Text
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
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateSegment

instance Core.NFData UpdateSegment

instance Core.ToHeaders UpdateSegment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateSegment where
  toJSON UpdateSegment' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("WriteSegmentRequest" Core..= writeSegmentRequest)
          ]
      )

instance Core.ToPath UpdateSegment where
  toPath UpdateSegment' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/segments/",
        Core.toBS segmentId
      ]

instance Core.ToQuery UpdateSegment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateSegmentResponse' smart constructor.
data UpdateSegmentResponse = UpdateSegmentResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    segmentResponse :: SegmentResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
updateSegmentResponse_httpStatus :: Lens.Lens' UpdateSegmentResponse Core.Int
updateSegmentResponse_httpStatus = Lens.lens (\UpdateSegmentResponse' {httpStatus} -> httpStatus) (\s@UpdateSegmentResponse' {} a -> s {httpStatus = a} :: UpdateSegmentResponse)

-- | Undocumented member.
updateSegmentResponse_segmentResponse :: Lens.Lens' UpdateSegmentResponse SegmentResponse
updateSegmentResponse_segmentResponse = Lens.lens (\UpdateSegmentResponse' {segmentResponse} -> segmentResponse) (\s@UpdateSegmentResponse' {} a -> s {segmentResponse = a} :: UpdateSegmentResponse)

instance Core.NFData UpdateSegmentResponse
