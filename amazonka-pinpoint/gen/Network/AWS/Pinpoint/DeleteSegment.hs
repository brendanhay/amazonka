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
-- Module      : Network.AWS.Pinpoint.DeleteSegment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a segment from an application.
module Network.AWS.Pinpoint.DeleteSegment
  ( -- * Creating a Request
    DeleteSegment (..),
    newDeleteSegment,

    -- * Request Lenses
    deleteSegment_segmentId,
    deleteSegment_applicationId,

    -- * Destructuring the Response
    DeleteSegmentResponse (..),
    newDeleteSegmentResponse,

    -- * Response Lenses
    deleteSegmentResponse_httpStatus,
    deleteSegmentResponse_segmentResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSegment' smart constructor.
data DeleteSegment = DeleteSegment'
  { -- | The unique identifier for the segment.
    segmentId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentId', 'deleteSegment_segmentId' - The unique identifier for the segment.
--
-- 'applicationId', 'deleteSegment_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteSegment ::
  -- | 'segmentId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  DeleteSegment
newDeleteSegment pSegmentId_ pApplicationId_ =
  DeleteSegment'
    { segmentId = pSegmentId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the segment.
deleteSegment_segmentId :: Lens.Lens' DeleteSegment Core.Text
deleteSegment_segmentId = Lens.lens (\DeleteSegment' {segmentId} -> segmentId) (\s@DeleteSegment' {} a -> s {segmentId = a} :: DeleteSegment)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteSegment_applicationId :: Lens.Lens' DeleteSegment Core.Text
deleteSegment_applicationId = Lens.lens (\DeleteSegment' {applicationId} -> applicationId) (\s@DeleteSegment' {} a -> s {applicationId = a} :: DeleteSegment)

instance Core.AWSRequest DeleteSegment where
  type
    AWSResponse DeleteSegment =
      DeleteSegmentResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSegmentResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable DeleteSegment

instance Core.NFData DeleteSegment

instance Core.ToHeaders DeleteSegment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteSegment where
  toPath DeleteSegment' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/segments/",
        Core.toBS segmentId
      ]

instance Core.ToQuery DeleteSegment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteSegmentResponse' smart constructor.
data DeleteSegmentResponse = DeleteSegmentResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    segmentResponse :: SegmentResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSegmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSegmentResponse_httpStatus' - The response's http status code.
--
-- 'segmentResponse', 'deleteSegmentResponse_segmentResponse' - Undocumented member.
newDeleteSegmentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'segmentResponse'
  SegmentResponse ->
  DeleteSegmentResponse
newDeleteSegmentResponse
  pHttpStatus_
  pSegmentResponse_ =
    DeleteSegmentResponse'
      { httpStatus = pHttpStatus_,
        segmentResponse = pSegmentResponse_
      }

-- | The response's http status code.
deleteSegmentResponse_httpStatus :: Lens.Lens' DeleteSegmentResponse Core.Int
deleteSegmentResponse_httpStatus = Lens.lens (\DeleteSegmentResponse' {httpStatus} -> httpStatus) (\s@DeleteSegmentResponse' {} a -> s {httpStatus = a} :: DeleteSegmentResponse)

-- | Undocumented member.
deleteSegmentResponse_segmentResponse :: Lens.Lens' DeleteSegmentResponse SegmentResponse
deleteSegmentResponse_segmentResponse = Lens.lens (\DeleteSegmentResponse' {segmentResponse} -> segmentResponse) (\s@DeleteSegmentResponse' {} a -> s {segmentResponse = a} :: DeleteSegmentResponse)

instance Core.NFData DeleteSegmentResponse
