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
-- Module      : Network.AWS.Pinpoint.GetSegment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other
-- settings for a specific segment that\'s associated with an application.
module Network.AWS.Pinpoint.GetSegment
  ( -- * Creating a Request
    GetSegment (..),
    newGetSegment,

    -- * Request Lenses
    getSegment_segmentId,
    getSegment_applicationId,

    -- * Destructuring the Response
    GetSegmentResponse (..),
    newGetSegmentResponse,

    -- * Response Lenses
    getSegmentResponse_httpStatus,
    getSegmentResponse_segmentResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSegment' smart constructor.
data GetSegment = GetSegment'
  { -- | The unique identifier for the segment.
    segmentId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentId', 'getSegment_segmentId' - The unique identifier for the segment.
--
-- 'applicationId', 'getSegment_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetSegment ::
  -- | 'segmentId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  GetSegment
newGetSegment pSegmentId_ pApplicationId_ =
  GetSegment'
    { segmentId = pSegmentId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the segment.
getSegment_segmentId :: Lens.Lens' GetSegment Core.Text
getSegment_segmentId = Lens.lens (\GetSegment' {segmentId} -> segmentId) (\s@GetSegment' {} a -> s {segmentId = a} :: GetSegment)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getSegment_applicationId :: Lens.Lens' GetSegment Core.Text
getSegment_applicationId = Lens.lens (\GetSegment' {applicationId} -> applicationId) (\s@GetSegment' {} a -> s {applicationId = a} :: GetSegment)

instance Core.AWSRequest GetSegment where
  type AWSResponse GetSegment = GetSegmentResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetSegment

instance Core.NFData GetSegment

instance Core.ToHeaders GetSegment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetSegment where
  toPath GetSegment' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/segments/",
        Core.toBS segmentId
      ]

instance Core.ToQuery GetSegment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSegmentResponse' smart constructor.
data GetSegmentResponse = GetSegmentResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    segmentResponse :: SegmentResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSegmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSegmentResponse_httpStatus' - The response's http status code.
--
-- 'segmentResponse', 'getSegmentResponse_segmentResponse' - Undocumented member.
newGetSegmentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'segmentResponse'
  SegmentResponse ->
  GetSegmentResponse
newGetSegmentResponse pHttpStatus_ pSegmentResponse_ =
  GetSegmentResponse'
    { httpStatus = pHttpStatus_,
      segmentResponse = pSegmentResponse_
    }

-- | The response's http status code.
getSegmentResponse_httpStatus :: Lens.Lens' GetSegmentResponse Core.Int
getSegmentResponse_httpStatus = Lens.lens (\GetSegmentResponse' {httpStatus} -> httpStatus) (\s@GetSegmentResponse' {} a -> s {httpStatus = a} :: GetSegmentResponse)

-- | Undocumented member.
getSegmentResponse_segmentResponse :: Lens.Lens' GetSegmentResponse SegmentResponse
getSegmentResponse_segmentResponse = Lens.lens (\GetSegmentResponse' {segmentResponse} -> segmentResponse) (\s@GetSegmentResponse' {} a -> s {segmentResponse = a} :: GetSegmentResponse)

instance Core.NFData GetSegmentResponse
