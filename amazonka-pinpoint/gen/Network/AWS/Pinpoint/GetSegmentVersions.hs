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
-- Module      : Network.AWS.Pinpoint.GetSegmentVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other
-- settings for all the versions of a specific segment that\'s associated
-- with an application.
module Network.AWS.Pinpoint.GetSegmentVersions
  ( -- * Creating a Request
    GetSegmentVersions (..),
    newGetSegmentVersions,

    -- * Request Lenses
    getSegmentVersions_pageSize,
    getSegmentVersions_token,
    getSegmentVersions_segmentId,
    getSegmentVersions_applicationId,

    -- * Destructuring the Response
    GetSegmentVersionsResponse (..),
    newGetSegmentVersionsResponse,

    -- * Response Lenses
    getSegmentVersionsResponse_httpStatus,
    getSegmentVersionsResponse_segmentsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSegmentVersions' smart constructor.
data GetSegmentVersions = GetSegmentVersions'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Core.Maybe Core.Text,
    -- | The unique identifier for the segment.
    segmentId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSegmentVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getSegmentVersions_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getSegmentVersions_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'segmentId', 'getSegmentVersions_segmentId' - The unique identifier for the segment.
--
-- 'applicationId', 'getSegmentVersions_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetSegmentVersions ::
  -- | 'segmentId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  GetSegmentVersions
newGetSegmentVersions pSegmentId_ pApplicationId_ =
  GetSegmentVersions'
    { pageSize = Core.Nothing,
      token = Core.Nothing,
      segmentId = pSegmentId_,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getSegmentVersions_pageSize :: Lens.Lens' GetSegmentVersions (Core.Maybe Core.Text)
getSegmentVersions_pageSize = Lens.lens (\GetSegmentVersions' {pageSize} -> pageSize) (\s@GetSegmentVersions' {} a -> s {pageSize = a} :: GetSegmentVersions)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getSegmentVersions_token :: Lens.Lens' GetSegmentVersions (Core.Maybe Core.Text)
getSegmentVersions_token = Lens.lens (\GetSegmentVersions' {token} -> token) (\s@GetSegmentVersions' {} a -> s {token = a} :: GetSegmentVersions)

-- | The unique identifier for the segment.
getSegmentVersions_segmentId :: Lens.Lens' GetSegmentVersions Core.Text
getSegmentVersions_segmentId = Lens.lens (\GetSegmentVersions' {segmentId} -> segmentId) (\s@GetSegmentVersions' {} a -> s {segmentId = a} :: GetSegmentVersions)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getSegmentVersions_applicationId :: Lens.Lens' GetSegmentVersions Core.Text
getSegmentVersions_applicationId = Lens.lens (\GetSegmentVersions' {applicationId} -> applicationId) (\s@GetSegmentVersions' {} a -> s {applicationId = a} :: GetSegmentVersions)

instance Core.AWSRequest GetSegmentVersions where
  type
    AWSResponse GetSegmentVersions =
      GetSegmentVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentVersionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetSegmentVersions

instance Core.NFData GetSegmentVersions

instance Core.ToHeaders GetSegmentVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetSegmentVersions where
  toPath GetSegmentVersions' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/segments/",
        Core.toBS segmentId,
        "/versions"
      ]

instance Core.ToQuery GetSegmentVersions where
  toQuery GetSegmentVersions' {..} =
    Core.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetSegmentVersionsResponse' smart constructor.
data GetSegmentVersionsResponse = GetSegmentVersionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    segmentsResponse :: SegmentsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSegmentVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSegmentVersionsResponse_httpStatus' - The response's http status code.
--
-- 'segmentsResponse', 'getSegmentVersionsResponse_segmentsResponse' - Undocumented member.
newGetSegmentVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'segmentsResponse'
  SegmentsResponse ->
  GetSegmentVersionsResponse
newGetSegmentVersionsResponse
  pHttpStatus_
  pSegmentsResponse_ =
    GetSegmentVersionsResponse'
      { httpStatus =
          pHttpStatus_,
        segmentsResponse = pSegmentsResponse_
      }

-- | The response's http status code.
getSegmentVersionsResponse_httpStatus :: Lens.Lens' GetSegmentVersionsResponse Core.Int
getSegmentVersionsResponse_httpStatus = Lens.lens (\GetSegmentVersionsResponse' {httpStatus} -> httpStatus) (\s@GetSegmentVersionsResponse' {} a -> s {httpStatus = a} :: GetSegmentVersionsResponse)

-- | Undocumented member.
getSegmentVersionsResponse_segmentsResponse :: Lens.Lens' GetSegmentVersionsResponse SegmentsResponse
getSegmentVersionsResponse_segmentsResponse = Lens.lens (\GetSegmentVersionsResponse' {segmentsResponse} -> segmentsResponse) (\s@GetSegmentVersionsResponse' {} a -> s {segmentsResponse = a} :: GetSegmentVersionsResponse)

instance Core.NFData GetSegmentVersionsResponse
