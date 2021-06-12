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
-- Module      : Network.AWS.Pinpoint.GetSegments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other
-- settings for all the segments that are associated with an application.
module Network.AWS.Pinpoint.GetSegments
  ( -- * Creating a Request
    GetSegments (..),
    newGetSegments,

    -- * Request Lenses
    getSegments_pageSize,
    getSegments_token,
    getSegments_applicationId,

    -- * Destructuring the Response
    GetSegmentsResponse (..),
    newGetSegmentsResponse,

    -- * Response Lenses
    getSegmentsResponse_httpStatus,
    getSegmentsResponse_segmentsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSegments' smart constructor.
data GetSegments = GetSegments'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Core.Maybe Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSegments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getSegments_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getSegments_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'applicationId', 'getSegments_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetSegments ::
  -- | 'applicationId'
  Core.Text ->
  GetSegments
newGetSegments pApplicationId_ =
  GetSegments'
    { pageSize = Core.Nothing,
      token = Core.Nothing,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getSegments_pageSize :: Lens.Lens' GetSegments (Core.Maybe Core.Text)
getSegments_pageSize = Lens.lens (\GetSegments' {pageSize} -> pageSize) (\s@GetSegments' {} a -> s {pageSize = a} :: GetSegments)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getSegments_token :: Lens.Lens' GetSegments (Core.Maybe Core.Text)
getSegments_token = Lens.lens (\GetSegments' {token} -> token) (\s@GetSegments' {} a -> s {token = a} :: GetSegments)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getSegments_applicationId :: Lens.Lens' GetSegments Core.Text
getSegments_applicationId = Lens.lens (\GetSegments' {applicationId} -> applicationId) (\s@GetSegments' {} a -> s {applicationId = a} :: GetSegments)

instance Core.AWSRequest GetSegments where
  type AWSResponse GetSegments = GetSegmentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetSegments

instance Core.NFData GetSegments

instance Core.ToHeaders GetSegments where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetSegments where
  toPath GetSegments' {..} =
    Core.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/segments"]

instance Core.ToQuery GetSegments where
  toQuery GetSegments' {..} =
    Core.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetSegmentsResponse' smart constructor.
data GetSegmentsResponse = GetSegmentsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    segmentsResponse :: SegmentsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSegmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSegmentsResponse_httpStatus' - The response's http status code.
--
-- 'segmentsResponse', 'getSegmentsResponse_segmentsResponse' - Undocumented member.
newGetSegmentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'segmentsResponse'
  SegmentsResponse ->
  GetSegmentsResponse
newGetSegmentsResponse
  pHttpStatus_
  pSegmentsResponse_ =
    GetSegmentsResponse'
      { httpStatus = pHttpStatus_,
        segmentsResponse = pSegmentsResponse_
      }

-- | The response's http status code.
getSegmentsResponse_httpStatus :: Lens.Lens' GetSegmentsResponse Core.Int
getSegmentsResponse_httpStatus = Lens.lens (\GetSegmentsResponse' {httpStatus} -> httpStatus) (\s@GetSegmentsResponse' {} a -> s {httpStatus = a} :: GetSegmentsResponse)

-- | Undocumented member.
getSegmentsResponse_segmentsResponse :: Lens.Lens' GetSegmentsResponse SegmentsResponse
getSegmentsResponse_segmentsResponse = Lens.lens (\GetSegmentsResponse' {segmentsResponse} -> segmentsResponse) (\s@GetSegmentsResponse' {} a -> s {segmentsResponse = a} :: GetSegmentsResponse)

instance Core.NFData GetSegmentsResponse
