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
-- Module      : Network.AWS.Pinpoint.GetCampaigns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other
-- settings for all the campaigns that are associated with an application.
module Network.AWS.Pinpoint.GetCampaigns
  ( -- * Creating a Request
    GetCampaigns (..),
    newGetCampaigns,

    -- * Request Lenses
    getCampaigns_pageSize,
    getCampaigns_token,
    getCampaigns_applicationId,

    -- * Destructuring the Response
    GetCampaignsResponse (..),
    newGetCampaignsResponse,

    -- * Response Lenses
    getCampaignsResponse_httpStatus,
    getCampaignsResponse_campaignsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCampaigns' smart constructor.
data GetCampaigns = GetCampaigns'
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
-- Create a value of 'GetCampaigns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getCampaigns_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getCampaigns_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'applicationId', 'getCampaigns_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetCampaigns ::
  -- | 'applicationId'
  Core.Text ->
  GetCampaigns
newGetCampaigns pApplicationId_ =
  GetCampaigns'
    { pageSize = Core.Nothing,
      token = Core.Nothing,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getCampaigns_pageSize :: Lens.Lens' GetCampaigns (Core.Maybe Core.Text)
getCampaigns_pageSize = Lens.lens (\GetCampaigns' {pageSize} -> pageSize) (\s@GetCampaigns' {} a -> s {pageSize = a} :: GetCampaigns)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getCampaigns_token :: Lens.Lens' GetCampaigns (Core.Maybe Core.Text)
getCampaigns_token = Lens.lens (\GetCampaigns' {token} -> token) (\s@GetCampaigns' {} a -> s {token = a} :: GetCampaigns)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getCampaigns_applicationId :: Lens.Lens' GetCampaigns Core.Text
getCampaigns_applicationId = Lens.lens (\GetCampaigns' {applicationId} -> applicationId) (\s@GetCampaigns' {} a -> s {applicationId = a} :: GetCampaigns)

instance Core.AWSRequest GetCampaigns where
  type AWSResponse GetCampaigns = GetCampaignsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetCampaigns

instance Core.NFData GetCampaigns

instance Core.ToHeaders GetCampaigns where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetCampaigns where
  toPath GetCampaigns' {..} =
    Core.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/campaigns"]

instance Core.ToQuery GetCampaigns where
  toQuery GetCampaigns' {..} =
    Core.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetCampaignsResponse' smart constructor.
data GetCampaignsResponse = GetCampaignsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    campaignsResponse :: CampaignsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCampaignsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCampaignsResponse_httpStatus' - The response's http status code.
--
-- 'campaignsResponse', 'getCampaignsResponse_campaignsResponse' - Undocumented member.
newGetCampaignsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'campaignsResponse'
  CampaignsResponse ->
  GetCampaignsResponse
newGetCampaignsResponse
  pHttpStatus_
  pCampaignsResponse_ =
    GetCampaignsResponse'
      { httpStatus = pHttpStatus_,
        campaignsResponse = pCampaignsResponse_
      }

-- | The response's http status code.
getCampaignsResponse_httpStatus :: Lens.Lens' GetCampaignsResponse Core.Int
getCampaignsResponse_httpStatus = Lens.lens (\GetCampaignsResponse' {httpStatus} -> httpStatus) (\s@GetCampaignsResponse' {} a -> s {httpStatus = a} :: GetCampaignsResponse)

-- | Undocumented member.
getCampaignsResponse_campaignsResponse :: Lens.Lens' GetCampaignsResponse CampaignsResponse
getCampaignsResponse_campaignsResponse = Lens.lens (\GetCampaignsResponse' {campaignsResponse} -> campaignsResponse) (\s@GetCampaignsResponse' {} a -> s {campaignsResponse = a} :: GetCampaignsResponse)

instance Core.NFData GetCampaignsResponse
