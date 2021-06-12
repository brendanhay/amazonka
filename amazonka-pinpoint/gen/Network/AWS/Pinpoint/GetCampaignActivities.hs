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
-- Module      : Network.AWS.Pinpoint.GetCampaignActivities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the activities for a campaign.
module Network.AWS.Pinpoint.GetCampaignActivities
  ( -- * Creating a Request
    GetCampaignActivities (..),
    newGetCampaignActivities,

    -- * Request Lenses
    getCampaignActivities_pageSize,
    getCampaignActivities_token,
    getCampaignActivities_applicationId,
    getCampaignActivities_campaignId,

    -- * Destructuring the Response
    GetCampaignActivitiesResponse (..),
    newGetCampaignActivitiesResponse,

    -- * Response Lenses
    getCampaignActivitiesResponse_httpStatus,
    getCampaignActivitiesResponse_activitiesResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCampaignActivities' smart constructor.
data GetCampaignActivities = GetCampaignActivities'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Core.Maybe Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the campaign.
    campaignId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCampaignActivities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getCampaignActivities_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getCampaignActivities_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'applicationId', 'getCampaignActivities_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'campaignId', 'getCampaignActivities_campaignId' - The unique identifier for the campaign.
newGetCampaignActivities ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'campaignId'
  Core.Text ->
  GetCampaignActivities
newGetCampaignActivities pApplicationId_ pCampaignId_ =
  GetCampaignActivities'
    { pageSize = Core.Nothing,
      token = Core.Nothing,
      applicationId = pApplicationId_,
      campaignId = pCampaignId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getCampaignActivities_pageSize :: Lens.Lens' GetCampaignActivities (Core.Maybe Core.Text)
getCampaignActivities_pageSize = Lens.lens (\GetCampaignActivities' {pageSize} -> pageSize) (\s@GetCampaignActivities' {} a -> s {pageSize = a} :: GetCampaignActivities)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getCampaignActivities_token :: Lens.Lens' GetCampaignActivities (Core.Maybe Core.Text)
getCampaignActivities_token = Lens.lens (\GetCampaignActivities' {token} -> token) (\s@GetCampaignActivities' {} a -> s {token = a} :: GetCampaignActivities)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getCampaignActivities_applicationId :: Lens.Lens' GetCampaignActivities Core.Text
getCampaignActivities_applicationId = Lens.lens (\GetCampaignActivities' {applicationId} -> applicationId) (\s@GetCampaignActivities' {} a -> s {applicationId = a} :: GetCampaignActivities)

-- | The unique identifier for the campaign.
getCampaignActivities_campaignId :: Lens.Lens' GetCampaignActivities Core.Text
getCampaignActivities_campaignId = Lens.lens (\GetCampaignActivities' {campaignId} -> campaignId) (\s@GetCampaignActivities' {} a -> s {campaignId = a} :: GetCampaignActivities)

instance Core.AWSRequest GetCampaignActivities where
  type
    AWSResponse GetCampaignActivities =
      GetCampaignActivitiesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignActivitiesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetCampaignActivities

instance Core.NFData GetCampaignActivities

instance Core.ToHeaders GetCampaignActivities where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetCampaignActivities where
  toPath GetCampaignActivities' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/campaigns/",
        Core.toBS campaignId,
        "/activities"
      ]

instance Core.ToQuery GetCampaignActivities where
  toQuery GetCampaignActivities' {..} =
    Core.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetCampaignActivitiesResponse' smart constructor.
data GetCampaignActivitiesResponse = GetCampaignActivitiesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    activitiesResponse :: ActivitiesResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCampaignActivitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCampaignActivitiesResponse_httpStatus' - The response's http status code.
--
-- 'activitiesResponse', 'getCampaignActivitiesResponse_activitiesResponse' - Undocumented member.
newGetCampaignActivitiesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'activitiesResponse'
  ActivitiesResponse ->
  GetCampaignActivitiesResponse
newGetCampaignActivitiesResponse
  pHttpStatus_
  pActivitiesResponse_ =
    GetCampaignActivitiesResponse'
      { httpStatus =
          pHttpStatus_,
        activitiesResponse = pActivitiesResponse_
      }

-- | The response's http status code.
getCampaignActivitiesResponse_httpStatus :: Lens.Lens' GetCampaignActivitiesResponse Core.Int
getCampaignActivitiesResponse_httpStatus = Lens.lens (\GetCampaignActivitiesResponse' {httpStatus} -> httpStatus) (\s@GetCampaignActivitiesResponse' {} a -> s {httpStatus = a} :: GetCampaignActivitiesResponse)

-- | Undocumented member.
getCampaignActivitiesResponse_activitiesResponse :: Lens.Lens' GetCampaignActivitiesResponse ActivitiesResponse
getCampaignActivitiesResponse_activitiesResponse = Lens.lens (\GetCampaignActivitiesResponse' {activitiesResponse} -> activitiesResponse) (\s@GetCampaignActivitiesResponse' {} a -> s {activitiesResponse = a} :: GetCampaignActivitiesResponse)

instance Core.NFData GetCampaignActivitiesResponse
