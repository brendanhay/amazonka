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
-- Module      : Network.AWS.Pinpoint.GetCampaign
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other
-- settings for a campaign.
module Network.AWS.Pinpoint.GetCampaign
  ( -- * Creating a Request
    GetCampaign (..),
    newGetCampaign,

    -- * Request Lenses
    getCampaign_campaignId,
    getCampaign_applicationId,

    -- * Destructuring the Response
    GetCampaignResponse (..),
    newGetCampaignResponse,

    -- * Response Lenses
    getCampaignResponse_httpStatus,
    getCampaignResponse_campaignResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCampaign' smart constructor.
data GetCampaign = GetCampaign'
  { -- | The unique identifier for the campaign.
    campaignId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignId', 'getCampaign_campaignId' - The unique identifier for the campaign.
--
-- 'applicationId', 'getCampaign_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetCampaign ::
  -- | 'campaignId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  GetCampaign
newGetCampaign pCampaignId_ pApplicationId_ =
  GetCampaign'
    { campaignId = pCampaignId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the campaign.
getCampaign_campaignId :: Lens.Lens' GetCampaign Core.Text
getCampaign_campaignId = Lens.lens (\GetCampaign' {campaignId} -> campaignId) (\s@GetCampaign' {} a -> s {campaignId = a} :: GetCampaign)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getCampaign_applicationId :: Lens.Lens' GetCampaign Core.Text
getCampaign_applicationId = Lens.lens (\GetCampaign' {applicationId} -> applicationId) (\s@GetCampaign' {} a -> s {applicationId = a} :: GetCampaign)

instance Core.AWSRequest GetCampaign where
  type AWSResponse GetCampaign = GetCampaignResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetCampaign

instance Core.NFData GetCampaign

instance Core.ToHeaders GetCampaign where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetCampaign where
  toPath GetCampaign' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/campaigns/",
        Core.toBS campaignId
      ]

instance Core.ToQuery GetCampaign where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCampaignResponse' smart constructor.
data GetCampaignResponse = GetCampaignResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    campaignResponse :: CampaignResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCampaignResponse_httpStatus' - The response's http status code.
--
-- 'campaignResponse', 'getCampaignResponse_campaignResponse' - Undocumented member.
newGetCampaignResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'campaignResponse'
  CampaignResponse ->
  GetCampaignResponse
newGetCampaignResponse
  pHttpStatus_
  pCampaignResponse_ =
    GetCampaignResponse'
      { httpStatus = pHttpStatus_,
        campaignResponse = pCampaignResponse_
      }

-- | The response's http status code.
getCampaignResponse_httpStatus :: Lens.Lens' GetCampaignResponse Core.Int
getCampaignResponse_httpStatus = Lens.lens (\GetCampaignResponse' {httpStatus} -> httpStatus) (\s@GetCampaignResponse' {} a -> s {httpStatus = a} :: GetCampaignResponse)

-- | Undocumented member.
getCampaignResponse_campaignResponse :: Lens.Lens' GetCampaignResponse CampaignResponse
getCampaignResponse_campaignResponse = Lens.lens (\GetCampaignResponse' {campaignResponse} -> campaignResponse) (\s@GetCampaignResponse' {} a -> s {campaignResponse = a} :: GetCampaignResponse)

instance Core.NFData GetCampaignResponse
