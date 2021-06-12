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
-- Module      : Network.AWS.Pinpoint.UpdateCampaign
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration and other settings for a campaign.
module Network.AWS.Pinpoint.UpdateCampaign
  ( -- * Creating a Request
    UpdateCampaign (..),
    newUpdateCampaign,

    -- * Request Lenses
    updateCampaign_campaignId,
    updateCampaign_applicationId,
    updateCampaign_writeCampaignRequest,

    -- * Destructuring the Response
    UpdateCampaignResponse (..),
    newUpdateCampaignResponse,

    -- * Response Lenses
    updateCampaignResponse_httpStatus,
    updateCampaignResponse_campaignResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCampaign' smart constructor.
data UpdateCampaign = UpdateCampaign'
  { -- | The unique identifier for the campaign.
    campaignId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    writeCampaignRequest :: WriteCampaignRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignId', 'updateCampaign_campaignId' - The unique identifier for the campaign.
--
-- 'applicationId', 'updateCampaign_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'writeCampaignRequest', 'updateCampaign_writeCampaignRequest' - Undocumented member.
newUpdateCampaign ::
  -- | 'campaignId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  -- | 'writeCampaignRequest'
  WriteCampaignRequest ->
  UpdateCampaign
newUpdateCampaign
  pCampaignId_
  pApplicationId_
  pWriteCampaignRequest_ =
    UpdateCampaign'
      { campaignId = pCampaignId_,
        applicationId = pApplicationId_,
        writeCampaignRequest = pWriteCampaignRequest_
      }

-- | The unique identifier for the campaign.
updateCampaign_campaignId :: Lens.Lens' UpdateCampaign Core.Text
updateCampaign_campaignId = Lens.lens (\UpdateCampaign' {campaignId} -> campaignId) (\s@UpdateCampaign' {} a -> s {campaignId = a} :: UpdateCampaign)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateCampaign_applicationId :: Lens.Lens' UpdateCampaign Core.Text
updateCampaign_applicationId = Lens.lens (\UpdateCampaign' {applicationId} -> applicationId) (\s@UpdateCampaign' {} a -> s {applicationId = a} :: UpdateCampaign)

-- | Undocumented member.
updateCampaign_writeCampaignRequest :: Lens.Lens' UpdateCampaign WriteCampaignRequest
updateCampaign_writeCampaignRequest = Lens.lens (\UpdateCampaign' {writeCampaignRequest} -> writeCampaignRequest) (\s@UpdateCampaign' {} a -> s {writeCampaignRequest = a} :: UpdateCampaign)

instance Core.AWSRequest UpdateCampaign where
  type
    AWSResponse UpdateCampaign =
      UpdateCampaignResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCampaignResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateCampaign

instance Core.NFData UpdateCampaign

instance Core.ToHeaders UpdateCampaign where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateCampaign where
  toJSON UpdateCampaign' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "WriteCampaignRequest"
                  Core..= writeCampaignRequest
              )
          ]
      )

instance Core.ToPath UpdateCampaign where
  toPath UpdateCampaign' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/campaigns/",
        Core.toBS campaignId
      ]

instance Core.ToQuery UpdateCampaign where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateCampaignResponse' smart constructor.
data UpdateCampaignResponse = UpdateCampaignResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    campaignResponse :: CampaignResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCampaignResponse_httpStatus' - The response's http status code.
--
-- 'campaignResponse', 'updateCampaignResponse_campaignResponse' - Undocumented member.
newUpdateCampaignResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'campaignResponse'
  CampaignResponse ->
  UpdateCampaignResponse
newUpdateCampaignResponse
  pHttpStatus_
  pCampaignResponse_ =
    UpdateCampaignResponse'
      { httpStatus = pHttpStatus_,
        campaignResponse = pCampaignResponse_
      }

-- | The response's http status code.
updateCampaignResponse_httpStatus :: Lens.Lens' UpdateCampaignResponse Core.Int
updateCampaignResponse_httpStatus = Lens.lens (\UpdateCampaignResponse' {httpStatus} -> httpStatus) (\s@UpdateCampaignResponse' {} a -> s {httpStatus = a} :: UpdateCampaignResponse)

-- | Undocumented member.
updateCampaignResponse_campaignResponse :: Lens.Lens' UpdateCampaignResponse CampaignResponse
updateCampaignResponse_campaignResponse = Lens.lens (\UpdateCampaignResponse' {campaignResponse} -> campaignResponse) (\s@UpdateCampaignResponse' {} a -> s {campaignResponse = a} :: UpdateCampaignResponse)

instance Core.NFData UpdateCampaignResponse
