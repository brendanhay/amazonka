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
-- Module      : Amazonka.Pinpoint.GetCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other
-- settings for a campaign.
module Amazonka.Pinpoint.GetCampaign
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCampaign' smart constructor.
data GetCampaign = GetCampaign'
  { -- | The unique identifier for the campaign.
    campaignId :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  GetCampaign
newGetCampaign pCampaignId_ pApplicationId_ =
  GetCampaign'
    { campaignId = pCampaignId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the campaign.
getCampaign_campaignId :: Lens.Lens' GetCampaign Prelude.Text
getCampaign_campaignId = Lens.lens (\GetCampaign' {campaignId} -> campaignId) (\s@GetCampaign' {} a -> s {campaignId = a} :: GetCampaign)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getCampaign_applicationId :: Lens.Lens' GetCampaign Prelude.Text
getCampaign_applicationId = Lens.lens (\GetCampaign' {applicationId} -> applicationId) (\s@GetCampaign' {} a -> s {applicationId = a} :: GetCampaign)

instance Core.AWSRequest GetCampaign where
  type AWSResponse GetCampaign = GetCampaignResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetCampaign where
  hashWithSalt _salt GetCampaign' {..} =
    _salt `Prelude.hashWithSalt` campaignId
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetCampaign where
  rnf GetCampaign' {..} =
    Prelude.rnf campaignId
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders GetCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCampaign where
  toPath GetCampaign' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/campaigns/",
        Data.toBS campaignId
      ]

instance Data.ToQuery GetCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCampaignResponse' smart constructor.
data GetCampaignResponse = GetCampaignResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    campaignResponse :: CampaignResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
getCampaignResponse_httpStatus :: Lens.Lens' GetCampaignResponse Prelude.Int
getCampaignResponse_httpStatus = Lens.lens (\GetCampaignResponse' {httpStatus} -> httpStatus) (\s@GetCampaignResponse' {} a -> s {httpStatus = a} :: GetCampaignResponse)

-- | Undocumented member.
getCampaignResponse_campaignResponse :: Lens.Lens' GetCampaignResponse CampaignResponse
getCampaignResponse_campaignResponse = Lens.lens (\GetCampaignResponse' {campaignResponse} -> campaignResponse) (\s@GetCampaignResponse' {} a -> s {campaignResponse = a} :: GetCampaignResponse)

instance Prelude.NFData GetCampaignResponse where
  rnf GetCampaignResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf campaignResponse
