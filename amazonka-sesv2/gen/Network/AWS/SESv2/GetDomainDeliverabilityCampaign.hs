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
-- Module      : Network.AWS.SESv2.GetDomainDeliverabilityCampaign
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve all the deliverability data for a specific campaign. This data
-- is available for a campaign only if the campaign sent email by using a
-- domain that the Deliverability dashboard is enabled for.
module Network.AWS.SESv2.GetDomainDeliverabilityCampaign
  ( -- * Creating a Request
    GetDomainDeliverabilityCampaign (..),
    newGetDomainDeliverabilityCampaign,

    -- * Request Lenses
    getDomainDeliverabilityCampaign_campaignId,

    -- * Destructuring the Response
    GetDomainDeliverabilityCampaignResponse (..),
    newGetDomainDeliverabilityCampaignResponse,

    -- * Response Lenses
    getDomainDeliverabilityCampaignResponse_httpStatus,
    getDomainDeliverabilityCampaignResponse_domainDeliverabilityCampaign,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | Retrieve all the deliverability data for a specific campaign. This data
-- is available for a campaign only if the campaign sent email by using a
-- domain that the Deliverability dashboard is enabled for
-- (@PutDeliverabilityDashboardOption@ operation).
--
-- /See:/ 'newGetDomainDeliverabilityCampaign' smart constructor.
data GetDomainDeliverabilityCampaign = GetDomainDeliverabilityCampaign'
  { -- | The unique identifier for the campaign. The Deliverability dashboard
    -- automatically generates and assigns this identifier to a campaign.
    campaignId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainDeliverabilityCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignId', 'getDomainDeliverabilityCampaign_campaignId' - The unique identifier for the campaign. The Deliverability dashboard
-- automatically generates and assigns this identifier to a campaign.
newGetDomainDeliverabilityCampaign ::
  -- | 'campaignId'
  Prelude.Text ->
  GetDomainDeliverabilityCampaign
newGetDomainDeliverabilityCampaign pCampaignId_ =
  GetDomainDeliverabilityCampaign'
    { campaignId =
        pCampaignId_
    }

-- | The unique identifier for the campaign. The Deliverability dashboard
-- automatically generates and assigns this identifier to a campaign.
getDomainDeliverabilityCampaign_campaignId :: Lens.Lens' GetDomainDeliverabilityCampaign Prelude.Text
getDomainDeliverabilityCampaign_campaignId = Lens.lens (\GetDomainDeliverabilityCampaign' {campaignId} -> campaignId) (\s@GetDomainDeliverabilityCampaign' {} a -> s {campaignId = a} :: GetDomainDeliverabilityCampaign)

instance
  Core.AWSRequest
    GetDomainDeliverabilityCampaign
  where
  type
    AWSResponse GetDomainDeliverabilityCampaign =
      GetDomainDeliverabilityCampaignResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainDeliverabilityCampaignResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "DomainDeliverabilityCampaign")
      )

instance
  Prelude.Hashable
    GetDomainDeliverabilityCampaign

instance
  Prelude.NFData
    GetDomainDeliverabilityCampaign

instance
  Core.ToHeaders
    GetDomainDeliverabilityCampaign
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDomainDeliverabilityCampaign where
  toPath GetDomainDeliverabilityCampaign' {..} =
    Prelude.mconcat
      [ "/v2/email/deliverability-dashboard/campaigns/",
        Core.toBS campaignId
      ]

instance Core.ToQuery GetDomainDeliverabilityCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | An object that contains all the deliverability data for a specific
-- campaign. This data is available for a campaign only if the campaign
-- sent email by using a domain that the Deliverability dashboard is
-- enabled for.
--
-- /See:/ 'newGetDomainDeliverabilityCampaignResponse' smart constructor.
data GetDomainDeliverabilityCampaignResponse = GetDomainDeliverabilityCampaignResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object that contains the deliverability data for the campaign.
    domainDeliverabilityCampaign :: DomainDeliverabilityCampaign
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainDeliverabilityCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDomainDeliverabilityCampaignResponse_httpStatus' - The response's http status code.
--
-- 'domainDeliverabilityCampaign', 'getDomainDeliverabilityCampaignResponse_domainDeliverabilityCampaign' - An object that contains the deliverability data for the campaign.
newGetDomainDeliverabilityCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainDeliverabilityCampaign'
  DomainDeliverabilityCampaign ->
  GetDomainDeliverabilityCampaignResponse
newGetDomainDeliverabilityCampaignResponse
  pHttpStatus_
  pDomainDeliverabilityCampaign_ =
    GetDomainDeliverabilityCampaignResponse'
      { httpStatus =
          pHttpStatus_,
        domainDeliverabilityCampaign =
          pDomainDeliverabilityCampaign_
      }

-- | The response's http status code.
getDomainDeliverabilityCampaignResponse_httpStatus :: Lens.Lens' GetDomainDeliverabilityCampaignResponse Prelude.Int
getDomainDeliverabilityCampaignResponse_httpStatus = Lens.lens (\GetDomainDeliverabilityCampaignResponse' {httpStatus} -> httpStatus) (\s@GetDomainDeliverabilityCampaignResponse' {} a -> s {httpStatus = a} :: GetDomainDeliverabilityCampaignResponse)

-- | An object that contains the deliverability data for the campaign.
getDomainDeliverabilityCampaignResponse_domainDeliverabilityCampaign :: Lens.Lens' GetDomainDeliverabilityCampaignResponse DomainDeliverabilityCampaign
getDomainDeliverabilityCampaignResponse_domainDeliverabilityCampaign = Lens.lens (\GetDomainDeliverabilityCampaignResponse' {domainDeliverabilityCampaign} -> domainDeliverabilityCampaign) (\s@GetDomainDeliverabilityCampaignResponse' {} a -> s {domainDeliverabilityCampaign = a} :: GetDomainDeliverabilityCampaignResponse)

instance
  Prelude.NFData
    GetDomainDeliverabilityCampaignResponse
