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
-- Module      : Amazonka.PinpointEmail.GetDomainDeliverabilityCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve all the deliverability data for a specific campaign. This data
-- is available for a campaign only if the campaign sent email by using a
-- domain that the Deliverability dashboard is enabled for
-- (@PutDeliverabilityDashboardOption@ operation).
module Amazonka.PinpointEmail.GetDomainDeliverabilityCampaign
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Retrieve all the deliverability data for a specific campaign. This data
-- is available for a campaign only if the campaign sent email by using a
-- domain that the Deliverability dashboard is enabled for
-- (@PutDeliverabilityDashboardOption@ operation).
--
-- /See:/ 'newGetDomainDeliverabilityCampaign' smart constructor.
data GetDomainDeliverabilityCampaign = GetDomainDeliverabilityCampaign'
  { -- | The unique identifier for the campaign. Amazon Pinpoint automatically
    -- generates and assigns this identifier to a campaign. This value is not
    -- the same as the campaign identifier that Amazon Pinpoint assigns to
    -- campaigns that you create and manage by using the Amazon Pinpoint API or
    -- the Amazon Pinpoint console.
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
-- 'campaignId', 'getDomainDeliverabilityCampaign_campaignId' - The unique identifier for the campaign. Amazon Pinpoint automatically
-- generates and assigns this identifier to a campaign. This value is not
-- the same as the campaign identifier that Amazon Pinpoint assigns to
-- campaigns that you create and manage by using the Amazon Pinpoint API or
-- the Amazon Pinpoint console.
newGetDomainDeliverabilityCampaign ::
  -- | 'campaignId'
  Prelude.Text ->
  GetDomainDeliverabilityCampaign
newGetDomainDeliverabilityCampaign pCampaignId_ =
  GetDomainDeliverabilityCampaign'
    { campaignId =
        pCampaignId_
    }

-- | The unique identifier for the campaign. Amazon Pinpoint automatically
-- generates and assigns this identifier to a campaign. This value is not
-- the same as the campaign identifier that Amazon Pinpoint assigns to
-- campaigns that you create and manage by using the Amazon Pinpoint API or
-- the Amazon Pinpoint console.
getDomainDeliverabilityCampaign_campaignId :: Lens.Lens' GetDomainDeliverabilityCampaign Prelude.Text
getDomainDeliverabilityCampaign_campaignId = Lens.lens (\GetDomainDeliverabilityCampaign' {campaignId} -> campaignId) (\s@GetDomainDeliverabilityCampaign' {} a -> s {campaignId = a} :: GetDomainDeliverabilityCampaign)

instance
  Core.AWSRequest
    GetDomainDeliverabilityCampaign
  where
  type
    AWSResponse GetDomainDeliverabilityCampaign =
      GetDomainDeliverabilityCampaignResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainDeliverabilityCampaignResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DomainDeliverabilityCampaign")
      )

instance
  Prelude.Hashable
    GetDomainDeliverabilityCampaign
  where
  hashWithSalt
    _salt
    GetDomainDeliverabilityCampaign' {..} =
      _salt `Prelude.hashWithSalt` campaignId

instance
  Prelude.NFData
    GetDomainDeliverabilityCampaign
  where
  rnf GetDomainDeliverabilityCampaign' {..} =
    Prelude.rnf campaignId

instance
  Data.ToHeaders
    GetDomainDeliverabilityCampaign
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDomainDeliverabilityCampaign where
  toPath GetDomainDeliverabilityCampaign' {..} =
    Prelude.mconcat
      [ "/v1/email/deliverability-dashboard/campaigns/",
        Data.toBS campaignId
      ]

instance Data.ToQuery GetDomainDeliverabilityCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | An object that contains all the deliverability data for a specific
-- campaign. This data is available for a campaign only if the campaign
-- sent email by using a domain that the Deliverability dashboard is
-- enabled for (@PutDeliverabilityDashboardOption@ operation).
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
  where
  rnf GetDomainDeliverabilityCampaignResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainDeliverabilityCampaign
