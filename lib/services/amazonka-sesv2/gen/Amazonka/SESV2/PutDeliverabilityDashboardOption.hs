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
-- Module      : Amazonka.SESV2.PutDeliverabilityDashboardOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable or disable the Deliverability dashboard. When you enable the
-- Deliverability dashboard, you gain access to reputation, deliverability,
-- and other metrics for the domains that you use to send email. You also
-- gain the ability to perform predictive inbox placement tests.
--
-- When you use the Deliverability dashboard, you pay a monthly
-- subscription charge, in addition to any other fees that you accrue by
-- using Amazon SES and other Amazon Web Services services. For more
-- information about the features and cost of a Deliverability dashboard
-- subscription, see
-- <http://aws.amazon.com/ses/pricing/ Amazon SES Pricing>.
module Amazonka.SESV2.PutDeliverabilityDashboardOption
  ( -- * Creating a Request
    PutDeliverabilityDashboardOption (..),
    newPutDeliverabilityDashboardOption,

    -- * Request Lenses
    putDeliverabilityDashboardOption_subscribedDomains,
    putDeliverabilityDashboardOption_dashboardEnabled,

    -- * Destructuring the Response
    PutDeliverabilityDashboardOptionResponse (..),
    newPutDeliverabilityDashboardOptionResponse,

    -- * Response Lenses
    putDeliverabilityDashboardOptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Enable or disable the Deliverability dashboard. When you enable the
-- Deliverability dashboard, you gain access to reputation, deliverability,
-- and other metrics for the domains that you use to send email using
-- Amazon SES API v2. You also gain the ability to perform predictive inbox
-- placement tests.
--
-- When you use the Deliverability dashboard, you pay a monthly
-- subscription charge, in addition to any other fees that you accrue by
-- using Amazon SES and other Amazon Web Services services. For more
-- information about the features and cost of a Deliverability dashboard
-- subscription, see
-- <http://aws.amazon.com/pinpoint/pricing/ Amazon Pinpoint Pricing>.
--
-- /See:/ 'newPutDeliverabilityDashboardOption' smart constructor.
data PutDeliverabilityDashboardOption = PutDeliverabilityDashboardOption'
  { -- | An array of objects, one for each verified domain that you use to send
    -- email and enabled the Deliverability dashboard for.
    subscribedDomains :: Prelude.Maybe [DomainDeliverabilityTrackingOption],
    -- | Specifies whether to enable the Deliverability dashboard. To enable the
    -- dashboard, set this value to @true@.
    dashboardEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDeliverabilityDashboardOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscribedDomains', 'putDeliverabilityDashboardOption_subscribedDomains' - An array of objects, one for each verified domain that you use to send
-- email and enabled the Deliverability dashboard for.
--
-- 'dashboardEnabled', 'putDeliverabilityDashboardOption_dashboardEnabled' - Specifies whether to enable the Deliverability dashboard. To enable the
-- dashboard, set this value to @true@.
newPutDeliverabilityDashboardOption ::
  -- | 'dashboardEnabled'
  Prelude.Bool ->
  PutDeliverabilityDashboardOption
newPutDeliverabilityDashboardOption
  pDashboardEnabled_ =
    PutDeliverabilityDashboardOption'
      { subscribedDomains =
          Prelude.Nothing,
        dashboardEnabled = pDashboardEnabled_
      }

-- | An array of objects, one for each verified domain that you use to send
-- email and enabled the Deliverability dashboard for.
putDeliverabilityDashboardOption_subscribedDomains :: Lens.Lens' PutDeliverabilityDashboardOption (Prelude.Maybe [DomainDeliverabilityTrackingOption])
putDeliverabilityDashboardOption_subscribedDomains = Lens.lens (\PutDeliverabilityDashboardOption' {subscribedDomains} -> subscribedDomains) (\s@PutDeliverabilityDashboardOption' {} a -> s {subscribedDomains = a} :: PutDeliverabilityDashboardOption) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to enable the Deliverability dashboard. To enable the
-- dashboard, set this value to @true@.
putDeliverabilityDashboardOption_dashboardEnabled :: Lens.Lens' PutDeliverabilityDashboardOption Prelude.Bool
putDeliverabilityDashboardOption_dashboardEnabled = Lens.lens (\PutDeliverabilityDashboardOption' {dashboardEnabled} -> dashboardEnabled) (\s@PutDeliverabilityDashboardOption' {} a -> s {dashboardEnabled = a} :: PutDeliverabilityDashboardOption)

instance
  Core.AWSRequest
    PutDeliverabilityDashboardOption
  where
  type
    AWSResponse PutDeliverabilityDashboardOption =
      PutDeliverabilityDashboardOptionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutDeliverabilityDashboardOptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutDeliverabilityDashboardOption
  where
  hashWithSalt
    _salt
    PutDeliverabilityDashboardOption' {..} =
      _salt `Prelude.hashWithSalt` subscribedDomains
        `Prelude.hashWithSalt` dashboardEnabled

instance
  Prelude.NFData
    PutDeliverabilityDashboardOption
  where
  rnf PutDeliverabilityDashboardOption' {..} =
    Prelude.rnf subscribedDomains
      `Prelude.seq` Prelude.rnf dashboardEnabled

instance
  Core.ToHeaders
    PutDeliverabilityDashboardOption
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

instance Core.ToJSON PutDeliverabilityDashboardOption where
  toJSON PutDeliverabilityDashboardOption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SubscribedDomains" Core..=)
              Prelude.<$> subscribedDomains,
            Prelude.Just
              ("DashboardEnabled" Core..= dashboardEnabled)
          ]
      )

instance Core.ToPath PutDeliverabilityDashboardOption where
  toPath =
    Prelude.const "/v2/email/deliverability-dashboard"

instance
  Core.ToQuery
    PutDeliverabilityDashboardOption
  where
  toQuery = Prelude.const Prelude.mempty

-- | A response that indicates whether the Deliverability dashboard is
-- enabled.
--
-- /See:/ 'newPutDeliverabilityDashboardOptionResponse' smart constructor.
data PutDeliverabilityDashboardOptionResponse = PutDeliverabilityDashboardOptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDeliverabilityDashboardOptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putDeliverabilityDashboardOptionResponse_httpStatus' - The response's http status code.
newPutDeliverabilityDashboardOptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDeliverabilityDashboardOptionResponse
newPutDeliverabilityDashboardOptionResponse
  pHttpStatus_ =
    PutDeliverabilityDashboardOptionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putDeliverabilityDashboardOptionResponse_httpStatus :: Lens.Lens' PutDeliverabilityDashboardOptionResponse Prelude.Int
putDeliverabilityDashboardOptionResponse_httpStatus = Lens.lens (\PutDeliverabilityDashboardOptionResponse' {httpStatus} -> httpStatus) (\s@PutDeliverabilityDashboardOptionResponse' {} a -> s {httpStatus = a} :: PutDeliverabilityDashboardOptionResponse)

instance
  Prelude.NFData
    PutDeliverabilityDashboardOptionResponse
  where
  rnf PutDeliverabilityDashboardOptionResponse' {..} =
    Prelude.rnf httpStatus
