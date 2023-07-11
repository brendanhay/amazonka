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
-- Module      : Amazonka.SESV2.GetDeliverabilityDashboardOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve information about the status of the Deliverability dashboard
-- for your account. When the Deliverability dashboard is enabled, you gain
-- access to reputation, deliverability, and other metrics for the domains
-- that you use to send email. You also gain the ability to perform
-- predictive inbox placement tests.
--
-- When you use the Deliverability dashboard, you pay a monthly
-- subscription charge, in addition to any other fees that you accrue by
-- using Amazon SES and other Amazon Web Services services. For more
-- information about the features and cost of a Deliverability dashboard
-- subscription, see
-- <http://aws.amazon.com/ses/pricing/ Amazon SES Pricing>.
module Amazonka.SESV2.GetDeliverabilityDashboardOptions
  ( -- * Creating a Request
    GetDeliverabilityDashboardOptions (..),
    newGetDeliverabilityDashboardOptions,

    -- * Destructuring the Response
    GetDeliverabilityDashboardOptionsResponse (..),
    newGetDeliverabilityDashboardOptionsResponse,

    -- * Response Lenses
    getDeliverabilityDashboardOptionsResponse_accountStatus,
    getDeliverabilityDashboardOptionsResponse_activeSubscribedDomains,
    getDeliverabilityDashboardOptionsResponse_pendingExpirationSubscribedDomains,
    getDeliverabilityDashboardOptionsResponse_subscriptionExpiryDate,
    getDeliverabilityDashboardOptionsResponse_httpStatus,
    getDeliverabilityDashboardOptionsResponse_dashboardEnabled,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Retrieve information about the status of the Deliverability dashboard
-- for your Amazon Web Services account. When the Deliverability dashboard
-- is enabled, you gain access to reputation, deliverability, and other
-- metrics for your domains. You also gain the ability to perform
-- predictive inbox placement tests.
--
-- When you use the Deliverability dashboard, you pay a monthly
-- subscription charge, in addition to any other fees that you accrue by
-- using Amazon SES and other Amazon Web Services services. For more
-- information about the features and cost of a Deliverability dashboard
-- subscription, see
-- <http://aws.amazon.com/pinpoint/pricing/ Amazon Pinpoint Pricing>.
--
-- /See:/ 'newGetDeliverabilityDashboardOptions' smart constructor.
data GetDeliverabilityDashboardOptions = GetDeliverabilityDashboardOptions'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeliverabilityDashboardOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDeliverabilityDashboardOptions ::
  GetDeliverabilityDashboardOptions
newGetDeliverabilityDashboardOptions =
  GetDeliverabilityDashboardOptions'

instance
  Core.AWSRequest
    GetDeliverabilityDashboardOptions
  where
  type
    AWSResponse GetDeliverabilityDashboardOptions =
      GetDeliverabilityDashboardOptionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeliverabilityDashboardOptionsResponse'
            Prelude.<$> (x Data..?> "AccountStatus")
            Prelude.<*> ( x
                            Data..?> "ActiveSubscribedDomains"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "PendingExpirationSubscribedDomains"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "SubscriptionExpiryDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DashboardEnabled")
      )

instance
  Prelude.Hashable
    GetDeliverabilityDashboardOptions
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetDeliverabilityDashboardOptions
  where
  rnf _ = ()

instance
  Data.ToHeaders
    GetDeliverabilityDashboardOptions
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

instance
  Data.ToPath
    GetDeliverabilityDashboardOptions
  where
  toPath =
    Prelude.const "/v2/email/deliverability-dashboard"

instance
  Data.ToQuery
    GetDeliverabilityDashboardOptions
  where
  toQuery = Prelude.const Prelude.mempty

-- | An object that shows the status of the Deliverability dashboard.
--
-- /See:/ 'newGetDeliverabilityDashboardOptionsResponse' smart constructor.
data GetDeliverabilityDashboardOptionsResponse = GetDeliverabilityDashboardOptionsResponse'
  { -- | The current status of your Deliverability dashboard subscription. If
    -- this value is @PENDING_EXPIRATION@, your subscription is scheduled to
    -- expire at the end of the current calendar month.
    accountStatus :: Prelude.Maybe DeliverabilityDashboardAccountStatus,
    -- | An array of objects, one for each verified domain that you use to send
    -- email and currently has an active Deliverability dashboard subscription
    -- that isn’t scheduled to expire at the end of the current calendar month.
    activeSubscribedDomains :: Prelude.Maybe [DomainDeliverabilityTrackingOption],
    -- | An array of objects, one for each verified domain that you use to send
    -- email and currently has an active Deliverability dashboard subscription
    -- that\'s scheduled to expire at the end of the current calendar month.
    pendingExpirationSubscribedDomains :: Prelude.Maybe [DomainDeliverabilityTrackingOption],
    -- | The date when your current subscription to the Deliverability dashboard
    -- is scheduled to expire, if your subscription is scheduled to expire at
    -- the end of the current calendar month. This value is null if you have an
    -- active subscription that isn’t due to expire at the end of the month.
    subscriptionExpiryDate :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Specifies whether the Deliverability dashboard is enabled. If this value
    -- is @true@, the dashboard is enabled.
    dashboardEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeliverabilityDashboardOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountStatus', 'getDeliverabilityDashboardOptionsResponse_accountStatus' - The current status of your Deliverability dashboard subscription. If
-- this value is @PENDING_EXPIRATION@, your subscription is scheduled to
-- expire at the end of the current calendar month.
--
-- 'activeSubscribedDomains', 'getDeliverabilityDashboardOptionsResponse_activeSubscribedDomains' - An array of objects, one for each verified domain that you use to send
-- email and currently has an active Deliverability dashboard subscription
-- that isn’t scheduled to expire at the end of the current calendar month.
--
-- 'pendingExpirationSubscribedDomains', 'getDeliverabilityDashboardOptionsResponse_pendingExpirationSubscribedDomains' - An array of objects, one for each verified domain that you use to send
-- email and currently has an active Deliverability dashboard subscription
-- that\'s scheduled to expire at the end of the current calendar month.
--
-- 'subscriptionExpiryDate', 'getDeliverabilityDashboardOptionsResponse_subscriptionExpiryDate' - The date when your current subscription to the Deliverability dashboard
-- is scheduled to expire, if your subscription is scheduled to expire at
-- the end of the current calendar month. This value is null if you have an
-- active subscription that isn’t due to expire at the end of the month.
--
-- 'httpStatus', 'getDeliverabilityDashboardOptionsResponse_httpStatus' - The response's http status code.
--
-- 'dashboardEnabled', 'getDeliverabilityDashboardOptionsResponse_dashboardEnabled' - Specifies whether the Deliverability dashboard is enabled. If this value
-- is @true@, the dashboard is enabled.
newGetDeliverabilityDashboardOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'dashboardEnabled'
  Prelude.Bool ->
  GetDeliverabilityDashboardOptionsResponse
newGetDeliverabilityDashboardOptionsResponse
  pHttpStatus_
  pDashboardEnabled_ =
    GetDeliverabilityDashboardOptionsResponse'
      { accountStatus =
          Prelude.Nothing,
        activeSubscribedDomains =
          Prelude.Nothing,
        pendingExpirationSubscribedDomains =
          Prelude.Nothing,
        subscriptionExpiryDate =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        dashboardEnabled =
          pDashboardEnabled_
      }

-- | The current status of your Deliverability dashboard subscription. If
-- this value is @PENDING_EXPIRATION@, your subscription is scheduled to
-- expire at the end of the current calendar month.
getDeliverabilityDashboardOptionsResponse_accountStatus :: Lens.Lens' GetDeliverabilityDashboardOptionsResponse (Prelude.Maybe DeliverabilityDashboardAccountStatus)
getDeliverabilityDashboardOptionsResponse_accountStatus = Lens.lens (\GetDeliverabilityDashboardOptionsResponse' {accountStatus} -> accountStatus) (\s@GetDeliverabilityDashboardOptionsResponse' {} a -> s {accountStatus = a} :: GetDeliverabilityDashboardOptionsResponse)

-- | An array of objects, one for each verified domain that you use to send
-- email and currently has an active Deliverability dashboard subscription
-- that isn’t scheduled to expire at the end of the current calendar month.
getDeliverabilityDashboardOptionsResponse_activeSubscribedDomains :: Lens.Lens' GetDeliverabilityDashboardOptionsResponse (Prelude.Maybe [DomainDeliverabilityTrackingOption])
getDeliverabilityDashboardOptionsResponse_activeSubscribedDomains = Lens.lens (\GetDeliverabilityDashboardOptionsResponse' {activeSubscribedDomains} -> activeSubscribedDomains) (\s@GetDeliverabilityDashboardOptionsResponse' {} a -> s {activeSubscribedDomains = a} :: GetDeliverabilityDashboardOptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects, one for each verified domain that you use to send
-- email and currently has an active Deliverability dashboard subscription
-- that\'s scheduled to expire at the end of the current calendar month.
getDeliverabilityDashboardOptionsResponse_pendingExpirationSubscribedDomains :: Lens.Lens' GetDeliverabilityDashboardOptionsResponse (Prelude.Maybe [DomainDeliverabilityTrackingOption])
getDeliverabilityDashboardOptionsResponse_pendingExpirationSubscribedDomains = Lens.lens (\GetDeliverabilityDashboardOptionsResponse' {pendingExpirationSubscribedDomains} -> pendingExpirationSubscribedDomains) (\s@GetDeliverabilityDashboardOptionsResponse' {} a -> s {pendingExpirationSubscribedDomains = a} :: GetDeliverabilityDashboardOptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date when your current subscription to the Deliverability dashboard
-- is scheduled to expire, if your subscription is scheduled to expire at
-- the end of the current calendar month. This value is null if you have an
-- active subscription that isn’t due to expire at the end of the month.
getDeliverabilityDashboardOptionsResponse_subscriptionExpiryDate :: Lens.Lens' GetDeliverabilityDashboardOptionsResponse (Prelude.Maybe Prelude.UTCTime)
getDeliverabilityDashboardOptionsResponse_subscriptionExpiryDate = Lens.lens (\GetDeliverabilityDashboardOptionsResponse' {subscriptionExpiryDate} -> subscriptionExpiryDate) (\s@GetDeliverabilityDashboardOptionsResponse' {} a -> s {subscriptionExpiryDate = a} :: GetDeliverabilityDashboardOptionsResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getDeliverabilityDashboardOptionsResponse_httpStatus :: Lens.Lens' GetDeliverabilityDashboardOptionsResponse Prelude.Int
getDeliverabilityDashboardOptionsResponse_httpStatus = Lens.lens (\GetDeliverabilityDashboardOptionsResponse' {httpStatus} -> httpStatus) (\s@GetDeliverabilityDashboardOptionsResponse' {} a -> s {httpStatus = a} :: GetDeliverabilityDashboardOptionsResponse)

-- | Specifies whether the Deliverability dashboard is enabled. If this value
-- is @true@, the dashboard is enabled.
getDeliverabilityDashboardOptionsResponse_dashboardEnabled :: Lens.Lens' GetDeliverabilityDashboardOptionsResponse Prelude.Bool
getDeliverabilityDashboardOptionsResponse_dashboardEnabled = Lens.lens (\GetDeliverabilityDashboardOptionsResponse' {dashboardEnabled} -> dashboardEnabled) (\s@GetDeliverabilityDashboardOptionsResponse' {} a -> s {dashboardEnabled = a} :: GetDeliverabilityDashboardOptionsResponse)

instance
  Prelude.NFData
    GetDeliverabilityDashboardOptionsResponse
  where
  rnf GetDeliverabilityDashboardOptionsResponse' {..} =
    Prelude.rnf accountStatus
      `Prelude.seq` Prelude.rnf activeSubscribedDomains
      `Prelude.seq` Prelude.rnf pendingExpirationSubscribedDomains
      `Prelude.seq` Prelude.rnf subscriptionExpiryDate
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf dashboardEnabled
