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
-- Module      : Network.AWS.SESv2.GetDeliverabilityDashboardOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- using Amazon SES and other AWS services. For more information about the
-- features and cost of a Deliverability dashboard subscription, see
-- <http://aws.amazon.com/ses/pricing/ Amazon SES Pricing>.
module Network.AWS.SESv2.GetDeliverabilityDashboardOptions
  ( -- * Creating a Request
    GetDeliverabilityDashboardOptions (..),
    newGetDeliverabilityDashboardOptions,

    -- * Destructuring the Response
    GetDeliverabilityDashboardOptionsResponse (..),
    newGetDeliverabilityDashboardOptionsResponse,

    -- * Response Lenses
    getDeliverabilityDashboardOptionsResponse_subscriptionExpiryDate,
    getDeliverabilityDashboardOptionsResponse_accountStatus,
    getDeliverabilityDashboardOptionsResponse_pendingExpirationSubscribedDomains,
    getDeliverabilityDashboardOptionsResponse_activeSubscribedDomains,
    getDeliverabilityDashboardOptionsResponse_httpStatus,
    getDeliverabilityDashboardOptionsResponse_dashboardEnabled,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | Retrieve information about the status of the Deliverability dashboard
-- for your AWS account. When the Deliverability dashboard is enabled, you
-- gain access to reputation, deliverability, and other metrics for your
-- domains. You also gain the ability to perform predictive inbox placement
-- tests.
--
-- When you use the Deliverability dashboard, you pay a monthly
-- subscription charge, in addition to any other fees that you accrue by
-- using Amazon SES and other AWS services. For more information about the
-- features and cost of a Deliverability dashboard subscription, see
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeliverabilityDashboardOptionsResponse'
            Prelude.<$> (x Core..?> "SubscriptionExpiryDate")
              Prelude.<*> (x Core..?> "AccountStatus")
              Prelude.<*> ( x Core..?> "PendingExpirationSubscribedDomains"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> ( x Core..?> "ActiveSubscribedDomains"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Core..:> "DashboardEnabled")
      )

instance
  Prelude.Hashable
    GetDeliverabilityDashboardOptions

instance
  Prelude.NFData
    GetDeliverabilityDashboardOptions

instance
  Core.ToHeaders
    GetDeliverabilityDashboardOptions
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

instance
  Core.ToPath
    GetDeliverabilityDashboardOptions
  where
  toPath =
    Prelude.const "/v2/email/deliverability-dashboard"

instance
  Core.ToQuery
    GetDeliverabilityDashboardOptions
  where
  toQuery = Prelude.const Prelude.mempty

-- | An object that shows the status of the Deliverability dashboard.
--
-- /See:/ 'newGetDeliverabilityDashboardOptionsResponse' smart constructor.
data GetDeliverabilityDashboardOptionsResponse = GetDeliverabilityDashboardOptionsResponse'
  { -- | The date, in Unix time format, when your current subscription to the
    -- Deliverability dashboard is scheduled to expire, if your subscription is
    -- scheduled to expire at the end of the current calendar month. This value
    -- is null if you have an active subscription that isn’t due to expire at
    -- the end of the month.
    subscriptionExpiryDate :: Prelude.Maybe Core.POSIX,
    -- | The current status of your Deliverability dashboard subscription. If
    -- this value is @PENDING_EXPIRATION@, your subscription is scheduled to
    -- expire at the end of the current calendar month.
    accountStatus :: Prelude.Maybe DeliverabilityDashboardAccountStatus,
    -- | An array of objects, one for each verified domain that you use to send
    -- email and currently has an active Deliverability dashboard subscription
    -- that\'s scheduled to expire at the end of the current calendar month.
    pendingExpirationSubscribedDomains :: Prelude.Maybe [DomainDeliverabilityTrackingOption],
    -- | An array of objects, one for each verified domain that you use to send
    -- email and currently has an active Deliverability dashboard subscription
    -- that isn’t scheduled to expire at the end of the current calendar month.
    activeSubscribedDomains :: Prelude.Maybe [DomainDeliverabilityTrackingOption],
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
-- 'subscriptionExpiryDate', 'getDeliverabilityDashboardOptionsResponse_subscriptionExpiryDate' - The date, in Unix time format, when your current subscription to the
-- Deliverability dashboard is scheduled to expire, if your subscription is
-- scheduled to expire at the end of the current calendar month. This value
-- is null if you have an active subscription that isn’t due to expire at
-- the end of the month.
--
-- 'accountStatus', 'getDeliverabilityDashboardOptionsResponse_accountStatus' - The current status of your Deliverability dashboard subscription. If
-- this value is @PENDING_EXPIRATION@, your subscription is scheduled to
-- expire at the end of the current calendar month.
--
-- 'pendingExpirationSubscribedDomains', 'getDeliverabilityDashboardOptionsResponse_pendingExpirationSubscribedDomains' - An array of objects, one for each verified domain that you use to send
-- email and currently has an active Deliverability dashboard subscription
-- that\'s scheduled to expire at the end of the current calendar month.
--
-- 'activeSubscribedDomains', 'getDeliverabilityDashboardOptionsResponse_activeSubscribedDomains' - An array of objects, one for each verified domain that you use to send
-- email and currently has an active Deliverability dashboard subscription
-- that isn’t scheduled to expire at the end of the current calendar month.
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
      { subscriptionExpiryDate =
          Prelude.Nothing,
        accountStatus = Prelude.Nothing,
        pendingExpirationSubscribedDomains =
          Prelude.Nothing,
        activeSubscribedDomains =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        dashboardEnabled =
          pDashboardEnabled_
      }

-- | The date, in Unix time format, when your current subscription to the
-- Deliverability dashboard is scheduled to expire, if your subscription is
-- scheduled to expire at the end of the current calendar month. This value
-- is null if you have an active subscription that isn’t due to expire at
-- the end of the month.
getDeliverabilityDashboardOptionsResponse_subscriptionExpiryDate :: Lens.Lens' GetDeliverabilityDashboardOptionsResponse (Prelude.Maybe Prelude.UTCTime)
getDeliverabilityDashboardOptionsResponse_subscriptionExpiryDate = Lens.lens (\GetDeliverabilityDashboardOptionsResponse' {subscriptionExpiryDate} -> subscriptionExpiryDate) (\s@GetDeliverabilityDashboardOptionsResponse' {} a -> s {subscriptionExpiryDate = a} :: GetDeliverabilityDashboardOptionsResponse) Prelude.. Lens.mapping Core._Time

-- | The current status of your Deliverability dashboard subscription. If
-- this value is @PENDING_EXPIRATION@, your subscription is scheduled to
-- expire at the end of the current calendar month.
getDeliverabilityDashboardOptionsResponse_accountStatus :: Lens.Lens' GetDeliverabilityDashboardOptionsResponse (Prelude.Maybe DeliverabilityDashboardAccountStatus)
getDeliverabilityDashboardOptionsResponse_accountStatus = Lens.lens (\GetDeliverabilityDashboardOptionsResponse' {accountStatus} -> accountStatus) (\s@GetDeliverabilityDashboardOptionsResponse' {} a -> s {accountStatus = a} :: GetDeliverabilityDashboardOptionsResponse)

-- | An array of objects, one for each verified domain that you use to send
-- email and currently has an active Deliverability dashboard subscription
-- that\'s scheduled to expire at the end of the current calendar month.
getDeliverabilityDashboardOptionsResponse_pendingExpirationSubscribedDomains :: Lens.Lens' GetDeliverabilityDashboardOptionsResponse (Prelude.Maybe [DomainDeliverabilityTrackingOption])
getDeliverabilityDashboardOptionsResponse_pendingExpirationSubscribedDomains = Lens.lens (\GetDeliverabilityDashboardOptionsResponse' {pendingExpirationSubscribedDomains} -> pendingExpirationSubscribedDomains) (\s@GetDeliverabilityDashboardOptionsResponse' {} a -> s {pendingExpirationSubscribedDomains = a} :: GetDeliverabilityDashboardOptionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An array of objects, one for each verified domain that you use to send
-- email and currently has an active Deliverability dashboard subscription
-- that isn’t scheduled to expire at the end of the current calendar month.
getDeliverabilityDashboardOptionsResponse_activeSubscribedDomains :: Lens.Lens' GetDeliverabilityDashboardOptionsResponse (Prelude.Maybe [DomainDeliverabilityTrackingOption])
getDeliverabilityDashboardOptionsResponse_activeSubscribedDomains = Lens.lens (\GetDeliverabilityDashboardOptionsResponse' {activeSubscribedDomains} -> activeSubscribedDomains) (\s@GetDeliverabilityDashboardOptionsResponse' {} a -> s {activeSubscribedDomains = a} :: GetDeliverabilityDashboardOptionsResponse) Prelude.. Lens.mapping Lens._Coerce

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
