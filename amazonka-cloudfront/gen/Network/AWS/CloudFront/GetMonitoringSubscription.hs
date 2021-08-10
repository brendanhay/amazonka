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
-- Module      : Network.AWS.CloudFront.GetMonitoringSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about whether additional CloudWatch metrics are enabled
-- for the specified CloudFront distribution.
module Network.AWS.CloudFront.GetMonitoringSubscription
  ( -- * Creating a Request
    GetMonitoringSubscription (..),
    newGetMonitoringSubscription,

    -- * Request Lenses
    getMonitoringSubscription_distributionId,

    -- * Destructuring the Response
    GetMonitoringSubscriptionResponse (..),
    newGetMonitoringSubscriptionResponse,

    -- * Response Lenses
    getMonitoringSubscriptionResponse_monitoringSubscription,
    getMonitoringSubscriptionResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMonitoringSubscription' smart constructor.
data GetMonitoringSubscription = GetMonitoringSubscription'
  { -- | The ID of the distribution that you are getting metrics information for.
    distributionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMonitoringSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionId', 'getMonitoringSubscription_distributionId' - The ID of the distribution that you are getting metrics information for.
newGetMonitoringSubscription ::
  -- | 'distributionId'
  Prelude.Text ->
  GetMonitoringSubscription
newGetMonitoringSubscription pDistributionId_ =
  GetMonitoringSubscription'
    { distributionId =
        pDistributionId_
    }

-- | The ID of the distribution that you are getting metrics information for.
getMonitoringSubscription_distributionId :: Lens.Lens' GetMonitoringSubscription Prelude.Text
getMonitoringSubscription_distributionId = Lens.lens (\GetMonitoringSubscription' {distributionId} -> distributionId) (\s@GetMonitoringSubscription' {} a -> s {distributionId = a} :: GetMonitoringSubscription)

instance Core.AWSRequest GetMonitoringSubscription where
  type
    AWSResponse GetMonitoringSubscription =
      GetMonitoringSubscriptionResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetMonitoringSubscriptionResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMonitoringSubscription

instance Prelude.NFData GetMonitoringSubscription

instance Core.ToHeaders GetMonitoringSubscription where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetMonitoringSubscription where
  toPath GetMonitoringSubscription' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distributions/",
        Core.toBS distributionId,
        "/monitoring-subscription"
      ]

instance Core.ToQuery GetMonitoringSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMonitoringSubscriptionResponse' smart constructor.
data GetMonitoringSubscriptionResponse = GetMonitoringSubscriptionResponse'
  { -- | A monitoring subscription. This structure contains information about
    -- whether additional CloudWatch metrics are enabled for a given CloudFront
    -- distribution.
    monitoringSubscription :: Prelude.Maybe MonitoringSubscription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMonitoringSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringSubscription', 'getMonitoringSubscriptionResponse_monitoringSubscription' - A monitoring subscription. This structure contains information about
-- whether additional CloudWatch metrics are enabled for a given CloudFront
-- distribution.
--
-- 'httpStatus', 'getMonitoringSubscriptionResponse_httpStatus' - The response's http status code.
newGetMonitoringSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMonitoringSubscriptionResponse
newGetMonitoringSubscriptionResponse pHttpStatus_ =
  GetMonitoringSubscriptionResponse'
    { monitoringSubscription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A monitoring subscription. This structure contains information about
-- whether additional CloudWatch metrics are enabled for a given CloudFront
-- distribution.
getMonitoringSubscriptionResponse_monitoringSubscription :: Lens.Lens' GetMonitoringSubscriptionResponse (Prelude.Maybe MonitoringSubscription)
getMonitoringSubscriptionResponse_monitoringSubscription = Lens.lens (\GetMonitoringSubscriptionResponse' {monitoringSubscription} -> monitoringSubscription) (\s@GetMonitoringSubscriptionResponse' {} a -> s {monitoringSubscription = a} :: GetMonitoringSubscriptionResponse)

-- | The response's http status code.
getMonitoringSubscriptionResponse_httpStatus :: Lens.Lens' GetMonitoringSubscriptionResponse Prelude.Int
getMonitoringSubscriptionResponse_httpStatus = Lens.lens (\GetMonitoringSubscriptionResponse' {httpStatus} -> httpStatus) (\s@GetMonitoringSubscriptionResponse' {} a -> s {httpStatus = a} :: GetMonitoringSubscriptionResponse)

instance
  Prelude.NFData
    GetMonitoringSubscriptionResponse
