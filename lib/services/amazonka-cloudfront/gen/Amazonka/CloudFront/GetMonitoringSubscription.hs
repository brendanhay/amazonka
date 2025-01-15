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
-- Module      : Amazonka.CloudFront.GetMonitoringSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about whether additional CloudWatch metrics are enabled
-- for the specified CloudFront distribution.
module Amazonka.CloudFront.GetMonitoringSubscription
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

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetMonitoringSubscriptionResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMonitoringSubscription where
  hashWithSalt _salt GetMonitoringSubscription' {..} =
    _salt `Prelude.hashWithSalt` distributionId

instance Prelude.NFData GetMonitoringSubscription where
  rnf GetMonitoringSubscription' {..} =
    Prelude.rnf distributionId

instance Data.ToHeaders GetMonitoringSubscription where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetMonitoringSubscription where
  toPath GetMonitoringSubscription' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distributions/",
        Data.toBS distributionId,
        "/monitoring-subscription/"
      ]

instance Data.ToQuery GetMonitoringSubscription where
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
  where
  rnf GetMonitoringSubscriptionResponse' {..} =
    Prelude.rnf monitoringSubscription `Prelude.seq`
      Prelude.rnf httpStatus
