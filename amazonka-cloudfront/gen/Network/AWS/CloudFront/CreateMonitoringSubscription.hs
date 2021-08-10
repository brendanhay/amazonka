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
-- Module      : Network.AWS.CloudFront.CreateMonitoringSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables additional CloudWatch metrics for the specified CloudFront
-- distribution. The additional metrics incur an additional cost.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/viewing-cloudfront-metrics.html#monitoring-console.distributions-additional Viewing additional CloudFront distribution metrics>
-- in the /Amazon CloudFront Developer Guide/.
module Network.AWS.CloudFront.CreateMonitoringSubscription
  ( -- * Creating a Request
    CreateMonitoringSubscription (..),
    newCreateMonitoringSubscription,

    -- * Request Lenses
    createMonitoringSubscription_monitoringSubscription,
    createMonitoringSubscription_distributionId,

    -- * Destructuring the Response
    CreateMonitoringSubscriptionResponse (..),
    newCreateMonitoringSubscriptionResponse,

    -- * Response Lenses
    createMonitoringSubscriptionResponse_monitoringSubscription,
    createMonitoringSubscriptionResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateMonitoringSubscription' smart constructor.
data CreateMonitoringSubscription = CreateMonitoringSubscription'
  { -- | A monitoring subscription. This structure contains information about
    -- whether additional CloudWatch metrics are enabled for a given CloudFront
    -- distribution.
    monitoringSubscription :: MonitoringSubscription,
    -- | The ID of the distribution that you are enabling metrics for.
    distributionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMonitoringSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringSubscription', 'createMonitoringSubscription_monitoringSubscription' - A monitoring subscription. This structure contains information about
-- whether additional CloudWatch metrics are enabled for a given CloudFront
-- distribution.
--
-- 'distributionId', 'createMonitoringSubscription_distributionId' - The ID of the distribution that you are enabling metrics for.
newCreateMonitoringSubscription ::
  -- | 'monitoringSubscription'
  MonitoringSubscription ->
  -- | 'distributionId'
  Prelude.Text ->
  CreateMonitoringSubscription
newCreateMonitoringSubscription
  pMonitoringSubscription_
  pDistributionId_ =
    CreateMonitoringSubscription'
      { monitoringSubscription =
          pMonitoringSubscription_,
        distributionId = pDistributionId_
      }

-- | A monitoring subscription. This structure contains information about
-- whether additional CloudWatch metrics are enabled for a given CloudFront
-- distribution.
createMonitoringSubscription_monitoringSubscription :: Lens.Lens' CreateMonitoringSubscription MonitoringSubscription
createMonitoringSubscription_monitoringSubscription = Lens.lens (\CreateMonitoringSubscription' {monitoringSubscription} -> monitoringSubscription) (\s@CreateMonitoringSubscription' {} a -> s {monitoringSubscription = a} :: CreateMonitoringSubscription)

-- | The ID of the distribution that you are enabling metrics for.
createMonitoringSubscription_distributionId :: Lens.Lens' CreateMonitoringSubscription Prelude.Text
createMonitoringSubscription_distributionId = Lens.lens (\CreateMonitoringSubscription' {distributionId} -> distributionId) (\s@CreateMonitoringSubscription' {} a -> s {distributionId = a} :: CreateMonitoringSubscription)

instance Core.AWSRequest CreateMonitoringSubscription where
  type
    AWSResponse CreateMonitoringSubscription =
      CreateMonitoringSubscriptionResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateMonitoringSubscriptionResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateMonitoringSubscription

instance Prelude.NFData CreateMonitoringSubscription

instance Core.ToElement CreateMonitoringSubscription where
  toElement CreateMonitoringSubscription' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}MonitoringSubscription"
      monitoringSubscription

instance Core.ToHeaders CreateMonitoringSubscription where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateMonitoringSubscription where
  toPath CreateMonitoringSubscription' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distributions/",
        Core.toBS distributionId,
        "/monitoring-subscription"
      ]

instance Core.ToQuery CreateMonitoringSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMonitoringSubscriptionResponse' smart constructor.
data CreateMonitoringSubscriptionResponse = CreateMonitoringSubscriptionResponse'
  { -- | A monitoring subscription. This structure contains information about
    -- whether additional CloudWatch metrics are enabled for a given CloudFront
    -- distribution.
    monitoringSubscription :: Prelude.Maybe MonitoringSubscription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMonitoringSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringSubscription', 'createMonitoringSubscriptionResponse_monitoringSubscription' - A monitoring subscription. This structure contains information about
-- whether additional CloudWatch metrics are enabled for a given CloudFront
-- distribution.
--
-- 'httpStatus', 'createMonitoringSubscriptionResponse_httpStatus' - The response's http status code.
newCreateMonitoringSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMonitoringSubscriptionResponse
newCreateMonitoringSubscriptionResponse pHttpStatus_ =
  CreateMonitoringSubscriptionResponse'
    { monitoringSubscription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A monitoring subscription. This structure contains information about
-- whether additional CloudWatch metrics are enabled for a given CloudFront
-- distribution.
createMonitoringSubscriptionResponse_monitoringSubscription :: Lens.Lens' CreateMonitoringSubscriptionResponse (Prelude.Maybe MonitoringSubscription)
createMonitoringSubscriptionResponse_monitoringSubscription = Lens.lens (\CreateMonitoringSubscriptionResponse' {monitoringSubscription} -> monitoringSubscription) (\s@CreateMonitoringSubscriptionResponse' {} a -> s {monitoringSubscription = a} :: CreateMonitoringSubscriptionResponse)

-- | The response's http status code.
createMonitoringSubscriptionResponse_httpStatus :: Lens.Lens' CreateMonitoringSubscriptionResponse Prelude.Int
createMonitoringSubscriptionResponse_httpStatus = Lens.lens (\CreateMonitoringSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateMonitoringSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateMonitoringSubscriptionResponse)

instance
  Prelude.NFData
    CreateMonitoringSubscriptionResponse
