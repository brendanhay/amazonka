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
-- Module      : Network.AWS.CloudFront.DeleteMonitoringSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables additional CloudWatch metrics for the specified CloudFront
-- distribution.
module Network.AWS.CloudFront.DeleteMonitoringSubscription
  ( -- * Creating a Request
    DeleteMonitoringSubscription (..),
    newDeleteMonitoringSubscription,

    -- * Request Lenses
    deleteMonitoringSubscription_distributionId,

    -- * Destructuring the Response
    DeleteMonitoringSubscriptionResponse (..),
    newDeleteMonitoringSubscriptionResponse,

    -- * Response Lenses
    deleteMonitoringSubscriptionResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteMonitoringSubscription' smart constructor.
data DeleteMonitoringSubscription = DeleteMonitoringSubscription'
  { -- | The ID of the distribution that you are disabling metrics for.
    distributionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMonitoringSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionId', 'deleteMonitoringSubscription_distributionId' - The ID of the distribution that you are disabling metrics for.
newDeleteMonitoringSubscription ::
  -- | 'distributionId'
  Core.Text ->
  DeleteMonitoringSubscription
newDeleteMonitoringSubscription pDistributionId_ =
  DeleteMonitoringSubscription'
    { distributionId =
        pDistributionId_
    }

-- | The ID of the distribution that you are disabling metrics for.
deleteMonitoringSubscription_distributionId :: Lens.Lens' DeleteMonitoringSubscription Core.Text
deleteMonitoringSubscription_distributionId = Lens.lens (\DeleteMonitoringSubscription' {distributionId} -> distributionId) (\s@DeleteMonitoringSubscription' {} a -> s {distributionId = a} :: DeleteMonitoringSubscription)

instance Core.AWSRequest DeleteMonitoringSubscription where
  type
    AWSResponse DeleteMonitoringSubscription =
      DeleteMonitoringSubscriptionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMonitoringSubscriptionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteMonitoringSubscription

instance Core.NFData DeleteMonitoringSubscription

instance Core.ToHeaders DeleteMonitoringSubscription where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteMonitoringSubscription where
  toPath DeleteMonitoringSubscription' {..} =
    Core.mconcat
      [ "/2020-05-31/distributions/",
        Core.toBS distributionId,
        "/monitoring-subscription"
      ]

instance Core.ToQuery DeleteMonitoringSubscription where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteMonitoringSubscriptionResponse' smart constructor.
data DeleteMonitoringSubscriptionResponse = DeleteMonitoringSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMonitoringSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMonitoringSubscriptionResponse_httpStatus' - The response's http status code.
newDeleteMonitoringSubscriptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteMonitoringSubscriptionResponse
newDeleteMonitoringSubscriptionResponse pHttpStatus_ =
  DeleteMonitoringSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMonitoringSubscriptionResponse_httpStatus :: Lens.Lens' DeleteMonitoringSubscriptionResponse Core.Int
deleteMonitoringSubscriptionResponse_httpStatus = Lens.lens (\DeleteMonitoringSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteMonitoringSubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteMonitoringSubscriptionResponse)

instance
  Core.NFData
    DeleteMonitoringSubscriptionResponse
