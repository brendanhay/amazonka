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
-- Module      : Amazonka.CloudFront.DeleteMonitoringSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables additional CloudWatch metrics for the specified CloudFront
-- distribution.
module Amazonka.CloudFront.DeleteMonitoringSubscription
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

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMonitoringSubscription' smart constructor.
data DeleteMonitoringSubscription = DeleteMonitoringSubscription'
  { -- | The ID of the distribution that you are disabling metrics for.
    distributionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteMonitoringSubscription
newDeleteMonitoringSubscription pDistributionId_ =
  DeleteMonitoringSubscription'
    { distributionId =
        pDistributionId_
    }

-- | The ID of the distribution that you are disabling metrics for.
deleteMonitoringSubscription_distributionId :: Lens.Lens' DeleteMonitoringSubscription Prelude.Text
deleteMonitoringSubscription_distributionId = Lens.lens (\DeleteMonitoringSubscription' {distributionId} -> distributionId) (\s@DeleteMonitoringSubscription' {} a -> s {distributionId = a} :: DeleteMonitoringSubscription)

instance Core.AWSRequest DeleteMonitoringSubscription where
  type
    AWSResponse DeleteMonitoringSubscription =
      DeleteMonitoringSubscriptionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMonitoringSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteMonitoringSubscription
  where
  hashWithSalt _salt DeleteMonitoringSubscription' {..} =
    _salt `Prelude.hashWithSalt` distributionId

instance Prelude.NFData DeleteMonitoringSubscription where
  rnf DeleteMonitoringSubscription' {..} =
    Prelude.rnf distributionId

instance Core.ToHeaders DeleteMonitoringSubscription where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteMonitoringSubscription where
  toPath DeleteMonitoringSubscription' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distributions/",
        Core.toBS distributionId,
        "/monitoring-subscription/"
      ]

instance Core.ToQuery DeleteMonitoringSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMonitoringSubscriptionResponse' smart constructor.
data DeleteMonitoringSubscriptionResponse = DeleteMonitoringSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteMonitoringSubscriptionResponse
newDeleteMonitoringSubscriptionResponse pHttpStatus_ =
  DeleteMonitoringSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMonitoringSubscriptionResponse_httpStatus :: Lens.Lens' DeleteMonitoringSubscriptionResponse Prelude.Int
deleteMonitoringSubscriptionResponse_httpStatus = Lens.lens (\DeleteMonitoringSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteMonitoringSubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteMonitoringSubscriptionResponse)

instance
  Prelude.NFData
    DeleteMonitoringSubscriptionResponse
  where
  rnf DeleteMonitoringSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
