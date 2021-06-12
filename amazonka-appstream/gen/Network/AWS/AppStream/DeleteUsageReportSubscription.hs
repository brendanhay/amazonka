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
-- Module      : Network.AWS.AppStream.DeleteUsageReportSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables usage report generation.
module Network.AWS.AppStream.DeleteUsageReportSubscription
  ( -- * Creating a Request
    DeleteUsageReportSubscription (..),
    newDeleteUsageReportSubscription,

    -- * Destructuring the Response
    DeleteUsageReportSubscriptionResponse (..),
    newDeleteUsageReportSubscriptionResponse,

    -- * Response Lenses
    deleteUsageReportSubscriptionResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUsageReportSubscription' smart constructor.
data DeleteUsageReportSubscription = DeleteUsageReportSubscription'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUsageReportSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUsageReportSubscription ::
  DeleteUsageReportSubscription
newDeleteUsageReportSubscription =
  DeleteUsageReportSubscription'

instance
  Core.AWSRequest
    DeleteUsageReportSubscription
  where
  type
    AWSResponse DeleteUsageReportSubscription =
      DeleteUsageReportSubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUsageReportSubscriptionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteUsageReportSubscription

instance Core.NFData DeleteUsageReportSubscription

instance Core.ToHeaders DeleteUsageReportSubscription where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DeleteUsageReportSubscription" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteUsageReportSubscription where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DeleteUsageReportSubscription where
  toPath = Core.const "/"

instance Core.ToQuery DeleteUsageReportSubscription where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteUsageReportSubscriptionResponse' smart constructor.
data DeleteUsageReportSubscriptionResponse = DeleteUsageReportSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUsageReportSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUsageReportSubscriptionResponse_httpStatus' - The response's http status code.
newDeleteUsageReportSubscriptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteUsageReportSubscriptionResponse
newDeleteUsageReportSubscriptionResponse pHttpStatus_ =
  DeleteUsageReportSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteUsageReportSubscriptionResponse_httpStatus :: Lens.Lens' DeleteUsageReportSubscriptionResponse Core.Int
deleteUsageReportSubscriptionResponse_httpStatus = Lens.lens (\DeleteUsageReportSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteUsageReportSubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteUsageReportSubscriptionResponse)

instance
  Core.NFData
    DeleteUsageReportSubscriptionResponse
