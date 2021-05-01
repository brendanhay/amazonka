{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUsageReportSubscription' smart constructor.
data DeleteUsageReportSubscription = DeleteUsageReportSubscription'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUsageReportSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUsageReportSubscription ::
  DeleteUsageReportSubscription
newDeleteUsageReportSubscription =
  DeleteUsageReportSubscription'

instance
  Prelude.AWSRequest
    DeleteUsageReportSubscription
  where
  type
    Rs DeleteUsageReportSubscription =
      DeleteUsageReportSubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUsageReportSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteUsageReportSubscription

instance Prelude.NFData DeleteUsageReportSubscription

instance
  Prelude.ToHeaders
    DeleteUsageReportSubscription
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.DeleteUsageReportSubscription" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteUsageReportSubscription where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DeleteUsageReportSubscription where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteUsageReportSubscription
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUsageReportSubscriptionResponse' smart constructor.
data DeleteUsageReportSubscriptionResponse = DeleteUsageReportSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteUsageReportSubscriptionResponse
newDeleteUsageReportSubscriptionResponse pHttpStatus_ =
  DeleteUsageReportSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteUsageReportSubscriptionResponse_httpStatus :: Lens.Lens' DeleteUsageReportSubscriptionResponse Prelude.Int
deleteUsageReportSubscriptionResponse_httpStatus = Lens.lens (\DeleteUsageReportSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteUsageReportSubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteUsageReportSubscriptionResponse)

instance
  Prelude.NFData
    DeleteUsageReportSubscriptionResponse
