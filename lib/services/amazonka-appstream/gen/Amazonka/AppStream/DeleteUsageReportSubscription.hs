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
-- Module      : Amazonka.AppStream.DeleteUsageReportSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables usage report generation.
module Amazonka.AppStream.DeleteUsageReportSubscription
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUsageReportSubscription' smart constructor.
data DeleteUsageReportSubscription = DeleteUsageReportSubscription'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUsageReportSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteUsageReportSubscription
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DeleteUsageReportSubscription where
  rnf _ = ()

instance Data.ToHeaders DeleteUsageReportSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DeleteUsageReportSubscription" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteUsageReportSubscription where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DeleteUsageReportSubscription where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUsageReportSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUsageReportSubscriptionResponse' smart constructor.
data DeleteUsageReportSubscriptionResponse = DeleteUsageReportSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DeleteUsageReportSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
