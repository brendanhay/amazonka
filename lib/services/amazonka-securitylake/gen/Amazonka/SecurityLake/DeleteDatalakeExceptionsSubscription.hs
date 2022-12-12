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
-- Module      : Amazonka.SecurityLake.DeleteDatalakeExceptionsSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified notification subscription in Security Lake.
-- Deletes the specified subscription notifications in the specified
-- organization.
module Amazonka.SecurityLake.DeleteDatalakeExceptionsSubscription
  ( -- * Creating a Request
    DeleteDatalakeExceptionsSubscription (..),
    newDeleteDatalakeExceptionsSubscription,

    -- * Destructuring the Response
    DeleteDatalakeExceptionsSubscriptionResponse (..),
    newDeleteDatalakeExceptionsSubscriptionResponse,

    -- * Response Lenses
    deleteDatalakeExceptionsSubscriptionResponse_httpStatus,
    deleteDatalakeExceptionsSubscriptionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteDatalakeExceptionsSubscription' smart constructor.
data DeleteDatalakeExceptionsSubscription = DeleteDatalakeExceptionsSubscription'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatalakeExceptionsSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDatalakeExceptionsSubscription ::
  DeleteDatalakeExceptionsSubscription
newDeleteDatalakeExceptionsSubscription =
  DeleteDatalakeExceptionsSubscription'

instance
  Core.AWSRequest
    DeleteDatalakeExceptionsSubscription
  where
  type
    AWSResponse DeleteDatalakeExceptionsSubscription =
      DeleteDatalakeExceptionsSubscriptionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDatalakeExceptionsSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "status")
      )

instance
  Prelude.Hashable
    DeleteDatalakeExceptionsSubscription
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DeleteDatalakeExceptionsSubscription
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DeleteDatalakeExceptionsSubscription
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
    DeleteDatalakeExceptionsSubscription
  where
  toPath =
    Prelude.const
      "/v1/datalake/exceptions/subscription"

instance
  Data.ToQuery
    DeleteDatalakeExceptionsSubscription
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDatalakeExceptionsSubscriptionResponse' smart constructor.
data DeleteDatalakeExceptionsSubscriptionResponse = DeleteDatalakeExceptionsSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Retrieves the status of the delete Security Lake operation for an
    -- account.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatalakeExceptionsSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDatalakeExceptionsSubscriptionResponse_httpStatus' - The response's http status code.
--
-- 'status', 'deleteDatalakeExceptionsSubscriptionResponse_status' - Retrieves the status of the delete Security Lake operation for an
-- account.
newDeleteDatalakeExceptionsSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  Prelude.Text ->
  DeleteDatalakeExceptionsSubscriptionResponse
newDeleteDatalakeExceptionsSubscriptionResponse
  pHttpStatus_
  pStatus_ =
    DeleteDatalakeExceptionsSubscriptionResponse'
      { httpStatus =
          pHttpStatus_,
        status = pStatus_
      }

-- | The response's http status code.
deleteDatalakeExceptionsSubscriptionResponse_httpStatus :: Lens.Lens' DeleteDatalakeExceptionsSubscriptionResponse Prelude.Int
deleteDatalakeExceptionsSubscriptionResponse_httpStatus = Lens.lens (\DeleteDatalakeExceptionsSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteDatalakeExceptionsSubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteDatalakeExceptionsSubscriptionResponse)

-- | Retrieves the status of the delete Security Lake operation for an
-- account.
deleteDatalakeExceptionsSubscriptionResponse_status :: Lens.Lens' DeleteDatalakeExceptionsSubscriptionResponse Prelude.Text
deleteDatalakeExceptionsSubscriptionResponse_status = Lens.lens (\DeleteDatalakeExceptionsSubscriptionResponse' {status} -> status) (\s@DeleteDatalakeExceptionsSubscriptionResponse' {} a -> s {status = a} :: DeleteDatalakeExceptionsSubscriptionResponse)

instance
  Prelude.NFData
    DeleteDatalakeExceptionsSubscriptionResponse
  where
  rnf DeleteDatalakeExceptionsSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
