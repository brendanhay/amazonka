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
-- Module      : Amazonka.SecurityLake.DeleteDatalakeDelegatedAdmin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Amazon Security Lake delegated administrator account for the
-- organization. This API can only be called by the organization management
-- account. The organization management account cannot be the delegated
-- administrator account.
module Amazonka.SecurityLake.DeleteDatalakeDelegatedAdmin
  ( -- * Creating a Request
    DeleteDatalakeDelegatedAdmin (..),
    newDeleteDatalakeDelegatedAdmin,

    -- * Request Lenses
    deleteDatalakeDelegatedAdmin_account,

    -- * Destructuring the Response
    DeleteDatalakeDelegatedAdminResponse (..),
    newDeleteDatalakeDelegatedAdminResponse,

    -- * Response Lenses
    deleteDatalakeDelegatedAdminResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteDatalakeDelegatedAdmin' smart constructor.
data DeleteDatalakeDelegatedAdmin = DeleteDatalakeDelegatedAdmin'
  { -- | The account ID the Security Lake delegated administrator.
    account :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatalakeDelegatedAdmin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'deleteDatalakeDelegatedAdmin_account' - The account ID the Security Lake delegated administrator.
newDeleteDatalakeDelegatedAdmin ::
  -- | 'account'
  Prelude.Text ->
  DeleteDatalakeDelegatedAdmin
newDeleteDatalakeDelegatedAdmin pAccount_ =
  DeleteDatalakeDelegatedAdmin' {account = pAccount_}

-- | The account ID the Security Lake delegated administrator.
deleteDatalakeDelegatedAdmin_account :: Lens.Lens' DeleteDatalakeDelegatedAdmin Prelude.Text
deleteDatalakeDelegatedAdmin_account = Lens.lens (\DeleteDatalakeDelegatedAdmin' {account} -> account) (\s@DeleteDatalakeDelegatedAdmin' {} a -> s {account = a} :: DeleteDatalakeDelegatedAdmin)

instance Core.AWSRequest DeleteDatalakeDelegatedAdmin where
  type
    AWSResponse DeleteDatalakeDelegatedAdmin =
      DeleteDatalakeDelegatedAdminResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDatalakeDelegatedAdminResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteDatalakeDelegatedAdmin
  where
  hashWithSalt _salt DeleteDatalakeDelegatedAdmin' {..} =
    _salt `Prelude.hashWithSalt` account

instance Prelude.NFData DeleteDatalakeDelegatedAdmin where
  rnf DeleteDatalakeDelegatedAdmin' {..} =
    Prelude.rnf account

instance Data.ToHeaders DeleteDatalakeDelegatedAdmin where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDatalakeDelegatedAdmin where
  toPath DeleteDatalakeDelegatedAdmin' {..} =
    Prelude.mconcat
      ["/v1/datalake/delegate/", Data.toBS account]

instance Data.ToQuery DeleteDatalakeDelegatedAdmin where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDatalakeDelegatedAdminResponse' smart constructor.
data DeleteDatalakeDelegatedAdminResponse = DeleteDatalakeDelegatedAdminResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatalakeDelegatedAdminResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDatalakeDelegatedAdminResponse_httpStatus' - The response's http status code.
newDeleteDatalakeDelegatedAdminResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDatalakeDelegatedAdminResponse
newDeleteDatalakeDelegatedAdminResponse pHttpStatus_ =
  DeleteDatalakeDelegatedAdminResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDatalakeDelegatedAdminResponse_httpStatus :: Lens.Lens' DeleteDatalakeDelegatedAdminResponse Prelude.Int
deleteDatalakeDelegatedAdminResponse_httpStatus = Lens.lens (\DeleteDatalakeDelegatedAdminResponse' {httpStatus} -> httpStatus) (\s@DeleteDatalakeDelegatedAdminResponse' {} a -> s {httpStatus = a} :: DeleteDatalakeDelegatedAdminResponse)

instance
  Prelude.NFData
    DeleteDatalakeDelegatedAdminResponse
  where
  rnf DeleteDatalakeDelegatedAdminResponse' {..} =
    Prelude.rnf httpStatus
