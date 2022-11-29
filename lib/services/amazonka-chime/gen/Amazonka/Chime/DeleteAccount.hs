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
-- Module      : Amazonka.Chime.DeleteAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Chime account. You must suspend all users
-- before deleting @Team@ account. You can use the BatchSuspendUser action
-- to dodo.
--
-- For @EnterpriseLWA@ and @EnterpriseAD@ accounts, you must release the
-- claimed domains for your Amazon Chime account before deletion. As soon
-- as you release the domain, all users under that account are suspended.
--
-- Deleted accounts appear in your @Disabled@ accounts list for 90 days. To
-- restore deleted account from your @Disabled@ accounts list, you must
-- contact AWS Support.
--
-- After 90 days, deleted accounts are permanently removed from your
-- @Disabled@ accounts list.
module Amazonka.Chime.DeleteAccount
  ( -- * Creating a Request
    DeleteAccount (..),
    newDeleteAccount,

    -- * Request Lenses
    deleteAccount_accountId,

    -- * Destructuring the Response
    DeleteAccountResponse (..),
    newDeleteAccountResponse,

    -- * Response Lenses
    deleteAccountResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccount' smart constructor.
data DeleteAccount = DeleteAccount'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteAccount_accountId' - The Amazon Chime account ID.
newDeleteAccount ::
  -- | 'accountId'
  Prelude.Text ->
  DeleteAccount
newDeleteAccount pAccountId_ =
  DeleteAccount' {accountId = pAccountId_}

-- | The Amazon Chime account ID.
deleteAccount_accountId :: Lens.Lens' DeleteAccount Prelude.Text
deleteAccount_accountId = Lens.lens (\DeleteAccount' {accountId} -> accountId) (\s@DeleteAccount' {} a -> s {accountId = a} :: DeleteAccount)

instance Core.AWSRequest DeleteAccount where
  type
    AWSResponse DeleteAccount =
      DeleteAccountResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAccount where
  hashWithSalt _salt DeleteAccount' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData DeleteAccount where
  rnf DeleteAccount' {..} = Prelude.rnf accountId

instance Core.ToHeaders DeleteAccount where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteAccount where
  toPath DeleteAccount' {..} =
    Prelude.mconcat ["/accounts/", Core.toBS accountId]

instance Core.ToQuery DeleteAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccountResponse' smart constructor.
data DeleteAccountResponse = DeleteAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAccountResponse_httpStatus' - The response's http status code.
newDeleteAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAccountResponse
newDeleteAccountResponse pHttpStatus_ =
  DeleteAccountResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteAccountResponse_httpStatus :: Lens.Lens' DeleteAccountResponse Prelude.Int
deleteAccountResponse_httpStatus = Lens.lens (\DeleteAccountResponse' {httpStatus} -> httpStatus) (\s@DeleteAccountResponse' {} a -> s {httpStatus = a} :: DeleteAccountResponse)

instance Prelude.NFData DeleteAccountResponse where
  rnf DeleteAccountResponse' {..} =
    Prelude.rnf httpStatus
