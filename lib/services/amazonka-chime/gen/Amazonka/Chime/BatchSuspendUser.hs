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
-- Module      : Amazonka.Chime.BatchSuspendUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends up to 50 users from a @Team@ or @EnterpriseLWA@ Amazon Chime
-- account. For more information about different account types, see
-- <https://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts>
-- in the /Amazon Chime Administration Guide/.
--
-- Users suspended from a @Team@ account are disassociated from the
-- account,but they can continue to use Amazon Chime as free users. To
-- remove the suspension from suspended @Team@ account users, invite them
-- to the @Team@ account again. You can use the InviteUsers action to do
-- so.
--
-- Users suspended from an @EnterpriseLWA@ account are immediately signed
-- out of Amazon Chime and can no longer sign in. To remove the suspension
-- from suspended @EnterpriseLWA@ account users, use the BatchUnsuspendUser
-- action.
--
-- To sign out users without suspending them, use the LogoutUser action.
module Amazonka.Chime.BatchSuspendUser
  ( -- * Creating a Request
    BatchSuspendUser (..),
    newBatchSuspendUser,

    -- * Request Lenses
    batchSuspendUser_accountId,
    batchSuspendUser_userIdList,

    -- * Destructuring the Response
    BatchSuspendUserResponse (..),
    newBatchSuspendUserResponse,

    -- * Response Lenses
    batchSuspendUserResponse_userErrors,
    batchSuspendUserResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchSuspendUser' smart constructor.
data BatchSuspendUser = BatchSuspendUser'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The request containing the user IDs to suspend.
    userIdList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchSuspendUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'batchSuspendUser_accountId' - The Amazon Chime account ID.
--
-- 'userIdList', 'batchSuspendUser_userIdList' - The request containing the user IDs to suspend.
newBatchSuspendUser ::
  -- | 'accountId'
  Prelude.Text ->
  BatchSuspendUser
newBatchSuspendUser pAccountId_ =
  BatchSuspendUser'
    { accountId = pAccountId_,
      userIdList = Prelude.mempty
    }

-- | The Amazon Chime account ID.
batchSuspendUser_accountId :: Lens.Lens' BatchSuspendUser Prelude.Text
batchSuspendUser_accountId = Lens.lens (\BatchSuspendUser' {accountId} -> accountId) (\s@BatchSuspendUser' {} a -> s {accountId = a} :: BatchSuspendUser)

-- | The request containing the user IDs to suspend.
batchSuspendUser_userIdList :: Lens.Lens' BatchSuspendUser [Prelude.Text]
batchSuspendUser_userIdList = Lens.lens (\BatchSuspendUser' {userIdList} -> userIdList) (\s@BatchSuspendUser' {} a -> s {userIdList = a} :: BatchSuspendUser) Prelude.. Lens.coerced

instance Core.AWSRequest BatchSuspendUser where
  type
    AWSResponse BatchSuspendUser =
      BatchSuspendUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchSuspendUserResponse'
            Prelude.<$> (x Data..?> "UserErrors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchSuspendUser where
  hashWithSalt _salt BatchSuspendUser' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` userIdList

instance Prelude.NFData BatchSuspendUser where
  rnf BatchSuspendUser' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf userIdList

instance Data.ToHeaders BatchSuspendUser where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchSuspendUser where
  toJSON BatchSuspendUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("UserIdList" Data..= userIdList)]
      )

instance Data.ToPath BatchSuspendUser where
  toPath BatchSuspendUser' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS accountId, "/users"]

instance Data.ToQuery BatchSuspendUser where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=suspend"])

-- | /See:/ 'newBatchSuspendUserResponse' smart constructor.
data BatchSuspendUserResponse = BatchSuspendUserResponse'
  { -- | If the BatchSuspendUser action fails for one or more of the user IDs in
    -- the request, a list of the user IDs is returned, along with error codes
    -- and error messages.
    userErrors :: Prelude.Maybe [UserError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchSuspendUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userErrors', 'batchSuspendUserResponse_userErrors' - If the BatchSuspendUser action fails for one or more of the user IDs in
-- the request, a list of the user IDs is returned, along with error codes
-- and error messages.
--
-- 'httpStatus', 'batchSuspendUserResponse_httpStatus' - The response's http status code.
newBatchSuspendUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchSuspendUserResponse
newBatchSuspendUserResponse pHttpStatus_ =
  BatchSuspendUserResponse'
    { userErrors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the BatchSuspendUser action fails for one or more of the user IDs in
-- the request, a list of the user IDs is returned, along with error codes
-- and error messages.
batchSuspendUserResponse_userErrors :: Lens.Lens' BatchSuspendUserResponse (Prelude.Maybe [UserError])
batchSuspendUserResponse_userErrors = Lens.lens (\BatchSuspendUserResponse' {userErrors} -> userErrors) (\s@BatchSuspendUserResponse' {} a -> s {userErrors = a} :: BatchSuspendUserResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchSuspendUserResponse_httpStatus :: Lens.Lens' BatchSuspendUserResponse Prelude.Int
batchSuspendUserResponse_httpStatus = Lens.lens (\BatchSuspendUserResponse' {httpStatus} -> httpStatus) (\s@BatchSuspendUserResponse' {} a -> s {httpStatus = a} :: BatchSuspendUserResponse)

instance Prelude.NFData BatchSuspendUserResponse where
  rnf BatchSuspendUserResponse' {..} =
    Prelude.rnf userErrors `Prelude.seq`
      Prelude.rnf httpStatus
