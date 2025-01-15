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
-- Module      : Amazonka.SecurityHub.DeleteMembers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified member accounts from Security Hub.
--
-- Can be used to delete member accounts that belong to an organization as
-- well as member accounts that were invited manually.
module Amazonka.SecurityHub.DeleteMembers
  ( -- * Creating a Request
    DeleteMembers (..),
    newDeleteMembers,

    -- * Request Lenses
    deleteMembers_accountIds,

    -- * Destructuring the Response
    DeleteMembersResponse (..),
    newDeleteMembersResponse,

    -- * Response Lenses
    deleteMembersResponse_unprocessedAccounts,
    deleteMembersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newDeleteMembers' smart constructor.
data DeleteMembers = DeleteMembers'
  { -- | The list of account IDs for the member accounts to delete.
    accountIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'deleteMembers_accountIds' - The list of account IDs for the member accounts to delete.
newDeleteMembers ::
  DeleteMembers
newDeleteMembers =
  DeleteMembers' {accountIds = Prelude.mempty}

-- | The list of account IDs for the member accounts to delete.
deleteMembers_accountIds :: Lens.Lens' DeleteMembers [Prelude.Text]
deleteMembers_accountIds = Lens.lens (\DeleteMembers' {accountIds} -> accountIds) (\s@DeleteMembers' {} a -> s {accountIds = a} :: DeleteMembers) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteMembers where
  type
    AWSResponse DeleteMembers =
      DeleteMembersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMembersResponse'
            Prelude.<$> ( x
                            Data..?> "UnprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMembers where
  hashWithSalt _salt DeleteMembers' {..} =
    _salt `Prelude.hashWithSalt` accountIds

instance Prelude.NFData DeleteMembers where
  rnf DeleteMembers' {..} = Prelude.rnf accountIds

instance Data.ToHeaders DeleteMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMembers where
  toJSON DeleteMembers' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AccountIds" Data..= accountIds)]
      )

instance Data.ToPath DeleteMembers where
  toPath = Prelude.const "/members/delete"

instance Data.ToQuery DeleteMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMembersResponse' smart constructor.
data DeleteMembersResponse = DeleteMembersResponse'
  { -- | The list of Amazon Web Services accounts that were not deleted. For each
    -- account, the list includes the account ID and the email address.
    unprocessedAccounts :: Prelude.Maybe [Result],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedAccounts', 'deleteMembersResponse_unprocessedAccounts' - The list of Amazon Web Services accounts that were not deleted. For each
-- account, the list includes the account ID and the email address.
--
-- 'httpStatus', 'deleteMembersResponse_httpStatus' - The response's http status code.
newDeleteMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMembersResponse
newDeleteMembersResponse pHttpStatus_ =
  DeleteMembersResponse'
    { unprocessedAccounts =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of Amazon Web Services accounts that were not deleted. For each
-- account, the list includes the account ID and the email address.
deleteMembersResponse_unprocessedAccounts :: Lens.Lens' DeleteMembersResponse (Prelude.Maybe [Result])
deleteMembersResponse_unprocessedAccounts = Lens.lens (\DeleteMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@DeleteMembersResponse' {} a -> s {unprocessedAccounts = a} :: DeleteMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteMembersResponse_httpStatus :: Lens.Lens' DeleteMembersResponse Prelude.Int
deleteMembersResponse_httpStatus = Lens.lens (\DeleteMembersResponse' {httpStatus} -> httpStatus) (\s@DeleteMembersResponse' {} a -> s {httpStatus = a} :: DeleteMembersResponse)

instance Prelude.NFData DeleteMembersResponse where
  rnf DeleteMembersResponse' {..} =
    Prelude.rnf unprocessedAccounts `Prelude.seq`
      Prelude.rnf httpStatus
