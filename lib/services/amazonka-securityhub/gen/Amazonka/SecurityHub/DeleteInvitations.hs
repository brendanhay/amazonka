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
-- Module      : Amazonka.SecurityHub.DeleteInvitations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes invitations received by the Amazon Web Services account to
-- become a member account.
--
-- This operation is only used by accounts that are not part of an
-- organization. Organization accounts do not receive invitations.
module Amazonka.SecurityHub.DeleteInvitations
  ( -- * Creating a Request
    DeleteInvitations (..),
    newDeleteInvitations,

    -- * Request Lenses
    deleteInvitations_accountIds,

    -- * Destructuring the Response
    DeleteInvitationsResponse (..),
    newDeleteInvitationsResponse,

    -- * Response Lenses
    deleteInvitationsResponse_unprocessedAccounts,
    deleteInvitationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newDeleteInvitations' smart constructor.
data DeleteInvitations = DeleteInvitations'
  { -- | The list of the account IDs that sent the invitations to delete.
    accountIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInvitations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'deleteInvitations_accountIds' - The list of the account IDs that sent the invitations to delete.
newDeleteInvitations ::
  DeleteInvitations
newDeleteInvitations =
  DeleteInvitations' {accountIds = Prelude.mempty}

-- | The list of the account IDs that sent the invitations to delete.
deleteInvitations_accountIds :: Lens.Lens' DeleteInvitations [Prelude.Text]
deleteInvitations_accountIds = Lens.lens (\DeleteInvitations' {accountIds} -> accountIds) (\s@DeleteInvitations' {} a -> s {accountIds = a} :: DeleteInvitations) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteInvitations where
  type
    AWSResponse DeleteInvitations =
      DeleteInvitationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInvitationsResponse'
            Prelude.<$> ( x
                            Data..?> "UnprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInvitations where
  hashWithSalt _salt DeleteInvitations' {..} =
    _salt `Prelude.hashWithSalt` accountIds

instance Prelude.NFData DeleteInvitations where
  rnf DeleteInvitations' {..} = Prelude.rnf accountIds

instance Data.ToHeaders DeleteInvitations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteInvitations where
  toJSON DeleteInvitations' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AccountIds" Data..= accountIds)]
      )

instance Data.ToPath DeleteInvitations where
  toPath = Prelude.const "/invitations/delete"

instance Data.ToQuery DeleteInvitations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInvitationsResponse' smart constructor.
data DeleteInvitationsResponse = DeleteInvitationsResponse'
  { -- | The list of Amazon Web Services accounts for which the invitations were
    -- not deleted. For each account, the list includes the account ID and the
    -- email address.
    unprocessedAccounts :: Prelude.Maybe [Result],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInvitationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedAccounts', 'deleteInvitationsResponse_unprocessedAccounts' - The list of Amazon Web Services accounts for which the invitations were
-- not deleted. For each account, the list includes the account ID and the
-- email address.
--
-- 'httpStatus', 'deleteInvitationsResponse_httpStatus' - The response's http status code.
newDeleteInvitationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteInvitationsResponse
newDeleteInvitationsResponse pHttpStatus_ =
  DeleteInvitationsResponse'
    { unprocessedAccounts =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of Amazon Web Services accounts for which the invitations were
-- not deleted. For each account, the list includes the account ID and the
-- email address.
deleteInvitationsResponse_unprocessedAccounts :: Lens.Lens' DeleteInvitationsResponse (Prelude.Maybe [Result])
deleteInvitationsResponse_unprocessedAccounts = Lens.lens (\DeleteInvitationsResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@DeleteInvitationsResponse' {} a -> s {unprocessedAccounts = a} :: DeleteInvitationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteInvitationsResponse_httpStatus :: Lens.Lens' DeleteInvitationsResponse Prelude.Int
deleteInvitationsResponse_httpStatus = Lens.lens (\DeleteInvitationsResponse' {httpStatus} -> httpStatus) (\s@DeleteInvitationsResponse' {} a -> s {httpStatus = a} :: DeleteInvitationsResponse)

instance Prelude.NFData DeleteInvitationsResponse where
  rnf DeleteInvitationsResponse' {..} =
    Prelude.rnf unprocessedAccounts `Prelude.seq`
      Prelude.rnf httpStatus
