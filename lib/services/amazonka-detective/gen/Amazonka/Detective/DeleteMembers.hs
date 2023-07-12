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
-- Module      : Amazonka.Detective.DeleteMembers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified member accounts from the behavior graph. The
-- removed accounts no longer contribute data to the behavior graph. This
-- operation can only be called by the administrator account for the
-- behavior graph.
--
-- For invited accounts, the removed accounts are deleted from the list of
-- accounts in the behavior graph. To restore the account, the
-- administrator account must send another invitation.
--
-- For organization accounts in the organization behavior graph, the
-- Detective administrator account can always enable the organization
-- account again. Organization accounts that are not enabled as member
-- accounts are not included in the @ListMembers@ results for the
-- organization behavior graph.
--
-- An administrator account cannot use @DeleteMembers@ to remove their own
-- account from the behavior graph. To disable a behavior graph, the
-- administrator account uses the @DeleteGraph@ API method.
module Amazonka.Detective.DeleteMembers
  ( -- * Creating a Request
    DeleteMembers (..),
    newDeleteMembers,

    -- * Request Lenses
    deleteMembers_graphArn,
    deleteMembers_accountIds,

    -- * Destructuring the Response
    DeleteMembersResponse (..),
    newDeleteMembersResponse,

    -- * Response Lenses
    deleteMembersResponse_accountIds,
    deleteMembersResponse_unprocessedAccounts,
    deleteMembersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMembers' smart constructor.
data DeleteMembers = DeleteMembers'
  { -- | The ARN of the behavior graph to remove members from.
    graphArn :: Prelude.Text,
    -- | The list of Amazon Web Services account identifiers for the member
    -- accounts to remove from the behavior graph. You can remove up to 50
    -- member accounts at a time.
    accountIds :: Prelude.NonEmpty Prelude.Text
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
-- 'graphArn', 'deleteMembers_graphArn' - The ARN of the behavior graph to remove members from.
--
-- 'accountIds', 'deleteMembers_accountIds' - The list of Amazon Web Services account identifiers for the member
-- accounts to remove from the behavior graph. You can remove up to 50
-- member accounts at a time.
newDeleteMembers ::
  -- | 'graphArn'
  Prelude.Text ->
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  DeleteMembers
newDeleteMembers pGraphArn_ pAccountIds_ =
  DeleteMembers'
    { graphArn = pGraphArn_,
      accountIds = Lens.coerced Lens.# pAccountIds_
    }

-- | The ARN of the behavior graph to remove members from.
deleteMembers_graphArn :: Lens.Lens' DeleteMembers Prelude.Text
deleteMembers_graphArn = Lens.lens (\DeleteMembers' {graphArn} -> graphArn) (\s@DeleteMembers' {} a -> s {graphArn = a} :: DeleteMembers)

-- | The list of Amazon Web Services account identifiers for the member
-- accounts to remove from the behavior graph. You can remove up to 50
-- member accounts at a time.
deleteMembers_accountIds :: Lens.Lens' DeleteMembers (Prelude.NonEmpty Prelude.Text)
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
            Prelude.<$> (x Data..?> "AccountIds")
            Prelude.<*> ( x
                            Data..?> "UnprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMembers where
  hashWithSalt _salt DeleteMembers' {..} =
    _salt
      `Prelude.hashWithSalt` graphArn
      `Prelude.hashWithSalt` accountIds

instance Prelude.NFData DeleteMembers where
  rnf DeleteMembers' {..} =
    Prelude.rnf graphArn
      `Prelude.seq` Prelude.rnf accountIds

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
          [ Prelude.Just ("GraphArn" Data..= graphArn),
            Prelude.Just ("AccountIds" Data..= accountIds)
          ]
      )

instance Data.ToPath DeleteMembers where
  toPath = Prelude.const "/graph/members/removal"

instance Data.ToQuery DeleteMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMembersResponse' smart constructor.
data DeleteMembersResponse = DeleteMembersResponse'
  { -- | The list of Amazon Web Services account identifiers for the member
    -- accounts that Detective successfully removed from the behavior graph.
    accountIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The list of member accounts that Detective was not able to remove from
    -- the behavior graph. For each member account, provides the reason that
    -- the deletion could not be processed.
    unprocessedAccounts :: Prelude.Maybe [UnprocessedAccount],
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
-- 'accountIds', 'deleteMembersResponse_accountIds' - The list of Amazon Web Services account identifiers for the member
-- accounts that Detective successfully removed from the behavior graph.
--
-- 'unprocessedAccounts', 'deleteMembersResponse_unprocessedAccounts' - The list of member accounts that Detective was not able to remove from
-- the behavior graph. For each member account, provides the reason that
-- the deletion could not be processed.
--
-- 'httpStatus', 'deleteMembersResponse_httpStatus' - The response's http status code.
newDeleteMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMembersResponse
newDeleteMembersResponse pHttpStatus_ =
  DeleteMembersResponse'
    { accountIds =
        Prelude.Nothing,
      unprocessedAccounts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of Amazon Web Services account identifiers for the member
-- accounts that Detective successfully removed from the behavior graph.
deleteMembersResponse_accountIds :: Lens.Lens' DeleteMembersResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
deleteMembersResponse_accountIds = Lens.lens (\DeleteMembersResponse' {accountIds} -> accountIds) (\s@DeleteMembersResponse' {} a -> s {accountIds = a} :: DeleteMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of member accounts that Detective was not able to remove from
-- the behavior graph. For each member account, provides the reason that
-- the deletion could not be processed.
deleteMembersResponse_unprocessedAccounts :: Lens.Lens' DeleteMembersResponse (Prelude.Maybe [UnprocessedAccount])
deleteMembersResponse_unprocessedAccounts = Lens.lens (\DeleteMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@DeleteMembersResponse' {} a -> s {unprocessedAccounts = a} :: DeleteMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteMembersResponse_httpStatus :: Lens.Lens' DeleteMembersResponse Prelude.Int
deleteMembersResponse_httpStatus = Lens.lens (\DeleteMembersResponse' {httpStatus} -> httpStatus) (\s@DeleteMembersResponse' {} a -> s {httpStatus = a} :: DeleteMembersResponse)

instance Prelude.NFData DeleteMembersResponse where
  rnf DeleteMembersResponse' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf unprocessedAccounts
      `Prelude.seq` Prelude.rnf httpStatus
