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
-- Module      : Amazonka.SecurityHub.GetMembers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details for the Security Hub member accounts for the
-- specified account IDs.
--
-- An administrator account can be either the delegated Security Hub
-- administrator account for an organization or an administrator account
-- that enabled Security Hub manually.
--
-- The results include both member accounts that are managed using
-- Organizations and accounts that were invited manually.
module Amazonka.SecurityHub.GetMembers
  ( -- * Creating a Request
    GetMembers (..),
    newGetMembers,

    -- * Request Lenses
    getMembers_accountIds,

    -- * Destructuring the Response
    GetMembersResponse (..),
    newGetMembersResponse,

    -- * Response Lenses
    getMembersResponse_members,
    getMembersResponse_unprocessedAccounts,
    getMembersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newGetMembers' smart constructor.
data GetMembers = GetMembers'
  { -- | The list of account IDs for the Security Hub member accounts to return
    -- the details for.
    accountIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'getMembers_accountIds' - The list of account IDs for the Security Hub member accounts to return
-- the details for.
newGetMembers ::
  GetMembers
newGetMembers =
  GetMembers' {accountIds = Prelude.mempty}

-- | The list of account IDs for the Security Hub member accounts to return
-- the details for.
getMembers_accountIds :: Lens.Lens' GetMembers [Prelude.Text]
getMembers_accountIds = Lens.lens (\GetMembers' {accountIds} -> accountIds) (\s@GetMembers' {} a -> s {accountIds = a} :: GetMembers) Prelude.. Lens.coerced

instance Core.AWSRequest GetMembers where
  type AWSResponse GetMembers = GetMembersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMembersResponse'
            Prelude.<$> (x Data..?> "Members" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "UnprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMembers where
  hashWithSalt _salt GetMembers' {..} =
    _salt `Prelude.hashWithSalt` accountIds

instance Prelude.NFData GetMembers where
  rnf GetMembers' {..} = Prelude.rnf accountIds

instance Data.ToHeaders GetMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetMembers where
  toJSON GetMembers' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AccountIds" Data..= accountIds)]
      )

instance Data.ToPath GetMembers where
  toPath = Prelude.const "/members/get"

instance Data.ToQuery GetMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMembersResponse' smart constructor.
data GetMembersResponse = GetMembersResponse'
  { -- | The list of details about the Security Hub member accounts.
    members :: Prelude.Maybe [Member],
    -- | The list of Amazon Web Services accounts that could not be processed.
    -- For each account, the list includes the account ID and the email
    -- address.
    unprocessedAccounts :: Prelude.Maybe [Result],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'members', 'getMembersResponse_members' - The list of details about the Security Hub member accounts.
--
-- 'unprocessedAccounts', 'getMembersResponse_unprocessedAccounts' - The list of Amazon Web Services accounts that could not be processed.
-- For each account, the list includes the account ID and the email
-- address.
--
-- 'httpStatus', 'getMembersResponse_httpStatus' - The response's http status code.
newGetMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMembersResponse
newGetMembersResponse pHttpStatus_ =
  GetMembersResponse'
    { members = Prelude.Nothing,
      unprocessedAccounts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of details about the Security Hub member accounts.
getMembersResponse_members :: Lens.Lens' GetMembersResponse (Prelude.Maybe [Member])
getMembersResponse_members = Lens.lens (\GetMembersResponse' {members} -> members) (\s@GetMembersResponse' {} a -> s {members = a} :: GetMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of Amazon Web Services accounts that could not be processed.
-- For each account, the list includes the account ID and the email
-- address.
getMembersResponse_unprocessedAccounts :: Lens.Lens' GetMembersResponse (Prelude.Maybe [Result])
getMembersResponse_unprocessedAccounts = Lens.lens (\GetMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@GetMembersResponse' {} a -> s {unprocessedAccounts = a} :: GetMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getMembersResponse_httpStatus :: Lens.Lens' GetMembersResponse Prelude.Int
getMembersResponse_httpStatus = Lens.lens (\GetMembersResponse' {httpStatus} -> httpStatus) (\s@GetMembersResponse' {} a -> s {httpStatus = a} :: GetMembersResponse)

instance Prelude.NFData GetMembersResponse where
  rnf GetMembersResponse' {..} =
    Prelude.rnf members
      `Prelude.seq` Prelude.rnf unprocessedAccounts
      `Prelude.seq` Prelude.rnf httpStatus
