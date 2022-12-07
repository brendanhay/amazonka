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
-- Module      : Amazonka.Chime.DisassociateSigninDelegateGroupsFromAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified sign-in delegate groups from the specified
-- Amazon Chime account.
module Amazonka.Chime.DisassociateSigninDelegateGroupsFromAccount
  ( -- * Creating a Request
    DisassociateSigninDelegateGroupsFromAccount (..),
    newDisassociateSigninDelegateGroupsFromAccount,

    -- * Request Lenses
    disassociateSigninDelegateGroupsFromAccount_accountId,
    disassociateSigninDelegateGroupsFromAccount_groupNames,

    -- * Destructuring the Response
    DisassociateSigninDelegateGroupsFromAccountResponse (..),
    newDisassociateSigninDelegateGroupsFromAccountResponse,

    -- * Response Lenses
    disassociateSigninDelegateGroupsFromAccountResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateSigninDelegateGroupsFromAccount' smart constructor.
data DisassociateSigninDelegateGroupsFromAccount = DisassociateSigninDelegateGroupsFromAccount'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The sign-in delegate group names.
    groupNames :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateSigninDelegateGroupsFromAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'disassociateSigninDelegateGroupsFromAccount_accountId' - The Amazon Chime account ID.
--
-- 'groupNames', 'disassociateSigninDelegateGroupsFromAccount_groupNames' - The sign-in delegate group names.
newDisassociateSigninDelegateGroupsFromAccount ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'groupNames'
  Prelude.NonEmpty Prelude.Text ->
  DisassociateSigninDelegateGroupsFromAccount
newDisassociateSigninDelegateGroupsFromAccount
  pAccountId_
  pGroupNames_ =
    DisassociateSigninDelegateGroupsFromAccount'
      { accountId =
          pAccountId_,
        groupNames =
          Lens.coerced
            Lens.# pGroupNames_
      }

-- | The Amazon Chime account ID.
disassociateSigninDelegateGroupsFromAccount_accountId :: Lens.Lens' DisassociateSigninDelegateGroupsFromAccount Prelude.Text
disassociateSigninDelegateGroupsFromAccount_accountId = Lens.lens (\DisassociateSigninDelegateGroupsFromAccount' {accountId} -> accountId) (\s@DisassociateSigninDelegateGroupsFromAccount' {} a -> s {accountId = a} :: DisassociateSigninDelegateGroupsFromAccount)

-- | The sign-in delegate group names.
disassociateSigninDelegateGroupsFromAccount_groupNames :: Lens.Lens' DisassociateSigninDelegateGroupsFromAccount (Prelude.NonEmpty Prelude.Text)
disassociateSigninDelegateGroupsFromAccount_groupNames = Lens.lens (\DisassociateSigninDelegateGroupsFromAccount' {groupNames} -> groupNames) (\s@DisassociateSigninDelegateGroupsFromAccount' {} a -> s {groupNames = a} :: DisassociateSigninDelegateGroupsFromAccount) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DisassociateSigninDelegateGroupsFromAccount
  where
  type
    AWSResponse
      DisassociateSigninDelegateGroupsFromAccount =
      DisassociateSigninDelegateGroupsFromAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateSigninDelegateGroupsFromAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateSigninDelegateGroupsFromAccount
  where
  hashWithSalt
    _salt
    DisassociateSigninDelegateGroupsFromAccount' {..} =
      _salt `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` groupNames

instance
  Prelude.NFData
    DisassociateSigninDelegateGroupsFromAccount
  where
  rnf DisassociateSigninDelegateGroupsFromAccount' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf groupNames

instance
  Data.ToHeaders
    DisassociateSigninDelegateGroupsFromAccount
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    DisassociateSigninDelegateGroupsFromAccount
  where
  toJSON
    DisassociateSigninDelegateGroupsFromAccount' {..} =
      Data.object
        ( Prelude.catMaybes
            [Prelude.Just ("GroupNames" Data..= groupNames)]
        )

instance
  Data.ToPath
    DisassociateSigninDelegateGroupsFromAccount
  where
  toPath
    DisassociateSigninDelegateGroupsFromAccount' {..} =
      Prelude.mconcat ["/accounts/", Data.toBS accountId]

instance
  Data.ToQuery
    DisassociateSigninDelegateGroupsFromAccount
  where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          ["operation=disassociate-signin-delegate-groups"]
      )

-- | /See:/ 'newDisassociateSigninDelegateGroupsFromAccountResponse' smart constructor.
data DisassociateSigninDelegateGroupsFromAccountResponse = DisassociateSigninDelegateGroupsFromAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateSigninDelegateGroupsFromAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateSigninDelegateGroupsFromAccountResponse_httpStatus' - The response's http status code.
newDisassociateSigninDelegateGroupsFromAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateSigninDelegateGroupsFromAccountResponse
newDisassociateSigninDelegateGroupsFromAccountResponse
  pHttpStatus_ =
    DisassociateSigninDelegateGroupsFromAccountResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateSigninDelegateGroupsFromAccountResponse_httpStatus :: Lens.Lens' DisassociateSigninDelegateGroupsFromAccountResponse Prelude.Int
disassociateSigninDelegateGroupsFromAccountResponse_httpStatus = Lens.lens (\DisassociateSigninDelegateGroupsFromAccountResponse' {httpStatus} -> httpStatus) (\s@DisassociateSigninDelegateGroupsFromAccountResponse' {} a -> s {httpStatus = a} :: DisassociateSigninDelegateGroupsFromAccountResponse)

instance
  Prelude.NFData
    DisassociateSigninDelegateGroupsFromAccountResponse
  where
  rnf
    DisassociateSigninDelegateGroupsFromAccountResponse' {..} =
      Prelude.rnf httpStatus
