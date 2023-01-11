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
-- Module      : Amazonka.Chime.GetAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details for the specified Amazon Chime account, such as
-- account type and supported licenses.
module Amazonka.Chime.GetAccount
  ( -- * Creating a Request
    GetAccount (..),
    newGetAccount,

    -- * Request Lenses
    getAccount_accountId,

    -- * Destructuring the Response
    GetAccountResponse (..),
    newGetAccountResponse,

    -- * Response Lenses
    getAccountResponse_account,
    getAccountResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccount' smart constructor.
data GetAccount = GetAccount'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getAccount_accountId' - The Amazon Chime account ID.
newGetAccount ::
  -- | 'accountId'
  Prelude.Text ->
  GetAccount
newGetAccount pAccountId_ =
  GetAccount' {accountId = pAccountId_}

-- | The Amazon Chime account ID.
getAccount_accountId :: Lens.Lens' GetAccount Prelude.Text
getAccount_accountId = Lens.lens (\GetAccount' {accountId} -> accountId) (\s@GetAccount' {} a -> s {accountId = a} :: GetAccount)

instance Core.AWSRequest GetAccount where
  type AWSResponse GetAccount = GetAccountResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountResponse'
            Prelude.<$> (x Data..?> "Account")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccount where
  hashWithSalt _salt GetAccount' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData GetAccount where
  rnf GetAccount' {..} = Prelude.rnf accountId

instance Data.ToHeaders GetAccount where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetAccount where
  toPath GetAccount' {..} =
    Prelude.mconcat ["/accounts/", Data.toBS accountId]

instance Data.ToQuery GetAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccountResponse' smart constructor.
data GetAccountResponse = GetAccountResponse'
  { -- | The Amazon Chime account details.
    account :: Prelude.Maybe Account,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'getAccountResponse_account' - The Amazon Chime account details.
--
-- 'httpStatus', 'getAccountResponse_httpStatus' - The response's http status code.
newGetAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccountResponse
newGetAccountResponse pHttpStatus_ =
  GetAccountResponse'
    { account = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Chime account details.
getAccountResponse_account :: Lens.Lens' GetAccountResponse (Prelude.Maybe Account)
getAccountResponse_account = Lens.lens (\GetAccountResponse' {account} -> account) (\s@GetAccountResponse' {} a -> s {account = a} :: GetAccountResponse)

-- | The response's http status code.
getAccountResponse_httpStatus :: Lens.Lens' GetAccountResponse Prelude.Int
getAccountResponse_httpStatus = Lens.lens (\GetAccountResponse' {httpStatus} -> httpStatus) (\s@GetAccountResponse' {} a -> s {httpStatus = a} :: GetAccountResponse)

instance Prelude.NFData GetAccountResponse where
  rnf GetAccountResponse' {..} =
    Prelude.rnf account
      `Prelude.seq` Prelude.rnf httpStatus
