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
-- Module      : Amazonka.Organizations.DescribeAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves Organizations-related information about the specified account.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- Amazon Web Services service.
module Amazonka.Organizations.DescribeAccount
  ( -- * Creating a Request
    DescribeAccount (..),
    newDescribeAccount,

    -- * Request Lenses
    describeAccount_accountId,

    -- * Destructuring the Response
    DescribeAccountResponse (..),
    newDescribeAccountResponse,

    -- * Response Lenses
    describeAccountResponse_account,
    describeAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccount' smart constructor.
data DescribeAccount = DescribeAccount'
  { -- | The unique identifier (ID) of the Amazon Web Services account that you
    -- want information about. You can get the ID from the ListAccounts or
    -- ListAccountsForParent operations.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
    -- string requires exactly 12 digits.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'describeAccount_accountId' - The unique identifier (ID) of the Amazon Web Services account that you
-- want information about. You can get the ID from the ListAccounts or
-- ListAccountsForParent operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
newDescribeAccount ::
  -- | 'accountId'
  Prelude.Text ->
  DescribeAccount
newDescribeAccount pAccountId_ =
  DescribeAccount' {accountId = pAccountId_}

-- | The unique identifier (ID) of the Amazon Web Services account that you
-- want information about. You can get the ID from the ListAccounts or
-- ListAccountsForParent operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
describeAccount_accountId :: Lens.Lens' DescribeAccount Prelude.Text
describeAccount_accountId = Lens.lens (\DescribeAccount' {accountId} -> accountId) (\s@DescribeAccount' {} a -> s {accountId = a} :: DescribeAccount)

instance Core.AWSRequest DescribeAccount where
  type
    AWSResponse DescribeAccount =
      DescribeAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountResponse'
            Prelude.<$> (x Data..?> "Account")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccount where
  hashWithSalt _salt DescribeAccount' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData DescribeAccount where
  rnf DescribeAccount' {..} = Prelude.rnf accountId

instance Data.ToHeaders DescribeAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.DescribeAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAccount where
  toJSON DescribeAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AccountId" Data..= accountId)]
      )

instance Data.ToPath DescribeAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountResponse' smart constructor.
data DescribeAccountResponse = DescribeAccountResponse'
  { -- | A structure that contains information about the requested account.
    account :: Prelude.Maybe Account,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'describeAccountResponse_account' - A structure that contains information about the requested account.
--
-- 'httpStatus', 'describeAccountResponse_httpStatus' - The response's http status code.
newDescribeAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountResponse
newDescribeAccountResponse pHttpStatus_ =
  DescribeAccountResponse'
    { account = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains information about the requested account.
describeAccountResponse_account :: Lens.Lens' DescribeAccountResponse (Prelude.Maybe Account)
describeAccountResponse_account = Lens.lens (\DescribeAccountResponse' {account} -> account) (\s@DescribeAccountResponse' {} a -> s {account = a} :: DescribeAccountResponse)

-- | The response's http status code.
describeAccountResponse_httpStatus :: Lens.Lens' DescribeAccountResponse Prelude.Int
describeAccountResponse_httpStatus = Lens.lens (\DescribeAccountResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountResponse' {} a -> s {httpStatus = a} :: DescribeAccountResponse)

instance Prelude.NFData DescribeAccountResponse where
  rnf DescribeAccountResponse' {..} =
    Prelude.rnf account
      `Prelude.seq` Prelude.rnf httpStatus
