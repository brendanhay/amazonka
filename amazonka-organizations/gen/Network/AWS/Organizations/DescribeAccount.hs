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
-- Module      : Network.AWS.Organizations.DescribeAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves AWS Organizations-related information about the specified
-- account.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- AWS service.
module Network.AWS.Organizations.DescribeAccount
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAccount' smart constructor.
data DescribeAccount = DescribeAccount'
  { -- | The unique identifier (ID) of the AWS account that you want information
    -- about. You can get the ID from the ListAccounts or ListAccountsForParent
    -- operations.
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
-- 'accountId', 'describeAccount_accountId' - The unique identifier (ID) of the AWS account that you want information
-- about. You can get the ID from the ListAccounts or ListAccountsForParent
-- operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
newDescribeAccount ::
  -- | 'accountId'
  Prelude.Text ->
  DescribeAccount
newDescribeAccount pAccountId_ =
  DescribeAccount' {accountId = pAccountId_}

-- | The unique identifier (ID) of the AWS account that you want information
-- about. You can get the ID from the ListAccounts or ListAccountsForParent
-- operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
describeAccount_accountId :: Lens.Lens' DescribeAccount Prelude.Text
describeAccount_accountId = Lens.lens (\DescribeAccount' {accountId} -> accountId) (\s@DescribeAccount' {} a -> s {accountId = a} :: DescribeAccount)

instance Core.AWSRequest DescribeAccount where
  type
    AWSResponse DescribeAccount =
      DescribeAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountResponse'
            Prelude.<$> (x Core..?> "Account")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccount

instance Prelude.NFData DescribeAccount

instance Core.ToHeaders DescribeAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.DescribeAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAccount where
  toJSON DescribeAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("AccountId" Core..= accountId)]
      )

instance Core.ToPath DescribeAccount where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAccount where
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

instance Prelude.NFData DescribeAccountResponse
