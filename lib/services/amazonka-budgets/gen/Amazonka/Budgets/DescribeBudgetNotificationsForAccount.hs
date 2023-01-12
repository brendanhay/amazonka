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
-- Module      : Amazonka.Budgets.DescribeBudgetNotificationsForAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the budget names and notifications that are associated with an
-- account.
--
-- This operation returns paginated results.
module Amazonka.Budgets.DescribeBudgetNotificationsForAccount
  ( -- * Creating a Request
    DescribeBudgetNotificationsForAccount (..),
    newDescribeBudgetNotificationsForAccount,

    -- * Request Lenses
    describeBudgetNotificationsForAccount_maxResults,
    describeBudgetNotificationsForAccount_nextToken,
    describeBudgetNotificationsForAccount_accountId,

    -- * Destructuring the Response
    DescribeBudgetNotificationsForAccountResponse (..),
    newDescribeBudgetNotificationsForAccountResponse,

    -- * Response Lenses
    describeBudgetNotificationsForAccountResponse_budgetNotificationsForAccount,
    describeBudgetNotificationsForAccountResponse_nextToken,
    describeBudgetNotificationsForAccountResponse_httpStatus,
  )
where

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBudgetNotificationsForAccount' smart constructor.
data DescribeBudgetNotificationsForAccount = DescribeBudgetNotificationsForAccount'
  { -- | An integer that shows how many budget name entries a paginated response
    -- contains.
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetNotificationsForAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeBudgetNotificationsForAccount_maxResults' - An integer that shows how many budget name entries a paginated response
-- contains.
--
-- 'nextToken', 'describeBudgetNotificationsForAccount_nextToken' - Undocumented member.
--
-- 'accountId', 'describeBudgetNotificationsForAccount_accountId' - Undocumented member.
newDescribeBudgetNotificationsForAccount ::
  -- | 'accountId'
  Prelude.Text ->
  DescribeBudgetNotificationsForAccount
newDescribeBudgetNotificationsForAccount pAccountId_ =
  DescribeBudgetNotificationsForAccount'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | An integer that shows how many budget name entries a paginated response
-- contains.
describeBudgetNotificationsForAccount_maxResults :: Lens.Lens' DescribeBudgetNotificationsForAccount (Prelude.Maybe Prelude.Natural)
describeBudgetNotificationsForAccount_maxResults = Lens.lens (\DescribeBudgetNotificationsForAccount' {maxResults} -> maxResults) (\s@DescribeBudgetNotificationsForAccount' {} a -> s {maxResults = a} :: DescribeBudgetNotificationsForAccount)

-- | Undocumented member.
describeBudgetNotificationsForAccount_nextToken :: Lens.Lens' DescribeBudgetNotificationsForAccount (Prelude.Maybe Prelude.Text)
describeBudgetNotificationsForAccount_nextToken = Lens.lens (\DescribeBudgetNotificationsForAccount' {nextToken} -> nextToken) (\s@DescribeBudgetNotificationsForAccount' {} a -> s {nextToken = a} :: DescribeBudgetNotificationsForAccount)

-- | Undocumented member.
describeBudgetNotificationsForAccount_accountId :: Lens.Lens' DescribeBudgetNotificationsForAccount Prelude.Text
describeBudgetNotificationsForAccount_accountId = Lens.lens (\DescribeBudgetNotificationsForAccount' {accountId} -> accountId) (\s@DescribeBudgetNotificationsForAccount' {} a -> s {accountId = a} :: DescribeBudgetNotificationsForAccount)

instance
  Core.AWSPager
    DescribeBudgetNotificationsForAccount
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBudgetNotificationsForAccountResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeBudgetNotificationsForAccountResponse_budgetNotificationsForAccount
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeBudgetNotificationsForAccount_nextToken
          Lens..~ rs
            Lens.^? describeBudgetNotificationsForAccountResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeBudgetNotificationsForAccount
  where
  type
    AWSResponse
      DescribeBudgetNotificationsForAccount =
      DescribeBudgetNotificationsForAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetNotificationsForAccountResponse'
            Prelude.<$> ( x Data..?> "BudgetNotificationsForAccount"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeBudgetNotificationsForAccount
  where
  hashWithSalt
    _salt
    DescribeBudgetNotificationsForAccount' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` accountId

instance
  Prelude.NFData
    DescribeBudgetNotificationsForAccount
  where
  rnf DescribeBudgetNotificationsForAccount' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accountId

instance
  Data.ToHeaders
    DescribeBudgetNotificationsForAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.DescribeBudgetNotificationsForAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeBudgetNotificationsForAccount
  where
  toJSON DescribeBudgetNotificationsForAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("AccountId" Data..= accountId)
          ]
      )

instance
  Data.ToPath
    DescribeBudgetNotificationsForAccount
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeBudgetNotificationsForAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBudgetNotificationsForAccountResponse' smart constructor.
data DescribeBudgetNotificationsForAccountResponse = DescribeBudgetNotificationsForAccountResponse'
  { -- | A list of budget names and associated notifications for an account.
    budgetNotificationsForAccount :: Prelude.Maybe [BudgetNotificationsForAccount],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetNotificationsForAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'budgetNotificationsForAccount', 'describeBudgetNotificationsForAccountResponse_budgetNotificationsForAccount' - A list of budget names and associated notifications for an account.
--
-- 'nextToken', 'describeBudgetNotificationsForAccountResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeBudgetNotificationsForAccountResponse_httpStatus' - The response's http status code.
newDescribeBudgetNotificationsForAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBudgetNotificationsForAccountResponse
newDescribeBudgetNotificationsForAccountResponse
  pHttpStatus_ =
    DescribeBudgetNotificationsForAccountResponse'
      { budgetNotificationsForAccount =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of budget names and associated notifications for an account.
describeBudgetNotificationsForAccountResponse_budgetNotificationsForAccount :: Lens.Lens' DescribeBudgetNotificationsForAccountResponse (Prelude.Maybe [BudgetNotificationsForAccount])
describeBudgetNotificationsForAccountResponse_budgetNotificationsForAccount = Lens.lens (\DescribeBudgetNotificationsForAccountResponse' {budgetNotificationsForAccount} -> budgetNotificationsForAccount) (\s@DescribeBudgetNotificationsForAccountResponse' {} a -> s {budgetNotificationsForAccount = a} :: DescribeBudgetNotificationsForAccountResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeBudgetNotificationsForAccountResponse_nextToken :: Lens.Lens' DescribeBudgetNotificationsForAccountResponse (Prelude.Maybe Prelude.Text)
describeBudgetNotificationsForAccountResponse_nextToken = Lens.lens (\DescribeBudgetNotificationsForAccountResponse' {nextToken} -> nextToken) (\s@DescribeBudgetNotificationsForAccountResponse' {} a -> s {nextToken = a} :: DescribeBudgetNotificationsForAccountResponse)

-- | The response's http status code.
describeBudgetNotificationsForAccountResponse_httpStatus :: Lens.Lens' DescribeBudgetNotificationsForAccountResponse Prelude.Int
describeBudgetNotificationsForAccountResponse_httpStatus = Lens.lens (\DescribeBudgetNotificationsForAccountResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetNotificationsForAccountResponse' {} a -> s {httpStatus = a} :: DescribeBudgetNotificationsForAccountResponse)

instance
  Prelude.NFData
    DescribeBudgetNotificationsForAccountResponse
  where
  rnf
    DescribeBudgetNotificationsForAccountResponse' {..} =
      Prelude.rnf budgetNotificationsForAccount
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
