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
-- Module      : Network.AWS.Budgets.DescribeBudgetActionsForAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all of the budget actions for an account.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetActionsForAccount
  ( -- * Creating a Request
    DescribeBudgetActionsForAccount (..),
    newDescribeBudgetActionsForAccount,

    -- * Request Lenses
    describeBudgetActionsForAccount_nextToken,
    describeBudgetActionsForAccount_maxResults,
    describeBudgetActionsForAccount_accountId,

    -- * Destructuring the Response
    DescribeBudgetActionsForAccountResponse (..),
    newDescribeBudgetActionsForAccountResponse,

    -- * Response Lenses
    describeBudgetActionsForAccountResponse_nextToken,
    describeBudgetActionsForAccountResponse_httpStatus,
    describeBudgetActionsForAccountResponse_actions,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBudgetActionsForAccount' smart constructor.
data DescribeBudgetActionsForAccount = DescribeBudgetActionsForAccount'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetActionsForAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBudgetActionsForAccount_nextToken' - Undocumented member.
--
-- 'maxResults', 'describeBudgetActionsForAccount_maxResults' - Undocumented member.
--
-- 'accountId', 'describeBudgetActionsForAccount_accountId' - Undocumented member.
newDescribeBudgetActionsForAccount ::
  -- | 'accountId'
  Prelude.Text ->
  DescribeBudgetActionsForAccount
newDescribeBudgetActionsForAccount pAccountId_ =
  DescribeBudgetActionsForAccount'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | Undocumented member.
describeBudgetActionsForAccount_nextToken :: Lens.Lens' DescribeBudgetActionsForAccount (Prelude.Maybe Prelude.Text)
describeBudgetActionsForAccount_nextToken = Lens.lens (\DescribeBudgetActionsForAccount' {nextToken} -> nextToken) (\s@DescribeBudgetActionsForAccount' {} a -> s {nextToken = a} :: DescribeBudgetActionsForAccount)

-- | Undocumented member.
describeBudgetActionsForAccount_maxResults :: Lens.Lens' DescribeBudgetActionsForAccount (Prelude.Maybe Prelude.Natural)
describeBudgetActionsForAccount_maxResults = Lens.lens (\DescribeBudgetActionsForAccount' {maxResults} -> maxResults) (\s@DescribeBudgetActionsForAccount' {} a -> s {maxResults = a} :: DescribeBudgetActionsForAccount)

-- | Undocumented member.
describeBudgetActionsForAccount_accountId :: Lens.Lens' DescribeBudgetActionsForAccount Prelude.Text
describeBudgetActionsForAccount_accountId = Lens.lens (\DescribeBudgetActionsForAccount' {accountId} -> accountId) (\s@DescribeBudgetActionsForAccount' {} a -> s {accountId = a} :: DescribeBudgetActionsForAccount)

instance
  Core.AWSPager
    DescribeBudgetActionsForAccount
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBudgetActionsForAccountResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. describeBudgetActionsForAccountResponse_actions
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeBudgetActionsForAccount_nextToken
          Lens..~ rs
          Lens.^? describeBudgetActionsForAccountResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeBudgetActionsForAccount
  where
  type
    AWSResponse DescribeBudgetActionsForAccount =
      DescribeBudgetActionsForAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetActionsForAccountResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Actions" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    DescribeBudgetActionsForAccount

instance
  Prelude.NFData
    DescribeBudgetActionsForAccount

instance
  Core.ToHeaders
    DescribeBudgetActionsForAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.DescribeBudgetActionsForAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeBudgetActionsForAccount where
  toJSON DescribeBudgetActionsForAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("AccountId" Core..= accountId)
          ]
      )

instance Core.ToPath DescribeBudgetActionsForAccount where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeBudgetActionsForAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBudgetActionsForAccountResponse' smart constructor.
data DescribeBudgetActionsForAccountResponse = DescribeBudgetActionsForAccountResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of the budget action resources information.
    actions :: [Action]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetActionsForAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBudgetActionsForAccountResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeBudgetActionsForAccountResponse_httpStatus' - The response's http status code.
--
-- 'actions', 'describeBudgetActionsForAccountResponse_actions' - A list of the budget action resources information.
newDescribeBudgetActionsForAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBudgetActionsForAccountResponse
newDescribeBudgetActionsForAccountResponse
  pHttpStatus_ =
    DescribeBudgetActionsForAccountResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        actions = Prelude.mempty
      }

-- | Undocumented member.
describeBudgetActionsForAccountResponse_nextToken :: Lens.Lens' DescribeBudgetActionsForAccountResponse (Prelude.Maybe Prelude.Text)
describeBudgetActionsForAccountResponse_nextToken = Lens.lens (\DescribeBudgetActionsForAccountResponse' {nextToken} -> nextToken) (\s@DescribeBudgetActionsForAccountResponse' {} a -> s {nextToken = a} :: DescribeBudgetActionsForAccountResponse)

-- | The response's http status code.
describeBudgetActionsForAccountResponse_httpStatus :: Lens.Lens' DescribeBudgetActionsForAccountResponse Prelude.Int
describeBudgetActionsForAccountResponse_httpStatus = Lens.lens (\DescribeBudgetActionsForAccountResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetActionsForAccountResponse' {} a -> s {httpStatus = a} :: DescribeBudgetActionsForAccountResponse)

-- | A list of the budget action resources information.
describeBudgetActionsForAccountResponse_actions :: Lens.Lens' DescribeBudgetActionsForAccountResponse [Action]
describeBudgetActionsForAccountResponse_actions = Lens.lens (\DescribeBudgetActionsForAccountResponse' {actions} -> actions) (\s@DescribeBudgetActionsForAccountResponse' {} a -> s {actions = a} :: DescribeBudgetActionsForAccountResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    DescribeBudgetActionsForAccountResponse
