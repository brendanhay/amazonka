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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBudgetActionsForAccount' smart constructor.
data DescribeBudgetActionsForAccount = DescribeBudgetActionsForAccount'
  { nextToken :: Core.Maybe Core.Text,
    maxResults :: Core.Maybe Core.Natural,
    accountId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeBudgetActionsForAccount
newDescribeBudgetActionsForAccount pAccountId_ =
  DescribeBudgetActionsForAccount'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      accountId = pAccountId_
    }

-- | Undocumented member.
describeBudgetActionsForAccount_nextToken :: Lens.Lens' DescribeBudgetActionsForAccount (Core.Maybe Core.Text)
describeBudgetActionsForAccount_nextToken = Lens.lens (\DescribeBudgetActionsForAccount' {nextToken} -> nextToken) (\s@DescribeBudgetActionsForAccount' {} a -> s {nextToken = a} :: DescribeBudgetActionsForAccount)

-- | Undocumented member.
describeBudgetActionsForAccount_maxResults :: Lens.Lens' DescribeBudgetActionsForAccount (Core.Maybe Core.Natural)
describeBudgetActionsForAccount_maxResults = Lens.lens (\DescribeBudgetActionsForAccount' {maxResults} -> maxResults) (\s@DescribeBudgetActionsForAccount' {} a -> s {maxResults = a} :: DescribeBudgetActionsForAccount)

-- | Undocumented member.
describeBudgetActionsForAccount_accountId :: Lens.Lens' DescribeBudgetActionsForAccount Core.Text
describeBudgetActionsForAccount_accountId = Lens.lens (\DescribeBudgetActionsForAccount' {accountId} -> accountId) (\s@DescribeBudgetActionsForAccount' {} a -> s {accountId = a} :: DescribeBudgetActionsForAccount)

instance
  Core.AWSPager
    DescribeBudgetActionsForAccount
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBudgetActionsForAccountResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. describeBudgetActionsForAccountResponse_actions
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeBudgetActionsForAccount_nextToken
          Lens..~ rs
          Lens.^? describeBudgetActionsForAccountResponse_nextToken
            Core.. Lens._Just

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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "Actions" Core..!@ Core.mempty)
      )

instance
  Core.Hashable
    DescribeBudgetActionsForAccount

instance Core.NFData DescribeBudgetActionsForAccount

instance
  Core.ToHeaders
    DescribeBudgetActionsForAccount
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.DescribeBudgetActionsForAccount" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeBudgetActionsForAccount where
  toJSON DescribeBudgetActionsForAccount' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("AccountId" Core..= accountId)
          ]
      )

instance Core.ToPath DescribeBudgetActionsForAccount where
  toPath = Core.const "/"

instance Core.ToQuery DescribeBudgetActionsForAccount where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeBudgetActionsForAccountResponse' smart constructor.
data DescribeBudgetActionsForAccountResponse = DescribeBudgetActionsForAccountResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of the budget action resources information.
    actions :: [Action]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeBudgetActionsForAccountResponse
newDescribeBudgetActionsForAccountResponse
  pHttpStatus_ =
    DescribeBudgetActionsForAccountResponse'
      { nextToken =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        actions = Core.mempty
      }

-- | Undocumented member.
describeBudgetActionsForAccountResponse_nextToken :: Lens.Lens' DescribeBudgetActionsForAccountResponse (Core.Maybe Core.Text)
describeBudgetActionsForAccountResponse_nextToken = Lens.lens (\DescribeBudgetActionsForAccountResponse' {nextToken} -> nextToken) (\s@DescribeBudgetActionsForAccountResponse' {} a -> s {nextToken = a} :: DescribeBudgetActionsForAccountResponse)

-- | The response's http status code.
describeBudgetActionsForAccountResponse_httpStatus :: Lens.Lens' DescribeBudgetActionsForAccountResponse Core.Int
describeBudgetActionsForAccountResponse_httpStatus = Lens.lens (\DescribeBudgetActionsForAccountResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetActionsForAccountResponse' {} a -> s {httpStatus = a} :: DescribeBudgetActionsForAccountResponse)

-- | A list of the budget action resources information.
describeBudgetActionsForAccountResponse_actions :: Lens.Lens' DescribeBudgetActionsForAccountResponse [Action]
describeBudgetActionsForAccountResponse_actions = Lens.lens (\DescribeBudgetActionsForAccountResponse' {actions} -> actions) (\s@DescribeBudgetActionsForAccountResponse' {} a -> s {actions = a} :: DescribeBudgetActionsForAccountResponse) Core.. Lens._Coerce

instance
  Core.NFData
    DescribeBudgetActionsForAccountResponse
