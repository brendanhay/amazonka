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
-- Module      : Amazonka.Budgets.DescribeBudgetPerformanceHistory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the history for @DAILY@, @MONTHLY@, and @QUARTERLY@ budgets.
-- Budget history isn\'t available for @ANNUAL@ budgets.
--
-- This operation returns paginated results.
module Amazonka.Budgets.DescribeBudgetPerformanceHistory
  ( -- * Creating a Request
    DescribeBudgetPerformanceHistory (..),
    newDescribeBudgetPerformanceHistory,

    -- * Request Lenses
    describeBudgetPerformanceHistory_maxResults,
    describeBudgetPerformanceHistory_nextToken,
    describeBudgetPerformanceHistory_timePeriod,
    describeBudgetPerformanceHistory_accountId,
    describeBudgetPerformanceHistory_budgetName,

    -- * Destructuring the Response
    DescribeBudgetPerformanceHistoryResponse (..),
    newDescribeBudgetPerformanceHistoryResponse,

    -- * Response Lenses
    describeBudgetPerformanceHistoryResponse_budgetPerformanceHistory,
    describeBudgetPerformanceHistoryResponse_nextToken,
    describeBudgetPerformanceHistoryResponse_httpStatus,
  )
where

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBudgetPerformanceHistory' smart constructor.
data DescribeBudgetPerformanceHistory = DescribeBudgetPerformanceHistory'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Retrieves how often the budget went into an @ALARM@ state for the
    -- specified time period.
    timePeriod :: Prelude.Maybe TimePeriod,
    accountId :: Prelude.Text,
    budgetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetPerformanceHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeBudgetPerformanceHistory_maxResults' - Undocumented member.
--
-- 'nextToken', 'describeBudgetPerformanceHistory_nextToken' - Undocumented member.
--
-- 'timePeriod', 'describeBudgetPerformanceHistory_timePeriod' - Retrieves how often the budget went into an @ALARM@ state for the
-- specified time period.
--
-- 'accountId', 'describeBudgetPerformanceHistory_accountId' - Undocumented member.
--
-- 'budgetName', 'describeBudgetPerformanceHistory_budgetName' - Undocumented member.
newDescribeBudgetPerformanceHistory ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  DescribeBudgetPerformanceHistory
newDescribeBudgetPerformanceHistory
  pAccountId_
  pBudgetName_ =
    DescribeBudgetPerformanceHistory'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        timePeriod = Prelude.Nothing,
        accountId = pAccountId_,
        budgetName = pBudgetName_
      }

-- | Undocumented member.
describeBudgetPerformanceHistory_maxResults :: Lens.Lens' DescribeBudgetPerformanceHistory (Prelude.Maybe Prelude.Natural)
describeBudgetPerformanceHistory_maxResults = Lens.lens (\DescribeBudgetPerformanceHistory' {maxResults} -> maxResults) (\s@DescribeBudgetPerformanceHistory' {} a -> s {maxResults = a} :: DescribeBudgetPerformanceHistory)

-- | Undocumented member.
describeBudgetPerformanceHistory_nextToken :: Lens.Lens' DescribeBudgetPerformanceHistory (Prelude.Maybe Prelude.Text)
describeBudgetPerformanceHistory_nextToken = Lens.lens (\DescribeBudgetPerformanceHistory' {nextToken} -> nextToken) (\s@DescribeBudgetPerformanceHistory' {} a -> s {nextToken = a} :: DescribeBudgetPerformanceHistory)

-- | Retrieves how often the budget went into an @ALARM@ state for the
-- specified time period.
describeBudgetPerformanceHistory_timePeriod :: Lens.Lens' DescribeBudgetPerformanceHistory (Prelude.Maybe TimePeriod)
describeBudgetPerformanceHistory_timePeriod = Lens.lens (\DescribeBudgetPerformanceHistory' {timePeriod} -> timePeriod) (\s@DescribeBudgetPerformanceHistory' {} a -> s {timePeriod = a} :: DescribeBudgetPerformanceHistory)

-- | Undocumented member.
describeBudgetPerformanceHistory_accountId :: Lens.Lens' DescribeBudgetPerformanceHistory Prelude.Text
describeBudgetPerformanceHistory_accountId = Lens.lens (\DescribeBudgetPerformanceHistory' {accountId} -> accountId) (\s@DescribeBudgetPerformanceHistory' {} a -> s {accountId = a} :: DescribeBudgetPerformanceHistory)

-- | Undocumented member.
describeBudgetPerformanceHistory_budgetName :: Lens.Lens' DescribeBudgetPerformanceHistory Prelude.Text
describeBudgetPerformanceHistory_budgetName = Lens.lens (\DescribeBudgetPerformanceHistory' {budgetName} -> budgetName) (\s@DescribeBudgetPerformanceHistory' {} a -> s {budgetName = a} :: DescribeBudgetPerformanceHistory)

instance
  Core.AWSPager
    DescribeBudgetPerformanceHistory
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBudgetPerformanceHistoryResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeBudgetPerformanceHistoryResponse_budgetPerformanceHistory
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeBudgetPerformanceHistory_nextToken
              Lens..~ rs
              Lens.^? describeBudgetPerformanceHistoryResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeBudgetPerformanceHistory
  where
  type
    AWSResponse DescribeBudgetPerformanceHistory =
      DescribeBudgetPerformanceHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetPerformanceHistoryResponse'
            Prelude.<$> (x Data..?> "BudgetPerformanceHistory")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeBudgetPerformanceHistory
  where
  hashWithSalt
    _salt
    DescribeBudgetPerformanceHistory' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` timePeriod
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` budgetName

instance
  Prelude.NFData
    DescribeBudgetPerformanceHistory
  where
  rnf DescribeBudgetPerformanceHistory' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf timePeriod `Prelude.seq`
          Prelude.rnf accountId `Prelude.seq`
            Prelude.rnf budgetName

instance
  Data.ToHeaders
    DescribeBudgetPerformanceHistory
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.DescribeBudgetPerformanceHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBudgetPerformanceHistory where
  toJSON DescribeBudgetPerformanceHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("TimePeriod" Data..=) Prelude.<$> timePeriod,
            Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName)
          ]
      )

instance Data.ToPath DescribeBudgetPerformanceHistory where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeBudgetPerformanceHistory
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBudgetPerformanceHistoryResponse' smart constructor.
data DescribeBudgetPerformanceHistoryResponse = DescribeBudgetPerformanceHistoryResponse'
  { -- | The history of how often the budget has gone into an @ALARM@ state.
    --
    -- For @DAILY@ budgets, the history saves the state of the budget for the
    -- last 60 days. For @MONTHLY@ budgets, the history saves the state of the
    -- budget for the current month plus the last 12 months. For @QUARTERLY@
    -- budgets, the history saves the state of the budget for the last four
    -- quarters.
    budgetPerformanceHistory :: Prelude.Maybe BudgetPerformanceHistory,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetPerformanceHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'budgetPerformanceHistory', 'describeBudgetPerformanceHistoryResponse_budgetPerformanceHistory' - The history of how often the budget has gone into an @ALARM@ state.
--
-- For @DAILY@ budgets, the history saves the state of the budget for the
-- last 60 days. For @MONTHLY@ budgets, the history saves the state of the
-- budget for the current month plus the last 12 months. For @QUARTERLY@
-- budgets, the history saves the state of the budget for the last four
-- quarters.
--
-- 'nextToken', 'describeBudgetPerformanceHistoryResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeBudgetPerformanceHistoryResponse_httpStatus' - The response's http status code.
newDescribeBudgetPerformanceHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBudgetPerformanceHistoryResponse
newDescribeBudgetPerformanceHistoryResponse
  pHttpStatus_ =
    DescribeBudgetPerformanceHistoryResponse'
      { budgetPerformanceHistory =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The history of how often the budget has gone into an @ALARM@ state.
--
-- For @DAILY@ budgets, the history saves the state of the budget for the
-- last 60 days. For @MONTHLY@ budgets, the history saves the state of the
-- budget for the current month plus the last 12 months. For @QUARTERLY@
-- budgets, the history saves the state of the budget for the last four
-- quarters.
describeBudgetPerformanceHistoryResponse_budgetPerformanceHistory :: Lens.Lens' DescribeBudgetPerformanceHistoryResponse (Prelude.Maybe BudgetPerformanceHistory)
describeBudgetPerformanceHistoryResponse_budgetPerformanceHistory = Lens.lens (\DescribeBudgetPerformanceHistoryResponse' {budgetPerformanceHistory} -> budgetPerformanceHistory) (\s@DescribeBudgetPerformanceHistoryResponse' {} a -> s {budgetPerformanceHistory = a} :: DescribeBudgetPerformanceHistoryResponse)

-- | Undocumented member.
describeBudgetPerformanceHistoryResponse_nextToken :: Lens.Lens' DescribeBudgetPerformanceHistoryResponse (Prelude.Maybe Prelude.Text)
describeBudgetPerformanceHistoryResponse_nextToken = Lens.lens (\DescribeBudgetPerformanceHistoryResponse' {nextToken} -> nextToken) (\s@DescribeBudgetPerformanceHistoryResponse' {} a -> s {nextToken = a} :: DescribeBudgetPerformanceHistoryResponse)

-- | The response's http status code.
describeBudgetPerformanceHistoryResponse_httpStatus :: Lens.Lens' DescribeBudgetPerformanceHistoryResponse Prelude.Int
describeBudgetPerformanceHistoryResponse_httpStatus = Lens.lens (\DescribeBudgetPerformanceHistoryResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetPerformanceHistoryResponse' {} a -> s {httpStatus = a} :: DescribeBudgetPerformanceHistoryResponse)

instance
  Prelude.NFData
    DescribeBudgetPerformanceHistoryResponse
  where
  rnf DescribeBudgetPerformanceHistoryResponse' {..} =
    Prelude.rnf budgetPerformanceHistory `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
