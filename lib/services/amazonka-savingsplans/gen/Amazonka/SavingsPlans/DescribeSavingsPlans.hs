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
-- Module      : Amazonka.SavingsPlans.DescribeSavingsPlans
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Savings Plans.
module Amazonka.SavingsPlans.DescribeSavingsPlans
  ( -- * Creating a Request
    DescribeSavingsPlans (..),
    newDescribeSavingsPlans,

    -- * Request Lenses
    describeSavingsPlans_states,
    describeSavingsPlans_savingsPlanIds,
    describeSavingsPlans_filters,
    describeSavingsPlans_nextToken,
    describeSavingsPlans_savingsPlanArns,
    describeSavingsPlans_maxResults,

    -- * Destructuring the Response
    DescribeSavingsPlansResponse (..),
    newDescribeSavingsPlansResponse,

    -- * Response Lenses
    describeSavingsPlansResponse_savingsPlans,
    describeSavingsPlansResponse_nextToken,
    describeSavingsPlansResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SavingsPlans.Types

-- | /See:/ 'newDescribeSavingsPlans' smart constructor.
data DescribeSavingsPlans = DescribeSavingsPlans'
  { -- | The states.
    states :: Prelude.Maybe [SavingsPlanState],
    -- | The IDs of the Savings Plans.
    savingsPlanIds :: Prelude.Maybe [Prelude.Text],
    -- | The filters.
    filters :: Prelude.Maybe [SavingsPlanFilter],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARN) of the Savings Plans.
    savingsPlanArns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- additional results, make another call with the returned token value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSavingsPlans' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'states', 'describeSavingsPlans_states' - The states.
--
-- 'savingsPlanIds', 'describeSavingsPlans_savingsPlanIds' - The IDs of the Savings Plans.
--
-- 'filters', 'describeSavingsPlans_filters' - The filters.
--
-- 'nextToken', 'describeSavingsPlans_nextToken' - The token for the next page of results.
--
-- 'savingsPlanArns', 'describeSavingsPlans_savingsPlanArns' - The Amazon Resource Names (ARN) of the Savings Plans.
--
-- 'maxResults', 'describeSavingsPlans_maxResults' - The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
newDescribeSavingsPlans ::
  DescribeSavingsPlans
newDescribeSavingsPlans =
  DescribeSavingsPlans'
    { states = Prelude.Nothing,
      savingsPlanIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      savingsPlanArns = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The states.
describeSavingsPlans_states :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe [SavingsPlanState])
describeSavingsPlans_states = Lens.lens (\DescribeSavingsPlans' {states} -> states) (\s@DescribeSavingsPlans' {} a -> s {states = a} :: DescribeSavingsPlans) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the Savings Plans.
describeSavingsPlans_savingsPlanIds :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe [Prelude.Text])
describeSavingsPlans_savingsPlanIds = Lens.lens (\DescribeSavingsPlans' {savingsPlanIds} -> savingsPlanIds) (\s@DescribeSavingsPlans' {} a -> s {savingsPlanIds = a} :: DescribeSavingsPlans) Prelude.. Lens.mapping Lens.coerced

-- | The filters.
describeSavingsPlans_filters :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe [SavingsPlanFilter])
describeSavingsPlans_filters = Lens.lens (\DescribeSavingsPlans' {filters} -> filters) (\s@DescribeSavingsPlans' {} a -> s {filters = a} :: DescribeSavingsPlans) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
describeSavingsPlans_nextToken :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe Prelude.Text)
describeSavingsPlans_nextToken = Lens.lens (\DescribeSavingsPlans' {nextToken} -> nextToken) (\s@DescribeSavingsPlans' {} a -> s {nextToken = a} :: DescribeSavingsPlans)

-- | The Amazon Resource Names (ARN) of the Savings Plans.
describeSavingsPlans_savingsPlanArns :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe [Prelude.Text])
describeSavingsPlans_savingsPlanArns = Lens.lens (\DescribeSavingsPlans' {savingsPlanArns} -> savingsPlanArns) (\s@DescribeSavingsPlans' {} a -> s {savingsPlanArns = a} :: DescribeSavingsPlans) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
describeSavingsPlans_maxResults :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe Prelude.Natural)
describeSavingsPlans_maxResults = Lens.lens (\DescribeSavingsPlans' {maxResults} -> maxResults) (\s@DescribeSavingsPlans' {} a -> s {maxResults = a} :: DescribeSavingsPlans)

instance Core.AWSRequest DescribeSavingsPlans where
  type
    AWSResponse DescribeSavingsPlans =
      DescribeSavingsPlansResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSavingsPlansResponse'
            Prelude.<$> (x Core..?> "savingsPlans" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSavingsPlans

instance Prelude.NFData DescribeSavingsPlans

instance Core.ToHeaders DescribeSavingsPlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSavingsPlans where
  toJSON DescribeSavingsPlans' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("states" Core..=) Prelude.<$> states,
            ("savingsPlanIds" Core..=)
              Prelude.<$> savingsPlanIds,
            ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("savingsPlanArns" Core..=)
              Prelude.<$> savingsPlanArns,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeSavingsPlans where
  toPath = Prelude.const "/DescribeSavingsPlans"

instance Core.ToQuery DescribeSavingsPlans where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSavingsPlansResponse' smart constructor.
data DescribeSavingsPlansResponse = DescribeSavingsPlansResponse'
  { -- | Information about the Savings Plans.
    savingsPlans :: Prelude.Maybe [SavingsPlan],
    -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSavingsPlansResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'savingsPlans', 'describeSavingsPlansResponse_savingsPlans' - Information about the Savings Plans.
--
-- 'nextToken', 'describeSavingsPlansResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'httpStatus', 'describeSavingsPlansResponse_httpStatus' - The response's http status code.
newDescribeSavingsPlansResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSavingsPlansResponse
newDescribeSavingsPlansResponse pHttpStatus_ =
  DescribeSavingsPlansResponse'
    { savingsPlans =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Savings Plans.
describeSavingsPlansResponse_savingsPlans :: Lens.Lens' DescribeSavingsPlansResponse (Prelude.Maybe [SavingsPlan])
describeSavingsPlansResponse_savingsPlans = Lens.lens (\DescribeSavingsPlansResponse' {savingsPlans} -> savingsPlans) (\s@DescribeSavingsPlansResponse' {} a -> s {savingsPlans = a} :: DescribeSavingsPlansResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeSavingsPlansResponse_nextToken :: Lens.Lens' DescribeSavingsPlansResponse (Prelude.Maybe Prelude.Text)
describeSavingsPlansResponse_nextToken = Lens.lens (\DescribeSavingsPlansResponse' {nextToken} -> nextToken) (\s@DescribeSavingsPlansResponse' {} a -> s {nextToken = a} :: DescribeSavingsPlansResponse)

-- | The response's http status code.
describeSavingsPlansResponse_httpStatus :: Lens.Lens' DescribeSavingsPlansResponse Prelude.Int
describeSavingsPlansResponse_httpStatus = Lens.lens (\DescribeSavingsPlansResponse' {httpStatus} -> httpStatus) (\s@DescribeSavingsPlansResponse' {} a -> s {httpStatus = a} :: DescribeSavingsPlansResponse)

instance Prelude.NFData DescribeSavingsPlansResponse
