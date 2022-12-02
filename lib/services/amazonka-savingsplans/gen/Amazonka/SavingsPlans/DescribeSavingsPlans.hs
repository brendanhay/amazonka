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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeSavingsPlans_nextToken,
    describeSavingsPlans_savingsPlanArns,
    describeSavingsPlans_filters,
    describeSavingsPlans_maxResults,
    describeSavingsPlans_savingsPlanIds,
    describeSavingsPlans_states,

    -- * Destructuring the Response
    DescribeSavingsPlansResponse (..),
    newDescribeSavingsPlansResponse,

    -- * Response Lenses
    describeSavingsPlansResponse_nextToken,
    describeSavingsPlansResponse_savingsPlans,
    describeSavingsPlansResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SavingsPlans.Types

-- | /See:/ 'newDescribeSavingsPlans' smart constructor.
data DescribeSavingsPlans = DescribeSavingsPlans'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARN) of the Savings Plans.
    savingsPlanArns :: Prelude.Maybe [Prelude.Text],
    -- | The filters.
    filters :: Prelude.Maybe [SavingsPlanFilter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- additional results, make another call with the returned token value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The IDs of the Savings Plans.
    savingsPlanIds :: Prelude.Maybe [Prelude.Text],
    -- | The states.
    states :: Prelude.Maybe [SavingsPlanState]
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
-- 'nextToken', 'describeSavingsPlans_nextToken' - The token for the next page of results.
--
-- 'savingsPlanArns', 'describeSavingsPlans_savingsPlanArns' - The Amazon Resource Names (ARN) of the Savings Plans.
--
-- 'filters', 'describeSavingsPlans_filters' - The filters.
--
-- 'maxResults', 'describeSavingsPlans_maxResults' - The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
--
-- 'savingsPlanIds', 'describeSavingsPlans_savingsPlanIds' - The IDs of the Savings Plans.
--
-- 'states', 'describeSavingsPlans_states' - The states.
newDescribeSavingsPlans ::
  DescribeSavingsPlans
newDescribeSavingsPlans =
  DescribeSavingsPlans'
    { nextToken = Prelude.Nothing,
      savingsPlanArns = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      savingsPlanIds = Prelude.Nothing,
      states = Prelude.Nothing
    }

-- | The token for the next page of results.
describeSavingsPlans_nextToken :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe Prelude.Text)
describeSavingsPlans_nextToken = Lens.lens (\DescribeSavingsPlans' {nextToken} -> nextToken) (\s@DescribeSavingsPlans' {} a -> s {nextToken = a} :: DescribeSavingsPlans)

-- | The Amazon Resource Names (ARN) of the Savings Plans.
describeSavingsPlans_savingsPlanArns :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe [Prelude.Text])
describeSavingsPlans_savingsPlanArns = Lens.lens (\DescribeSavingsPlans' {savingsPlanArns} -> savingsPlanArns) (\s@DescribeSavingsPlans' {} a -> s {savingsPlanArns = a} :: DescribeSavingsPlans) Prelude.. Lens.mapping Lens.coerced

-- | The filters.
describeSavingsPlans_filters :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe [SavingsPlanFilter])
describeSavingsPlans_filters = Lens.lens (\DescribeSavingsPlans' {filters} -> filters) (\s@DescribeSavingsPlans' {} a -> s {filters = a} :: DescribeSavingsPlans) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
describeSavingsPlans_maxResults :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe Prelude.Natural)
describeSavingsPlans_maxResults = Lens.lens (\DescribeSavingsPlans' {maxResults} -> maxResults) (\s@DescribeSavingsPlans' {} a -> s {maxResults = a} :: DescribeSavingsPlans)

-- | The IDs of the Savings Plans.
describeSavingsPlans_savingsPlanIds :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe [Prelude.Text])
describeSavingsPlans_savingsPlanIds = Lens.lens (\DescribeSavingsPlans' {savingsPlanIds} -> savingsPlanIds) (\s@DescribeSavingsPlans' {} a -> s {savingsPlanIds = a} :: DescribeSavingsPlans) Prelude.. Lens.mapping Lens.coerced

-- | The states.
describeSavingsPlans_states :: Lens.Lens' DescribeSavingsPlans (Prelude.Maybe [SavingsPlanState])
describeSavingsPlans_states = Lens.lens (\DescribeSavingsPlans' {states} -> states) (\s@DescribeSavingsPlans' {} a -> s {states = a} :: DescribeSavingsPlans) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeSavingsPlans where
  type
    AWSResponse DescribeSavingsPlans =
      DescribeSavingsPlansResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSavingsPlansResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "savingsPlans" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSavingsPlans where
  hashWithSalt _salt DescribeSavingsPlans' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` savingsPlanArns
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` savingsPlanIds
      `Prelude.hashWithSalt` states

instance Prelude.NFData DescribeSavingsPlans where
  rnf DescribeSavingsPlans' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf savingsPlanArns
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf savingsPlanIds
      `Prelude.seq` Prelude.rnf states

instance Data.ToHeaders DescribeSavingsPlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSavingsPlans where
  toJSON DescribeSavingsPlans' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("savingsPlanArns" Data..=)
              Prelude.<$> savingsPlanArns,
            ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("savingsPlanIds" Data..=)
              Prelude.<$> savingsPlanIds,
            ("states" Data..=) Prelude.<$> states
          ]
      )

instance Data.ToPath DescribeSavingsPlans where
  toPath = Prelude.const "/DescribeSavingsPlans"

instance Data.ToQuery DescribeSavingsPlans where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSavingsPlansResponse' smart constructor.
data DescribeSavingsPlansResponse = DescribeSavingsPlansResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the Savings Plans.
    savingsPlans :: Prelude.Maybe [SavingsPlan],
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
-- 'nextToken', 'describeSavingsPlansResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'savingsPlans', 'describeSavingsPlansResponse_savingsPlans' - Information about the Savings Plans.
--
-- 'httpStatus', 'describeSavingsPlansResponse_httpStatus' - The response's http status code.
newDescribeSavingsPlansResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSavingsPlansResponse
newDescribeSavingsPlansResponse pHttpStatus_ =
  DescribeSavingsPlansResponse'
    { nextToken =
        Prelude.Nothing,
      savingsPlans = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeSavingsPlansResponse_nextToken :: Lens.Lens' DescribeSavingsPlansResponse (Prelude.Maybe Prelude.Text)
describeSavingsPlansResponse_nextToken = Lens.lens (\DescribeSavingsPlansResponse' {nextToken} -> nextToken) (\s@DescribeSavingsPlansResponse' {} a -> s {nextToken = a} :: DescribeSavingsPlansResponse)

-- | Information about the Savings Plans.
describeSavingsPlansResponse_savingsPlans :: Lens.Lens' DescribeSavingsPlansResponse (Prelude.Maybe [SavingsPlan])
describeSavingsPlansResponse_savingsPlans = Lens.lens (\DescribeSavingsPlansResponse' {savingsPlans} -> savingsPlans) (\s@DescribeSavingsPlansResponse' {} a -> s {savingsPlans = a} :: DescribeSavingsPlansResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSavingsPlansResponse_httpStatus :: Lens.Lens' DescribeSavingsPlansResponse Prelude.Int
describeSavingsPlansResponse_httpStatus = Lens.lens (\DescribeSavingsPlansResponse' {httpStatus} -> httpStatus) (\s@DescribeSavingsPlansResponse' {} a -> s {httpStatus = a} :: DescribeSavingsPlansResponse)

instance Prelude.NFData DescribeSavingsPlansResponse where
  rnf DescribeSavingsPlansResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf savingsPlans
      `Prelude.seq` Prelude.rnf httpStatus
