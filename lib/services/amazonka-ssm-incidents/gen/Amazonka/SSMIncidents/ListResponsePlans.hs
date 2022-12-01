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
-- Module      : Amazonka.SSMIncidents.ListResponsePlans
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all response plans in your account.
--
-- This operation returns paginated results.
module Amazonka.SSMIncidents.ListResponsePlans
  ( -- * Creating a Request
    ListResponsePlans (..),
    newListResponsePlans,

    -- * Request Lenses
    listResponsePlans_nextToken,
    listResponsePlans_maxResults,

    -- * Destructuring the Response
    ListResponsePlansResponse (..),
    newListResponsePlansResponse,

    -- * Response Lenses
    listResponsePlansResponse_nextToken,
    listResponsePlansResponse_httpStatus,
    listResponsePlansResponse_responsePlanSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newListResponsePlans' smart constructor.
data ListResponsePlans = ListResponsePlans'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response plans per page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResponsePlans' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResponsePlans_nextToken' - The pagination token to continue to the next page of results.
--
-- 'maxResults', 'listResponsePlans_maxResults' - The maximum number of response plans per page.
newListResponsePlans ::
  ListResponsePlans
newListResponsePlans =
  ListResponsePlans'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token to continue to the next page of results.
listResponsePlans_nextToken :: Lens.Lens' ListResponsePlans (Prelude.Maybe Prelude.Text)
listResponsePlans_nextToken = Lens.lens (\ListResponsePlans' {nextToken} -> nextToken) (\s@ListResponsePlans' {} a -> s {nextToken = a} :: ListResponsePlans)

-- | The maximum number of response plans per page.
listResponsePlans_maxResults :: Lens.Lens' ListResponsePlans (Prelude.Maybe Prelude.Natural)
listResponsePlans_maxResults = Lens.lens (\ListResponsePlans' {maxResults} -> maxResults) (\s@ListResponsePlans' {} a -> s {maxResults = a} :: ListResponsePlans)

instance Core.AWSPager ListResponsePlans where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResponsePlansResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listResponsePlansResponse_responsePlanSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResponsePlans_nextToken
          Lens..~ rs
          Lens.^? listResponsePlansResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListResponsePlans where
  type
    AWSResponse ListResponsePlans =
      ListResponsePlansResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResponsePlansResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "responsePlanSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListResponsePlans where
  hashWithSalt _salt ListResponsePlans' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListResponsePlans where
  rnf ListResponsePlans' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListResponsePlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResponsePlans where
  toJSON ListResponsePlans' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListResponsePlans where
  toPath = Prelude.const "/listResponsePlans"

instance Core.ToQuery ListResponsePlans where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResponsePlansResponse' smart constructor.
data ListResponsePlansResponse = ListResponsePlansResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Details of each response plan.
    responsePlanSummaries :: [ResponsePlanSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResponsePlansResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResponsePlansResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'httpStatus', 'listResponsePlansResponse_httpStatus' - The response's http status code.
--
-- 'responsePlanSummaries', 'listResponsePlansResponse_responsePlanSummaries' - Details of each response plan.
newListResponsePlansResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResponsePlansResponse
newListResponsePlansResponse pHttpStatus_ =
  ListResponsePlansResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      responsePlanSummaries = Prelude.mempty
    }

-- | The pagination token to continue to the next page of results.
listResponsePlansResponse_nextToken :: Lens.Lens' ListResponsePlansResponse (Prelude.Maybe Prelude.Text)
listResponsePlansResponse_nextToken = Lens.lens (\ListResponsePlansResponse' {nextToken} -> nextToken) (\s@ListResponsePlansResponse' {} a -> s {nextToken = a} :: ListResponsePlansResponse)

-- | The response's http status code.
listResponsePlansResponse_httpStatus :: Lens.Lens' ListResponsePlansResponse Prelude.Int
listResponsePlansResponse_httpStatus = Lens.lens (\ListResponsePlansResponse' {httpStatus} -> httpStatus) (\s@ListResponsePlansResponse' {} a -> s {httpStatus = a} :: ListResponsePlansResponse)

-- | Details of each response plan.
listResponsePlansResponse_responsePlanSummaries :: Lens.Lens' ListResponsePlansResponse [ResponsePlanSummary]
listResponsePlansResponse_responsePlanSummaries = Lens.lens (\ListResponsePlansResponse' {responsePlanSummaries} -> responsePlanSummaries) (\s@ListResponsePlansResponse' {} a -> s {responsePlanSummaries = a} :: ListResponsePlansResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListResponsePlansResponse where
  rnf ListResponsePlansResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf responsePlanSummaries
