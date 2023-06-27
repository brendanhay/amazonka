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
-- Module      : Amazonka.KendraRanking.ListRescoreExecutionPlans
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your rescore execution plans. A rescore execution plan is an
-- Amazon Kendra Intelligent Ranking resource used for provisioning the
-- @Rescore@ API.
module Amazonka.KendraRanking.ListRescoreExecutionPlans
  ( -- * Creating a Request
    ListRescoreExecutionPlans (..),
    newListRescoreExecutionPlans,

    -- * Request Lenses
    listRescoreExecutionPlans_maxResults,
    listRescoreExecutionPlans_nextToken,

    -- * Destructuring the Response
    ListRescoreExecutionPlansResponse (..),
    newListRescoreExecutionPlansResponse,

    -- * Response Lenses
    listRescoreExecutionPlansResponse_nextToken,
    listRescoreExecutionPlansResponse_summaryItems,
    listRescoreExecutionPlansResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KendraRanking.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRescoreExecutionPlans' smart constructor.
data ListRescoreExecutionPlans = ListRescoreExecutionPlans'
  { -- | The maximum number of rescore execution plans to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response is truncated, Amazon Kendra Intelligent Ranking returns
    -- a pagination token in the response. You can use this pagination token to
    -- retrieve the next set of rescore execution plans.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRescoreExecutionPlans' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRescoreExecutionPlans_maxResults' - The maximum number of rescore execution plans to return.
--
-- 'nextToken', 'listRescoreExecutionPlans_nextToken' - If the response is truncated, Amazon Kendra Intelligent Ranking returns
-- a pagination token in the response. You can use this pagination token to
-- retrieve the next set of rescore execution plans.
newListRescoreExecutionPlans ::
  ListRescoreExecutionPlans
newListRescoreExecutionPlans =
  ListRescoreExecutionPlans'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of rescore execution plans to return.
listRescoreExecutionPlans_maxResults :: Lens.Lens' ListRescoreExecutionPlans (Prelude.Maybe Prelude.Natural)
listRescoreExecutionPlans_maxResults = Lens.lens (\ListRescoreExecutionPlans' {maxResults} -> maxResults) (\s@ListRescoreExecutionPlans' {} a -> s {maxResults = a} :: ListRescoreExecutionPlans)

-- | If the response is truncated, Amazon Kendra Intelligent Ranking returns
-- a pagination token in the response. You can use this pagination token to
-- retrieve the next set of rescore execution plans.
listRescoreExecutionPlans_nextToken :: Lens.Lens' ListRescoreExecutionPlans (Prelude.Maybe Prelude.Text)
listRescoreExecutionPlans_nextToken = Lens.lens (\ListRescoreExecutionPlans' {nextToken} -> nextToken) (\s@ListRescoreExecutionPlans' {} a -> s {nextToken = a} :: ListRescoreExecutionPlans)

instance Core.AWSRequest ListRescoreExecutionPlans where
  type
    AWSResponse ListRescoreExecutionPlans =
      ListRescoreExecutionPlansResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRescoreExecutionPlansResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SummaryItems" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRescoreExecutionPlans where
  hashWithSalt _salt ListRescoreExecutionPlans' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListRescoreExecutionPlans where
  rnf ListRescoreExecutionPlans' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListRescoreExecutionPlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraRerankingFrontendService.ListRescoreExecutionPlans" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRescoreExecutionPlans where
  toJSON ListRescoreExecutionPlans' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListRescoreExecutionPlans where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRescoreExecutionPlans where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRescoreExecutionPlansResponse' smart constructor.
data ListRescoreExecutionPlansResponse = ListRescoreExecutionPlansResponse'
  { -- | If the response is truncated, Amazon Kendra Intelligent Ranking returns
    -- a pagination token in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of summary information for one or more rescore execution plans.
    summaryItems :: Prelude.Maybe [RescoreExecutionPlanSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRescoreExecutionPlansResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRescoreExecutionPlansResponse_nextToken' - If the response is truncated, Amazon Kendra Intelligent Ranking returns
-- a pagination token in the response.
--
-- 'summaryItems', 'listRescoreExecutionPlansResponse_summaryItems' - An array of summary information for one or more rescore execution plans.
--
-- 'httpStatus', 'listRescoreExecutionPlansResponse_httpStatus' - The response's http status code.
newListRescoreExecutionPlansResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRescoreExecutionPlansResponse
newListRescoreExecutionPlansResponse pHttpStatus_ =
  ListRescoreExecutionPlansResponse'
    { nextToken =
        Prelude.Nothing,
      summaryItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Kendra Intelligent Ranking returns
-- a pagination token in the response.
listRescoreExecutionPlansResponse_nextToken :: Lens.Lens' ListRescoreExecutionPlansResponse (Prelude.Maybe Prelude.Text)
listRescoreExecutionPlansResponse_nextToken = Lens.lens (\ListRescoreExecutionPlansResponse' {nextToken} -> nextToken) (\s@ListRescoreExecutionPlansResponse' {} a -> s {nextToken = a} :: ListRescoreExecutionPlansResponse)

-- | An array of summary information for one or more rescore execution plans.
listRescoreExecutionPlansResponse_summaryItems :: Lens.Lens' ListRescoreExecutionPlansResponse (Prelude.Maybe [RescoreExecutionPlanSummary])
listRescoreExecutionPlansResponse_summaryItems = Lens.lens (\ListRescoreExecutionPlansResponse' {summaryItems} -> summaryItems) (\s@ListRescoreExecutionPlansResponse' {} a -> s {summaryItems = a} :: ListRescoreExecutionPlansResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRescoreExecutionPlansResponse_httpStatus :: Lens.Lens' ListRescoreExecutionPlansResponse Prelude.Int
listRescoreExecutionPlansResponse_httpStatus = Lens.lens (\ListRescoreExecutionPlansResponse' {httpStatus} -> httpStatus) (\s@ListRescoreExecutionPlansResponse' {} a -> s {httpStatus = a} :: ListRescoreExecutionPlansResponse)

instance
  Prelude.NFData
    ListRescoreExecutionPlansResponse
  where
  rnf ListRescoreExecutionPlansResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf summaryItems
      `Prelude.seq` Prelude.rnf httpStatus
