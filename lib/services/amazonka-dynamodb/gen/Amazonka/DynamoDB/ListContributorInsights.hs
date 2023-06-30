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
-- Module      : Amazonka.DynamoDB.ListContributorInsights
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ContributorInsightsSummary for a table and all its
-- global secondary indexes.
module Amazonka.DynamoDB.ListContributorInsights
  ( -- * Creating a Request
    ListContributorInsights (..),
    newListContributorInsights,

    -- * Request Lenses
    listContributorInsights_maxResults,
    listContributorInsights_nextToken,
    listContributorInsights_tableName,

    -- * Destructuring the Response
    ListContributorInsightsResponse (..),
    newListContributorInsightsResponse,

    -- * Response Lenses
    listContributorInsightsResponse_contributorInsightsSummaries,
    listContributorInsightsResponse_nextToken,
    listContributorInsightsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListContributorInsights' smart constructor.
data ListContributorInsights = ListContributorInsights'
  { -- | Maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A token to for the desired page, if there is one.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the table.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContributorInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listContributorInsights_maxResults' - Maximum number of results to return per page.
--
-- 'nextToken', 'listContributorInsights_nextToken' - A token to for the desired page, if there is one.
--
-- 'tableName', 'listContributorInsights_tableName' - The name of the table.
newListContributorInsights ::
  ListContributorInsights
newListContributorInsights =
  ListContributorInsights'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | Maximum number of results to return per page.
listContributorInsights_maxResults :: Lens.Lens' ListContributorInsights (Prelude.Maybe Prelude.Int)
listContributorInsights_maxResults = Lens.lens (\ListContributorInsights' {maxResults} -> maxResults) (\s@ListContributorInsights' {} a -> s {maxResults = a} :: ListContributorInsights)

-- | A token to for the desired page, if there is one.
listContributorInsights_nextToken :: Lens.Lens' ListContributorInsights (Prelude.Maybe Prelude.Text)
listContributorInsights_nextToken = Lens.lens (\ListContributorInsights' {nextToken} -> nextToken) (\s@ListContributorInsights' {} a -> s {nextToken = a} :: ListContributorInsights)

-- | The name of the table.
listContributorInsights_tableName :: Lens.Lens' ListContributorInsights (Prelude.Maybe Prelude.Text)
listContributorInsights_tableName = Lens.lens (\ListContributorInsights' {tableName} -> tableName) (\s@ListContributorInsights' {} a -> s {tableName = a} :: ListContributorInsights)

instance Core.AWSRequest ListContributorInsights where
  type
    AWSResponse ListContributorInsights =
      ListContributorInsightsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContributorInsightsResponse'
            Prelude.<$> ( x
                            Data..?> "ContributorInsightsSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContributorInsights where
  hashWithSalt _salt ListContributorInsights' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData ListContributorInsights where
  rnf ListContributorInsights' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToHeaders ListContributorInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.ListContributorInsights" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListContributorInsights where
  toJSON ListContributorInsights' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("TableName" Data..=) Prelude.<$> tableName
          ]
      )

instance Data.ToPath ListContributorInsights where
  toPath = Prelude.const "/"

instance Data.ToQuery ListContributorInsights where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListContributorInsightsResponse' smart constructor.
data ListContributorInsightsResponse = ListContributorInsightsResponse'
  { -- | A list of ContributorInsightsSummary.
    contributorInsightsSummaries :: Prelude.Maybe [ContributorInsightsSummary],
    -- | A token to go to the next page if there is one.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContributorInsightsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contributorInsightsSummaries', 'listContributorInsightsResponse_contributorInsightsSummaries' - A list of ContributorInsightsSummary.
--
-- 'nextToken', 'listContributorInsightsResponse_nextToken' - A token to go to the next page if there is one.
--
-- 'httpStatus', 'listContributorInsightsResponse_httpStatus' - The response's http status code.
newListContributorInsightsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContributorInsightsResponse
newListContributorInsightsResponse pHttpStatus_ =
  ListContributorInsightsResponse'
    { contributorInsightsSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ContributorInsightsSummary.
listContributorInsightsResponse_contributorInsightsSummaries :: Lens.Lens' ListContributorInsightsResponse (Prelude.Maybe [ContributorInsightsSummary])
listContributorInsightsResponse_contributorInsightsSummaries = Lens.lens (\ListContributorInsightsResponse' {contributorInsightsSummaries} -> contributorInsightsSummaries) (\s@ListContributorInsightsResponse' {} a -> s {contributorInsightsSummaries = a} :: ListContributorInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token to go to the next page if there is one.
listContributorInsightsResponse_nextToken :: Lens.Lens' ListContributorInsightsResponse (Prelude.Maybe Prelude.Text)
listContributorInsightsResponse_nextToken = Lens.lens (\ListContributorInsightsResponse' {nextToken} -> nextToken) (\s@ListContributorInsightsResponse' {} a -> s {nextToken = a} :: ListContributorInsightsResponse)

-- | The response's http status code.
listContributorInsightsResponse_httpStatus :: Lens.Lens' ListContributorInsightsResponse Prelude.Int
listContributorInsightsResponse_httpStatus = Lens.lens (\ListContributorInsightsResponse' {httpStatus} -> httpStatus) (\s@ListContributorInsightsResponse' {} a -> s {httpStatus = a} :: ListContributorInsightsResponse)

instance
  Prelude.NFData
    ListContributorInsightsResponse
  where
  rnf ListContributorInsightsResponse' {..} =
    Prelude.rnf contributorInsightsSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
