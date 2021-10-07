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
-- Module      : Network.AWS.DynamoDB.ListContributorInsights
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ContributorInsightsSummary for a table and all its
-- global secondary indexes.
module Network.AWS.DynamoDB.ListContributorInsights
  ( -- * Creating a Request
    ListContributorInsights (..),
    newListContributorInsights,

    -- * Request Lenses
    listContributorInsights_nextToken,
    listContributorInsights_tableName,
    listContributorInsights_maxResults,

    -- * Destructuring the Response
    ListContributorInsightsResponse (..),
    newListContributorInsightsResponse,

    -- * Response Lenses
    listContributorInsightsResponse_contributorInsightsSummaries,
    listContributorInsightsResponse_nextToken,
    listContributorInsightsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListContributorInsights' smart constructor.
data ListContributorInsights = ListContributorInsights'
  { -- | A token to for the desired page, if there is one.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the table.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'nextToken', 'listContributorInsights_nextToken' - A token to for the desired page, if there is one.
--
-- 'tableName', 'listContributorInsights_tableName' - The name of the table.
--
-- 'maxResults', 'listContributorInsights_maxResults' - Maximum number of results to return per page.
newListContributorInsights ::
  ListContributorInsights
newListContributorInsights =
  ListContributorInsights'
    { nextToken =
        Prelude.Nothing,
      tableName = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token to for the desired page, if there is one.
listContributorInsights_nextToken :: Lens.Lens' ListContributorInsights (Prelude.Maybe Prelude.Text)
listContributorInsights_nextToken = Lens.lens (\ListContributorInsights' {nextToken} -> nextToken) (\s@ListContributorInsights' {} a -> s {nextToken = a} :: ListContributorInsights)

-- | The name of the table.
listContributorInsights_tableName :: Lens.Lens' ListContributorInsights (Prelude.Maybe Prelude.Text)
listContributorInsights_tableName = Lens.lens (\ListContributorInsights' {tableName} -> tableName) (\s@ListContributorInsights' {} a -> s {tableName = a} :: ListContributorInsights)

-- | Maximum number of results to return per page.
listContributorInsights_maxResults :: Lens.Lens' ListContributorInsights (Prelude.Maybe Prelude.Int)
listContributorInsights_maxResults = Lens.lens (\ListContributorInsights' {maxResults} -> maxResults) (\s@ListContributorInsights' {} a -> s {maxResults = a} :: ListContributorInsights)

instance Core.AWSRequest ListContributorInsights where
  type
    AWSResponse ListContributorInsights =
      ListContributorInsightsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContributorInsightsResponse'
            Prelude.<$> ( x Core..?> "ContributorInsightsSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContributorInsights

instance Prelude.NFData ListContributorInsights

instance Core.ToHeaders ListContributorInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.ListContributorInsights" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListContributorInsights where
  toJSON ListContributorInsights' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("TableName" Core..=) Prelude.<$> tableName,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListContributorInsights where
  toPath = Prelude.const "/"

instance Core.ToQuery ListContributorInsights where
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
listContributorInsightsResponse_contributorInsightsSummaries = Lens.lens (\ListContributorInsightsResponse' {contributorInsightsSummaries} -> contributorInsightsSummaries) (\s@ListContributorInsightsResponse' {} a -> s {contributorInsightsSummaries = a} :: ListContributorInsightsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A token to go to the next page if there is one.
listContributorInsightsResponse_nextToken :: Lens.Lens' ListContributorInsightsResponse (Prelude.Maybe Prelude.Text)
listContributorInsightsResponse_nextToken = Lens.lens (\ListContributorInsightsResponse' {nextToken} -> nextToken) (\s@ListContributorInsightsResponse' {} a -> s {nextToken = a} :: ListContributorInsightsResponse)

-- | The response's http status code.
listContributorInsightsResponse_httpStatus :: Lens.Lens' ListContributorInsightsResponse Prelude.Int
listContributorInsightsResponse_httpStatus = Lens.lens (\ListContributorInsightsResponse' {httpStatus} -> httpStatus) (\s@ListContributorInsightsResponse' {} a -> s {httpStatus = a} :: ListContributorInsightsResponse)

instance
  Prelude.NFData
    ListContributorInsightsResponse
