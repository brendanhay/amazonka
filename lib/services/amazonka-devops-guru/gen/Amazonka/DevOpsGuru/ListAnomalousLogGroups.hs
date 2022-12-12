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
-- Module      : Amazonka.DevOpsGuru.ListAnomalousLogGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of log groups that contain log anomalies.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.ListAnomalousLogGroups
  ( -- * Creating a Request
    ListAnomalousLogGroups (..),
    newListAnomalousLogGroups,

    -- * Request Lenses
    listAnomalousLogGroups_maxResults,
    listAnomalousLogGroups_nextToken,
    listAnomalousLogGroups_insightId,

    -- * Destructuring the Response
    ListAnomalousLogGroupsResponse (..),
    newListAnomalousLogGroupsResponse,

    -- * Response Lenses
    listAnomalousLogGroupsResponse_nextToken,
    listAnomalousLogGroupsResponse_httpStatus,
    listAnomalousLogGroupsResponse_insightId,
    listAnomalousLogGroupsResponse_anomalousLogGroups,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAnomalousLogGroups' smart constructor.
data ListAnomalousLogGroups = ListAnomalousLogGroups'
  { -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the insight containing the log groups.
    insightId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomalousLogGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAnomalousLogGroups_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'listAnomalousLogGroups_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'insightId', 'listAnomalousLogGroups_insightId' - The ID of the insight containing the log groups.
newListAnomalousLogGroups ::
  -- | 'insightId'
  Prelude.Text ->
  ListAnomalousLogGroups
newListAnomalousLogGroups pInsightId_ =
  ListAnomalousLogGroups'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      insightId = pInsightId_
    }

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listAnomalousLogGroups_maxResults :: Lens.Lens' ListAnomalousLogGroups (Prelude.Maybe Prelude.Natural)
listAnomalousLogGroups_maxResults = Lens.lens (\ListAnomalousLogGroups' {maxResults} -> maxResults) (\s@ListAnomalousLogGroups' {} a -> s {maxResults = a} :: ListAnomalousLogGroups)

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listAnomalousLogGroups_nextToken :: Lens.Lens' ListAnomalousLogGroups (Prelude.Maybe Prelude.Text)
listAnomalousLogGroups_nextToken = Lens.lens (\ListAnomalousLogGroups' {nextToken} -> nextToken) (\s@ListAnomalousLogGroups' {} a -> s {nextToken = a} :: ListAnomalousLogGroups)

-- | The ID of the insight containing the log groups.
listAnomalousLogGroups_insightId :: Lens.Lens' ListAnomalousLogGroups Prelude.Text
listAnomalousLogGroups_insightId = Lens.lens (\ListAnomalousLogGroups' {insightId} -> insightId) (\s@ListAnomalousLogGroups' {} a -> s {insightId = a} :: ListAnomalousLogGroups)

instance Core.AWSPager ListAnomalousLogGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAnomalousLogGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAnomalousLogGroupsResponse_anomalousLogGroups
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAnomalousLogGroups_nextToken
          Lens..~ rs
          Lens.^? listAnomalousLogGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAnomalousLogGroups where
  type
    AWSResponse ListAnomalousLogGroups =
      ListAnomalousLogGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAnomalousLogGroupsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "InsightId")
            Prelude.<*> ( x Data..?> "AnomalousLogGroups"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAnomalousLogGroups where
  hashWithSalt _salt ListAnomalousLogGroups' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` insightId

instance Prelude.NFData ListAnomalousLogGroups where
  rnf ListAnomalousLogGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf insightId

instance Data.ToHeaders ListAnomalousLogGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAnomalousLogGroups where
  toJSON ListAnomalousLogGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("InsightId" Data..= insightId)
          ]
      )

instance Data.ToPath ListAnomalousLogGroups where
  toPath = Prelude.const "/list-log-anomalies"

instance Data.ToQuery ListAnomalousLogGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAnomalousLogGroupsResponse' smart constructor.
data ListAnomalousLogGroupsResponse = ListAnomalousLogGroupsResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the insight containing the log groups.
    insightId :: Prelude.Text,
    -- | The list of Amazon CloudWatch log groups that are related to an insight.
    anomalousLogGroups :: [AnomalousLogGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomalousLogGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAnomalousLogGroupsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'listAnomalousLogGroupsResponse_httpStatus' - The response's http status code.
--
-- 'insightId', 'listAnomalousLogGroupsResponse_insightId' - The ID of the insight containing the log groups.
--
-- 'anomalousLogGroups', 'listAnomalousLogGroupsResponse_anomalousLogGroups' - The list of Amazon CloudWatch log groups that are related to an insight.
newListAnomalousLogGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'insightId'
  Prelude.Text ->
  ListAnomalousLogGroupsResponse
newListAnomalousLogGroupsResponse
  pHttpStatus_
  pInsightId_ =
    ListAnomalousLogGroupsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        insightId = pInsightId_,
        anomalousLogGroups = Prelude.mempty
      }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listAnomalousLogGroupsResponse_nextToken :: Lens.Lens' ListAnomalousLogGroupsResponse (Prelude.Maybe Prelude.Text)
listAnomalousLogGroupsResponse_nextToken = Lens.lens (\ListAnomalousLogGroupsResponse' {nextToken} -> nextToken) (\s@ListAnomalousLogGroupsResponse' {} a -> s {nextToken = a} :: ListAnomalousLogGroupsResponse)

-- | The response's http status code.
listAnomalousLogGroupsResponse_httpStatus :: Lens.Lens' ListAnomalousLogGroupsResponse Prelude.Int
listAnomalousLogGroupsResponse_httpStatus = Lens.lens (\ListAnomalousLogGroupsResponse' {httpStatus} -> httpStatus) (\s@ListAnomalousLogGroupsResponse' {} a -> s {httpStatus = a} :: ListAnomalousLogGroupsResponse)

-- | The ID of the insight containing the log groups.
listAnomalousLogGroupsResponse_insightId :: Lens.Lens' ListAnomalousLogGroupsResponse Prelude.Text
listAnomalousLogGroupsResponse_insightId = Lens.lens (\ListAnomalousLogGroupsResponse' {insightId} -> insightId) (\s@ListAnomalousLogGroupsResponse' {} a -> s {insightId = a} :: ListAnomalousLogGroupsResponse)

-- | The list of Amazon CloudWatch log groups that are related to an insight.
listAnomalousLogGroupsResponse_anomalousLogGroups :: Lens.Lens' ListAnomalousLogGroupsResponse [AnomalousLogGroup]
listAnomalousLogGroupsResponse_anomalousLogGroups = Lens.lens (\ListAnomalousLogGroupsResponse' {anomalousLogGroups} -> anomalousLogGroups) (\s@ListAnomalousLogGroupsResponse' {} a -> s {anomalousLogGroups = a} :: ListAnomalousLogGroupsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAnomalousLogGroupsResponse
  where
  rnf ListAnomalousLogGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf insightId
      `Prelude.seq` Prelude.rnf anomalousLogGroups
