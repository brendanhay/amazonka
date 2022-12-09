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
-- Module      : Amazonka.SageMaker.ListEdgeDeploymentPlans
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all edge deployment plans.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListEdgeDeploymentPlans
  ( -- * Creating a Request
    ListEdgeDeploymentPlans (..),
    newListEdgeDeploymentPlans,

    -- * Request Lenses
    listEdgeDeploymentPlans_creationTimeAfter,
    listEdgeDeploymentPlans_creationTimeBefore,
    listEdgeDeploymentPlans_deviceFleetNameContains,
    listEdgeDeploymentPlans_lastModifiedTimeAfter,
    listEdgeDeploymentPlans_lastModifiedTimeBefore,
    listEdgeDeploymentPlans_maxResults,
    listEdgeDeploymentPlans_nameContains,
    listEdgeDeploymentPlans_nextToken,
    listEdgeDeploymentPlans_sortBy,
    listEdgeDeploymentPlans_sortOrder,

    -- * Destructuring the Response
    ListEdgeDeploymentPlansResponse (..),
    newListEdgeDeploymentPlansResponse,

    -- * Response Lenses
    listEdgeDeploymentPlansResponse_nextToken,
    listEdgeDeploymentPlansResponse_httpStatus,
    listEdgeDeploymentPlansResponse_edgeDeploymentPlanSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListEdgeDeploymentPlans' smart constructor.
data ListEdgeDeploymentPlans = ListEdgeDeploymentPlans'
  { -- | Selects edge deployment plans created after this time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Selects edge deployment plans created before this time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Selects edge deployment plans with a device fleet name containing this
    -- name.
    deviceFleetNameContains :: Prelude.Maybe Prelude.Text,
    -- | Selects edge deployment plans that were last updated after this time.
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Selects edge deployment plans that were last updated before this time.
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of results to select (50 by default).
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Selects edge deployment plans with names containing this name.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The column by which to sort the edge deployment plans. Can be one of
    -- @NAME@, @DEVICEFLEETNAME@, @CREATIONTIME@, @LASTMODIFIEDTIME@.
    sortBy :: Prelude.Maybe ListEdgeDeploymentPlansSortBy,
    -- | The direction of the sorting (ascending or descending).
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEdgeDeploymentPlans' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listEdgeDeploymentPlans_creationTimeAfter' - Selects edge deployment plans created after this time.
--
-- 'creationTimeBefore', 'listEdgeDeploymentPlans_creationTimeBefore' - Selects edge deployment plans created before this time.
--
-- 'deviceFleetNameContains', 'listEdgeDeploymentPlans_deviceFleetNameContains' - Selects edge deployment plans with a device fleet name containing this
-- name.
--
-- 'lastModifiedTimeAfter', 'listEdgeDeploymentPlans_lastModifiedTimeAfter' - Selects edge deployment plans that were last updated after this time.
--
-- 'lastModifiedTimeBefore', 'listEdgeDeploymentPlans_lastModifiedTimeBefore' - Selects edge deployment plans that were last updated before this time.
--
-- 'maxResults', 'listEdgeDeploymentPlans_maxResults' - The maximum number of results to select (50 by default).
--
-- 'nameContains', 'listEdgeDeploymentPlans_nameContains' - Selects edge deployment plans with names containing this name.
--
-- 'nextToken', 'listEdgeDeploymentPlans_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'sortBy', 'listEdgeDeploymentPlans_sortBy' - The column by which to sort the edge deployment plans. Can be one of
-- @NAME@, @DEVICEFLEETNAME@, @CREATIONTIME@, @LASTMODIFIEDTIME@.
--
-- 'sortOrder', 'listEdgeDeploymentPlans_sortOrder' - The direction of the sorting (ascending or descending).
newListEdgeDeploymentPlans ::
  ListEdgeDeploymentPlans
newListEdgeDeploymentPlans =
  ListEdgeDeploymentPlans'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      deviceFleetNameContains = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | Selects edge deployment plans created after this time.
listEdgeDeploymentPlans_creationTimeAfter :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.UTCTime)
listEdgeDeploymentPlans_creationTimeAfter = Lens.lens (\ListEdgeDeploymentPlans' {creationTimeAfter} -> creationTimeAfter) (\s@ListEdgeDeploymentPlans' {} a -> s {creationTimeAfter = a} :: ListEdgeDeploymentPlans) Prelude.. Lens.mapping Data._Time

-- | Selects edge deployment plans created before this time.
listEdgeDeploymentPlans_creationTimeBefore :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.UTCTime)
listEdgeDeploymentPlans_creationTimeBefore = Lens.lens (\ListEdgeDeploymentPlans' {creationTimeBefore} -> creationTimeBefore) (\s@ListEdgeDeploymentPlans' {} a -> s {creationTimeBefore = a} :: ListEdgeDeploymentPlans) Prelude.. Lens.mapping Data._Time

-- | Selects edge deployment plans with a device fleet name containing this
-- name.
listEdgeDeploymentPlans_deviceFleetNameContains :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.Text)
listEdgeDeploymentPlans_deviceFleetNameContains = Lens.lens (\ListEdgeDeploymentPlans' {deviceFleetNameContains} -> deviceFleetNameContains) (\s@ListEdgeDeploymentPlans' {} a -> s {deviceFleetNameContains = a} :: ListEdgeDeploymentPlans)

-- | Selects edge deployment plans that were last updated after this time.
listEdgeDeploymentPlans_lastModifiedTimeAfter :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.UTCTime)
listEdgeDeploymentPlans_lastModifiedTimeAfter = Lens.lens (\ListEdgeDeploymentPlans' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListEdgeDeploymentPlans' {} a -> s {lastModifiedTimeAfter = a} :: ListEdgeDeploymentPlans) Prelude.. Lens.mapping Data._Time

-- | Selects edge deployment plans that were last updated before this time.
listEdgeDeploymentPlans_lastModifiedTimeBefore :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.UTCTime)
listEdgeDeploymentPlans_lastModifiedTimeBefore = Lens.lens (\ListEdgeDeploymentPlans' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListEdgeDeploymentPlans' {} a -> s {lastModifiedTimeBefore = a} :: ListEdgeDeploymentPlans) Prelude.. Lens.mapping Data._Time

-- | The maximum number of results to select (50 by default).
listEdgeDeploymentPlans_maxResults :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.Int)
listEdgeDeploymentPlans_maxResults = Lens.lens (\ListEdgeDeploymentPlans' {maxResults} -> maxResults) (\s@ListEdgeDeploymentPlans' {} a -> s {maxResults = a} :: ListEdgeDeploymentPlans)

-- | Selects edge deployment plans with names containing this name.
listEdgeDeploymentPlans_nameContains :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.Text)
listEdgeDeploymentPlans_nameContains = Lens.lens (\ListEdgeDeploymentPlans' {nameContains} -> nameContains) (\s@ListEdgeDeploymentPlans' {} a -> s {nameContains = a} :: ListEdgeDeploymentPlans)

-- | The response from the last list when returning a list large enough to
-- need tokening.
listEdgeDeploymentPlans_nextToken :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.Text)
listEdgeDeploymentPlans_nextToken = Lens.lens (\ListEdgeDeploymentPlans' {nextToken} -> nextToken) (\s@ListEdgeDeploymentPlans' {} a -> s {nextToken = a} :: ListEdgeDeploymentPlans)

-- | The column by which to sort the edge deployment plans. Can be one of
-- @NAME@, @DEVICEFLEETNAME@, @CREATIONTIME@, @LASTMODIFIEDTIME@.
listEdgeDeploymentPlans_sortBy :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe ListEdgeDeploymentPlansSortBy)
listEdgeDeploymentPlans_sortBy = Lens.lens (\ListEdgeDeploymentPlans' {sortBy} -> sortBy) (\s@ListEdgeDeploymentPlans' {} a -> s {sortBy = a} :: ListEdgeDeploymentPlans)

-- | The direction of the sorting (ascending or descending).
listEdgeDeploymentPlans_sortOrder :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe SortOrder)
listEdgeDeploymentPlans_sortOrder = Lens.lens (\ListEdgeDeploymentPlans' {sortOrder} -> sortOrder) (\s@ListEdgeDeploymentPlans' {} a -> s {sortOrder = a} :: ListEdgeDeploymentPlans)

instance Core.AWSPager ListEdgeDeploymentPlans where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEdgeDeploymentPlansResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEdgeDeploymentPlansResponse_edgeDeploymentPlanSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEdgeDeploymentPlans_nextToken
          Lens..~ rs
          Lens.^? listEdgeDeploymentPlansResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEdgeDeploymentPlans where
  type
    AWSResponse ListEdgeDeploymentPlans =
      ListEdgeDeploymentPlansResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEdgeDeploymentPlansResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "EdgeDeploymentPlanSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListEdgeDeploymentPlans where
  hashWithSalt _salt ListEdgeDeploymentPlans' {..} =
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` deviceFleetNameContains
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListEdgeDeploymentPlans where
  rnf ListEdgeDeploymentPlans' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf deviceFleetNameContains
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListEdgeDeploymentPlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListEdgeDeploymentPlans" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEdgeDeploymentPlans where
  toJSON ListEdgeDeploymentPlans' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("DeviceFleetNameContains" Data..=)
              Prelude.<$> deviceFleetNameContains,
            ("LastModifiedTimeAfter" Data..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Data..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListEdgeDeploymentPlans where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEdgeDeploymentPlans where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEdgeDeploymentPlansResponse' smart constructor.
data ListEdgeDeploymentPlansResponse = ListEdgeDeploymentPlansResponse'
  { -- | The token to use when calling the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of summaries of edge deployment plans.
    edgeDeploymentPlanSummaries :: [EdgeDeploymentPlanSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEdgeDeploymentPlansResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEdgeDeploymentPlansResponse_nextToken' - The token to use when calling the next page of results.
--
-- 'httpStatus', 'listEdgeDeploymentPlansResponse_httpStatus' - The response's http status code.
--
-- 'edgeDeploymentPlanSummaries', 'listEdgeDeploymentPlansResponse_edgeDeploymentPlanSummaries' - List of summaries of edge deployment plans.
newListEdgeDeploymentPlansResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEdgeDeploymentPlansResponse
newListEdgeDeploymentPlansResponse pHttpStatus_ =
  ListEdgeDeploymentPlansResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      edgeDeploymentPlanSummaries =
        Prelude.mempty
    }

-- | The token to use when calling the next page of results.
listEdgeDeploymentPlansResponse_nextToken :: Lens.Lens' ListEdgeDeploymentPlansResponse (Prelude.Maybe Prelude.Text)
listEdgeDeploymentPlansResponse_nextToken = Lens.lens (\ListEdgeDeploymentPlansResponse' {nextToken} -> nextToken) (\s@ListEdgeDeploymentPlansResponse' {} a -> s {nextToken = a} :: ListEdgeDeploymentPlansResponse)

-- | The response's http status code.
listEdgeDeploymentPlansResponse_httpStatus :: Lens.Lens' ListEdgeDeploymentPlansResponse Prelude.Int
listEdgeDeploymentPlansResponse_httpStatus = Lens.lens (\ListEdgeDeploymentPlansResponse' {httpStatus} -> httpStatus) (\s@ListEdgeDeploymentPlansResponse' {} a -> s {httpStatus = a} :: ListEdgeDeploymentPlansResponse)

-- | List of summaries of edge deployment plans.
listEdgeDeploymentPlansResponse_edgeDeploymentPlanSummaries :: Lens.Lens' ListEdgeDeploymentPlansResponse [EdgeDeploymentPlanSummary]
listEdgeDeploymentPlansResponse_edgeDeploymentPlanSummaries = Lens.lens (\ListEdgeDeploymentPlansResponse' {edgeDeploymentPlanSummaries} -> edgeDeploymentPlanSummaries) (\s@ListEdgeDeploymentPlansResponse' {} a -> s {edgeDeploymentPlanSummaries = a} :: ListEdgeDeploymentPlansResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListEdgeDeploymentPlansResponse
  where
  rnf ListEdgeDeploymentPlansResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf edgeDeploymentPlanSummaries
