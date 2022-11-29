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
    listEdgeDeploymentPlans_sortOrder,
    listEdgeDeploymentPlans_nextToken,
    listEdgeDeploymentPlans_lastModifiedTimeAfter,
    listEdgeDeploymentPlans_nameContains,
    listEdgeDeploymentPlans_lastModifiedTimeBefore,
    listEdgeDeploymentPlans_creationTimeBefore,
    listEdgeDeploymentPlans_sortBy,
    listEdgeDeploymentPlans_maxResults,
    listEdgeDeploymentPlans_deviceFleetNameContains,
    listEdgeDeploymentPlans_creationTimeAfter,

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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListEdgeDeploymentPlans' smart constructor.
data ListEdgeDeploymentPlans = ListEdgeDeploymentPlans'
  { -- | The direction of the sorting (ascending or descending).
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Selects edge deployment plans that were last updated after this time.
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | Selects edge deployment plans with names containing this name.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | Selects edge deployment plans that were last updated before this time.
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | Selects edge deployment plans created before this time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The column by which to sort the edge deployment plans. Can be one of
    -- @NAME@, @DEVICEFLEETNAME@, @CREATIONTIME@, @LASTMODIFIEDTIME@.
    sortBy :: Prelude.Maybe ListEdgeDeploymentPlansSortBy,
    -- | The maximum number of results to select (50 by default).
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Selects edge deployment plans with a device fleet name containing this
    -- name.
    deviceFleetNameContains :: Prelude.Maybe Prelude.Text,
    -- | Selects edge deployment plans created after this time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX
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
-- 'sortOrder', 'listEdgeDeploymentPlans_sortOrder' - The direction of the sorting (ascending or descending).
--
-- 'nextToken', 'listEdgeDeploymentPlans_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'lastModifiedTimeAfter', 'listEdgeDeploymentPlans_lastModifiedTimeAfter' - Selects edge deployment plans that were last updated after this time.
--
-- 'nameContains', 'listEdgeDeploymentPlans_nameContains' - Selects edge deployment plans with names containing this name.
--
-- 'lastModifiedTimeBefore', 'listEdgeDeploymentPlans_lastModifiedTimeBefore' - Selects edge deployment plans that were last updated before this time.
--
-- 'creationTimeBefore', 'listEdgeDeploymentPlans_creationTimeBefore' - Selects edge deployment plans created before this time.
--
-- 'sortBy', 'listEdgeDeploymentPlans_sortBy' - The column by which to sort the edge deployment plans. Can be one of
-- @NAME@, @DEVICEFLEETNAME@, @CREATIONTIME@, @LASTMODIFIEDTIME@.
--
-- 'maxResults', 'listEdgeDeploymentPlans_maxResults' - The maximum number of results to select (50 by default).
--
-- 'deviceFleetNameContains', 'listEdgeDeploymentPlans_deviceFleetNameContains' - Selects edge deployment plans with a device fleet name containing this
-- name.
--
-- 'creationTimeAfter', 'listEdgeDeploymentPlans_creationTimeAfter' - Selects edge deployment plans created after this time.
newListEdgeDeploymentPlans ::
  ListEdgeDeploymentPlans
newListEdgeDeploymentPlans =
  ListEdgeDeploymentPlans'
    { sortOrder =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      deviceFleetNameContains = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | The direction of the sorting (ascending or descending).
listEdgeDeploymentPlans_sortOrder :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe SortOrder)
listEdgeDeploymentPlans_sortOrder = Lens.lens (\ListEdgeDeploymentPlans' {sortOrder} -> sortOrder) (\s@ListEdgeDeploymentPlans' {} a -> s {sortOrder = a} :: ListEdgeDeploymentPlans)

-- | The response from the last list when returning a list large enough to
-- need tokening.
listEdgeDeploymentPlans_nextToken :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.Text)
listEdgeDeploymentPlans_nextToken = Lens.lens (\ListEdgeDeploymentPlans' {nextToken} -> nextToken) (\s@ListEdgeDeploymentPlans' {} a -> s {nextToken = a} :: ListEdgeDeploymentPlans)

-- | Selects edge deployment plans that were last updated after this time.
listEdgeDeploymentPlans_lastModifiedTimeAfter :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.UTCTime)
listEdgeDeploymentPlans_lastModifiedTimeAfter = Lens.lens (\ListEdgeDeploymentPlans' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListEdgeDeploymentPlans' {} a -> s {lastModifiedTimeAfter = a} :: ListEdgeDeploymentPlans) Prelude.. Lens.mapping Core._Time

-- | Selects edge deployment plans with names containing this name.
listEdgeDeploymentPlans_nameContains :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.Text)
listEdgeDeploymentPlans_nameContains = Lens.lens (\ListEdgeDeploymentPlans' {nameContains} -> nameContains) (\s@ListEdgeDeploymentPlans' {} a -> s {nameContains = a} :: ListEdgeDeploymentPlans)

-- | Selects edge deployment plans that were last updated before this time.
listEdgeDeploymentPlans_lastModifiedTimeBefore :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.UTCTime)
listEdgeDeploymentPlans_lastModifiedTimeBefore = Lens.lens (\ListEdgeDeploymentPlans' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListEdgeDeploymentPlans' {} a -> s {lastModifiedTimeBefore = a} :: ListEdgeDeploymentPlans) Prelude.. Lens.mapping Core._Time

-- | Selects edge deployment plans created before this time.
listEdgeDeploymentPlans_creationTimeBefore :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.UTCTime)
listEdgeDeploymentPlans_creationTimeBefore = Lens.lens (\ListEdgeDeploymentPlans' {creationTimeBefore} -> creationTimeBefore) (\s@ListEdgeDeploymentPlans' {} a -> s {creationTimeBefore = a} :: ListEdgeDeploymentPlans) Prelude.. Lens.mapping Core._Time

-- | The column by which to sort the edge deployment plans. Can be one of
-- @NAME@, @DEVICEFLEETNAME@, @CREATIONTIME@, @LASTMODIFIEDTIME@.
listEdgeDeploymentPlans_sortBy :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe ListEdgeDeploymentPlansSortBy)
listEdgeDeploymentPlans_sortBy = Lens.lens (\ListEdgeDeploymentPlans' {sortBy} -> sortBy) (\s@ListEdgeDeploymentPlans' {} a -> s {sortBy = a} :: ListEdgeDeploymentPlans)

-- | The maximum number of results to select (50 by default).
listEdgeDeploymentPlans_maxResults :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.Int)
listEdgeDeploymentPlans_maxResults = Lens.lens (\ListEdgeDeploymentPlans' {maxResults} -> maxResults) (\s@ListEdgeDeploymentPlans' {} a -> s {maxResults = a} :: ListEdgeDeploymentPlans)

-- | Selects edge deployment plans with a device fleet name containing this
-- name.
listEdgeDeploymentPlans_deviceFleetNameContains :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.Text)
listEdgeDeploymentPlans_deviceFleetNameContains = Lens.lens (\ListEdgeDeploymentPlans' {deviceFleetNameContains} -> deviceFleetNameContains) (\s@ListEdgeDeploymentPlans' {} a -> s {deviceFleetNameContains = a} :: ListEdgeDeploymentPlans)

-- | Selects edge deployment plans created after this time.
listEdgeDeploymentPlans_creationTimeAfter :: Lens.Lens' ListEdgeDeploymentPlans (Prelude.Maybe Prelude.UTCTime)
listEdgeDeploymentPlans_creationTimeAfter = Lens.lens (\ListEdgeDeploymentPlans' {creationTimeAfter} -> creationTimeAfter) (\s@ListEdgeDeploymentPlans' {} a -> s {creationTimeAfter = a} :: ListEdgeDeploymentPlans) Prelude.. Lens.mapping Core._Time

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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "EdgeDeploymentPlanSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListEdgeDeploymentPlans where
  hashWithSalt _salt ListEdgeDeploymentPlans' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` deviceFleetNameContains
      `Prelude.hashWithSalt` creationTimeAfter

instance Prelude.NFData ListEdgeDeploymentPlans where
  rnf ListEdgeDeploymentPlans' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf deviceFleetNameContains
      `Prelude.seq` Prelude.rnf creationTimeAfter

instance Core.ToHeaders ListEdgeDeploymentPlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListEdgeDeploymentPlans" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListEdgeDeploymentPlans where
  toJSON ListEdgeDeploymentPlans' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("DeviceFleetNameContains" Core..=)
              Prelude.<$> deviceFleetNameContains,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListEdgeDeploymentPlans where
  toPath = Prelude.const "/"

instance Core.ToQuery ListEdgeDeploymentPlans where
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
