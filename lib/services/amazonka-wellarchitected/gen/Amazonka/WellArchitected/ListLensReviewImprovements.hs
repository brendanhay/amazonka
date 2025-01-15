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
-- Module      : Amazonka.WellArchitected.ListLensReviewImprovements
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List lens review improvements.
module Amazonka.WellArchitected.ListLensReviewImprovements
  ( -- * Creating a Request
    ListLensReviewImprovements (..),
    newListLensReviewImprovements,

    -- * Request Lenses
    listLensReviewImprovements_maxResults,
    listLensReviewImprovements_milestoneNumber,
    listLensReviewImprovements_nextToken,
    listLensReviewImprovements_pillarId,
    listLensReviewImprovements_workloadId,
    listLensReviewImprovements_lensAlias,

    -- * Destructuring the Response
    ListLensReviewImprovementsResponse (..),
    newListLensReviewImprovementsResponse,

    -- * Response Lenses
    listLensReviewImprovementsResponse_improvementSummaries,
    listLensReviewImprovementsResponse_lensAlias,
    listLensReviewImprovementsResponse_lensArn,
    listLensReviewImprovementsResponse_milestoneNumber,
    listLensReviewImprovementsResponse_nextToken,
    listLensReviewImprovementsResponse_workloadId,
    listLensReviewImprovementsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to list lens review improvements.
--
-- /See:/ 'newListLensReviewImprovements' smart constructor.
data ListLensReviewImprovements = ListLensReviewImprovements'
  { -- | The maximum number of results to return for this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    milestoneNumber :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    pillarId :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Text,
    lensAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLensReviewImprovements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLensReviewImprovements_maxResults' - The maximum number of results to return for this request.
--
-- 'milestoneNumber', 'listLensReviewImprovements_milestoneNumber' - Undocumented member.
--
-- 'nextToken', 'listLensReviewImprovements_nextToken' - Undocumented member.
--
-- 'pillarId', 'listLensReviewImprovements_pillarId' - Undocumented member.
--
-- 'workloadId', 'listLensReviewImprovements_workloadId' - Undocumented member.
--
-- 'lensAlias', 'listLensReviewImprovements_lensAlias' - Undocumented member.
newListLensReviewImprovements ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'lensAlias'
  Prelude.Text ->
  ListLensReviewImprovements
newListLensReviewImprovements
  pWorkloadId_
  pLensAlias_ =
    ListLensReviewImprovements'
      { maxResults =
          Prelude.Nothing,
        milestoneNumber = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        pillarId = Prelude.Nothing,
        workloadId = pWorkloadId_,
        lensAlias = pLensAlias_
      }

-- | The maximum number of results to return for this request.
listLensReviewImprovements_maxResults :: Lens.Lens' ListLensReviewImprovements (Prelude.Maybe Prelude.Natural)
listLensReviewImprovements_maxResults = Lens.lens (\ListLensReviewImprovements' {maxResults} -> maxResults) (\s@ListLensReviewImprovements' {} a -> s {maxResults = a} :: ListLensReviewImprovements)

-- | Undocumented member.
listLensReviewImprovements_milestoneNumber :: Lens.Lens' ListLensReviewImprovements (Prelude.Maybe Prelude.Natural)
listLensReviewImprovements_milestoneNumber = Lens.lens (\ListLensReviewImprovements' {milestoneNumber} -> milestoneNumber) (\s@ListLensReviewImprovements' {} a -> s {milestoneNumber = a} :: ListLensReviewImprovements)

-- | Undocumented member.
listLensReviewImprovements_nextToken :: Lens.Lens' ListLensReviewImprovements (Prelude.Maybe Prelude.Text)
listLensReviewImprovements_nextToken = Lens.lens (\ListLensReviewImprovements' {nextToken} -> nextToken) (\s@ListLensReviewImprovements' {} a -> s {nextToken = a} :: ListLensReviewImprovements)

-- | Undocumented member.
listLensReviewImprovements_pillarId :: Lens.Lens' ListLensReviewImprovements (Prelude.Maybe Prelude.Text)
listLensReviewImprovements_pillarId = Lens.lens (\ListLensReviewImprovements' {pillarId} -> pillarId) (\s@ListLensReviewImprovements' {} a -> s {pillarId = a} :: ListLensReviewImprovements)

-- | Undocumented member.
listLensReviewImprovements_workloadId :: Lens.Lens' ListLensReviewImprovements Prelude.Text
listLensReviewImprovements_workloadId = Lens.lens (\ListLensReviewImprovements' {workloadId} -> workloadId) (\s@ListLensReviewImprovements' {} a -> s {workloadId = a} :: ListLensReviewImprovements)

-- | Undocumented member.
listLensReviewImprovements_lensAlias :: Lens.Lens' ListLensReviewImprovements Prelude.Text
listLensReviewImprovements_lensAlias = Lens.lens (\ListLensReviewImprovements' {lensAlias} -> lensAlias) (\s@ListLensReviewImprovements' {} a -> s {lensAlias = a} :: ListLensReviewImprovements)

instance Core.AWSRequest ListLensReviewImprovements where
  type
    AWSResponse ListLensReviewImprovements =
      ListLensReviewImprovementsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLensReviewImprovementsResponse'
            Prelude.<$> ( x
                            Data..?> "ImprovementSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "LensAlias")
            Prelude.<*> (x Data..?> "LensArn")
            Prelude.<*> (x Data..?> "MilestoneNumber")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "WorkloadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLensReviewImprovements where
  hashWithSalt _salt ListLensReviewImprovements' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` milestoneNumber
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pillarId
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` lensAlias

instance Prelude.NFData ListLensReviewImprovements where
  rnf ListLensReviewImprovements' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf milestoneNumber `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf pillarId `Prelude.seq`
            Prelude.rnf workloadId `Prelude.seq`
              Prelude.rnf lensAlias

instance Data.ToHeaders ListLensReviewImprovements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListLensReviewImprovements where
  toPath ListLensReviewImprovements' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/lensReviews/",
        Data.toBS lensAlias,
        "/improvements"
      ]

instance Data.ToQuery ListLensReviewImprovements where
  toQuery ListLensReviewImprovements' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "MilestoneNumber" Data.=: milestoneNumber,
        "NextToken" Data.=: nextToken,
        "PillarId" Data.=: pillarId
      ]

-- | Output of a list lens review improvements call.
--
-- /See:/ 'newListLensReviewImprovementsResponse' smart constructor.
data ListLensReviewImprovementsResponse = ListLensReviewImprovementsResponse'
  { improvementSummaries :: Prelude.Maybe [ImprovementSummary],
    lensAlias :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    milestoneNumber :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLensReviewImprovementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'improvementSummaries', 'listLensReviewImprovementsResponse_improvementSummaries' - Undocumented member.
--
-- 'lensAlias', 'listLensReviewImprovementsResponse_lensAlias' - Undocumented member.
--
-- 'lensArn', 'listLensReviewImprovementsResponse_lensArn' - The ARN for the lens.
--
-- 'milestoneNumber', 'listLensReviewImprovementsResponse_milestoneNumber' - Undocumented member.
--
-- 'nextToken', 'listLensReviewImprovementsResponse_nextToken' - Undocumented member.
--
-- 'workloadId', 'listLensReviewImprovementsResponse_workloadId' - Undocumented member.
--
-- 'httpStatus', 'listLensReviewImprovementsResponse_httpStatus' - The response's http status code.
newListLensReviewImprovementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLensReviewImprovementsResponse
newListLensReviewImprovementsResponse pHttpStatus_ =
  ListLensReviewImprovementsResponse'
    { improvementSummaries =
        Prelude.Nothing,
      lensAlias = Prelude.Nothing,
      lensArn = Prelude.Nothing,
      milestoneNumber = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listLensReviewImprovementsResponse_improvementSummaries :: Lens.Lens' ListLensReviewImprovementsResponse (Prelude.Maybe [ImprovementSummary])
listLensReviewImprovementsResponse_improvementSummaries = Lens.lens (\ListLensReviewImprovementsResponse' {improvementSummaries} -> improvementSummaries) (\s@ListLensReviewImprovementsResponse' {} a -> s {improvementSummaries = a} :: ListLensReviewImprovementsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listLensReviewImprovementsResponse_lensAlias :: Lens.Lens' ListLensReviewImprovementsResponse (Prelude.Maybe Prelude.Text)
listLensReviewImprovementsResponse_lensAlias = Lens.lens (\ListLensReviewImprovementsResponse' {lensAlias} -> lensAlias) (\s@ListLensReviewImprovementsResponse' {} a -> s {lensAlias = a} :: ListLensReviewImprovementsResponse)

-- | The ARN for the lens.
listLensReviewImprovementsResponse_lensArn :: Lens.Lens' ListLensReviewImprovementsResponse (Prelude.Maybe Prelude.Text)
listLensReviewImprovementsResponse_lensArn = Lens.lens (\ListLensReviewImprovementsResponse' {lensArn} -> lensArn) (\s@ListLensReviewImprovementsResponse' {} a -> s {lensArn = a} :: ListLensReviewImprovementsResponse)

-- | Undocumented member.
listLensReviewImprovementsResponse_milestoneNumber :: Lens.Lens' ListLensReviewImprovementsResponse (Prelude.Maybe Prelude.Natural)
listLensReviewImprovementsResponse_milestoneNumber = Lens.lens (\ListLensReviewImprovementsResponse' {milestoneNumber} -> milestoneNumber) (\s@ListLensReviewImprovementsResponse' {} a -> s {milestoneNumber = a} :: ListLensReviewImprovementsResponse)

-- | Undocumented member.
listLensReviewImprovementsResponse_nextToken :: Lens.Lens' ListLensReviewImprovementsResponse (Prelude.Maybe Prelude.Text)
listLensReviewImprovementsResponse_nextToken = Lens.lens (\ListLensReviewImprovementsResponse' {nextToken} -> nextToken) (\s@ListLensReviewImprovementsResponse' {} a -> s {nextToken = a} :: ListLensReviewImprovementsResponse)

-- | Undocumented member.
listLensReviewImprovementsResponse_workloadId :: Lens.Lens' ListLensReviewImprovementsResponse (Prelude.Maybe Prelude.Text)
listLensReviewImprovementsResponse_workloadId = Lens.lens (\ListLensReviewImprovementsResponse' {workloadId} -> workloadId) (\s@ListLensReviewImprovementsResponse' {} a -> s {workloadId = a} :: ListLensReviewImprovementsResponse)

-- | The response's http status code.
listLensReviewImprovementsResponse_httpStatus :: Lens.Lens' ListLensReviewImprovementsResponse Prelude.Int
listLensReviewImprovementsResponse_httpStatus = Lens.lens (\ListLensReviewImprovementsResponse' {httpStatus} -> httpStatus) (\s@ListLensReviewImprovementsResponse' {} a -> s {httpStatus = a} :: ListLensReviewImprovementsResponse)

instance
  Prelude.NFData
    ListLensReviewImprovementsResponse
  where
  rnf ListLensReviewImprovementsResponse' {..} =
    Prelude.rnf improvementSummaries `Prelude.seq`
      Prelude.rnf lensAlias `Prelude.seq`
        Prelude.rnf lensArn `Prelude.seq`
          Prelude.rnf milestoneNumber `Prelude.seq`
            Prelude.rnf nextToken `Prelude.seq`
              Prelude.rnf workloadId `Prelude.seq`
                Prelude.rnf httpStatus
