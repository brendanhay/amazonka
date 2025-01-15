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
-- Module      : Amazonka.WellArchitected.ListAnswers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List of answers.
module Amazonka.WellArchitected.ListAnswers
  ( -- * Creating a Request
    ListAnswers (..),
    newListAnswers,

    -- * Request Lenses
    listAnswers_maxResults,
    listAnswers_milestoneNumber,
    listAnswers_nextToken,
    listAnswers_pillarId,
    listAnswers_workloadId,
    listAnswers_lensAlias,

    -- * Destructuring the Response
    ListAnswersResponse (..),
    newListAnswersResponse,

    -- * Response Lenses
    listAnswersResponse_answerSummaries,
    listAnswersResponse_lensAlias,
    listAnswersResponse_lensArn,
    listAnswersResponse_milestoneNumber,
    listAnswersResponse_nextToken,
    listAnswersResponse_workloadId,
    listAnswersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to list answers.
--
-- /See:/ 'newListAnswers' smart constructor.
data ListAnswers = ListAnswers'
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
-- Create a value of 'ListAnswers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAnswers_maxResults' - The maximum number of results to return for this request.
--
-- 'milestoneNumber', 'listAnswers_milestoneNumber' - Undocumented member.
--
-- 'nextToken', 'listAnswers_nextToken' - Undocumented member.
--
-- 'pillarId', 'listAnswers_pillarId' - Undocumented member.
--
-- 'workloadId', 'listAnswers_workloadId' - Undocumented member.
--
-- 'lensAlias', 'listAnswers_lensAlias' - Undocumented member.
newListAnswers ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'lensAlias'
  Prelude.Text ->
  ListAnswers
newListAnswers pWorkloadId_ pLensAlias_ =
  ListAnswers'
    { maxResults = Prelude.Nothing,
      milestoneNumber = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      pillarId = Prelude.Nothing,
      workloadId = pWorkloadId_,
      lensAlias = pLensAlias_
    }

-- | The maximum number of results to return for this request.
listAnswers_maxResults :: Lens.Lens' ListAnswers (Prelude.Maybe Prelude.Natural)
listAnswers_maxResults = Lens.lens (\ListAnswers' {maxResults} -> maxResults) (\s@ListAnswers' {} a -> s {maxResults = a} :: ListAnswers)

-- | Undocumented member.
listAnswers_milestoneNumber :: Lens.Lens' ListAnswers (Prelude.Maybe Prelude.Natural)
listAnswers_milestoneNumber = Lens.lens (\ListAnswers' {milestoneNumber} -> milestoneNumber) (\s@ListAnswers' {} a -> s {milestoneNumber = a} :: ListAnswers)

-- | Undocumented member.
listAnswers_nextToken :: Lens.Lens' ListAnswers (Prelude.Maybe Prelude.Text)
listAnswers_nextToken = Lens.lens (\ListAnswers' {nextToken} -> nextToken) (\s@ListAnswers' {} a -> s {nextToken = a} :: ListAnswers)

-- | Undocumented member.
listAnswers_pillarId :: Lens.Lens' ListAnswers (Prelude.Maybe Prelude.Text)
listAnswers_pillarId = Lens.lens (\ListAnswers' {pillarId} -> pillarId) (\s@ListAnswers' {} a -> s {pillarId = a} :: ListAnswers)

-- | Undocumented member.
listAnswers_workloadId :: Lens.Lens' ListAnswers Prelude.Text
listAnswers_workloadId = Lens.lens (\ListAnswers' {workloadId} -> workloadId) (\s@ListAnswers' {} a -> s {workloadId = a} :: ListAnswers)

-- | Undocumented member.
listAnswers_lensAlias :: Lens.Lens' ListAnswers Prelude.Text
listAnswers_lensAlias = Lens.lens (\ListAnswers' {lensAlias} -> lensAlias) (\s@ListAnswers' {} a -> s {lensAlias = a} :: ListAnswers)

instance Core.AWSRequest ListAnswers where
  type AWSResponse ListAnswers = ListAnswersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAnswersResponse'
            Prelude.<$> ( x
                            Data..?> "AnswerSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "LensAlias")
            Prelude.<*> (x Data..?> "LensArn")
            Prelude.<*> (x Data..?> "MilestoneNumber")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "WorkloadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAnswers where
  hashWithSalt _salt ListAnswers' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` milestoneNumber
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pillarId
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` lensAlias

instance Prelude.NFData ListAnswers where
  rnf ListAnswers' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf milestoneNumber `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf pillarId `Prelude.seq`
            Prelude.rnf workloadId `Prelude.seq`
              Prelude.rnf lensAlias

instance Data.ToHeaders ListAnswers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAnswers where
  toPath ListAnswers' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/lensReviews/",
        Data.toBS lensAlias,
        "/answers"
      ]

instance Data.ToQuery ListAnswers where
  toQuery ListAnswers' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "MilestoneNumber" Data.=: milestoneNumber,
        "NextToken" Data.=: nextToken,
        "PillarId" Data.=: pillarId
      ]

-- | Output of a list answers call.
--
-- /See:/ 'newListAnswersResponse' smart constructor.
data ListAnswersResponse = ListAnswersResponse'
  { answerSummaries :: Prelude.Maybe [AnswerSummary],
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
-- Create a value of 'ListAnswersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'answerSummaries', 'listAnswersResponse_answerSummaries' - Undocumented member.
--
-- 'lensAlias', 'listAnswersResponse_lensAlias' - Undocumented member.
--
-- 'lensArn', 'listAnswersResponse_lensArn' - The ARN for the lens.
--
-- 'milestoneNumber', 'listAnswersResponse_milestoneNumber' - Undocumented member.
--
-- 'nextToken', 'listAnswersResponse_nextToken' - Undocumented member.
--
-- 'workloadId', 'listAnswersResponse_workloadId' - Undocumented member.
--
-- 'httpStatus', 'listAnswersResponse_httpStatus' - The response's http status code.
newListAnswersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAnswersResponse
newListAnswersResponse pHttpStatus_ =
  ListAnswersResponse'
    { answerSummaries =
        Prelude.Nothing,
      lensAlias = Prelude.Nothing,
      lensArn = Prelude.Nothing,
      milestoneNumber = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listAnswersResponse_answerSummaries :: Lens.Lens' ListAnswersResponse (Prelude.Maybe [AnswerSummary])
listAnswersResponse_answerSummaries = Lens.lens (\ListAnswersResponse' {answerSummaries} -> answerSummaries) (\s@ListAnswersResponse' {} a -> s {answerSummaries = a} :: ListAnswersResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listAnswersResponse_lensAlias :: Lens.Lens' ListAnswersResponse (Prelude.Maybe Prelude.Text)
listAnswersResponse_lensAlias = Lens.lens (\ListAnswersResponse' {lensAlias} -> lensAlias) (\s@ListAnswersResponse' {} a -> s {lensAlias = a} :: ListAnswersResponse)

-- | The ARN for the lens.
listAnswersResponse_lensArn :: Lens.Lens' ListAnswersResponse (Prelude.Maybe Prelude.Text)
listAnswersResponse_lensArn = Lens.lens (\ListAnswersResponse' {lensArn} -> lensArn) (\s@ListAnswersResponse' {} a -> s {lensArn = a} :: ListAnswersResponse)

-- | Undocumented member.
listAnswersResponse_milestoneNumber :: Lens.Lens' ListAnswersResponse (Prelude.Maybe Prelude.Natural)
listAnswersResponse_milestoneNumber = Lens.lens (\ListAnswersResponse' {milestoneNumber} -> milestoneNumber) (\s@ListAnswersResponse' {} a -> s {milestoneNumber = a} :: ListAnswersResponse)

-- | Undocumented member.
listAnswersResponse_nextToken :: Lens.Lens' ListAnswersResponse (Prelude.Maybe Prelude.Text)
listAnswersResponse_nextToken = Lens.lens (\ListAnswersResponse' {nextToken} -> nextToken) (\s@ListAnswersResponse' {} a -> s {nextToken = a} :: ListAnswersResponse)

-- | Undocumented member.
listAnswersResponse_workloadId :: Lens.Lens' ListAnswersResponse (Prelude.Maybe Prelude.Text)
listAnswersResponse_workloadId = Lens.lens (\ListAnswersResponse' {workloadId} -> workloadId) (\s@ListAnswersResponse' {} a -> s {workloadId = a} :: ListAnswersResponse)

-- | The response's http status code.
listAnswersResponse_httpStatus :: Lens.Lens' ListAnswersResponse Prelude.Int
listAnswersResponse_httpStatus = Lens.lens (\ListAnswersResponse' {httpStatus} -> httpStatus) (\s@ListAnswersResponse' {} a -> s {httpStatus = a} :: ListAnswersResponse)

instance Prelude.NFData ListAnswersResponse where
  rnf ListAnswersResponse' {..} =
    Prelude.rnf answerSummaries `Prelude.seq`
      Prelude.rnf lensAlias `Prelude.seq`
        Prelude.rnf lensArn `Prelude.seq`
          Prelude.rnf milestoneNumber `Prelude.seq`
            Prelude.rnf nextToken `Prelude.seq`
              Prelude.rnf workloadId `Prelude.seq`
                Prelude.rnf httpStatus
