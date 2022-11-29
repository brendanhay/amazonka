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
-- Module      : Amazonka.WellArchitected.ListCheckSummaries
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List of Trusted Advisor checks summarized for all accounts related to
-- the workload.
module Amazonka.WellArchitected.ListCheckSummaries
  ( -- * Creating a Request
    ListCheckSummaries (..),
    newListCheckSummaries,

    -- * Request Lenses
    listCheckSummaries_nextToken,
    listCheckSummaries_maxResults,
    listCheckSummaries_workloadId,
    listCheckSummaries_lensArn,
    listCheckSummaries_pillarId,
    listCheckSummaries_questionId,
    listCheckSummaries_choiceId,

    -- * Destructuring the Response
    ListCheckSummariesResponse (..),
    newListCheckSummariesResponse,

    -- * Response Lenses
    listCheckSummariesResponse_nextToken,
    listCheckSummariesResponse_checkSummaries,
    listCheckSummariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newListCheckSummaries' smart constructor.
data ListCheckSummaries = ListCheckSummaries'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    workloadId :: Prelude.Text,
    -- | Well-Architected Lens ARN.
    lensArn :: Prelude.Text,
    pillarId :: Prelude.Text,
    questionId :: Prelude.Text,
    choiceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCheckSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCheckSummaries_nextToken' - Undocumented member.
--
-- 'maxResults', 'listCheckSummaries_maxResults' - Undocumented member.
--
-- 'workloadId', 'listCheckSummaries_workloadId' - Undocumented member.
--
-- 'lensArn', 'listCheckSummaries_lensArn' - Well-Architected Lens ARN.
--
-- 'pillarId', 'listCheckSummaries_pillarId' - Undocumented member.
--
-- 'questionId', 'listCheckSummaries_questionId' - Undocumented member.
--
-- 'choiceId', 'listCheckSummaries_choiceId' - Undocumented member.
newListCheckSummaries ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'lensArn'
  Prelude.Text ->
  -- | 'pillarId'
  Prelude.Text ->
  -- | 'questionId'
  Prelude.Text ->
  -- | 'choiceId'
  Prelude.Text ->
  ListCheckSummaries
newListCheckSummaries
  pWorkloadId_
  pLensArn_
  pPillarId_
  pQuestionId_
  pChoiceId_ =
    ListCheckSummaries'
      { nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        workloadId = pWorkloadId_,
        lensArn = pLensArn_,
        pillarId = pPillarId_,
        questionId = pQuestionId_,
        choiceId = pChoiceId_
      }

-- | Undocumented member.
listCheckSummaries_nextToken :: Lens.Lens' ListCheckSummaries (Prelude.Maybe Prelude.Text)
listCheckSummaries_nextToken = Lens.lens (\ListCheckSummaries' {nextToken} -> nextToken) (\s@ListCheckSummaries' {} a -> s {nextToken = a} :: ListCheckSummaries)

-- | Undocumented member.
listCheckSummaries_maxResults :: Lens.Lens' ListCheckSummaries (Prelude.Maybe Prelude.Natural)
listCheckSummaries_maxResults = Lens.lens (\ListCheckSummaries' {maxResults} -> maxResults) (\s@ListCheckSummaries' {} a -> s {maxResults = a} :: ListCheckSummaries)

-- | Undocumented member.
listCheckSummaries_workloadId :: Lens.Lens' ListCheckSummaries Prelude.Text
listCheckSummaries_workloadId = Lens.lens (\ListCheckSummaries' {workloadId} -> workloadId) (\s@ListCheckSummaries' {} a -> s {workloadId = a} :: ListCheckSummaries)

-- | Well-Architected Lens ARN.
listCheckSummaries_lensArn :: Lens.Lens' ListCheckSummaries Prelude.Text
listCheckSummaries_lensArn = Lens.lens (\ListCheckSummaries' {lensArn} -> lensArn) (\s@ListCheckSummaries' {} a -> s {lensArn = a} :: ListCheckSummaries)

-- | Undocumented member.
listCheckSummaries_pillarId :: Lens.Lens' ListCheckSummaries Prelude.Text
listCheckSummaries_pillarId = Lens.lens (\ListCheckSummaries' {pillarId} -> pillarId) (\s@ListCheckSummaries' {} a -> s {pillarId = a} :: ListCheckSummaries)

-- | Undocumented member.
listCheckSummaries_questionId :: Lens.Lens' ListCheckSummaries Prelude.Text
listCheckSummaries_questionId = Lens.lens (\ListCheckSummaries' {questionId} -> questionId) (\s@ListCheckSummaries' {} a -> s {questionId = a} :: ListCheckSummaries)

-- | Undocumented member.
listCheckSummaries_choiceId :: Lens.Lens' ListCheckSummaries Prelude.Text
listCheckSummaries_choiceId = Lens.lens (\ListCheckSummaries' {choiceId} -> choiceId) (\s@ListCheckSummaries' {} a -> s {choiceId = a} :: ListCheckSummaries)

instance Core.AWSRequest ListCheckSummaries where
  type
    AWSResponse ListCheckSummaries =
      ListCheckSummariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCheckSummariesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "CheckSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCheckSummaries where
  hashWithSalt _salt ListCheckSummaries' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` pillarId
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` choiceId

instance Prelude.NFData ListCheckSummaries where
  rnf ListCheckSummaries' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf pillarId
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf choiceId

instance Core.ToHeaders ListCheckSummaries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCheckSummaries where
  toJSON ListCheckSummaries' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("LensArn" Core..= lensArn),
            Prelude.Just ("PillarId" Core..= pillarId),
            Prelude.Just ("QuestionId" Core..= questionId),
            Prelude.Just ("ChoiceId" Core..= choiceId)
          ]
      )

instance Core.ToPath ListCheckSummaries where
  toPath ListCheckSummaries' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Core.toBS workloadId,
        "/checkSummaries"
      ]

instance Core.ToQuery ListCheckSummaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCheckSummariesResponse' smart constructor.
data ListCheckSummariesResponse = ListCheckSummariesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of Trusted Advisor summaries related to the Well-Architected best
    -- practice.
    checkSummaries :: Prelude.Maybe [CheckSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCheckSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCheckSummariesResponse_nextToken' - Undocumented member.
--
-- 'checkSummaries', 'listCheckSummariesResponse_checkSummaries' - List of Trusted Advisor summaries related to the Well-Architected best
-- practice.
--
-- 'httpStatus', 'listCheckSummariesResponse_httpStatus' - The response's http status code.
newListCheckSummariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCheckSummariesResponse
newListCheckSummariesResponse pHttpStatus_ =
  ListCheckSummariesResponse'
    { nextToken =
        Prelude.Nothing,
      checkSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listCheckSummariesResponse_nextToken :: Lens.Lens' ListCheckSummariesResponse (Prelude.Maybe Prelude.Text)
listCheckSummariesResponse_nextToken = Lens.lens (\ListCheckSummariesResponse' {nextToken} -> nextToken) (\s@ListCheckSummariesResponse' {} a -> s {nextToken = a} :: ListCheckSummariesResponse)

-- | List of Trusted Advisor summaries related to the Well-Architected best
-- practice.
listCheckSummariesResponse_checkSummaries :: Lens.Lens' ListCheckSummariesResponse (Prelude.Maybe [CheckSummary])
listCheckSummariesResponse_checkSummaries = Lens.lens (\ListCheckSummariesResponse' {checkSummaries} -> checkSummaries) (\s@ListCheckSummariesResponse' {} a -> s {checkSummaries = a} :: ListCheckSummariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCheckSummariesResponse_httpStatus :: Lens.Lens' ListCheckSummariesResponse Prelude.Int
listCheckSummariesResponse_httpStatus = Lens.lens (\ListCheckSummariesResponse' {httpStatus} -> httpStatus) (\s@ListCheckSummariesResponse' {} a -> s {httpStatus = a} :: ListCheckSummariesResponse)

instance Prelude.NFData ListCheckSummariesResponse where
  rnf ListCheckSummariesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf checkSummaries
      `Prelude.seq` Prelude.rnf httpStatus
