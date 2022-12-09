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
    listCheckSummaries_maxResults,
    listCheckSummaries_nextToken,
    listCheckSummaries_workloadId,
    listCheckSummaries_lensArn,
    listCheckSummaries_pillarId,
    listCheckSummaries_questionId,
    listCheckSummaries_choiceId,

    -- * Destructuring the Response
    ListCheckSummariesResponse (..),
    newListCheckSummariesResponse,

    -- * Response Lenses
    listCheckSummariesResponse_checkSummaries,
    listCheckSummariesResponse_nextToken,
    listCheckSummariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newListCheckSummaries' smart constructor.
data ListCheckSummaries = ListCheckSummaries'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listCheckSummaries_maxResults' - Undocumented member.
--
-- 'nextToken', 'listCheckSummaries_nextToken' - Undocumented member.
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
      { maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        workloadId = pWorkloadId_,
        lensArn = pLensArn_,
        pillarId = pPillarId_,
        questionId = pQuestionId_,
        choiceId = pChoiceId_
      }

-- | Undocumented member.
listCheckSummaries_maxResults :: Lens.Lens' ListCheckSummaries (Prelude.Maybe Prelude.Natural)
listCheckSummaries_maxResults = Lens.lens (\ListCheckSummaries' {maxResults} -> maxResults) (\s@ListCheckSummaries' {} a -> s {maxResults = a} :: ListCheckSummaries)

-- | Undocumented member.
listCheckSummaries_nextToken :: Lens.Lens' ListCheckSummaries (Prelude.Maybe Prelude.Text)
listCheckSummaries_nextToken = Lens.lens (\ListCheckSummaries' {nextToken} -> nextToken) (\s@ListCheckSummaries' {} a -> s {nextToken = a} :: ListCheckSummaries)

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
            Prelude.<$> (x Data..?> "CheckSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCheckSummaries where
  hashWithSalt _salt ListCheckSummaries' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` pillarId
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` choiceId

instance Prelude.NFData ListCheckSummaries where
  rnf ListCheckSummaries' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf pillarId
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf choiceId

instance Data.ToHeaders ListCheckSummaries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCheckSummaries where
  toJSON ListCheckSummaries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("LensArn" Data..= lensArn),
            Prelude.Just ("PillarId" Data..= pillarId),
            Prelude.Just ("QuestionId" Data..= questionId),
            Prelude.Just ("ChoiceId" Data..= choiceId)
          ]
      )

instance Data.ToPath ListCheckSummaries where
  toPath ListCheckSummaries' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/checkSummaries"
      ]

instance Data.ToQuery ListCheckSummaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCheckSummariesResponse' smart constructor.
data ListCheckSummariesResponse = ListCheckSummariesResponse'
  { -- | List of Trusted Advisor summaries related to the Well-Architected best
    -- practice.
    checkSummaries :: Prelude.Maybe [CheckSummary],
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'checkSummaries', 'listCheckSummariesResponse_checkSummaries' - List of Trusted Advisor summaries related to the Well-Architected best
-- practice.
--
-- 'nextToken', 'listCheckSummariesResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'listCheckSummariesResponse_httpStatus' - The response's http status code.
newListCheckSummariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCheckSummariesResponse
newListCheckSummariesResponse pHttpStatus_ =
  ListCheckSummariesResponse'
    { checkSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of Trusted Advisor summaries related to the Well-Architected best
-- practice.
listCheckSummariesResponse_checkSummaries :: Lens.Lens' ListCheckSummariesResponse (Prelude.Maybe [CheckSummary])
listCheckSummariesResponse_checkSummaries = Lens.lens (\ListCheckSummariesResponse' {checkSummaries} -> checkSummaries) (\s@ListCheckSummariesResponse' {} a -> s {checkSummaries = a} :: ListCheckSummariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listCheckSummariesResponse_nextToken :: Lens.Lens' ListCheckSummariesResponse (Prelude.Maybe Prelude.Text)
listCheckSummariesResponse_nextToken = Lens.lens (\ListCheckSummariesResponse' {nextToken} -> nextToken) (\s@ListCheckSummariesResponse' {} a -> s {nextToken = a} :: ListCheckSummariesResponse)

-- | The response's http status code.
listCheckSummariesResponse_httpStatus :: Lens.Lens' ListCheckSummariesResponse Prelude.Int
listCheckSummariesResponse_httpStatus = Lens.lens (\ListCheckSummariesResponse' {httpStatus} -> httpStatus) (\s@ListCheckSummariesResponse' {} a -> s {httpStatus = a} :: ListCheckSummariesResponse)

instance Prelude.NFData ListCheckSummariesResponse where
  rnf ListCheckSummariesResponse' {..} =
    Prelude.rnf checkSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
