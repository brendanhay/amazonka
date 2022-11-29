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
-- Module      : Amazonka.WellArchitected.GetAnswer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the answer to a specific question in a workload review.
module Amazonka.WellArchitected.GetAnswer
  ( -- * Creating a Request
    GetAnswer (..),
    newGetAnswer,

    -- * Request Lenses
    getAnswer_milestoneNumber,
    getAnswer_workloadId,
    getAnswer_lensAlias,
    getAnswer_questionId,

    -- * Destructuring the Response
    GetAnswerResponse (..),
    newGetAnswerResponse,

    -- * Response Lenses
    getAnswerResponse_lensArn,
    getAnswerResponse_lensAlias,
    getAnswerResponse_answer,
    getAnswerResponse_milestoneNumber,
    getAnswerResponse_workloadId,
    getAnswerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to get answer.
--
-- /See:/ 'newGetAnswer' smart constructor.
data GetAnswer = GetAnswer'
  { milestoneNumber :: Prelude.Maybe Prelude.Natural,
    workloadId :: Prelude.Text,
    lensAlias :: Prelude.Text,
    questionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnswer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'milestoneNumber', 'getAnswer_milestoneNumber' - Undocumented member.
--
-- 'workloadId', 'getAnswer_workloadId' - Undocumented member.
--
-- 'lensAlias', 'getAnswer_lensAlias' - Undocumented member.
--
-- 'questionId', 'getAnswer_questionId' - Undocumented member.
newGetAnswer ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'lensAlias'
  Prelude.Text ->
  -- | 'questionId'
  Prelude.Text ->
  GetAnswer
newGetAnswer pWorkloadId_ pLensAlias_ pQuestionId_ =
  GetAnswer'
    { milestoneNumber = Prelude.Nothing,
      workloadId = pWorkloadId_,
      lensAlias = pLensAlias_,
      questionId = pQuestionId_
    }

-- | Undocumented member.
getAnswer_milestoneNumber :: Lens.Lens' GetAnswer (Prelude.Maybe Prelude.Natural)
getAnswer_milestoneNumber = Lens.lens (\GetAnswer' {milestoneNumber} -> milestoneNumber) (\s@GetAnswer' {} a -> s {milestoneNumber = a} :: GetAnswer)

-- | Undocumented member.
getAnswer_workloadId :: Lens.Lens' GetAnswer Prelude.Text
getAnswer_workloadId = Lens.lens (\GetAnswer' {workloadId} -> workloadId) (\s@GetAnswer' {} a -> s {workloadId = a} :: GetAnswer)

-- | Undocumented member.
getAnswer_lensAlias :: Lens.Lens' GetAnswer Prelude.Text
getAnswer_lensAlias = Lens.lens (\GetAnswer' {lensAlias} -> lensAlias) (\s@GetAnswer' {} a -> s {lensAlias = a} :: GetAnswer)

-- | Undocumented member.
getAnswer_questionId :: Lens.Lens' GetAnswer Prelude.Text
getAnswer_questionId = Lens.lens (\GetAnswer' {questionId} -> questionId) (\s@GetAnswer' {} a -> s {questionId = a} :: GetAnswer)

instance Core.AWSRequest GetAnswer where
  type AWSResponse GetAnswer = GetAnswerResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnswerResponse'
            Prelude.<$> (x Core..?> "LensArn")
            Prelude.<*> (x Core..?> "LensAlias")
            Prelude.<*> (x Core..?> "Answer")
            Prelude.<*> (x Core..?> "MilestoneNumber")
            Prelude.<*> (x Core..?> "WorkloadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAnswer where
  hashWithSalt _salt GetAnswer' {..} =
    _salt `Prelude.hashWithSalt` milestoneNumber
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` questionId

instance Prelude.NFData GetAnswer where
  rnf GetAnswer' {..} =
    Prelude.rnf milestoneNumber
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf questionId

instance Core.ToHeaders GetAnswer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetAnswer where
  toPath GetAnswer' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Core.toBS workloadId,
        "/lensReviews/",
        Core.toBS lensAlias,
        "/answers/",
        Core.toBS questionId
      ]

instance Core.ToQuery GetAnswer where
  toQuery GetAnswer' {..} =
    Prelude.mconcat
      ["MilestoneNumber" Core.=: milestoneNumber]

-- | Output of a get answer call.
--
-- /See:/ 'newGetAnswerResponse' smart constructor.
data GetAnswerResponse = GetAnswerResponse'
  { -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    lensAlias :: Prelude.Maybe Prelude.Text,
    answer :: Prelude.Maybe Answer,
    milestoneNumber :: Prelude.Maybe Prelude.Natural,
    workloadId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnswerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensArn', 'getAnswerResponse_lensArn' - The ARN for the lens.
--
-- 'lensAlias', 'getAnswerResponse_lensAlias' - Undocumented member.
--
-- 'answer', 'getAnswerResponse_answer' - Undocumented member.
--
-- 'milestoneNumber', 'getAnswerResponse_milestoneNumber' - Undocumented member.
--
-- 'workloadId', 'getAnswerResponse_workloadId' - Undocumented member.
--
-- 'httpStatus', 'getAnswerResponse_httpStatus' - The response's http status code.
newGetAnswerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAnswerResponse
newGetAnswerResponse pHttpStatus_ =
  GetAnswerResponse'
    { lensArn = Prelude.Nothing,
      lensAlias = Prelude.Nothing,
      answer = Prelude.Nothing,
      milestoneNumber = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the lens.
getAnswerResponse_lensArn :: Lens.Lens' GetAnswerResponse (Prelude.Maybe Prelude.Text)
getAnswerResponse_lensArn = Lens.lens (\GetAnswerResponse' {lensArn} -> lensArn) (\s@GetAnswerResponse' {} a -> s {lensArn = a} :: GetAnswerResponse)

-- | Undocumented member.
getAnswerResponse_lensAlias :: Lens.Lens' GetAnswerResponse (Prelude.Maybe Prelude.Text)
getAnswerResponse_lensAlias = Lens.lens (\GetAnswerResponse' {lensAlias} -> lensAlias) (\s@GetAnswerResponse' {} a -> s {lensAlias = a} :: GetAnswerResponse)

-- | Undocumented member.
getAnswerResponse_answer :: Lens.Lens' GetAnswerResponse (Prelude.Maybe Answer)
getAnswerResponse_answer = Lens.lens (\GetAnswerResponse' {answer} -> answer) (\s@GetAnswerResponse' {} a -> s {answer = a} :: GetAnswerResponse)

-- | Undocumented member.
getAnswerResponse_milestoneNumber :: Lens.Lens' GetAnswerResponse (Prelude.Maybe Prelude.Natural)
getAnswerResponse_milestoneNumber = Lens.lens (\GetAnswerResponse' {milestoneNumber} -> milestoneNumber) (\s@GetAnswerResponse' {} a -> s {milestoneNumber = a} :: GetAnswerResponse)

-- | Undocumented member.
getAnswerResponse_workloadId :: Lens.Lens' GetAnswerResponse (Prelude.Maybe Prelude.Text)
getAnswerResponse_workloadId = Lens.lens (\GetAnswerResponse' {workloadId} -> workloadId) (\s@GetAnswerResponse' {} a -> s {workloadId = a} :: GetAnswerResponse)

-- | The response's http status code.
getAnswerResponse_httpStatus :: Lens.Lens' GetAnswerResponse Prelude.Int
getAnswerResponse_httpStatus = Lens.lens (\GetAnswerResponse' {httpStatus} -> httpStatus) (\s@GetAnswerResponse' {} a -> s {httpStatus = a} :: GetAnswerResponse)

instance Prelude.NFData GetAnswerResponse where
  rnf GetAnswerResponse' {..} =
    Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf answer
      `Prelude.seq` Prelude.rnf milestoneNumber
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf httpStatus
