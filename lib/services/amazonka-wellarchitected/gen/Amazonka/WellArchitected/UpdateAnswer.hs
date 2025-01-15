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
-- Module      : Amazonka.WellArchitected.UpdateAnswer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the answer to a specific question in a workload review.
module Amazonka.WellArchitected.UpdateAnswer
  ( -- * Creating a Request
    UpdateAnswer (..),
    newUpdateAnswer,

    -- * Request Lenses
    updateAnswer_choiceUpdates,
    updateAnswer_isApplicable,
    updateAnswer_notes,
    updateAnswer_reason,
    updateAnswer_selectedChoices,
    updateAnswer_workloadId,
    updateAnswer_lensAlias,
    updateAnswer_questionId,

    -- * Destructuring the Response
    UpdateAnswerResponse (..),
    newUpdateAnswerResponse,

    -- * Response Lenses
    updateAnswerResponse_answer,
    updateAnswerResponse_lensAlias,
    updateAnswerResponse_lensArn,
    updateAnswerResponse_workloadId,
    updateAnswerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to update answer.
--
-- /See:/ 'newUpdateAnswer' smart constructor.
data UpdateAnswer = UpdateAnswer'
  { -- | A list of choices to update on a question in your workload. The String
    -- key corresponds to the choice ID to be updated.
    choiceUpdates :: Prelude.Maybe (Prelude.HashMap Prelude.Text ChoiceUpdate),
    isApplicable :: Prelude.Maybe Prelude.Bool,
    notes :: Prelude.Maybe Prelude.Text,
    -- | The reason why a question is not applicable to your workload.
    reason :: Prelude.Maybe AnswerReason,
    selectedChoices :: Prelude.Maybe [Prelude.Text],
    workloadId :: Prelude.Text,
    lensAlias :: Prelude.Text,
    questionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAnswer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'choiceUpdates', 'updateAnswer_choiceUpdates' - A list of choices to update on a question in your workload. The String
-- key corresponds to the choice ID to be updated.
--
-- 'isApplicable', 'updateAnswer_isApplicable' - Undocumented member.
--
-- 'notes', 'updateAnswer_notes' - Undocumented member.
--
-- 'reason', 'updateAnswer_reason' - The reason why a question is not applicable to your workload.
--
-- 'selectedChoices', 'updateAnswer_selectedChoices' - Undocumented member.
--
-- 'workloadId', 'updateAnswer_workloadId' - Undocumented member.
--
-- 'lensAlias', 'updateAnswer_lensAlias' - Undocumented member.
--
-- 'questionId', 'updateAnswer_questionId' - Undocumented member.
newUpdateAnswer ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'lensAlias'
  Prelude.Text ->
  -- | 'questionId'
  Prelude.Text ->
  UpdateAnswer
newUpdateAnswer pWorkloadId_ pLensAlias_ pQuestionId_ =
  UpdateAnswer'
    { choiceUpdates = Prelude.Nothing,
      isApplicable = Prelude.Nothing,
      notes = Prelude.Nothing,
      reason = Prelude.Nothing,
      selectedChoices = Prelude.Nothing,
      workloadId = pWorkloadId_,
      lensAlias = pLensAlias_,
      questionId = pQuestionId_
    }

-- | A list of choices to update on a question in your workload. The String
-- key corresponds to the choice ID to be updated.
updateAnswer_choiceUpdates :: Lens.Lens' UpdateAnswer (Prelude.Maybe (Prelude.HashMap Prelude.Text ChoiceUpdate))
updateAnswer_choiceUpdates = Lens.lens (\UpdateAnswer' {choiceUpdates} -> choiceUpdates) (\s@UpdateAnswer' {} a -> s {choiceUpdates = a} :: UpdateAnswer) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateAnswer_isApplicable :: Lens.Lens' UpdateAnswer (Prelude.Maybe Prelude.Bool)
updateAnswer_isApplicable = Lens.lens (\UpdateAnswer' {isApplicable} -> isApplicable) (\s@UpdateAnswer' {} a -> s {isApplicable = a} :: UpdateAnswer)

-- | Undocumented member.
updateAnswer_notes :: Lens.Lens' UpdateAnswer (Prelude.Maybe Prelude.Text)
updateAnswer_notes = Lens.lens (\UpdateAnswer' {notes} -> notes) (\s@UpdateAnswer' {} a -> s {notes = a} :: UpdateAnswer)

-- | The reason why a question is not applicable to your workload.
updateAnswer_reason :: Lens.Lens' UpdateAnswer (Prelude.Maybe AnswerReason)
updateAnswer_reason = Lens.lens (\UpdateAnswer' {reason} -> reason) (\s@UpdateAnswer' {} a -> s {reason = a} :: UpdateAnswer)

-- | Undocumented member.
updateAnswer_selectedChoices :: Lens.Lens' UpdateAnswer (Prelude.Maybe [Prelude.Text])
updateAnswer_selectedChoices = Lens.lens (\UpdateAnswer' {selectedChoices} -> selectedChoices) (\s@UpdateAnswer' {} a -> s {selectedChoices = a} :: UpdateAnswer) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateAnswer_workloadId :: Lens.Lens' UpdateAnswer Prelude.Text
updateAnswer_workloadId = Lens.lens (\UpdateAnswer' {workloadId} -> workloadId) (\s@UpdateAnswer' {} a -> s {workloadId = a} :: UpdateAnswer)

-- | Undocumented member.
updateAnswer_lensAlias :: Lens.Lens' UpdateAnswer Prelude.Text
updateAnswer_lensAlias = Lens.lens (\UpdateAnswer' {lensAlias} -> lensAlias) (\s@UpdateAnswer' {} a -> s {lensAlias = a} :: UpdateAnswer)

-- | Undocumented member.
updateAnswer_questionId :: Lens.Lens' UpdateAnswer Prelude.Text
updateAnswer_questionId = Lens.lens (\UpdateAnswer' {questionId} -> questionId) (\s@UpdateAnswer' {} a -> s {questionId = a} :: UpdateAnswer)

instance Core.AWSRequest UpdateAnswer where
  type AWSResponse UpdateAnswer = UpdateAnswerResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAnswerResponse'
            Prelude.<$> (x Data..?> "Answer")
            Prelude.<*> (x Data..?> "LensAlias")
            Prelude.<*> (x Data..?> "LensArn")
            Prelude.<*> (x Data..?> "WorkloadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAnswer where
  hashWithSalt _salt UpdateAnswer' {..} =
    _salt
      `Prelude.hashWithSalt` choiceUpdates
      `Prelude.hashWithSalt` isApplicable
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` selectedChoices
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` questionId

instance Prelude.NFData UpdateAnswer where
  rnf UpdateAnswer' {..} =
    Prelude.rnf choiceUpdates `Prelude.seq`
      Prelude.rnf isApplicable `Prelude.seq`
        Prelude.rnf notes `Prelude.seq`
          Prelude.rnf reason `Prelude.seq`
            Prelude.rnf selectedChoices `Prelude.seq`
              Prelude.rnf workloadId `Prelude.seq`
                Prelude.rnf lensAlias `Prelude.seq`
                  Prelude.rnf questionId

instance Data.ToHeaders UpdateAnswer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAnswer where
  toJSON UpdateAnswer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChoiceUpdates" Data..=) Prelude.<$> choiceUpdates,
            ("IsApplicable" Data..=) Prelude.<$> isApplicable,
            ("Notes" Data..=) Prelude.<$> notes,
            ("Reason" Data..=) Prelude.<$> reason,
            ("SelectedChoices" Data..=)
              Prelude.<$> selectedChoices
          ]
      )

instance Data.ToPath UpdateAnswer where
  toPath UpdateAnswer' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/lensReviews/",
        Data.toBS lensAlias,
        "/answers/",
        Data.toBS questionId
      ]

instance Data.ToQuery UpdateAnswer where
  toQuery = Prelude.const Prelude.mempty

-- | Output of a update answer call.
--
-- /See:/ 'newUpdateAnswerResponse' smart constructor.
data UpdateAnswerResponse = UpdateAnswerResponse'
  { answer :: Prelude.Maybe Answer,
    lensAlias :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAnswerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'answer', 'updateAnswerResponse_answer' - Undocumented member.
--
-- 'lensAlias', 'updateAnswerResponse_lensAlias' - Undocumented member.
--
-- 'lensArn', 'updateAnswerResponse_lensArn' - The ARN for the lens.
--
-- 'workloadId', 'updateAnswerResponse_workloadId' - Undocumented member.
--
-- 'httpStatus', 'updateAnswerResponse_httpStatus' - The response's http status code.
newUpdateAnswerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAnswerResponse
newUpdateAnswerResponse pHttpStatus_ =
  UpdateAnswerResponse'
    { answer = Prelude.Nothing,
      lensAlias = Prelude.Nothing,
      lensArn = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateAnswerResponse_answer :: Lens.Lens' UpdateAnswerResponse (Prelude.Maybe Answer)
updateAnswerResponse_answer = Lens.lens (\UpdateAnswerResponse' {answer} -> answer) (\s@UpdateAnswerResponse' {} a -> s {answer = a} :: UpdateAnswerResponse)

-- | Undocumented member.
updateAnswerResponse_lensAlias :: Lens.Lens' UpdateAnswerResponse (Prelude.Maybe Prelude.Text)
updateAnswerResponse_lensAlias = Lens.lens (\UpdateAnswerResponse' {lensAlias} -> lensAlias) (\s@UpdateAnswerResponse' {} a -> s {lensAlias = a} :: UpdateAnswerResponse)

-- | The ARN for the lens.
updateAnswerResponse_lensArn :: Lens.Lens' UpdateAnswerResponse (Prelude.Maybe Prelude.Text)
updateAnswerResponse_lensArn = Lens.lens (\UpdateAnswerResponse' {lensArn} -> lensArn) (\s@UpdateAnswerResponse' {} a -> s {lensArn = a} :: UpdateAnswerResponse)

-- | Undocumented member.
updateAnswerResponse_workloadId :: Lens.Lens' UpdateAnswerResponse (Prelude.Maybe Prelude.Text)
updateAnswerResponse_workloadId = Lens.lens (\UpdateAnswerResponse' {workloadId} -> workloadId) (\s@UpdateAnswerResponse' {} a -> s {workloadId = a} :: UpdateAnswerResponse)

-- | The response's http status code.
updateAnswerResponse_httpStatus :: Lens.Lens' UpdateAnswerResponse Prelude.Int
updateAnswerResponse_httpStatus = Lens.lens (\UpdateAnswerResponse' {httpStatus} -> httpStatus) (\s@UpdateAnswerResponse' {} a -> s {httpStatus = a} :: UpdateAnswerResponse)

instance Prelude.NFData UpdateAnswerResponse where
  rnf UpdateAnswerResponse' {..} =
    Prelude.rnf answer `Prelude.seq`
      Prelude.rnf lensAlias `Prelude.seq`
        Prelude.rnf lensArn `Prelude.seq`
          Prelude.rnf workloadId `Prelude.seq`
            Prelude.rnf httpStatus
