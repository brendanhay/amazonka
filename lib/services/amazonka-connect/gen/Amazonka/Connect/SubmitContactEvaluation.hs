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
-- Module      : Amazonka.Connect.SubmitContactEvaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a contact evaluation in the specified Amazon Connect instance.
-- Answers included in the request are merged with existing answers for the
-- given evaluation. If no answers or notes are passed, the evaluation is
-- submitted with the existing answers and notes. You can delete an answer
-- or note by passing an empty object (@{}@) to the question identifier.
--
-- If a contact evaluation is already in submitted state, this operation
-- will trigger a resubmission.
module Amazonka.Connect.SubmitContactEvaluation
  ( -- * Creating a Request
    SubmitContactEvaluation (..),
    newSubmitContactEvaluation,

    -- * Request Lenses
    submitContactEvaluation_answers,
    submitContactEvaluation_notes,
    submitContactEvaluation_instanceId,
    submitContactEvaluation_evaluationId,

    -- * Destructuring the Response
    SubmitContactEvaluationResponse (..),
    newSubmitContactEvaluationResponse,

    -- * Response Lenses
    submitContactEvaluationResponse_httpStatus,
    submitContactEvaluationResponse_evaluationId,
    submitContactEvaluationResponse_evaluationArn,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSubmitContactEvaluation' smart constructor.
data SubmitContactEvaluation = SubmitContactEvaluation'
  { -- | A map of question identifiers to answer value.
    answers :: Prelude.Maybe (Prelude.HashMap Prelude.Text EvaluationAnswerInput),
    -- | A map of question identifiers to note value.
    notes :: Prelude.Maybe (Prelude.HashMap Prelude.Text EvaluationNote),
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | A unique identifier for the contact evaluation.
    evaluationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubmitContactEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'answers', 'submitContactEvaluation_answers' - A map of question identifiers to answer value.
--
-- 'notes', 'submitContactEvaluation_notes' - A map of question identifiers to note value.
--
-- 'instanceId', 'submitContactEvaluation_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'evaluationId', 'submitContactEvaluation_evaluationId' - A unique identifier for the contact evaluation.
newSubmitContactEvaluation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'evaluationId'
  Prelude.Text ->
  SubmitContactEvaluation
newSubmitContactEvaluation
  pInstanceId_
  pEvaluationId_ =
    SubmitContactEvaluation'
      { answers = Prelude.Nothing,
        notes = Prelude.Nothing,
        instanceId = pInstanceId_,
        evaluationId = pEvaluationId_
      }

-- | A map of question identifiers to answer value.
submitContactEvaluation_answers :: Lens.Lens' SubmitContactEvaluation (Prelude.Maybe (Prelude.HashMap Prelude.Text EvaluationAnswerInput))
submitContactEvaluation_answers = Lens.lens (\SubmitContactEvaluation' {answers} -> answers) (\s@SubmitContactEvaluation' {} a -> s {answers = a} :: SubmitContactEvaluation) Prelude.. Lens.mapping Lens.coerced

-- | A map of question identifiers to note value.
submitContactEvaluation_notes :: Lens.Lens' SubmitContactEvaluation (Prelude.Maybe (Prelude.HashMap Prelude.Text EvaluationNote))
submitContactEvaluation_notes = Lens.lens (\SubmitContactEvaluation' {notes} -> notes) (\s@SubmitContactEvaluation' {} a -> s {notes = a} :: SubmitContactEvaluation) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
submitContactEvaluation_instanceId :: Lens.Lens' SubmitContactEvaluation Prelude.Text
submitContactEvaluation_instanceId = Lens.lens (\SubmitContactEvaluation' {instanceId} -> instanceId) (\s@SubmitContactEvaluation' {} a -> s {instanceId = a} :: SubmitContactEvaluation)

-- | A unique identifier for the contact evaluation.
submitContactEvaluation_evaluationId :: Lens.Lens' SubmitContactEvaluation Prelude.Text
submitContactEvaluation_evaluationId = Lens.lens (\SubmitContactEvaluation' {evaluationId} -> evaluationId) (\s@SubmitContactEvaluation' {} a -> s {evaluationId = a} :: SubmitContactEvaluation)

instance Core.AWSRequest SubmitContactEvaluation where
  type
    AWSResponse SubmitContactEvaluation =
      SubmitContactEvaluationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitContactEvaluationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EvaluationId")
            Prelude.<*> (x Data..:> "EvaluationArn")
      )

instance Prelude.Hashable SubmitContactEvaluation where
  hashWithSalt _salt SubmitContactEvaluation' {..} =
    _salt
      `Prelude.hashWithSalt` answers
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` evaluationId

instance Prelude.NFData SubmitContactEvaluation where
  rnf SubmitContactEvaluation' {..} =
    Prelude.rnf answers
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf evaluationId

instance Data.ToHeaders SubmitContactEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SubmitContactEvaluation where
  toJSON SubmitContactEvaluation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Answers" Data..=) Prelude.<$> answers,
            ("Notes" Data..=) Prelude.<$> notes
          ]
      )

instance Data.ToPath SubmitContactEvaluation where
  toPath SubmitContactEvaluation' {..} =
    Prelude.mconcat
      [ "/contact-evaluations/",
        Data.toBS instanceId,
        "/",
        Data.toBS evaluationId,
        "/submit"
      ]

instance Data.ToQuery SubmitContactEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSubmitContactEvaluationResponse' smart constructor.
data SubmitContactEvaluationResponse = SubmitContactEvaluationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique identifier for the contact evaluation.
    evaluationId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the contact evaluation resource.
    evaluationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubmitContactEvaluationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'submitContactEvaluationResponse_httpStatus' - The response's http status code.
--
-- 'evaluationId', 'submitContactEvaluationResponse_evaluationId' - A unique identifier for the contact evaluation.
--
-- 'evaluationArn', 'submitContactEvaluationResponse_evaluationArn' - The Amazon Resource Name (ARN) for the contact evaluation resource.
newSubmitContactEvaluationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'evaluationId'
  Prelude.Text ->
  -- | 'evaluationArn'
  Prelude.Text ->
  SubmitContactEvaluationResponse
newSubmitContactEvaluationResponse
  pHttpStatus_
  pEvaluationId_
  pEvaluationArn_ =
    SubmitContactEvaluationResponse'
      { httpStatus =
          pHttpStatus_,
        evaluationId = pEvaluationId_,
        evaluationArn = pEvaluationArn_
      }

-- | The response's http status code.
submitContactEvaluationResponse_httpStatus :: Lens.Lens' SubmitContactEvaluationResponse Prelude.Int
submitContactEvaluationResponse_httpStatus = Lens.lens (\SubmitContactEvaluationResponse' {httpStatus} -> httpStatus) (\s@SubmitContactEvaluationResponse' {} a -> s {httpStatus = a} :: SubmitContactEvaluationResponse)

-- | A unique identifier for the contact evaluation.
submitContactEvaluationResponse_evaluationId :: Lens.Lens' SubmitContactEvaluationResponse Prelude.Text
submitContactEvaluationResponse_evaluationId = Lens.lens (\SubmitContactEvaluationResponse' {evaluationId} -> evaluationId) (\s@SubmitContactEvaluationResponse' {} a -> s {evaluationId = a} :: SubmitContactEvaluationResponse)

-- | The Amazon Resource Name (ARN) for the contact evaluation resource.
submitContactEvaluationResponse_evaluationArn :: Lens.Lens' SubmitContactEvaluationResponse Prelude.Text
submitContactEvaluationResponse_evaluationArn = Lens.lens (\SubmitContactEvaluationResponse' {evaluationArn} -> evaluationArn) (\s@SubmitContactEvaluationResponse' {} a -> s {evaluationArn = a} :: SubmitContactEvaluationResponse)

instance
  Prelude.NFData
    SubmitContactEvaluationResponse
  where
  rnf SubmitContactEvaluationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluationId
      `Prelude.seq` Prelude.rnf evaluationArn
