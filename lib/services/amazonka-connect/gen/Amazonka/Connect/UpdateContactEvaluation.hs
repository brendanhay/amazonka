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
-- Module      : Amazonka.Connect.UpdateContactEvaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates details about a contact evaluation in the specified Amazon
-- Connect instance. A contact evaluation must be in draft state. Answers
-- included in the request are merged with existing answers for the given
-- evaluation. An answer or note can be deleted by passing an empty object
-- (@{}@) to the question identifier.
module Amazonka.Connect.UpdateContactEvaluation
  ( -- * Creating a Request
    UpdateContactEvaluation (..),
    newUpdateContactEvaluation,

    -- * Request Lenses
    updateContactEvaluation_answers,
    updateContactEvaluation_notes,
    updateContactEvaluation_instanceId,
    updateContactEvaluation_evaluationId,

    -- * Destructuring the Response
    UpdateContactEvaluationResponse (..),
    newUpdateContactEvaluationResponse,

    -- * Response Lenses
    updateContactEvaluationResponse_httpStatus,
    updateContactEvaluationResponse_evaluationId,
    updateContactEvaluationResponse_evaluationArn,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContactEvaluation' smart constructor.
data UpdateContactEvaluation = UpdateContactEvaluation'
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
-- Create a value of 'UpdateContactEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'answers', 'updateContactEvaluation_answers' - A map of question identifiers to answer value.
--
-- 'notes', 'updateContactEvaluation_notes' - A map of question identifiers to note value.
--
-- 'instanceId', 'updateContactEvaluation_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'evaluationId', 'updateContactEvaluation_evaluationId' - A unique identifier for the contact evaluation.
newUpdateContactEvaluation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'evaluationId'
  Prelude.Text ->
  UpdateContactEvaluation
newUpdateContactEvaluation
  pInstanceId_
  pEvaluationId_ =
    UpdateContactEvaluation'
      { answers = Prelude.Nothing,
        notes = Prelude.Nothing,
        instanceId = pInstanceId_,
        evaluationId = pEvaluationId_
      }

-- | A map of question identifiers to answer value.
updateContactEvaluation_answers :: Lens.Lens' UpdateContactEvaluation (Prelude.Maybe (Prelude.HashMap Prelude.Text EvaluationAnswerInput))
updateContactEvaluation_answers = Lens.lens (\UpdateContactEvaluation' {answers} -> answers) (\s@UpdateContactEvaluation' {} a -> s {answers = a} :: UpdateContactEvaluation) Prelude.. Lens.mapping Lens.coerced

-- | A map of question identifiers to note value.
updateContactEvaluation_notes :: Lens.Lens' UpdateContactEvaluation (Prelude.Maybe (Prelude.HashMap Prelude.Text EvaluationNote))
updateContactEvaluation_notes = Lens.lens (\UpdateContactEvaluation' {notes} -> notes) (\s@UpdateContactEvaluation' {} a -> s {notes = a} :: UpdateContactEvaluation) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
updateContactEvaluation_instanceId :: Lens.Lens' UpdateContactEvaluation Prelude.Text
updateContactEvaluation_instanceId = Lens.lens (\UpdateContactEvaluation' {instanceId} -> instanceId) (\s@UpdateContactEvaluation' {} a -> s {instanceId = a} :: UpdateContactEvaluation)

-- | A unique identifier for the contact evaluation.
updateContactEvaluation_evaluationId :: Lens.Lens' UpdateContactEvaluation Prelude.Text
updateContactEvaluation_evaluationId = Lens.lens (\UpdateContactEvaluation' {evaluationId} -> evaluationId) (\s@UpdateContactEvaluation' {} a -> s {evaluationId = a} :: UpdateContactEvaluation)

instance Core.AWSRequest UpdateContactEvaluation where
  type
    AWSResponse UpdateContactEvaluation =
      UpdateContactEvaluationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContactEvaluationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EvaluationId")
            Prelude.<*> (x Data..:> "EvaluationArn")
      )

instance Prelude.Hashable UpdateContactEvaluation where
  hashWithSalt _salt UpdateContactEvaluation' {..} =
    _salt
      `Prelude.hashWithSalt` answers
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` evaluationId

instance Prelude.NFData UpdateContactEvaluation where
  rnf UpdateContactEvaluation' {..} =
    Prelude.rnf answers
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf evaluationId

instance Data.ToHeaders UpdateContactEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContactEvaluation where
  toJSON UpdateContactEvaluation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Answers" Data..=) Prelude.<$> answers,
            ("Notes" Data..=) Prelude.<$> notes
          ]
      )

instance Data.ToPath UpdateContactEvaluation where
  toPath UpdateContactEvaluation' {..} =
    Prelude.mconcat
      [ "/contact-evaluations/",
        Data.toBS instanceId,
        "/",
        Data.toBS evaluationId
      ]

instance Data.ToQuery UpdateContactEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContactEvaluationResponse' smart constructor.
data UpdateContactEvaluationResponse = UpdateContactEvaluationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique identifier for the contact evaluation.
    evaluationId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the contact evaluation resource.
    evaluationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactEvaluationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateContactEvaluationResponse_httpStatus' - The response's http status code.
--
-- 'evaluationId', 'updateContactEvaluationResponse_evaluationId' - A unique identifier for the contact evaluation.
--
-- 'evaluationArn', 'updateContactEvaluationResponse_evaluationArn' - The Amazon Resource Name (ARN) for the contact evaluation resource.
newUpdateContactEvaluationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'evaluationId'
  Prelude.Text ->
  -- | 'evaluationArn'
  Prelude.Text ->
  UpdateContactEvaluationResponse
newUpdateContactEvaluationResponse
  pHttpStatus_
  pEvaluationId_
  pEvaluationArn_ =
    UpdateContactEvaluationResponse'
      { httpStatus =
          pHttpStatus_,
        evaluationId = pEvaluationId_,
        evaluationArn = pEvaluationArn_
      }

-- | The response's http status code.
updateContactEvaluationResponse_httpStatus :: Lens.Lens' UpdateContactEvaluationResponse Prelude.Int
updateContactEvaluationResponse_httpStatus = Lens.lens (\UpdateContactEvaluationResponse' {httpStatus} -> httpStatus) (\s@UpdateContactEvaluationResponse' {} a -> s {httpStatus = a} :: UpdateContactEvaluationResponse)

-- | A unique identifier for the contact evaluation.
updateContactEvaluationResponse_evaluationId :: Lens.Lens' UpdateContactEvaluationResponse Prelude.Text
updateContactEvaluationResponse_evaluationId = Lens.lens (\UpdateContactEvaluationResponse' {evaluationId} -> evaluationId) (\s@UpdateContactEvaluationResponse' {} a -> s {evaluationId = a} :: UpdateContactEvaluationResponse)

-- | The Amazon Resource Name (ARN) for the contact evaluation resource.
updateContactEvaluationResponse_evaluationArn :: Lens.Lens' UpdateContactEvaluationResponse Prelude.Text
updateContactEvaluationResponse_evaluationArn = Lens.lens (\UpdateContactEvaluationResponse' {evaluationArn} -> evaluationArn) (\s@UpdateContactEvaluationResponse' {} a -> s {evaluationArn = a} :: UpdateContactEvaluationResponse)

instance
  Prelude.NFData
    UpdateContactEvaluationResponse
  where
  rnf UpdateContactEvaluationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluationId
      `Prelude.seq` Prelude.rnf evaluationArn
