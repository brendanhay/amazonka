{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MachineLearning.DeleteEvaluation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the @DELETED@ status to an @Evaluation@, rendering it unusable.
--
-- After invoking the @DeleteEvaluation@ operation, you can use the
-- @GetEvaluation@ operation to verify that the status of the @Evaluation@
-- changed to @DELETED@.
--
-- Caution
--
-- The results of the @DeleteEvaluation@ operation are irreversible.
module Network.AWS.MachineLearning.DeleteEvaluation
  ( -- * Creating a Request
    DeleteEvaluation (..),
    newDeleteEvaluation,

    -- * Request Lenses
    deleteEvaluation_evaluationId,

    -- * Destructuring the Response
    DeleteEvaluationResponse (..),
    newDeleteEvaluationResponse,

    -- * Response Lenses
    deleteEvaluationResponse_evaluationId,
    deleteEvaluationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEvaluation' smart constructor.
data DeleteEvaluation = DeleteEvaluation'
  { -- | A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
    evaluationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationId', 'deleteEvaluation_evaluationId' - A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
newDeleteEvaluation ::
  -- | 'evaluationId'
  Prelude.Text ->
  DeleteEvaluation
newDeleteEvaluation pEvaluationId_ =
  DeleteEvaluation' {evaluationId = pEvaluationId_}

-- | A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
deleteEvaluation_evaluationId :: Lens.Lens' DeleteEvaluation Prelude.Text
deleteEvaluation_evaluationId = Lens.lens (\DeleteEvaluation' {evaluationId} -> evaluationId) (\s@DeleteEvaluation' {} a -> s {evaluationId = a} :: DeleteEvaluation)

instance Prelude.AWSRequest DeleteEvaluation where
  type Rs DeleteEvaluation = DeleteEvaluationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEvaluationResponse'
            Prelude.<$> (x Prelude..?> "EvaluationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEvaluation

instance Prelude.NFData DeleteEvaluation

instance Prelude.ToHeaders DeleteEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonML_20141212.DeleteEvaluation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteEvaluation where
  toJSON DeleteEvaluation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EvaluationId" Prelude..= evaluationId)
          ]
      )

instance Prelude.ToPath DeleteEvaluation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @DeleteEvaluation@ operation. The output
-- indicates that Amazon Machine Learning (Amazon ML) received the request.
--
-- You can use the @GetEvaluation@ operation and check the value of the
-- @Status@ parameter to see whether an @Evaluation@ is marked as
-- @DELETED@.
--
-- /See:/ 'newDeleteEvaluationResponse' smart constructor.
data DeleteEvaluationResponse = DeleteEvaluationResponse'
  { -- | A user-supplied ID that uniquely identifies the @Evaluation@. This value
    -- should be identical to the value of the @EvaluationId@ in the request.
    evaluationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEvaluationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationId', 'deleteEvaluationResponse_evaluationId' - A user-supplied ID that uniquely identifies the @Evaluation@. This value
-- should be identical to the value of the @EvaluationId@ in the request.
--
-- 'httpStatus', 'deleteEvaluationResponse_httpStatus' - The response's http status code.
newDeleteEvaluationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEvaluationResponse
newDeleteEvaluationResponse pHttpStatus_ =
  DeleteEvaluationResponse'
    { evaluationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @Evaluation@. This value
-- should be identical to the value of the @EvaluationId@ in the request.
deleteEvaluationResponse_evaluationId :: Lens.Lens' DeleteEvaluationResponse (Prelude.Maybe Prelude.Text)
deleteEvaluationResponse_evaluationId = Lens.lens (\DeleteEvaluationResponse' {evaluationId} -> evaluationId) (\s@DeleteEvaluationResponse' {} a -> s {evaluationId = a} :: DeleteEvaluationResponse)

-- | The response's http status code.
deleteEvaluationResponse_httpStatus :: Lens.Lens' DeleteEvaluationResponse Prelude.Int
deleteEvaluationResponse_httpStatus = Lens.lens (\DeleteEvaluationResponse' {httpStatus} -> httpStatus) (\s@DeleteEvaluationResponse' {} a -> s {httpStatus = a} :: DeleteEvaluationResponse)

instance Prelude.NFData DeleteEvaluationResponse
