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
-- Module      : Amazonka.Connect.DescribeContactEvaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a contact evaluation in the specified Amazon Connect instance.
module Amazonka.Connect.DescribeContactEvaluation
  ( -- * Creating a Request
    DescribeContactEvaluation (..),
    newDescribeContactEvaluation,

    -- * Request Lenses
    describeContactEvaluation_instanceId,
    describeContactEvaluation_evaluationId,

    -- * Destructuring the Response
    DescribeContactEvaluationResponse (..),
    newDescribeContactEvaluationResponse,

    -- * Response Lenses
    describeContactEvaluationResponse_httpStatus,
    describeContactEvaluationResponse_evaluation,
    describeContactEvaluationResponse_evaluationForm,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeContactEvaluation' smart constructor.
data DescribeContactEvaluation = DescribeContactEvaluation'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | A unique identifier for the contact evaluation.
    evaluationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContactEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeContactEvaluation_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'evaluationId', 'describeContactEvaluation_evaluationId' - A unique identifier for the contact evaluation.
newDescribeContactEvaluation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'evaluationId'
  Prelude.Text ->
  DescribeContactEvaluation
newDescribeContactEvaluation
  pInstanceId_
  pEvaluationId_ =
    DescribeContactEvaluation'
      { instanceId =
          pInstanceId_,
        evaluationId = pEvaluationId_
      }

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
describeContactEvaluation_instanceId :: Lens.Lens' DescribeContactEvaluation Prelude.Text
describeContactEvaluation_instanceId = Lens.lens (\DescribeContactEvaluation' {instanceId} -> instanceId) (\s@DescribeContactEvaluation' {} a -> s {instanceId = a} :: DescribeContactEvaluation)

-- | A unique identifier for the contact evaluation.
describeContactEvaluation_evaluationId :: Lens.Lens' DescribeContactEvaluation Prelude.Text
describeContactEvaluation_evaluationId = Lens.lens (\DescribeContactEvaluation' {evaluationId} -> evaluationId) (\s@DescribeContactEvaluation' {} a -> s {evaluationId = a} :: DescribeContactEvaluation)

instance Core.AWSRequest DescribeContactEvaluation where
  type
    AWSResponse DescribeContactEvaluation =
      DescribeContactEvaluationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContactEvaluationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Evaluation")
            Prelude.<*> (x Data..:> "EvaluationForm")
      )

instance Prelude.Hashable DescribeContactEvaluation where
  hashWithSalt _salt DescribeContactEvaluation' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` evaluationId

instance Prelude.NFData DescribeContactEvaluation where
  rnf DescribeContactEvaluation' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf evaluationId

instance Data.ToHeaders DescribeContactEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeContactEvaluation where
  toPath DescribeContactEvaluation' {..} =
    Prelude.mconcat
      [ "/contact-evaluations/",
        Data.toBS instanceId,
        "/",
        Data.toBS evaluationId
      ]

instance Data.ToQuery DescribeContactEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContactEvaluationResponse' smart constructor.
data DescribeContactEvaluationResponse = DescribeContactEvaluationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the evaluation form completed for a specific contact.
    evaluation :: Evaluation,
    -- | Information about the evaluation form.
    evaluationForm :: EvaluationFormContent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContactEvaluationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeContactEvaluationResponse_httpStatus' - The response's http status code.
--
-- 'evaluation', 'describeContactEvaluationResponse_evaluation' - Information about the evaluation form completed for a specific contact.
--
-- 'evaluationForm', 'describeContactEvaluationResponse_evaluationForm' - Information about the evaluation form.
newDescribeContactEvaluationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'evaluation'
  Evaluation ->
  -- | 'evaluationForm'
  EvaluationFormContent ->
  DescribeContactEvaluationResponse
newDescribeContactEvaluationResponse
  pHttpStatus_
  pEvaluation_
  pEvaluationForm_ =
    DescribeContactEvaluationResponse'
      { httpStatus =
          pHttpStatus_,
        evaluation = pEvaluation_,
        evaluationForm = pEvaluationForm_
      }

-- | The response's http status code.
describeContactEvaluationResponse_httpStatus :: Lens.Lens' DescribeContactEvaluationResponse Prelude.Int
describeContactEvaluationResponse_httpStatus = Lens.lens (\DescribeContactEvaluationResponse' {httpStatus} -> httpStatus) (\s@DescribeContactEvaluationResponse' {} a -> s {httpStatus = a} :: DescribeContactEvaluationResponse)

-- | Information about the evaluation form completed for a specific contact.
describeContactEvaluationResponse_evaluation :: Lens.Lens' DescribeContactEvaluationResponse Evaluation
describeContactEvaluationResponse_evaluation = Lens.lens (\DescribeContactEvaluationResponse' {evaluation} -> evaluation) (\s@DescribeContactEvaluationResponse' {} a -> s {evaluation = a} :: DescribeContactEvaluationResponse)

-- | Information about the evaluation form.
describeContactEvaluationResponse_evaluationForm :: Lens.Lens' DescribeContactEvaluationResponse EvaluationFormContent
describeContactEvaluationResponse_evaluationForm = Lens.lens (\DescribeContactEvaluationResponse' {evaluationForm} -> evaluationForm) (\s@DescribeContactEvaluationResponse' {} a -> s {evaluationForm = a} :: DescribeContactEvaluationResponse)

instance
  Prelude.NFData
    DescribeContactEvaluationResponse
  where
  rnf DescribeContactEvaluationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluation
      `Prelude.seq` Prelude.rnf evaluationForm
