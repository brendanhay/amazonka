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
-- Module      : Amazonka.Connect.DescribeEvaluationForm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an evaluation form in the specified Amazon Connect instance.
-- If the version property is not provided, the latest version of the
-- evaluation form is described.
module Amazonka.Connect.DescribeEvaluationForm
  ( -- * Creating a Request
    DescribeEvaluationForm (..),
    newDescribeEvaluationForm,

    -- * Request Lenses
    describeEvaluationForm_evaluationFormVersion,
    describeEvaluationForm_instanceId,
    describeEvaluationForm_evaluationFormId,

    -- * Destructuring the Response
    DescribeEvaluationFormResponse (..),
    newDescribeEvaluationFormResponse,

    -- * Response Lenses
    describeEvaluationFormResponse_httpStatus,
    describeEvaluationFormResponse_evaluationForm,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEvaluationForm' smart constructor.
data DescribeEvaluationForm = DescribeEvaluationForm'
  { -- | A version of the evaluation form.
    evaluationFormVersion :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | A unique identifier for the contact evaluation.
    evaluationFormId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEvaluationForm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationFormVersion', 'describeEvaluationForm_evaluationFormVersion' - A version of the evaluation form.
--
-- 'instanceId', 'describeEvaluationForm_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'evaluationFormId', 'describeEvaluationForm_evaluationFormId' - A unique identifier for the contact evaluation.
newDescribeEvaluationForm ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  DescribeEvaluationForm
newDescribeEvaluationForm
  pInstanceId_
  pEvaluationFormId_ =
    DescribeEvaluationForm'
      { evaluationFormVersion =
          Prelude.Nothing,
        instanceId = pInstanceId_,
        evaluationFormId = pEvaluationFormId_
      }

-- | A version of the evaluation form.
describeEvaluationForm_evaluationFormVersion :: Lens.Lens' DescribeEvaluationForm (Prelude.Maybe Prelude.Natural)
describeEvaluationForm_evaluationFormVersion = Lens.lens (\DescribeEvaluationForm' {evaluationFormVersion} -> evaluationFormVersion) (\s@DescribeEvaluationForm' {} a -> s {evaluationFormVersion = a} :: DescribeEvaluationForm)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
describeEvaluationForm_instanceId :: Lens.Lens' DescribeEvaluationForm Prelude.Text
describeEvaluationForm_instanceId = Lens.lens (\DescribeEvaluationForm' {instanceId} -> instanceId) (\s@DescribeEvaluationForm' {} a -> s {instanceId = a} :: DescribeEvaluationForm)

-- | A unique identifier for the contact evaluation.
describeEvaluationForm_evaluationFormId :: Lens.Lens' DescribeEvaluationForm Prelude.Text
describeEvaluationForm_evaluationFormId = Lens.lens (\DescribeEvaluationForm' {evaluationFormId} -> evaluationFormId) (\s@DescribeEvaluationForm' {} a -> s {evaluationFormId = a} :: DescribeEvaluationForm)

instance Core.AWSRequest DescribeEvaluationForm where
  type
    AWSResponse DescribeEvaluationForm =
      DescribeEvaluationFormResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEvaluationFormResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EvaluationForm")
      )

instance Prelude.Hashable DescribeEvaluationForm where
  hashWithSalt _salt DescribeEvaluationForm' {..} =
    _salt
      `Prelude.hashWithSalt` evaluationFormVersion
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` evaluationFormId

instance Prelude.NFData DescribeEvaluationForm where
  rnf DescribeEvaluationForm' {..} =
    Prelude.rnf evaluationFormVersion
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf evaluationFormId

instance Data.ToHeaders DescribeEvaluationForm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeEvaluationForm where
  toPath DescribeEvaluationForm' {..} =
    Prelude.mconcat
      [ "/evaluation-forms/",
        Data.toBS instanceId,
        "/",
        Data.toBS evaluationFormId
      ]

instance Data.ToQuery DescribeEvaluationForm where
  toQuery DescribeEvaluationForm' {..} =
    Prelude.mconcat
      ["version" Data.=: evaluationFormVersion]

-- | /See:/ 'newDescribeEvaluationFormResponse' smart constructor.
data DescribeEvaluationFormResponse = DescribeEvaluationFormResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the evaluation form.
    evaluationForm :: EvaluationForm
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEvaluationFormResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeEvaluationFormResponse_httpStatus' - The response's http status code.
--
-- 'evaluationForm', 'describeEvaluationFormResponse_evaluationForm' - Information about the evaluation form.
newDescribeEvaluationFormResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'evaluationForm'
  EvaluationForm ->
  DescribeEvaluationFormResponse
newDescribeEvaluationFormResponse
  pHttpStatus_
  pEvaluationForm_ =
    DescribeEvaluationFormResponse'
      { httpStatus =
          pHttpStatus_,
        evaluationForm = pEvaluationForm_
      }

-- | The response's http status code.
describeEvaluationFormResponse_httpStatus :: Lens.Lens' DescribeEvaluationFormResponse Prelude.Int
describeEvaluationFormResponse_httpStatus = Lens.lens (\DescribeEvaluationFormResponse' {httpStatus} -> httpStatus) (\s@DescribeEvaluationFormResponse' {} a -> s {httpStatus = a} :: DescribeEvaluationFormResponse)

-- | Information about the evaluation form.
describeEvaluationFormResponse_evaluationForm :: Lens.Lens' DescribeEvaluationFormResponse EvaluationForm
describeEvaluationFormResponse_evaluationForm = Lens.lens (\DescribeEvaluationFormResponse' {evaluationForm} -> evaluationForm) (\s@DescribeEvaluationFormResponse' {} a -> s {evaluationForm = a} :: DescribeEvaluationFormResponse)

instance
  Prelude.NFData
    DescribeEvaluationFormResponse
  where
  rnf DescribeEvaluationFormResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluationForm
