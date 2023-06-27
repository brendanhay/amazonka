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
-- Module      : Amazonka.Connect.DeactivateEvaluationForm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates an evaluation form in the specified Amazon Connect instance.
-- After a form is deactivated, it is no longer available for users to
-- start new evaluations based on the form.
module Amazonka.Connect.DeactivateEvaluationForm
  ( -- * Creating a Request
    DeactivateEvaluationForm (..),
    newDeactivateEvaluationForm,

    -- * Request Lenses
    deactivateEvaluationForm_instanceId,
    deactivateEvaluationForm_evaluationFormId,
    deactivateEvaluationForm_evaluationFormVersion,

    -- * Destructuring the Response
    DeactivateEvaluationFormResponse (..),
    newDeactivateEvaluationFormResponse,

    -- * Response Lenses
    deactivateEvaluationFormResponse_httpStatus,
    deactivateEvaluationFormResponse_evaluationFormId,
    deactivateEvaluationFormResponse_evaluationFormArn,
    deactivateEvaluationFormResponse_evaluationFormVersion,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeactivateEvaluationForm' smart constructor.
data DeactivateEvaluationForm = DeactivateEvaluationForm'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | A version of the evaluation form. If the version property is not
    -- provided, the latest version of the evaluation form is deactivated.
    evaluationFormVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateEvaluationForm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deactivateEvaluationForm_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'evaluationFormId', 'deactivateEvaluationForm_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'evaluationFormVersion', 'deactivateEvaluationForm_evaluationFormVersion' - A version of the evaluation form. If the version property is not
-- provided, the latest version of the evaluation form is deactivated.
newDeactivateEvaluationForm ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'evaluationFormVersion'
  Prelude.Natural ->
  DeactivateEvaluationForm
newDeactivateEvaluationForm
  pInstanceId_
  pEvaluationFormId_
  pEvaluationFormVersion_ =
    DeactivateEvaluationForm'
      { instanceId =
          pInstanceId_,
        evaluationFormId = pEvaluationFormId_,
        evaluationFormVersion = pEvaluationFormVersion_
      }

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
deactivateEvaluationForm_instanceId :: Lens.Lens' DeactivateEvaluationForm Prelude.Text
deactivateEvaluationForm_instanceId = Lens.lens (\DeactivateEvaluationForm' {instanceId} -> instanceId) (\s@DeactivateEvaluationForm' {} a -> s {instanceId = a} :: DeactivateEvaluationForm)

-- | The unique identifier for the evaluation form.
deactivateEvaluationForm_evaluationFormId :: Lens.Lens' DeactivateEvaluationForm Prelude.Text
deactivateEvaluationForm_evaluationFormId = Lens.lens (\DeactivateEvaluationForm' {evaluationFormId} -> evaluationFormId) (\s@DeactivateEvaluationForm' {} a -> s {evaluationFormId = a} :: DeactivateEvaluationForm)

-- | A version of the evaluation form. If the version property is not
-- provided, the latest version of the evaluation form is deactivated.
deactivateEvaluationForm_evaluationFormVersion :: Lens.Lens' DeactivateEvaluationForm Prelude.Natural
deactivateEvaluationForm_evaluationFormVersion = Lens.lens (\DeactivateEvaluationForm' {evaluationFormVersion} -> evaluationFormVersion) (\s@DeactivateEvaluationForm' {} a -> s {evaluationFormVersion = a} :: DeactivateEvaluationForm)

instance Core.AWSRequest DeactivateEvaluationForm where
  type
    AWSResponse DeactivateEvaluationForm =
      DeactivateEvaluationFormResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeactivateEvaluationFormResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EvaluationFormId")
            Prelude.<*> (x Data..:> "EvaluationFormArn")
            Prelude.<*> (x Data..:> "EvaluationFormVersion")
      )

instance Prelude.Hashable DeactivateEvaluationForm where
  hashWithSalt _salt DeactivateEvaluationForm' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` evaluationFormId
      `Prelude.hashWithSalt` evaluationFormVersion

instance Prelude.NFData DeactivateEvaluationForm where
  rnf DeactivateEvaluationForm' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf evaluationFormVersion

instance Data.ToHeaders DeactivateEvaluationForm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeactivateEvaluationForm where
  toJSON DeactivateEvaluationForm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EvaluationFormVersion"
                  Data..= evaluationFormVersion
              )
          ]
      )

instance Data.ToPath DeactivateEvaluationForm where
  toPath DeactivateEvaluationForm' {..} =
    Prelude.mconcat
      [ "/evaluation-forms/",
        Data.toBS instanceId,
        "/",
        Data.toBS evaluationFormId,
        "/deactivate"
      ]

instance Data.ToQuery DeactivateEvaluationForm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeactivateEvaluationFormResponse' smart constructor.
data DeactivateEvaluationFormResponse = DeactivateEvaluationFormResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the evaluation form resource.
    evaluationFormArn :: Prelude.Text,
    -- | The version of the deactivated evaluation form resource.
    evaluationFormVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateEvaluationFormResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deactivateEvaluationFormResponse_httpStatus' - The response's http status code.
--
-- 'evaluationFormId', 'deactivateEvaluationFormResponse_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'evaluationFormArn', 'deactivateEvaluationFormResponse_evaluationFormArn' - The Amazon Resource Name (ARN) for the evaluation form resource.
--
-- 'evaluationFormVersion', 'deactivateEvaluationFormResponse_evaluationFormVersion' - The version of the deactivated evaluation form resource.
newDeactivateEvaluationFormResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'evaluationFormArn'
  Prelude.Text ->
  -- | 'evaluationFormVersion'
  Prelude.Natural ->
  DeactivateEvaluationFormResponse
newDeactivateEvaluationFormResponse
  pHttpStatus_
  pEvaluationFormId_
  pEvaluationFormArn_
  pEvaluationFormVersion_ =
    DeactivateEvaluationFormResponse'
      { httpStatus =
          pHttpStatus_,
        evaluationFormId = pEvaluationFormId_,
        evaluationFormArn = pEvaluationFormArn_,
        evaluationFormVersion =
          pEvaluationFormVersion_
      }

-- | The response's http status code.
deactivateEvaluationFormResponse_httpStatus :: Lens.Lens' DeactivateEvaluationFormResponse Prelude.Int
deactivateEvaluationFormResponse_httpStatus = Lens.lens (\DeactivateEvaluationFormResponse' {httpStatus} -> httpStatus) (\s@DeactivateEvaluationFormResponse' {} a -> s {httpStatus = a} :: DeactivateEvaluationFormResponse)

-- | The unique identifier for the evaluation form.
deactivateEvaluationFormResponse_evaluationFormId :: Lens.Lens' DeactivateEvaluationFormResponse Prelude.Text
deactivateEvaluationFormResponse_evaluationFormId = Lens.lens (\DeactivateEvaluationFormResponse' {evaluationFormId} -> evaluationFormId) (\s@DeactivateEvaluationFormResponse' {} a -> s {evaluationFormId = a} :: DeactivateEvaluationFormResponse)

-- | The Amazon Resource Name (ARN) for the evaluation form resource.
deactivateEvaluationFormResponse_evaluationFormArn :: Lens.Lens' DeactivateEvaluationFormResponse Prelude.Text
deactivateEvaluationFormResponse_evaluationFormArn = Lens.lens (\DeactivateEvaluationFormResponse' {evaluationFormArn} -> evaluationFormArn) (\s@DeactivateEvaluationFormResponse' {} a -> s {evaluationFormArn = a} :: DeactivateEvaluationFormResponse)

-- | The version of the deactivated evaluation form resource.
deactivateEvaluationFormResponse_evaluationFormVersion :: Lens.Lens' DeactivateEvaluationFormResponse Prelude.Natural
deactivateEvaluationFormResponse_evaluationFormVersion = Lens.lens (\DeactivateEvaluationFormResponse' {evaluationFormVersion} -> evaluationFormVersion) (\s@DeactivateEvaluationFormResponse' {} a -> s {evaluationFormVersion = a} :: DeactivateEvaluationFormResponse)

instance
  Prelude.NFData
    DeactivateEvaluationFormResponse
  where
  rnf DeactivateEvaluationFormResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf evaluationFormArn
      `Prelude.seq` Prelude.rnf evaluationFormVersion
