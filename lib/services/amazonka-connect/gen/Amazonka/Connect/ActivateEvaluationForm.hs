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
-- Module      : Amazonka.Connect.ActivateEvaluationForm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates an evaluation form in the specified Amazon Connect instance.
-- After the evaluation form is activated, it is available to start new
-- evaluations based on the form.
module Amazonka.Connect.ActivateEvaluationForm
  ( -- * Creating a Request
    ActivateEvaluationForm (..),
    newActivateEvaluationForm,

    -- * Request Lenses
    activateEvaluationForm_instanceId,
    activateEvaluationForm_evaluationFormId,
    activateEvaluationForm_evaluationFormVersion,

    -- * Destructuring the Response
    ActivateEvaluationFormResponse (..),
    newActivateEvaluationFormResponse,

    -- * Response Lenses
    activateEvaluationFormResponse_httpStatus,
    activateEvaluationFormResponse_evaluationFormId,
    activateEvaluationFormResponse_evaluationFormArn,
    activateEvaluationFormResponse_evaluationFormVersion,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newActivateEvaluationForm' smart constructor.
data ActivateEvaluationForm = ActivateEvaluationForm'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | The version of the evaluation form to activate. If the version property
    -- is not provided, the latest version of the evaluation form is activated.
    evaluationFormVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateEvaluationForm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'activateEvaluationForm_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'evaluationFormId', 'activateEvaluationForm_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'evaluationFormVersion', 'activateEvaluationForm_evaluationFormVersion' - The version of the evaluation form to activate. If the version property
-- is not provided, the latest version of the evaluation form is activated.
newActivateEvaluationForm ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'evaluationFormVersion'
  Prelude.Natural ->
  ActivateEvaluationForm
newActivateEvaluationForm
  pInstanceId_
  pEvaluationFormId_
  pEvaluationFormVersion_ =
    ActivateEvaluationForm'
      { instanceId = pInstanceId_,
        evaluationFormId = pEvaluationFormId_,
        evaluationFormVersion = pEvaluationFormVersion_
      }

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
activateEvaluationForm_instanceId :: Lens.Lens' ActivateEvaluationForm Prelude.Text
activateEvaluationForm_instanceId = Lens.lens (\ActivateEvaluationForm' {instanceId} -> instanceId) (\s@ActivateEvaluationForm' {} a -> s {instanceId = a} :: ActivateEvaluationForm)

-- | The unique identifier for the evaluation form.
activateEvaluationForm_evaluationFormId :: Lens.Lens' ActivateEvaluationForm Prelude.Text
activateEvaluationForm_evaluationFormId = Lens.lens (\ActivateEvaluationForm' {evaluationFormId} -> evaluationFormId) (\s@ActivateEvaluationForm' {} a -> s {evaluationFormId = a} :: ActivateEvaluationForm)

-- | The version of the evaluation form to activate. If the version property
-- is not provided, the latest version of the evaluation form is activated.
activateEvaluationForm_evaluationFormVersion :: Lens.Lens' ActivateEvaluationForm Prelude.Natural
activateEvaluationForm_evaluationFormVersion = Lens.lens (\ActivateEvaluationForm' {evaluationFormVersion} -> evaluationFormVersion) (\s@ActivateEvaluationForm' {} a -> s {evaluationFormVersion = a} :: ActivateEvaluationForm)

instance Core.AWSRequest ActivateEvaluationForm where
  type
    AWSResponse ActivateEvaluationForm =
      ActivateEvaluationFormResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ActivateEvaluationFormResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EvaluationFormId")
            Prelude.<*> (x Data..:> "EvaluationFormArn")
            Prelude.<*> (x Data..:> "EvaluationFormVersion")
      )

instance Prelude.Hashable ActivateEvaluationForm where
  hashWithSalt _salt ActivateEvaluationForm' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` evaluationFormId
      `Prelude.hashWithSalt` evaluationFormVersion

instance Prelude.NFData ActivateEvaluationForm where
  rnf ActivateEvaluationForm' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf evaluationFormVersion

instance Data.ToHeaders ActivateEvaluationForm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ActivateEvaluationForm where
  toJSON ActivateEvaluationForm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EvaluationFormVersion"
                  Data..= evaluationFormVersion
              )
          ]
      )

instance Data.ToPath ActivateEvaluationForm where
  toPath ActivateEvaluationForm' {..} =
    Prelude.mconcat
      [ "/evaluation-forms/",
        Data.toBS instanceId,
        "/",
        Data.toBS evaluationFormId,
        "/activate"
      ]

instance Data.ToQuery ActivateEvaluationForm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newActivateEvaluationFormResponse' smart constructor.
data ActivateEvaluationFormResponse = ActivateEvaluationFormResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the evaluation form resource.
    evaluationFormArn :: Prelude.Text,
    -- | A version of the evaluation form.
    evaluationFormVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateEvaluationFormResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'activateEvaluationFormResponse_httpStatus' - The response's http status code.
--
-- 'evaluationFormId', 'activateEvaluationFormResponse_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'evaluationFormArn', 'activateEvaluationFormResponse_evaluationFormArn' - The Amazon Resource Name (ARN) for the evaluation form resource.
--
-- 'evaluationFormVersion', 'activateEvaluationFormResponse_evaluationFormVersion' - A version of the evaluation form.
newActivateEvaluationFormResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'evaluationFormArn'
  Prelude.Text ->
  -- | 'evaluationFormVersion'
  Prelude.Natural ->
  ActivateEvaluationFormResponse
newActivateEvaluationFormResponse
  pHttpStatus_
  pEvaluationFormId_
  pEvaluationFormArn_
  pEvaluationFormVersion_ =
    ActivateEvaluationFormResponse'
      { httpStatus =
          pHttpStatus_,
        evaluationFormId = pEvaluationFormId_,
        evaluationFormArn = pEvaluationFormArn_,
        evaluationFormVersion =
          pEvaluationFormVersion_
      }

-- | The response's http status code.
activateEvaluationFormResponse_httpStatus :: Lens.Lens' ActivateEvaluationFormResponse Prelude.Int
activateEvaluationFormResponse_httpStatus = Lens.lens (\ActivateEvaluationFormResponse' {httpStatus} -> httpStatus) (\s@ActivateEvaluationFormResponse' {} a -> s {httpStatus = a} :: ActivateEvaluationFormResponse)

-- | The unique identifier for the evaluation form.
activateEvaluationFormResponse_evaluationFormId :: Lens.Lens' ActivateEvaluationFormResponse Prelude.Text
activateEvaluationFormResponse_evaluationFormId = Lens.lens (\ActivateEvaluationFormResponse' {evaluationFormId} -> evaluationFormId) (\s@ActivateEvaluationFormResponse' {} a -> s {evaluationFormId = a} :: ActivateEvaluationFormResponse)

-- | The Amazon Resource Name (ARN) for the evaluation form resource.
activateEvaluationFormResponse_evaluationFormArn :: Lens.Lens' ActivateEvaluationFormResponse Prelude.Text
activateEvaluationFormResponse_evaluationFormArn = Lens.lens (\ActivateEvaluationFormResponse' {evaluationFormArn} -> evaluationFormArn) (\s@ActivateEvaluationFormResponse' {} a -> s {evaluationFormArn = a} :: ActivateEvaluationFormResponse)

-- | A version of the evaluation form.
activateEvaluationFormResponse_evaluationFormVersion :: Lens.Lens' ActivateEvaluationFormResponse Prelude.Natural
activateEvaluationFormResponse_evaluationFormVersion = Lens.lens (\ActivateEvaluationFormResponse' {evaluationFormVersion} -> evaluationFormVersion) (\s@ActivateEvaluationFormResponse' {} a -> s {evaluationFormVersion = a} :: ActivateEvaluationFormResponse)

instance
  Prelude.NFData
    ActivateEvaluationFormResponse
  where
  rnf ActivateEvaluationFormResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf evaluationFormArn
      `Prelude.seq` Prelude.rnf evaluationFormVersion
