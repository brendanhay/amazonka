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
-- Module      : Amazonka.Connect.DeleteEvaluationForm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an evaluation form in the specified Amazon Connect instance.
--
-- -   If the version property is provided, only the specified version of
--     the evaluation form is deleted.
--
-- -   If no version is provided, then the full form (all versions) is
--     deleted.
module Amazonka.Connect.DeleteEvaluationForm
  ( -- * Creating a Request
    DeleteEvaluationForm (..),
    newDeleteEvaluationForm,

    -- * Request Lenses
    deleteEvaluationForm_evaluationFormVersion,
    deleteEvaluationForm_instanceId,
    deleteEvaluationForm_evaluationFormId,

    -- * Destructuring the Response
    DeleteEvaluationFormResponse (..),
    newDeleteEvaluationFormResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEvaluationForm' smart constructor.
data DeleteEvaluationForm = DeleteEvaluationForm'
  { -- | The unique identifier for the evaluation form.
    evaluationFormVersion :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEvaluationForm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationFormVersion', 'deleteEvaluationForm_evaluationFormVersion' - The unique identifier for the evaluation form.
--
-- 'instanceId', 'deleteEvaluationForm_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'evaluationFormId', 'deleteEvaluationForm_evaluationFormId' - The unique identifier for the evaluation form.
newDeleteEvaluationForm ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  DeleteEvaluationForm
newDeleteEvaluationForm
  pInstanceId_
  pEvaluationFormId_ =
    DeleteEvaluationForm'
      { evaluationFormVersion =
          Prelude.Nothing,
        instanceId = pInstanceId_,
        evaluationFormId = pEvaluationFormId_
      }

-- | The unique identifier for the evaluation form.
deleteEvaluationForm_evaluationFormVersion :: Lens.Lens' DeleteEvaluationForm (Prelude.Maybe Prelude.Natural)
deleteEvaluationForm_evaluationFormVersion = Lens.lens (\DeleteEvaluationForm' {evaluationFormVersion} -> evaluationFormVersion) (\s@DeleteEvaluationForm' {} a -> s {evaluationFormVersion = a} :: DeleteEvaluationForm)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
deleteEvaluationForm_instanceId :: Lens.Lens' DeleteEvaluationForm Prelude.Text
deleteEvaluationForm_instanceId = Lens.lens (\DeleteEvaluationForm' {instanceId} -> instanceId) (\s@DeleteEvaluationForm' {} a -> s {instanceId = a} :: DeleteEvaluationForm)

-- | The unique identifier for the evaluation form.
deleteEvaluationForm_evaluationFormId :: Lens.Lens' DeleteEvaluationForm Prelude.Text
deleteEvaluationForm_evaluationFormId = Lens.lens (\DeleteEvaluationForm' {evaluationFormId} -> evaluationFormId) (\s@DeleteEvaluationForm' {} a -> s {evaluationFormId = a} :: DeleteEvaluationForm)

instance Core.AWSRequest DeleteEvaluationForm where
  type
    AWSResponse DeleteEvaluationForm =
      DeleteEvaluationFormResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteEvaluationFormResponse'

instance Prelude.Hashable DeleteEvaluationForm where
  hashWithSalt _salt DeleteEvaluationForm' {..} =
    _salt
      `Prelude.hashWithSalt` evaluationFormVersion
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` evaluationFormId

instance Prelude.NFData DeleteEvaluationForm where
  rnf DeleteEvaluationForm' {..} =
    Prelude.rnf evaluationFormVersion
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf evaluationFormId

instance Data.ToHeaders DeleteEvaluationForm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteEvaluationForm where
  toPath DeleteEvaluationForm' {..} =
    Prelude.mconcat
      [ "/evaluation-forms/",
        Data.toBS instanceId,
        "/",
        Data.toBS evaluationFormId
      ]

instance Data.ToQuery DeleteEvaluationForm where
  toQuery DeleteEvaluationForm' {..} =
    Prelude.mconcat
      ["version" Data.=: evaluationFormVersion]

-- | /See:/ 'newDeleteEvaluationFormResponse' smart constructor.
data DeleteEvaluationFormResponse = DeleteEvaluationFormResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEvaluationFormResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEvaluationFormResponse ::
  DeleteEvaluationFormResponse
newDeleteEvaluationFormResponse =
  DeleteEvaluationFormResponse'

instance Prelude.NFData DeleteEvaluationFormResponse where
  rnf _ = ()
