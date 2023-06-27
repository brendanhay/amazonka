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
-- Module      : Amazonka.Connect.UpdateEvaluationForm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates details about a specific evaluation form version in the
-- specified Amazon Connect instance. Question and section identifiers
-- cannot be duplicated within the same evaluation form.
--
-- This operation does not support partial updates. Instead it does a full
-- update of evaluation form content.
module Amazonka.Connect.UpdateEvaluationForm
  ( -- * Creating a Request
    UpdateEvaluationForm (..),
    newUpdateEvaluationForm,

    -- * Request Lenses
    updateEvaluationForm_clientToken,
    updateEvaluationForm_createNewVersion,
    updateEvaluationForm_description,
    updateEvaluationForm_scoringStrategy,
    updateEvaluationForm_instanceId,
    updateEvaluationForm_evaluationFormId,
    updateEvaluationForm_evaluationFormVersion,
    updateEvaluationForm_title,
    updateEvaluationForm_items,

    -- * Destructuring the Response
    UpdateEvaluationFormResponse (..),
    newUpdateEvaluationFormResponse,

    -- * Response Lenses
    updateEvaluationFormResponse_httpStatus,
    updateEvaluationFormResponse_evaluationFormId,
    updateEvaluationFormResponse_evaluationFormArn,
    updateEvaluationFormResponse_evaluationFormVersion,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEvaluationForm' smart constructor.
data UpdateEvaluationForm = UpdateEvaluationForm'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A flag indicating whether the operation must create a new version.
    createNewVersion :: Prelude.Maybe Prelude.Bool,
    -- | The description of the evaluation form.
    description :: Prelude.Maybe Prelude.Text,
    -- | A scoring strategy of the evaluation form.
    scoringStrategy :: Prelude.Maybe EvaluationFormScoringStrategy,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | A version of the evaluation form to update.
    evaluationFormVersion :: Prelude.Natural,
    -- | A title of the evaluation form.
    title :: Prelude.Text,
    -- | Items that are part of the evaluation form. The total number of sections
    -- and questions must not exceed 100 each. Questions must be contained in a
    -- section.
    items :: Prelude.NonEmpty EvaluationFormItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEvaluationForm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateEvaluationForm_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'createNewVersion', 'updateEvaluationForm_createNewVersion' - A flag indicating whether the operation must create a new version.
--
-- 'description', 'updateEvaluationForm_description' - The description of the evaluation form.
--
-- 'scoringStrategy', 'updateEvaluationForm_scoringStrategy' - A scoring strategy of the evaluation form.
--
-- 'instanceId', 'updateEvaluationForm_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'evaluationFormId', 'updateEvaluationForm_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'evaluationFormVersion', 'updateEvaluationForm_evaluationFormVersion' - A version of the evaluation form to update.
--
-- 'title', 'updateEvaluationForm_title' - A title of the evaluation form.
--
-- 'items', 'updateEvaluationForm_items' - Items that are part of the evaluation form. The total number of sections
-- and questions must not exceed 100 each. Questions must be contained in a
-- section.
newUpdateEvaluationForm ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'evaluationFormVersion'
  Prelude.Natural ->
  -- | 'title'
  Prelude.Text ->
  -- | 'items'
  Prelude.NonEmpty EvaluationFormItem ->
  UpdateEvaluationForm
newUpdateEvaluationForm
  pInstanceId_
  pEvaluationFormId_
  pEvaluationFormVersion_
  pTitle_
  pItems_ =
    UpdateEvaluationForm'
      { clientToken =
          Prelude.Nothing,
        createNewVersion = Prelude.Nothing,
        description = Prelude.Nothing,
        scoringStrategy = Prelude.Nothing,
        instanceId = pInstanceId_,
        evaluationFormId = pEvaluationFormId_,
        evaluationFormVersion = pEvaluationFormVersion_,
        title = pTitle_,
        items = Lens.coerced Lens.# pItems_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
updateEvaluationForm_clientToken :: Lens.Lens' UpdateEvaluationForm (Prelude.Maybe Prelude.Text)
updateEvaluationForm_clientToken = Lens.lens (\UpdateEvaluationForm' {clientToken} -> clientToken) (\s@UpdateEvaluationForm' {} a -> s {clientToken = a} :: UpdateEvaluationForm)

-- | A flag indicating whether the operation must create a new version.
updateEvaluationForm_createNewVersion :: Lens.Lens' UpdateEvaluationForm (Prelude.Maybe Prelude.Bool)
updateEvaluationForm_createNewVersion = Lens.lens (\UpdateEvaluationForm' {createNewVersion} -> createNewVersion) (\s@UpdateEvaluationForm' {} a -> s {createNewVersion = a} :: UpdateEvaluationForm)

-- | The description of the evaluation form.
updateEvaluationForm_description :: Lens.Lens' UpdateEvaluationForm (Prelude.Maybe Prelude.Text)
updateEvaluationForm_description = Lens.lens (\UpdateEvaluationForm' {description} -> description) (\s@UpdateEvaluationForm' {} a -> s {description = a} :: UpdateEvaluationForm)

-- | A scoring strategy of the evaluation form.
updateEvaluationForm_scoringStrategy :: Lens.Lens' UpdateEvaluationForm (Prelude.Maybe EvaluationFormScoringStrategy)
updateEvaluationForm_scoringStrategy = Lens.lens (\UpdateEvaluationForm' {scoringStrategy} -> scoringStrategy) (\s@UpdateEvaluationForm' {} a -> s {scoringStrategy = a} :: UpdateEvaluationForm)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
updateEvaluationForm_instanceId :: Lens.Lens' UpdateEvaluationForm Prelude.Text
updateEvaluationForm_instanceId = Lens.lens (\UpdateEvaluationForm' {instanceId} -> instanceId) (\s@UpdateEvaluationForm' {} a -> s {instanceId = a} :: UpdateEvaluationForm)

-- | The unique identifier for the evaluation form.
updateEvaluationForm_evaluationFormId :: Lens.Lens' UpdateEvaluationForm Prelude.Text
updateEvaluationForm_evaluationFormId = Lens.lens (\UpdateEvaluationForm' {evaluationFormId} -> evaluationFormId) (\s@UpdateEvaluationForm' {} a -> s {evaluationFormId = a} :: UpdateEvaluationForm)

-- | A version of the evaluation form to update.
updateEvaluationForm_evaluationFormVersion :: Lens.Lens' UpdateEvaluationForm Prelude.Natural
updateEvaluationForm_evaluationFormVersion = Lens.lens (\UpdateEvaluationForm' {evaluationFormVersion} -> evaluationFormVersion) (\s@UpdateEvaluationForm' {} a -> s {evaluationFormVersion = a} :: UpdateEvaluationForm)

-- | A title of the evaluation form.
updateEvaluationForm_title :: Lens.Lens' UpdateEvaluationForm Prelude.Text
updateEvaluationForm_title = Lens.lens (\UpdateEvaluationForm' {title} -> title) (\s@UpdateEvaluationForm' {} a -> s {title = a} :: UpdateEvaluationForm)

-- | Items that are part of the evaluation form. The total number of sections
-- and questions must not exceed 100 each. Questions must be contained in a
-- section.
updateEvaluationForm_items :: Lens.Lens' UpdateEvaluationForm (Prelude.NonEmpty EvaluationFormItem)
updateEvaluationForm_items = Lens.lens (\UpdateEvaluationForm' {items} -> items) (\s@UpdateEvaluationForm' {} a -> s {items = a} :: UpdateEvaluationForm) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateEvaluationForm where
  type
    AWSResponse UpdateEvaluationForm =
      UpdateEvaluationFormResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEvaluationFormResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EvaluationFormId")
            Prelude.<*> (x Data..:> "EvaluationFormArn")
            Prelude.<*> (x Data..:> "EvaluationFormVersion")
      )

instance Prelude.Hashable UpdateEvaluationForm where
  hashWithSalt _salt UpdateEvaluationForm' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` createNewVersion
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` scoringStrategy
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` evaluationFormId
      `Prelude.hashWithSalt` evaluationFormVersion
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` items

instance Prelude.NFData UpdateEvaluationForm where
  rnf UpdateEvaluationForm' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf createNewVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf scoringStrategy
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf evaluationFormVersion
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf items

instance Data.ToHeaders UpdateEvaluationForm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEvaluationForm where
  toJSON UpdateEvaluationForm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("CreateNewVersion" Data..=)
              Prelude.<$> createNewVersion,
            ("Description" Data..=) Prelude.<$> description,
            ("ScoringStrategy" Data..=)
              Prelude.<$> scoringStrategy,
            Prelude.Just
              ( "EvaluationFormVersion"
                  Data..= evaluationFormVersion
              ),
            Prelude.Just ("Title" Data..= title),
            Prelude.Just ("Items" Data..= items)
          ]
      )

instance Data.ToPath UpdateEvaluationForm where
  toPath UpdateEvaluationForm' {..} =
    Prelude.mconcat
      [ "/evaluation-forms/",
        Data.toBS instanceId,
        "/",
        Data.toBS evaluationFormId
      ]

instance Data.ToQuery UpdateEvaluationForm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEvaluationFormResponse' smart constructor.
data UpdateEvaluationFormResponse = UpdateEvaluationFormResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the contact evaluation resource.
    evaluationFormArn :: Prelude.Text,
    -- | The version of the updated evaluation form resource.
    evaluationFormVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEvaluationFormResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEvaluationFormResponse_httpStatus' - The response's http status code.
--
-- 'evaluationFormId', 'updateEvaluationFormResponse_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'evaluationFormArn', 'updateEvaluationFormResponse_evaluationFormArn' - The Amazon Resource Name (ARN) for the contact evaluation resource.
--
-- 'evaluationFormVersion', 'updateEvaluationFormResponse_evaluationFormVersion' - The version of the updated evaluation form resource.
newUpdateEvaluationFormResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'evaluationFormArn'
  Prelude.Text ->
  -- | 'evaluationFormVersion'
  Prelude.Natural ->
  UpdateEvaluationFormResponse
newUpdateEvaluationFormResponse
  pHttpStatus_
  pEvaluationFormId_
  pEvaluationFormArn_
  pEvaluationFormVersion_ =
    UpdateEvaluationFormResponse'
      { httpStatus =
          pHttpStatus_,
        evaluationFormId = pEvaluationFormId_,
        evaluationFormArn = pEvaluationFormArn_,
        evaluationFormVersion =
          pEvaluationFormVersion_
      }

-- | The response's http status code.
updateEvaluationFormResponse_httpStatus :: Lens.Lens' UpdateEvaluationFormResponse Prelude.Int
updateEvaluationFormResponse_httpStatus = Lens.lens (\UpdateEvaluationFormResponse' {httpStatus} -> httpStatus) (\s@UpdateEvaluationFormResponse' {} a -> s {httpStatus = a} :: UpdateEvaluationFormResponse)

-- | The unique identifier for the evaluation form.
updateEvaluationFormResponse_evaluationFormId :: Lens.Lens' UpdateEvaluationFormResponse Prelude.Text
updateEvaluationFormResponse_evaluationFormId = Lens.lens (\UpdateEvaluationFormResponse' {evaluationFormId} -> evaluationFormId) (\s@UpdateEvaluationFormResponse' {} a -> s {evaluationFormId = a} :: UpdateEvaluationFormResponse)

-- | The Amazon Resource Name (ARN) for the contact evaluation resource.
updateEvaluationFormResponse_evaluationFormArn :: Lens.Lens' UpdateEvaluationFormResponse Prelude.Text
updateEvaluationFormResponse_evaluationFormArn = Lens.lens (\UpdateEvaluationFormResponse' {evaluationFormArn} -> evaluationFormArn) (\s@UpdateEvaluationFormResponse' {} a -> s {evaluationFormArn = a} :: UpdateEvaluationFormResponse)

-- | The version of the updated evaluation form resource.
updateEvaluationFormResponse_evaluationFormVersion :: Lens.Lens' UpdateEvaluationFormResponse Prelude.Natural
updateEvaluationFormResponse_evaluationFormVersion = Lens.lens (\UpdateEvaluationFormResponse' {evaluationFormVersion} -> evaluationFormVersion) (\s@UpdateEvaluationFormResponse' {} a -> s {evaluationFormVersion = a} :: UpdateEvaluationFormResponse)

instance Prelude.NFData UpdateEvaluationFormResponse where
  rnf UpdateEvaluationFormResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf evaluationFormArn
      `Prelude.seq` Prelude.rnf evaluationFormVersion
