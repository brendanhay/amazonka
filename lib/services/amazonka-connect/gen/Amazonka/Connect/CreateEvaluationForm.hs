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
-- Module      : Amazonka.Connect.CreateEvaluationForm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an evaluation form in the specified Amazon Connect instance. The
-- form can be used to define questions related to agent performance, and
-- create sections to organize such questions. Question and section
-- identifiers cannot be duplicated within the same evaluation form.
module Amazonka.Connect.CreateEvaluationForm
  ( -- * Creating a Request
    CreateEvaluationForm (..),
    newCreateEvaluationForm,

    -- * Request Lenses
    createEvaluationForm_clientToken,
    createEvaluationForm_description,
    createEvaluationForm_scoringStrategy,
    createEvaluationForm_instanceId,
    createEvaluationForm_title,
    createEvaluationForm_items,

    -- * Destructuring the Response
    CreateEvaluationFormResponse (..),
    newCreateEvaluationFormResponse,

    -- * Response Lenses
    createEvaluationFormResponse_httpStatus,
    createEvaluationFormResponse_evaluationFormId,
    createEvaluationFormResponse_evaluationFormArn,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEvaluationForm' smart constructor.
data CreateEvaluationForm = CreateEvaluationForm'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the evaluation form.
    description :: Prelude.Maybe Prelude.Text,
    -- | A scoring strategy of the evaluation form.
    scoringStrategy :: Prelude.Maybe EvaluationFormScoringStrategy,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | A title of the evaluation form.
    title :: Prelude.Text,
    -- | Items that are part of the evaluation form. The total number of sections
    -- and questions must not exceed 100 each. Questions must be contained in a
    -- section.
    items :: Prelude.NonEmpty EvaluationFormItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEvaluationForm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createEvaluationForm_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'description', 'createEvaluationForm_description' - The description of the evaluation form.
--
-- 'scoringStrategy', 'createEvaluationForm_scoringStrategy' - A scoring strategy of the evaluation form.
--
-- 'instanceId', 'createEvaluationForm_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'title', 'createEvaluationForm_title' - A title of the evaluation form.
--
-- 'items', 'createEvaluationForm_items' - Items that are part of the evaluation form. The total number of sections
-- and questions must not exceed 100 each. Questions must be contained in a
-- section.
newCreateEvaluationForm ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'items'
  Prelude.NonEmpty EvaluationFormItem ->
  CreateEvaluationForm
newCreateEvaluationForm pInstanceId_ pTitle_ pItems_ =
  CreateEvaluationForm'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      scoringStrategy = Prelude.Nothing,
      instanceId = pInstanceId_,
      title = pTitle_,
      items = Lens.coerced Lens.# pItems_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
createEvaluationForm_clientToken :: Lens.Lens' CreateEvaluationForm (Prelude.Maybe Prelude.Text)
createEvaluationForm_clientToken = Lens.lens (\CreateEvaluationForm' {clientToken} -> clientToken) (\s@CreateEvaluationForm' {} a -> s {clientToken = a} :: CreateEvaluationForm)

-- | The description of the evaluation form.
createEvaluationForm_description :: Lens.Lens' CreateEvaluationForm (Prelude.Maybe Prelude.Text)
createEvaluationForm_description = Lens.lens (\CreateEvaluationForm' {description} -> description) (\s@CreateEvaluationForm' {} a -> s {description = a} :: CreateEvaluationForm)

-- | A scoring strategy of the evaluation form.
createEvaluationForm_scoringStrategy :: Lens.Lens' CreateEvaluationForm (Prelude.Maybe EvaluationFormScoringStrategy)
createEvaluationForm_scoringStrategy = Lens.lens (\CreateEvaluationForm' {scoringStrategy} -> scoringStrategy) (\s@CreateEvaluationForm' {} a -> s {scoringStrategy = a} :: CreateEvaluationForm)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
createEvaluationForm_instanceId :: Lens.Lens' CreateEvaluationForm Prelude.Text
createEvaluationForm_instanceId = Lens.lens (\CreateEvaluationForm' {instanceId} -> instanceId) (\s@CreateEvaluationForm' {} a -> s {instanceId = a} :: CreateEvaluationForm)

-- | A title of the evaluation form.
createEvaluationForm_title :: Lens.Lens' CreateEvaluationForm Prelude.Text
createEvaluationForm_title = Lens.lens (\CreateEvaluationForm' {title} -> title) (\s@CreateEvaluationForm' {} a -> s {title = a} :: CreateEvaluationForm)

-- | Items that are part of the evaluation form. The total number of sections
-- and questions must not exceed 100 each. Questions must be contained in a
-- section.
createEvaluationForm_items :: Lens.Lens' CreateEvaluationForm (Prelude.NonEmpty EvaluationFormItem)
createEvaluationForm_items = Lens.lens (\CreateEvaluationForm' {items} -> items) (\s@CreateEvaluationForm' {} a -> s {items = a} :: CreateEvaluationForm) Prelude.. Lens.coerced

instance Core.AWSRequest CreateEvaluationForm where
  type
    AWSResponse CreateEvaluationForm =
      CreateEvaluationFormResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEvaluationFormResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EvaluationFormId")
            Prelude.<*> (x Data..:> "EvaluationFormArn")
      )

instance Prelude.Hashable CreateEvaluationForm where
  hashWithSalt _salt CreateEvaluationForm' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` scoringStrategy
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` items

instance Prelude.NFData CreateEvaluationForm where
  rnf CreateEvaluationForm' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf scoringStrategy
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf items

instance Data.ToHeaders CreateEvaluationForm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEvaluationForm where
  toJSON CreateEvaluationForm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("ScoringStrategy" Data..=)
              Prelude.<$> scoringStrategy,
            Prelude.Just ("Title" Data..= title),
            Prelude.Just ("Items" Data..= items)
          ]
      )

instance Data.ToPath CreateEvaluationForm where
  toPath CreateEvaluationForm' {..} =
    Prelude.mconcat
      ["/evaluation-forms/", Data.toBS instanceId]

instance Data.ToQuery CreateEvaluationForm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEvaluationFormResponse' smart constructor.
data CreateEvaluationFormResponse = CreateEvaluationFormResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the evaluation form resource.
    evaluationFormArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEvaluationFormResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEvaluationFormResponse_httpStatus' - The response's http status code.
--
-- 'evaluationFormId', 'createEvaluationFormResponse_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'evaluationFormArn', 'createEvaluationFormResponse_evaluationFormArn' - The Amazon Resource Name (ARN) for the evaluation form resource.
newCreateEvaluationFormResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'evaluationFormArn'
  Prelude.Text ->
  CreateEvaluationFormResponse
newCreateEvaluationFormResponse
  pHttpStatus_
  pEvaluationFormId_
  pEvaluationFormArn_ =
    CreateEvaluationFormResponse'
      { httpStatus =
          pHttpStatus_,
        evaluationFormId = pEvaluationFormId_,
        evaluationFormArn = pEvaluationFormArn_
      }

-- | The response's http status code.
createEvaluationFormResponse_httpStatus :: Lens.Lens' CreateEvaluationFormResponse Prelude.Int
createEvaluationFormResponse_httpStatus = Lens.lens (\CreateEvaluationFormResponse' {httpStatus} -> httpStatus) (\s@CreateEvaluationFormResponse' {} a -> s {httpStatus = a} :: CreateEvaluationFormResponse)

-- | The unique identifier for the evaluation form.
createEvaluationFormResponse_evaluationFormId :: Lens.Lens' CreateEvaluationFormResponse Prelude.Text
createEvaluationFormResponse_evaluationFormId = Lens.lens (\CreateEvaluationFormResponse' {evaluationFormId} -> evaluationFormId) (\s@CreateEvaluationFormResponse' {} a -> s {evaluationFormId = a} :: CreateEvaluationFormResponse)

-- | The Amazon Resource Name (ARN) for the evaluation form resource.
createEvaluationFormResponse_evaluationFormArn :: Lens.Lens' CreateEvaluationFormResponse Prelude.Text
createEvaluationFormResponse_evaluationFormArn = Lens.lens (\CreateEvaluationFormResponse' {evaluationFormArn} -> evaluationFormArn) (\s@CreateEvaluationFormResponse' {} a -> s {evaluationFormArn = a} :: CreateEvaluationFormResponse)

instance Prelude.NFData CreateEvaluationFormResponse where
  rnf CreateEvaluationFormResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf evaluationFormArn
