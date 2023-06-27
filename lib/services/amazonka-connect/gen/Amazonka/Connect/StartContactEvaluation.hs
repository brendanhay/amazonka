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
-- Module      : Amazonka.Connect.StartContactEvaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an empty evaluation in the specified Amazon Connect instance,
-- using the given evaluation form for the particular contact. The
-- evaluation form version used for the contact evaluation corresponds to
-- the currently activated version. If no version is activated for the
-- evaluation form, the contact evaluation cannot be started.
--
-- Evaluations created through the public API do not contain answer values
-- suggested from automation.
module Amazonka.Connect.StartContactEvaluation
  ( -- * Creating a Request
    StartContactEvaluation (..),
    newStartContactEvaluation,

    -- * Request Lenses
    startContactEvaluation_clientToken,
    startContactEvaluation_instanceId,
    startContactEvaluation_contactId,
    startContactEvaluation_evaluationFormId,

    -- * Destructuring the Response
    StartContactEvaluationResponse (..),
    newStartContactEvaluationResponse,

    -- * Response Lenses
    startContactEvaluationResponse_httpStatus,
    startContactEvaluationResponse_evaluationId,
    startContactEvaluationResponse_evaluationArn,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartContactEvaluation' smart constructor.
data StartContactEvaluation = StartContactEvaluation'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact in this instance of Amazon Connect.
    contactId :: Prelude.Text,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartContactEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startContactEvaluation_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'instanceId', 'startContactEvaluation_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'contactId', 'startContactEvaluation_contactId' - The identifier of the contact in this instance of Amazon Connect.
--
-- 'evaluationFormId', 'startContactEvaluation_evaluationFormId' - The unique identifier for the evaluation form.
newStartContactEvaluation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  StartContactEvaluation
newStartContactEvaluation
  pInstanceId_
  pContactId_
  pEvaluationFormId_ =
    StartContactEvaluation'
      { clientToken =
          Prelude.Nothing,
        instanceId = pInstanceId_,
        contactId = pContactId_,
        evaluationFormId = pEvaluationFormId_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
startContactEvaluation_clientToken :: Lens.Lens' StartContactEvaluation (Prelude.Maybe Prelude.Text)
startContactEvaluation_clientToken = Lens.lens (\StartContactEvaluation' {clientToken} -> clientToken) (\s@StartContactEvaluation' {} a -> s {clientToken = a} :: StartContactEvaluation)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
startContactEvaluation_instanceId :: Lens.Lens' StartContactEvaluation Prelude.Text
startContactEvaluation_instanceId = Lens.lens (\StartContactEvaluation' {instanceId} -> instanceId) (\s@StartContactEvaluation' {} a -> s {instanceId = a} :: StartContactEvaluation)

-- | The identifier of the contact in this instance of Amazon Connect.
startContactEvaluation_contactId :: Lens.Lens' StartContactEvaluation Prelude.Text
startContactEvaluation_contactId = Lens.lens (\StartContactEvaluation' {contactId} -> contactId) (\s@StartContactEvaluation' {} a -> s {contactId = a} :: StartContactEvaluation)

-- | The unique identifier for the evaluation form.
startContactEvaluation_evaluationFormId :: Lens.Lens' StartContactEvaluation Prelude.Text
startContactEvaluation_evaluationFormId = Lens.lens (\StartContactEvaluation' {evaluationFormId} -> evaluationFormId) (\s@StartContactEvaluation' {} a -> s {evaluationFormId = a} :: StartContactEvaluation)

instance Core.AWSRequest StartContactEvaluation where
  type
    AWSResponse StartContactEvaluation =
      StartContactEvaluationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartContactEvaluationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EvaluationId")
            Prelude.<*> (x Data..:> "EvaluationArn")
      )

instance Prelude.Hashable StartContactEvaluation where
  hashWithSalt _salt StartContactEvaluation' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` evaluationFormId

instance Prelude.NFData StartContactEvaluation where
  rnf StartContactEvaluation' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf evaluationFormId

instance Data.ToHeaders StartContactEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartContactEvaluation where
  toJSON StartContactEvaluation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("ContactId" Data..= contactId),
            Prelude.Just
              ("EvaluationFormId" Data..= evaluationFormId)
          ]
      )

instance Data.ToPath StartContactEvaluation where
  toPath StartContactEvaluation' {..} =
    Prelude.mconcat
      ["/contact-evaluations/", Data.toBS instanceId]

instance Data.ToQuery StartContactEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartContactEvaluationResponse' smart constructor.
data StartContactEvaluationResponse = StartContactEvaluationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique identifier for the contact evaluation.
    evaluationId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the contact evaluation resource.
    evaluationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartContactEvaluationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startContactEvaluationResponse_httpStatus' - The response's http status code.
--
-- 'evaluationId', 'startContactEvaluationResponse_evaluationId' - A unique identifier for the contact evaluation.
--
-- 'evaluationArn', 'startContactEvaluationResponse_evaluationArn' - The Amazon Resource Name (ARN) for the contact evaluation resource.
newStartContactEvaluationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'evaluationId'
  Prelude.Text ->
  -- | 'evaluationArn'
  Prelude.Text ->
  StartContactEvaluationResponse
newStartContactEvaluationResponse
  pHttpStatus_
  pEvaluationId_
  pEvaluationArn_ =
    StartContactEvaluationResponse'
      { httpStatus =
          pHttpStatus_,
        evaluationId = pEvaluationId_,
        evaluationArn = pEvaluationArn_
      }

-- | The response's http status code.
startContactEvaluationResponse_httpStatus :: Lens.Lens' StartContactEvaluationResponse Prelude.Int
startContactEvaluationResponse_httpStatus = Lens.lens (\StartContactEvaluationResponse' {httpStatus} -> httpStatus) (\s@StartContactEvaluationResponse' {} a -> s {httpStatus = a} :: StartContactEvaluationResponse)

-- | A unique identifier for the contact evaluation.
startContactEvaluationResponse_evaluationId :: Lens.Lens' StartContactEvaluationResponse Prelude.Text
startContactEvaluationResponse_evaluationId = Lens.lens (\StartContactEvaluationResponse' {evaluationId} -> evaluationId) (\s@StartContactEvaluationResponse' {} a -> s {evaluationId = a} :: StartContactEvaluationResponse)

-- | The Amazon Resource Name (ARN) for the contact evaluation resource.
startContactEvaluationResponse_evaluationArn :: Lens.Lens' StartContactEvaluationResponse Prelude.Text
startContactEvaluationResponse_evaluationArn = Lens.lens (\StartContactEvaluationResponse' {evaluationArn} -> evaluationArn) (\s@StartContactEvaluationResponse' {} a -> s {evaluationArn = a} :: StartContactEvaluationResponse)

instance
  Prelude.NFData
    StartContactEvaluationResponse
  where
  rnf StartContactEvaluationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluationId
      `Prelude.seq` Prelude.rnf evaluationArn
