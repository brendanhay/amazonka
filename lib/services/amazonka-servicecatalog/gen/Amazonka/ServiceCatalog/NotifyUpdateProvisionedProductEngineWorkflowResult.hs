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
-- Module      : Amazonka.ServiceCatalog.NotifyUpdateProvisionedProductEngineWorkflowResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies the result of the update engine execution.
module Amazonka.ServiceCatalog.NotifyUpdateProvisionedProductEngineWorkflowResult
  ( -- * Creating a Request
    NotifyUpdateProvisionedProductEngineWorkflowResult (..),
    newNotifyUpdateProvisionedProductEngineWorkflowResult,

    -- * Request Lenses
    notifyUpdateProvisionedProductEngineWorkflowResult_failureReason,
    notifyUpdateProvisionedProductEngineWorkflowResult_outputs,
    notifyUpdateProvisionedProductEngineWorkflowResult_workflowToken,
    notifyUpdateProvisionedProductEngineWorkflowResult_recordId,
    notifyUpdateProvisionedProductEngineWorkflowResult_status,
    notifyUpdateProvisionedProductEngineWorkflowResult_idempotencyToken,

    -- * Destructuring the Response
    NotifyUpdateProvisionedProductEngineWorkflowResultResponse (..),
    newNotifyUpdateProvisionedProductEngineWorkflowResultResponse,

    -- * Response Lenses
    notifyUpdateProvisionedProductEngineWorkflowResultResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newNotifyUpdateProvisionedProductEngineWorkflowResult' smart constructor.
data NotifyUpdateProvisionedProductEngineWorkflowResult = NotifyUpdateProvisionedProductEngineWorkflowResult'
  { -- | The reason why the update engine execution failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The output of the update engine execution.
    outputs :: Prelude.Maybe [RecordOutput],
    -- | The encrypted contents of the update engine execution payload that
    -- Service Catalog sends after the Terraform product update workflow
    -- starts.
    workflowToken :: Prelude.Text,
    -- | The identifier of the record.
    recordId :: Prelude.Text,
    -- | The status of the update engine execution.
    status :: EngineWorkflowStatus,
    -- | The idempotency token that identifies the update engine execution.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyUpdateProvisionedProductEngineWorkflowResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'notifyUpdateProvisionedProductEngineWorkflowResult_failureReason' - The reason why the update engine execution failed.
--
-- 'outputs', 'notifyUpdateProvisionedProductEngineWorkflowResult_outputs' - The output of the update engine execution.
--
-- 'workflowToken', 'notifyUpdateProvisionedProductEngineWorkflowResult_workflowToken' - The encrypted contents of the update engine execution payload that
-- Service Catalog sends after the Terraform product update workflow
-- starts.
--
-- 'recordId', 'notifyUpdateProvisionedProductEngineWorkflowResult_recordId' - The identifier of the record.
--
-- 'status', 'notifyUpdateProvisionedProductEngineWorkflowResult_status' - The status of the update engine execution.
--
-- 'idempotencyToken', 'notifyUpdateProvisionedProductEngineWorkflowResult_idempotencyToken' - The idempotency token that identifies the update engine execution.
newNotifyUpdateProvisionedProductEngineWorkflowResult ::
  -- | 'workflowToken'
  Prelude.Text ->
  -- | 'recordId'
  Prelude.Text ->
  -- | 'status'
  EngineWorkflowStatus ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  NotifyUpdateProvisionedProductEngineWorkflowResult
newNotifyUpdateProvisionedProductEngineWorkflowResult
  pWorkflowToken_
  pRecordId_
  pStatus_
  pIdempotencyToken_ =
    NotifyUpdateProvisionedProductEngineWorkflowResult'
      { failureReason =
          Prelude.Nothing,
        outputs =
          Prelude.Nothing,
        workflowToken =
          pWorkflowToken_,
        recordId = pRecordId_,
        status = pStatus_,
        idempotencyToken =
          pIdempotencyToken_
      }

-- | The reason why the update engine execution failed.
notifyUpdateProvisionedProductEngineWorkflowResult_failureReason :: Lens.Lens' NotifyUpdateProvisionedProductEngineWorkflowResult (Prelude.Maybe Prelude.Text)
notifyUpdateProvisionedProductEngineWorkflowResult_failureReason = Lens.lens (\NotifyUpdateProvisionedProductEngineWorkflowResult' {failureReason} -> failureReason) (\s@NotifyUpdateProvisionedProductEngineWorkflowResult' {} a -> s {failureReason = a} :: NotifyUpdateProvisionedProductEngineWorkflowResult)

-- | The output of the update engine execution.
notifyUpdateProvisionedProductEngineWorkflowResult_outputs :: Lens.Lens' NotifyUpdateProvisionedProductEngineWorkflowResult (Prelude.Maybe [RecordOutput])
notifyUpdateProvisionedProductEngineWorkflowResult_outputs = Lens.lens (\NotifyUpdateProvisionedProductEngineWorkflowResult' {outputs} -> outputs) (\s@NotifyUpdateProvisionedProductEngineWorkflowResult' {} a -> s {outputs = a} :: NotifyUpdateProvisionedProductEngineWorkflowResult) Prelude.. Lens.mapping Lens.coerced

-- | The encrypted contents of the update engine execution payload that
-- Service Catalog sends after the Terraform product update workflow
-- starts.
notifyUpdateProvisionedProductEngineWorkflowResult_workflowToken :: Lens.Lens' NotifyUpdateProvisionedProductEngineWorkflowResult Prelude.Text
notifyUpdateProvisionedProductEngineWorkflowResult_workflowToken = Lens.lens (\NotifyUpdateProvisionedProductEngineWorkflowResult' {workflowToken} -> workflowToken) (\s@NotifyUpdateProvisionedProductEngineWorkflowResult' {} a -> s {workflowToken = a} :: NotifyUpdateProvisionedProductEngineWorkflowResult)

-- | The identifier of the record.
notifyUpdateProvisionedProductEngineWorkflowResult_recordId :: Lens.Lens' NotifyUpdateProvisionedProductEngineWorkflowResult Prelude.Text
notifyUpdateProvisionedProductEngineWorkflowResult_recordId = Lens.lens (\NotifyUpdateProvisionedProductEngineWorkflowResult' {recordId} -> recordId) (\s@NotifyUpdateProvisionedProductEngineWorkflowResult' {} a -> s {recordId = a} :: NotifyUpdateProvisionedProductEngineWorkflowResult)

-- | The status of the update engine execution.
notifyUpdateProvisionedProductEngineWorkflowResult_status :: Lens.Lens' NotifyUpdateProvisionedProductEngineWorkflowResult EngineWorkflowStatus
notifyUpdateProvisionedProductEngineWorkflowResult_status = Lens.lens (\NotifyUpdateProvisionedProductEngineWorkflowResult' {status} -> status) (\s@NotifyUpdateProvisionedProductEngineWorkflowResult' {} a -> s {status = a} :: NotifyUpdateProvisionedProductEngineWorkflowResult)

-- | The idempotency token that identifies the update engine execution.
notifyUpdateProvisionedProductEngineWorkflowResult_idempotencyToken :: Lens.Lens' NotifyUpdateProvisionedProductEngineWorkflowResult Prelude.Text
notifyUpdateProvisionedProductEngineWorkflowResult_idempotencyToken = Lens.lens (\NotifyUpdateProvisionedProductEngineWorkflowResult' {idempotencyToken} -> idempotencyToken) (\s@NotifyUpdateProvisionedProductEngineWorkflowResult' {} a -> s {idempotencyToken = a} :: NotifyUpdateProvisionedProductEngineWorkflowResult)

instance
  Core.AWSRequest
    NotifyUpdateProvisionedProductEngineWorkflowResult
  where
  type
    AWSResponse
      NotifyUpdateProvisionedProductEngineWorkflowResult =
      NotifyUpdateProvisionedProductEngineWorkflowResultResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          NotifyUpdateProvisionedProductEngineWorkflowResultResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    NotifyUpdateProvisionedProductEngineWorkflowResult
  where
  hashWithSalt
    _salt
    NotifyUpdateProvisionedProductEngineWorkflowResult' {..} =
      _salt
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` outputs
        `Prelude.hashWithSalt` workflowToken
        `Prelude.hashWithSalt` recordId
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` idempotencyToken

instance
  Prelude.NFData
    NotifyUpdateProvisionedProductEngineWorkflowResult
  where
  rnf
    NotifyUpdateProvisionedProductEngineWorkflowResult' {..} =
      Prelude.rnf failureReason
        `Prelude.seq` Prelude.rnf outputs
        `Prelude.seq` Prelude.rnf workflowToken
        `Prelude.seq` Prelude.rnf recordId
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf idempotencyToken

instance
  Data.ToHeaders
    NotifyUpdateProvisionedProductEngineWorkflowResult
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.NotifyUpdateProvisionedProductEngineWorkflowResult" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    NotifyUpdateProvisionedProductEngineWorkflowResult
  where
  toJSON
    NotifyUpdateProvisionedProductEngineWorkflowResult' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("FailureReason" Data..=) Prelude.<$> failureReason,
              ("Outputs" Data..=) Prelude.<$> outputs,
              Prelude.Just ("WorkflowToken" Data..= workflowToken),
              Prelude.Just ("RecordId" Data..= recordId),
              Prelude.Just ("Status" Data..= status),
              Prelude.Just
                ("IdempotencyToken" Data..= idempotencyToken)
            ]
        )

instance
  Data.ToPath
    NotifyUpdateProvisionedProductEngineWorkflowResult
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    NotifyUpdateProvisionedProductEngineWorkflowResult
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newNotifyUpdateProvisionedProductEngineWorkflowResultResponse' smart constructor.
data NotifyUpdateProvisionedProductEngineWorkflowResultResponse = NotifyUpdateProvisionedProductEngineWorkflowResultResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyUpdateProvisionedProductEngineWorkflowResultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'notifyUpdateProvisionedProductEngineWorkflowResultResponse_httpStatus' - The response's http status code.
newNotifyUpdateProvisionedProductEngineWorkflowResultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  NotifyUpdateProvisionedProductEngineWorkflowResultResponse
newNotifyUpdateProvisionedProductEngineWorkflowResultResponse
  pHttpStatus_ =
    NotifyUpdateProvisionedProductEngineWorkflowResultResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
notifyUpdateProvisionedProductEngineWorkflowResultResponse_httpStatus :: Lens.Lens' NotifyUpdateProvisionedProductEngineWorkflowResultResponse Prelude.Int
notifyUpdateProvisionedProductEngineWorkflowResultResponse_httpStatus = Lens.lens (\NotifyUpdateProvisionedProductEngineWorkflowResultResponse' {httpStatus} -> httpStatus) (\s@NotifyUpdateProvisionedProductEngineWorkflowResultResponse' {} a -> s {httpStatus = a} :: NotifyUpdateProvisionedProductEngineWorkflowResultResponse)

instance
  Prelude.NFData
    NotifyUpdateProvisionedProductEngineWorkflowResultResponse
  where
  rnf
    NotifyUpdateProvisionedProductEngineWorkflowResultResponse' {..} =
      Prelude.rnf httpStatus
