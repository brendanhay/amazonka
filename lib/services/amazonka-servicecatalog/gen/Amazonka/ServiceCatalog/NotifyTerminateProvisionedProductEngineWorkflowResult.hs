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
-- Module      : Amazonka.ServiceCatalog.NotifyTerminateProvisionedProductEngineWorkflowResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies the result of the terminate engine execution.
module Amazonka.ServiceCatalog.NotifyTerminateProvisionedProductEngineWorkflowResult
  ( -- * Creating a Request
    NotifyTerminateProvisionedProductEngineWorkflowResult (..),
    newNotifyTerminateProvisionedProductEngineWorkflowResult,

    -- * Request Lenses
    notifyTerminateProvisionedProductEngineWorkflowResult_failureReason,
    notifyTerminateProvisionedProductEngineWorkflowResult_workflowToken,
    notifyTerminateProvisionedProductEngineWorkflowResult_recordId,
    notifyTerminateProvisionedProductEngineWorkflowResult_status,
    notifyTerminateProvisionedProductEngineWorkflowResult_idempotencyToken,

    -- * Destructuring the Response
    NotifyTerminateProvisionedProductEngineWorkflowResultResponse (..),
    newNotifyTerminateProvisionedProductEngineWorkflowResultResponse,

    -- * Response Lenses
    notifyTerminateProvisionedProductEngineWorkflowResultResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newNotifyTerminateProvisionedProductEngineWorkflowResult' smart constructor.
data NotifyTerminateProvisionedProductEngineWorkflowResult = NotifyTerminateProvisionedProductEngineWorkflowResult'
  { -- | The reason why the terminate engine execution failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The encrypted contents of the terminate engine execution payload that
    -- Service Catalog sends after the Terraform product terminate workflow
    -- starts.
    workflowToken :: Prelude.Text,
    -- | The identifier of the record.
    recordId :: Prelude.Text,
    -- | The status of the terminate engine execution.
    status :: EngineWorkflowStatus,
    -- | The idempotency token that identifies the terminate engine execution.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyTerminateProvisionedProductEngineWorkflowResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'notifyTerminateProvisionedProductEngineWorkflowResult_failureReason' - The reason why the terminate engine execution failed.
--
-- 'workflowToken', 'notifyTerminateProvisionedProductEngineWorkflowResult_workflowToken' - The encrypted contents of the terminate engine execution payload that
-- Service Catalog sends after the Terraform product terminate workflow
-- starts.
--
-- 'recordId', 'notifyTerminateProvisionedProductEngineWorkflowResult_recordId' - The identifier of the record.
--
-- 'status', 'notifyTerminateProvisionedProductEngineWorkflowResult_status' - The status of the terminate engine execution.
--
-- 'idempotencyToken', 'notifyTerminateProvisionedProductEngineWorkflowResult_idempotencyToken' - The idempotency token that identifies the terminate engine execution.
newNotifyTerminateProvisionedProductEngineWorkflowResult ::
  -- | 'workflowToken'
  Prelude.Text ->
  -- | 'recordId'
  Prelude.Text ->
  -- | 'status'
  EngineWorkflowStatus ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  NotifyTerminateProvisionedProductEngineWorkflowResult
newNotifyTerminateProvisionedProductEngineWorkflowResult
  pWorkflowToken_
  pRecordId_
  pStatus_
  pIdempotencyToken_ =
    NotifyTerminateProvisionedProductEngineWorkflowResult'
      { failureReason =
          Prelude.Nothing,
        workflowToken =
          pWorkflowToken_,
        recordId =
          pRecordId_,
        status = pStatus_,
        idempotencyToken =
          pIdempotencyToken_
      }

-- | The reason why the terminate engine execution failed.
notifyTerminateProvisionedProductEngineWorkflowResult_failureReason :: Lens.Lens' NotifyTerminateProvisionedProductEngineWorkflowResult (Prelude.Maybe Prelude.Text)
notifyTerminateProvisionedProductEngineWorkflowResult_failureReason = Lens.lens (\NotifyTerminateProvisionedProductEngineWorkflowResult' {failureReason} -> failureReason) (\s@NotifyTerminateProvisionedProductEngineWorkflowResult' {} a -> s {failureReason = a} :: NotifyTerminateProvisionedProductEngineWorkflowResult)

-- | The encrypted contents of the terminate engine execution payload that
-- Service Catalog sends after the Terraform product terminate workflow
-- starts.
notifyTerminateProvisionedProductEngineWorkflowResult_workflowToken :: Lens.Lens' NotifyTerminateProvisionedProductEngineWorkflowResult Prelude.Text
notifyTerminateProvisionedProductEngineWorkflowResult_workflowToken = Lens.lens (\NotifyTerminateProvisionedProductEngineWorkflowResult' {workflowToken} -> workflowToken) (\s@NotifyTerminateProvisionedProductEngineWorkflowResult' {} a -> s {workflowToken = a} :: NotifyTerminateProvisionedProductEngineWorkflowResult)

-- | The identifier of the record.
notifyTerminateProvisionedProductEngineWorkflowResult_recordId :: Lens.Lens' NotifyTerminateProvisionedProductEngineWorkflowResult Prelude.Text
notifyTerminateProvisionedProductEngineWorkflowResult_recordId = Lens.lens (\NotifyTerminateProvisionedProductEngineWorkflowResult' {recordId} -> recordId) (\s@NotifyTerminateProvisionedProductEngineWorkflowResult' {} a -> s {recordId = a} :: NotifyTerminateProvisionedProductEngineWorkflowResult)

-- | The status of the terminate engine execution.
notifyTerminateProvisionedProductEngineWorkflowResult_status :: Lens.Lens' NotifyTerminateProvisionedProductEngineWorkflowResult EngineWorkflowStatus
notifyTerminateProvisionedProductEngineWorkflowResult_status = Lens.lens (\NotifyTerminateProvisionedProductEngineWorkflowResult' {status} -> status) (\s@NotifyTerminateProvisionedProductEngineWorkflowResult' {} a -> s {status = a} :: NotifyTerminateProvisionedProductEngineWorkflowResult)

-- | The idempotency token that identifies the terminate engine execution.
notifyTerminateProvisionedProductEngineWorkflowResult_idempotencyToken :: Lens.Lens' NotifyTerminateProvisionedProductEngineWorkflowResult Prelude.Text
notifyTerminateProvisionedProductEngineWorkflowResult_idempotencyToken = Lens.lens (\NotifyTerminateProvisionedProductEngineWorkflowResult' {idempotencyToken} -> idempotencyToken) (\s@NotifyTerminateProvisionedProductEngineWorkflowResult' {} a -> s {idempotencyToken = a} :: NotifyTerminateProvisionedProductEngineWorkflowResult)

instance
  Core.AWSRequest
    NotifyTerminateProvisionedProductEngineWorkflowResult
  where
  type
    AWSResponse
      NotifyTerminateProvisionedProductEngineWorkflowResult =
      NotifyTerminateProvisionedProductEngineWorkflowResultResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          NotifyTerminateProvisionedProductEngineWorkflowResultResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    NotifyTerminateProvisionedProductEngineWorkflowResult
  where
  hashWithSalt
    _salt
    NotifyTerminateProvisionedProductEngineWorkflowResult' {..} =
      _salt
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` workflowToken
        `Prelude.hashWithSalt` recordId
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` idempotencyToken

instance
  Prelude.NFData
    NotifyTerminateProvisionedProductEngineWorkflowResult
  where
  rnf
    NotifyTerminateProvisionedProductEngineWorkflowResult' {..} =
      Prelude.rnf failureReason
        `Prelude.seq` Prelude.rnf workflowToken
        `Prelude.seq` Prelude.rnf recordId
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf idempotencyToken

instance
  Data.ToHeaders
    NotifyTerminateProvisionedProductEngineWorkflowResult
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.NotifyTerminateProvisionedProductEngineWorkflowResult" ::
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
    NotifyTerminateProvisionedProductEngineWorkflowResult
  where
  toJSON
    NotifyTerminateProvisionedProductEngineWorkflowResult' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("FailureReason" Data..=) Prelude.<$> failureReason,
              Prelude.Just ("WorkflowToken" Data..= workflowToken),
              Prelude.Just ("RecordId" Data..= recordId),
              Prelude.Just ("Status" Data..= status),
              Prelude.Just
                ("IdempotencyToken" Data..= idempotencyToken)
            ]
        )

instance
  Data.ToPath
    NotifyTerminateProvisionedProductEngineWorkflowResult
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    NotifyTerminateProvisionedProductEngineWorkflowResult
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newNotifyTerminateProvisionedProductEngineWorkflowResultResponse' smart constructor.
data NotifyTerminateProvisionedProductEngineWorkflowResultResponse = NotifyTerminateProvisionedProductEngineWorkflowResultResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyTerminateProvisionedProductEngineWorkflowResultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'notifyTerminateProvisionedProductEngineWorkflowResultResponse_httpStatus' - The response's http status code.
newNotifyTerminateProvisionedProductEngineWorkflowResultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  NotifyTerminateProvisionedProductEngineWorkflowResultResponse
newNotifyTerminateProvisionedProductEngineWorkflowResultResponse
  pHttpStatus_ =
    NotifyTerminateProvisionedProductEngineWorkflowResultResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
notifyTerminateProvisionedProductEngineWorkflowResultResponse_httpStatus :: Lens.Lens' NotifyTerminateProvisionedProductEngineWorkflowResultResponse Prelude.Int
notifyTerminateProvisionedProductEngineWorkflowResultResponse_httpStatus = Lens.lens (\NotifyTerminateProvisionedProductEngineWorkflowResultResponse' {httpStatus} -> httpStatus) (\s@NotifyTerminateProvisionedProductEngineWorkflowResultResponse' {} a -> s {httpStatus = a} :: NotifyTerminateProvisionedProductEngineWorkflowResultResponse)

instance
  Prelude.NFData
    NotifyTerminateProvisionedProductEngineWorkflowResultResponse
  where
  rnf
    NotifyTerminateProvisionedProductEngineWorkflowResultResponse' {..} =
      Prelude.rnf httpStatus
