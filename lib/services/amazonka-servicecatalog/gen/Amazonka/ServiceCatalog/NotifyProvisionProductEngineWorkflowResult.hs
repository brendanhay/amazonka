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
-- Module      : Amazonka.ServiceCatalog.NotifyProvisionProductEngineWorkflowResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies the result of the provisioning engine execution.
module Amazonka.ServiceCatalog.NotifyProvisionProductEngineWorkflowResult
  ( -- * Creating a Request
    NotifyProvisionProductEngineWorkflowResult (..),
    newNotifyProvisionProductEngineWorkflowResult,

    -- * Request Lenses
    notifyProvisionProductEngineWorkflowResult_failureReason,
    notifyProvisionProductEngineWorkflowResult_outputs,
    notifyProvisionProductEngineWorkflowResult_resourceIdentifier,
    notifyProvisionProductEngineWorkflowResult_workflowToken,
    notifyProvisionProductEngineWorkflowResult_recordId,
    notifyProvisionProductEngineWorkflowResult_status,
    notifyProvisionProductEngineWorkflowResult_idempotencyToken,

    -- * Destructuring the Response
    NotifyProvisionProductEngineWorkflowResultResponse (..),
    newNotifyProvisionProductEngineWorkflowResultResponse,

    -- * Response Lenses
    notifyProvisionProductEngineWorkflowResultResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newNotifyProvisionProductEngineWorkflowResult' smart constructor.
data NotifyProvisionProductEngineWorkflowResult = NotifyProvisionProductEngineWorkflowResult'
  { -- | The reason why the provisioning engine execution failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The output of the provisioning engine execution.
    outputs :: Prelude.Maybe [RecordOutput],
    -- | The ID for the provisioned product resources that are part of a resource
    -- group.
    resourceIdentifier :: Prelude.Maybe EngineWorkflowResourceIdentifier,
    -- | The encrypted contents of the provisioning engine execution payload that
    -- Service Catalog sends after the Terraform product provisioning workflow
    -- starts.
    workflowToken :: Prelude.Text,
    -- | The identifier of the record.
    recordId :: Prelude.Text,
    -- | The status of the provisioning engine execution.
    status :: EngineWorkflowStatus,
    -- | The idempotency token that identifies the provisioning engine execution.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyProvisionProductEngineWorkflowResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'notifyProvisionProductEngineWorkflowResult_failureReason' - The reason why the provisioning engine execution failed.
--
-- 'outputs', 'notifyProvisionProductEngineWorkflowResult_outputs' - The output of the provisioning engine execution.
--
-- 'resourceIdentifier', 'notifyProvisionProductEngineWorkflowResult_resourceIdentifier' - The ID for the provisioned product resources that are part of a resource
-- group.
--
-- 'workflowToken', 'notifyProvisionProductEngineWorkflowResult_workflowToken' - The encrypted contents of the provisioning engine execution payload that
-- Service Catalog sends after the Terraform product provisioning workflow
-- starts.
--
-- 'recordId', 'notifyProvisionProductEngineWorkflowResult_recordId' - The identifier of the record.
--
-- 'status', 'notifyProvisionProductEngineWorkflowResult_status' - The status of the provisioning engine execution.
--
-- 'idempotencyToken', 'notifyProvisionProductEngineWorkflowResult_idempotencyToken' - The idempotency token that identifies the provisioning engine execution.
newNotifyProvisionProductEngineWorkflowResult ::
  -- | 'workflowToken'
  Prelude.Text ->
  -- | 'recordId'
  Prelude.Text ->
  -- | 'status'
  EngineWorkflowStatus ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  NotifyProvisionProductEngineWorkflowResult
newNotifyProvisionProductEngineWorkflowResult
  pWorkflowToken_
  pRecordId_
  pStatus_
  pIdempotencyToken_ =
    NotifyProvisionProductEngineWorkflowResult'
      { failureReason =
          Prelude.Nothing,
        outputs = Prelude.Nothing,
        resourceIdentifier =
          Prelude.Nothing,
        workflowToken = pWorkflowToken_,
        recordId = pRecordId_,
        status = pStatus_,
        idempotencyToken =
          pIdempotencyToken_
      }

-- | The reason why the provisioning engine execution failed.
notifyProvisionProductEngineWorkflowResult_failureReason :: Lens.Lens' NotifyProvisionProductEngineWorkflowResult (Prelude.Maybe Prelude.Text)
notifyProvisionProductEngineWorkflowResult_failureReason = Lens.lens (\NotifyProvisionProductEngineWorkflowResult' {failureReason} -> failureReason) (\s@NotifyProvisionProductEngineWorkflowResult' {} a -> s {failureReason = a} :: NotifyProvisionProductEngineWorkflowResult)

-- | The output of the provisioning engine execution.
notifyProvisionProductEngineWorkflowResult_outputs :: Lens.Lens' NotifyProvisionProductEngineWorkflowResult (Prelude.Maybe [RecordOutput])
notifyProvisionProductEngineWorkflowResult_outputs = Lens.lens (\NotifyProvisionProductEngineWorkflowResult' {outputs} -> outputs) (\s@NotifyProvisionProductEngineWorkflowResult' {} a -> s {outputs = a} :: NotifyProvisionProductEngineWorkflowResult) Prelude.. Lens.mapping Lens.coerced

-- | The ID for the provisioned product resources that are part of a resource
-- group.
notifyProvisionProductEngineWorkflowResult_resourceIdentifier :: Lens.Lens' NotifyProvisionProductEngineWorkflowResult (Prelude.Maybe EngineWorkflowResourceIdentifier)
notifyProvisionProductEngineWorkflowResult_resourceIdentifier = Lens.lens (\NotifyProvisionProductEngineWorkflowResult' {resourceIdentifier} -> resourceIdentifier) (\s@NotifyProvisionProductEngineWorkflowResult' {} a -> s {resourceIdentifier = a} :: NotifyProvisionProductEngineWorkflowResult)

-- | The encrypted contents of the provisioning engine execution payload that
-- Service Catalog sends after the Terraform product provisioning workflow
-- starts.
notifyProvisionProductEngineWorkflowResult_workflowToken :: Lens.Lens' NotifyProvisionProductEngineWorkflowResult Prelude.Text
notifyProvisionProductEngineWorkflowResult_workflowToken = Lens.lens (\NotifyProvisionProductEngineWorkflowResult' {workflowToken} -> workflowToken) (\s@NotifyProvisionProductEngineWorkflowResult' {} a -> s {workflowToken = a} :: NotifyProvisionProductEngineWorkflowResult)

-- | The identifier of the record.
notifyProvisionProductEngineWorkflowResult_recordId :: Lens.Lens' NotifyProvisionProductEngineWorkflowResult Prelude.Text
notifyProvisionProductEngineWorkflowResult_recordId = Lens.lens (\NotifyProvisionProductEngineWorkflowResult' {recordId} -> recordId) (\s@NotifyProvisionProductEngineWorkflowResult' {} a -> s {recordId = a} :: NotifyProvisionProductEngineWorkflowResult)

-- | The status of the provisioning engine execution.
notifyProvisionProductEngineWorkflowResult_status :: Lens.Lens' NotifyProvisionProductEngineWorkflowResult EngineWorkflowStatus
notifyProvisionProductEngineWorkflowResult_status = Lens.lens (\NotifyProvisionProductEngineWorkflowResult' {status} -> status) (\s@NotifyProvisionProductEngineWorkflowResult' {} a -> s {status = a} :: NotifyProvisionProductEngineWorkflowResult)

-- | The idempotency token that identifies the provisioning engine execution.
notifyProvisionProductEngineWorkflowResult_idempotencyToken :: Lens.Lens' NotifyProvisionProductEngineWorkflowResult Prelude.Text
notifyProvisionProductEngineWorkflowResult_idempotencyToken = Lens.lens (\NotifyProvisionProductEngineWorkflowResult' {idempotencyToken} -> idempotencyToken) (\s@NotifyProvisionProductEngineWorkflowResult' {} a -> s {idempotencyToken = a} :: NotifyProvisionProductEngineWorkflowResult)

instance
  Core.AWSRequest
    NotifyProvisionProductEngineWorkflowResult
  where
  type
    AWSResponse
      NotifyProvisionProductEngineWorkflowResult =
      NotifyProvisionProductEngineWorkflowResultResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          NotifyProvisionProductEngineWorkflowResultResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    NotifyProvisionProductEngineWorkflowResult
  where
  hashWithSalt
    _salt
    NotifyProvisionProductEngineWorkflowResult' {..} =
      _salt
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` outputs
        `Prelude.hashWithSalt` resourceIdentifier
        `Prelude.hashWithSalt` workflowToken
        `Prelude.hashWithSalt` recordId
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` idempotencyToken

instance
  Prelude.NFData
    NotifyProvisionProductEngineWorkflowResult
  where
  rnf NotifyProvisionProductEngineWorkflowResult' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf workflowToken
      `Prelude.seq` Prelude.rnf recordId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf idempotencyToken

instance
  Data.ToHeaders
    NotifyProvisionProductEngineWorkflowResult
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.NotifyProvisionProductEngineWorkflowResult" ::
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
    NotifyProvisionProductEngineWorkflowResult
  where
  toJSON
    NotifyProvisionProductEngineWorkflowResult' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("FailureReason" Data..=) Prelude.<$> failureReason,
              ("Outputs" Data..=) Prelude.<$> outputs,
              ("ResourceIdentifier" Data..=)
                Prelude.<$> resourceIdentifier,
              Prelude.Just ("WorkflowToken" Data..= workflowToken),
              Prelude.Just ("RecordId" Data..= recordId),
              Prelude.Just ("Status" Data..= status),
              Prelude.Just
                ("IdempotencyToken" Data..= idempotencyToken)
            ]
        )

instance
  Data.ToPath
    NotifyProvisionProductEngineWorkflowResult
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    NotifyProvisionProductEngineWorkflowResult
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newNotifyProvisionProductEngineWorkflowResultResponse' smart constructor.
data NotifyProvisionProductEngineWorkflowResultResponse = NotifyProvisionProductEngineWorkflowResultResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyProvisionProductEngineWorkflowResultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'notifyProvisionProductEngineWorkflowResultResponse_httpStatus' - The response's http status code.
newNotifyProvisionProductEngineWorkflowResultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  NotifyProvisionProductEngineWorkflowResultResponse
newNotifyProvisionProductEngineWorkflowResultResponse
  pHttpStatus_ =
    NotifyProvisionProductEngineWorkflowResultResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
notifyProvisionProductEngineWorkflowResultResponse_httpStatus :: Lens.Lens' NotifyProvisionProductEngineWorkflowResultResponse Prelude.Int
notifyProvisionProductEngineWorkflowResultResponse_httpStatus = Lens.lens (\NotifyProvisionProductEngineWorkflowResultResponse' {httpStatus} -> httpStatus) (\s@NotifyProvisionProductEngineWorkflowResultResponse' {} a -> s {httpStatus = a} :: NotifyProvisionProductEngineWorkflowResultResponse)

instance
  Prelude.NFData
    NotifyProvisionProductEngineWorkflowResultResponse
  where
  rnf
    NotifyProvisionProductEngineWorkflowResultResponse' {..} =
      Prelude.rnf httpStatus
