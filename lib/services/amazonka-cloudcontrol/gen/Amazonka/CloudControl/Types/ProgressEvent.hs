{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudControl.Types.ProgressEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudControl.Types.ProgressEvent where

import Amazonka.CloudControl.Types.HandlerErrorCode
import Amazonka.CloudControl.Types.Operation
import Amazonka.CloudControl.Types.OperationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the current status of a resource operation request. For more
-- information, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-manage-requests.html Managing resource operation requests>
-- in the /Amazon Web Services Cloud Control API User Guide/.
--
-- /See:/ 'newProgressEvent' smart constructor.
data ProgressEvent = ProgressEvent'
  { -- | When to next request the status of this resource operation request.
    retryAfter :: Prelude.Maybe Core.POSIX,
    -- | A JSON string containing the resource model, consisting of each resource
    -- property and its current value.
    resourceModel :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The current status of the resource operation request.
    --
    -- -   @PENDING@: The resource operation hasn\'t yet started.
    --
    -- -   @IN_PROGRESS@: The resource operation is currently in progress.
    --
    -- -   @SUCCESS@: The resource operation has successfully completed.
    --
    -- -   @FAILED@: The resource operation has failed. Refer to the error code
    --     and status message for more information.
    --
    -- -   @CANCEL_IN_PROGRESS@: The resource operation is in the process of
    --     being canceled.
    --
    -- -   @CANCEL_COMPLETE@: The resource operation has been canceled.
    operationStatus :: Prelude.Maybe OperationStatus,
    -- | The name of the resource type used in the operation.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The unique token representing this resource operation request.
    --
    -- Use the @RequestToken@ with
    -- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
    -- to return the current status of a resource operation request.
    requestToken :: Prelude.Maybe Prelude.Text,
    -- | The primary identifier for the resource.
    --
    -- In some cases, the resource identifier may be available before the
    -- resource operation has reached a status of @SUCCESS@.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | For requests with a status of @FAILED@, the associated error code.
    --
    -- For error code definitions, see
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-test-contract-errors.html Handler error codes>
    -- in the /CloudFormation Command Line Interface User Guide for Extension
    -- Development/.
    errorCode :: Prelude.Maybe HandlerErrorCode,
    -- | When the resource operation request was initiated.
    eventTime :: Prelude.Maybe Core.POSIX,
    -- | Any message explaining the current status.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The resource operation type.
    operation :: Prelude.Maybe Operation
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProgressEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retryAfter', 'progressEvent_retryAfter' - When to next request the status of this resource operation request.
--
-- 'resourceModel', 'progressEvent_resourceModel' - A JSON string containing the resource model, consisting of each resource
-- property and its current value.
--
-- 'operationStatus', 'progressEvent_operationStatus' - The current status of the resource operation request.
--
-- -   @PENDING@: The resource operation hasn\'t yet started.
--
-- -   @IN_PROGRESS@: The resource operation is currently in progress.
--
-- -   @SUCCESS@: The resource operation has successfully completed.
--
-- -   @FAILED@: The resource operation has failed. Refer to the error code
--     and status message for more information.
--
-- -   @CANCEL_IN_PROGRESS@: The resource operation is in the process of
--     being canceled.
--
-- -   @CANCEL_COMPLETE@: The resource operation has been canceled.
--
-- 'typeName', 'progressEvent_typeName' - The name of the resource type used in the operation.
--
-- 'requestToken', 'progressEvent_requestToken' - The unique token representing this resource operation request.
--
-- Use the @RequestToken@ with
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- to return the current status of a resource operation request.
--
-- 'identifier', 'progressEvent_identifier' - The primary identifier for the resource.
--
-- In some cases, the resource identifier may be available before the
-- resource operation has reached a status of @SUCCESS@.
--
-- 'errorCode', 'progressEvent_errorCode' - For requests with a status of @FAILED@, the associated error code.
--
-- For error code definitions, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-test-contract-errors.html Handler error codes>
-- in the /CloudFormation Command Line Interface User Guide for Extension
-- Development/.
--
-- 'eventTime', 'progressEvent_eventTime' - When the resource operation request was initiated.
--
-- 'statusMessage', 'progressEvent_statusMessage' - Any message explaining the current status.
--
-- 'operation', 'progressEvent_operation' - The resource operation type.
newProgressEvent ::
  ProgressEvent
newProgressEvent =
  ProgressEvent'
    { retryAfter = Prelude.Nothing,
      resourceModel = Prelude.Nothing,
      operationStatus = Prelude.Nothing,
      typeName = Prelude.Nothing,
      requestToken = Prelude.Nothing,
      identifier = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      eventTime = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      operation = Prelude.Nothing
    }

-- | When to next request the status of this resource operation request.
progressEvent_retryAfter :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.UTCTime)
progressEvent_retryAfter = Lens.lens (\ProgressEvent' {retryAfter} -> retryAfter) (\s@ProgressEvent' {} a -> s {retryAfter = a} :: ProgressEvent) Prelude.. Lens.mapping Core._Time

-- | A JSON string containing the resource model, consisting of each resource
-- property and its current value.
progressEvent_resourceModel :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.Text)
progressEvent_resourceModel = Lens.lens (\ProgressEvent' {resourceModel} -> resourceModel) (\s@ProgressEvent' {} a -> s {resourceModel = a} :: ProgressEvent) Prelude.. Lens.mapping Core._Sensitive

-- | The current status of the resource operation request.
--
-- -   @PENDING@: The resource operation hasn\'t yet started.
--
-- -   @IN_PROGRESS@: The resource operation is currently in progress.
--
-- -   @SUCCESS@: The resource operation has successfully completed.
--
-- -   @FAILED@: The resource operation has failed. Refer to the error code
--     and status message for more information.
--
-- -   @CANCEL_IN_PROGRESS@: The resource operation is in the process of
--     being canceled.
--
-- -   @CANCEL_COMPLETE@: The resource operation has been canceled.
progressEvent_operationStatus :: Lens.Lens' ProgressEvent (Prelude.Maybe OperationStatus)
progressEvent_operationStatus = Lens.lens (\ProgressEvent' {operationStatus} -> operationStatus) (\s@ProgressEvent' {} a -> s {operationStatus = a} :: ProgressEvent)

-- | The name of the resource type used in the operation.
progressEvent_typeName :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.Text)
progressEvent_typeName = Lens.lens (\ProgressEvent' {typeName} -> typeName) (\s@ProgressEvent' {} a -> s {typeName = a} :: ProgressEvent)

-- | The unique token representing this resource operation request.
--
-- Use the @RequestToken@ with
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- to return the current status of a resource operation request.
progressEvent_requestToken :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.Text)
progressEvent_requestToken = Lens.lens (\ProgressEvent' {requestToken} -> requestToken) (\s@ProgressEvent' {} a -> s {requestToken = a} :: ProgressEvent)

-- | The primary identifier for the resource.
--
-- In some cases, the resource identifier may be available before the
-- resource operation has reached a status of @SUCCESS@.
progressEvent_identifier :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.Text)
progressEvent_identifier = Lens.lens (\ProgressEvent' {identifier} -> identifier) (\s@ProgressEvent' {} a -> s {identifier = a} :: ProgressEvent)

-- | For requests with a status of @FAILED@, the associated error code.
--
-- For error code definitions, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-test-contract-errors.html Handler error codes>
-- in the /CloudFormation Command Line Interface User Guide for Extension
-- Development/.
progressEvent_errorCode :: Lens.Lens' ProgressEvent (Prelude.Maybe HandlerErrorCode)
progressEvent_errorCode = Lens.lens (\ProgressEvent' {errorCode} -> errorCode) (\s@ProgressEvent' {} a -> s {errorCode = a} :: ProgressEvent)

-- | When the resource operation request was initiated.
progressEvent_eventTime :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.UTCTime)
progressEvent_eventTime = Lens.lens (\ProgressEvent' {eventTime} -> eventTime) (\s@ProgressEvent' {} a -> s {eventTime = a} :: ProgressEvent) Prelude.. Lens.mapping Core._Time

-- | Any message explaining the current status.
progressEvent_statusMessage :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.Text)
progressEvent_statusMessage = Lens.lens (\ProgressEvent' {statusMessage} -> statusMessage) (\s@ProgressEvent' {} a -> s {statusMessage = a} :: ProgressEvent)

-- | The resource operation type.
progressEvent_operation :: Lens.Lens' ProgressEvent (Prelude.Maybe Operation)
progressEvent_operation = Lens.lens (\ProgressEvent' {operation} -> operation) (\s@ProgressEvent' {} a -> s {operation = a} :: ProgressEvent)

instance Core.FromJSON ProgressEvent where
  parseJSON =
    Core.withObject
      "ProgressEvent"
      ( \x ->
          ProgressEvent'
            Prelude.<$> (x Core..:? "RetryAfter")
            Prelude.<*> (x Core..:? "ResourceModel")
            Prelude.<*> (x Core..:? "OperationStatus")
            Prelude.<*> (x Core..:? "TypeName")
            Prelude.<*> (x Core..:? "RequestToken")
            Prelude.<*> (x Core..:? "Identifier")
            Prelude.<*> (x Core..:? "ErrorCode")
            Prelude.<*> (x Core..:? "EventTime")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "Operation")
      )

instance Prelude.Hashable ProgressEvent where
  hashWithSalt _salt ProgressEvent' {..} =
    _salt `Prelude.hashWithSalt` retryAfter
      `Prelude.hashWithSalt` resourceModel
      `Prelude.hashWithSalt` operationStatus
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` requestToken
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` operation

instance Prelude.NFData ProgressEvent where
  rnf ProgressEvent' {..} =
    Prelude.rnf retryAfter
      `Prelude.seq` Prelude.rnf resourceModel
      `Prelude.seq` Prelude.rnf operationStatus
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf requestToken
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf eventTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf operation
