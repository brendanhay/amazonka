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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudControl.Types.ProgressEvent where

import Amazonka.CloudControl.Types.HandlerErrorCode
import Amazonka.CloudControl.Types.Operation
import Amazonka.CloudControl.Types.OperationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the current status of a resource operation request. For more
-- information, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-manage-requests.html Managing resource operation requests>
-- in the /Amazon Web Services Cloud Control API User Guide/.
--
-- /See:/ 'newProgressEvent' smart constructor.
data ProgressEvent = ProgressEvent'
  { -- | For requests with a status of @FAILED@, the associated error code.
    --
    -- For error code definitions, see
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-test-contract-errors.html Handler error codes>
    -- in the /CloudFormation Command Line Interface User Guide for Extension
    -- Development/.
    errorCode :: Prelude.Maybe HandlerErrorCode,
    -- | When the resource operation request was initiated.
    eventTime :: Prelude.Maybe Data.POSIX,
    -- | The primary identifier for the resource.
    --
    -- In some cases, the resource identifier may be available before the
    -- resource operation has reached a status of @SUCCESS@.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | The resource operation type.
    operation :: Prelude.Maybe Operation,
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
    -- | The unique token representing this resource operation request.
    --
    -- Use the @RequestToken@ with
    -- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
    -- to return the current status of a resource operation request.
    requestToken :: Prelude.Maybe Prelude.Text,
    -- | A JSON string containing the resource model, consisting of each resource
    -- property and its current value.
    resourceModel :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | When to next request the status of this resource operation request.
    retryAfter :: Prelude.Maybe Data.POSIX,
    -- | Any message explaining the current status.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource type used in the operation.
    typeName :: Prelude.Maybe Prelude.Text
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
-- 'errorCode', 'progressEvent_errorCode' - For requests with a status of @FAILED@, the associated error code.
--
-- For error code definitions, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-test-contract-errors.html Handler error codes>
-- in the /CloudFormation Command Line Interface User Guide for Extension
-- Development/.
--
-- 'eventTime', 'progressEvent_eventTime' - When the resource operation request was initiated.
--
-- 'identifier', 'progressEvent_identifier' - The primary identifier for the resource.
--
-- In some cases, the resource identifier may be available before the
-- resource operation has reached a status of @SUCCESS@.
--
-- 'operation', 'progressEvent_operation' - The resource operation type.
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
-- 'requestToken', 'progressEvent_requestToken' - The unique token representing this resource operation request.
--
-- Use the @RequestToken@ with
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- to return the current status of a resource operation request.
--
-- 'resourceModel', 'progressEvent_resourceModel' - A JSON string containing the resource model, consisting of each resource
-- property and its current value.
--
-- 'retryAfter', 'progressEvent_retryAfter' - When to next request the status of this resource operation request.
--
-- 'statusMessage', 'progressEvent_statusMessage' - Any message explaining the current status.
--
-- 'typeName', 'progressEvent_typeName' - The name of the resource type used in the operation.
newProgressEvent ::
  ProgressEvent
newProgressEvent =
  ProgressEvent'
    { errorCode = Prelude.Nothing,
      eventTime = Prelude.Nothing,
      identifier = Prelude.Nothing,
      operation = Prelude.Nothing,
      operationStatus = Prelude.Nothing,
      requestToken = Prelude.Nothing,
      resourceModel = Prelude.Nothing,
      retryAfter = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      typeName = Prelude.Nothing
    }

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
progressEvent_eventTime = Lens.lens (\ProgressEvent' {eventTime} -> eventTime) (\s@ProgressEvent' {} a -> s {eventTime = a} :: ProgressEvent) Prelude.. Lens.mapping Data._Time

-- | The primary identifier for the resource.
--
-- In some cases, the resource identifier may be available before the
-- resource operation has reached a status of @SUCCESS@.
progressEvent_identifier :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.Text)
progressEvent_identifier = Lens.lens (\ProgressEvent' {identifier} -> identifier) (\s@ProgressEvent' {} a -> s {identifier = a} :: ProgressEvent)

-- | The resource operation type.
progressEvent_operation :: Lens.Lens' ProgressEvent (Prelude.Maybe Operation)
progressEvent_operation = Lens.lens (\ProgressEvent' {operation} -> operation) (\s@ProgressEvent' {} a -> s {operation = a} :: ProgressEvent)

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

-- | The unique token representing this resource operation request.
--
-- Use the @RequestToken@ with
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- to return the current status of a resource operation request.
progressEvent_requestToken :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.Text)
progressEvent_requestToken = Lens.lens (\ProgressEvent' {requestToken} -> requestToken) (\s@ProgressEvent' {} a -> s {requestToken = a} :: ProgressEvent)

-- | A JSON string containing the resource model, consisting of each resource
-- property and its current value.
progressEvent_resourceModel :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.Text)
progressEvent_resourceModel = Lens.lens (\ProgressEvent' {resourceModel} -> resourceModel) (\s@ProgressEvent' {} a -> s {resourceModel = a} :: ProgressEvent) Prelude.. Lens.mapping Data._Sensitive

-- | When to next request the status of this resource operation request.
progressEvent_retryAfter :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.UTCTime)
progressEvent_retryAfter = Lens.lens (\ProgressEvent' {retryAfter} -> retryAfter) (\s@ProgressEvent' {} a -> s {retryAfter = a} :: ProgressEvent) Prelude.. Lens.mapping Data._Time

-- | Any message explaining the current status.
progressEvent_statusMessage :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.Text)
progressEvent_statusMessage = Lens.lens (\ProgressEvent' {statusMessage} -> statusMessage) (\s@ProgressEvent' {} a -> s {statusMessage = a} :: ProgressEvent)

-- | The name of the resource type used in the operation.
progressEvent_typeName :: Lens.Lens' ProgressEvent (Prelude.Maybe Prelude.Text)
progressEvent_typeName = Lens.lens (\ProgressEvent' {typeName} -> typeName) (\s@ProgressEvent' {} a -> s {typeName = a} :: ProgressEvent)

instance Data.FromJSON ProgressEvent where
  parseJSON =
    Data.withObject
      "ProgressEvent"
      ( \x ->
          ProgressEvent'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "EventTime")
            Prelude.<*> (x Data..:? "Identifier")
            Prelude.<*> (x Data..:? "Operation")
            Prelude.<*> (x Data..:? "OperationStatus")
            Prelude.<*> (x Data..:? "RequestToken")
            Prelude.<*> (x Data..:? "ResourceModel")
            Prelude.<*> (x Data..:? "RetryAfter")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "TypeName")
      )

instance Prelude.Hashable ProgressEvent where
  hashWithSalt _salt ProgressEvent' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` operationStatus
      `Prelude.hashWithSalt` requestToken
      `Prelude.hashWithSalt` resourceModel
      `Prelude.hashWithSalt` retryAfter
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` typeName

instance Prelude.NFData ProgressEvent where
  rnf ProgressEvent' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf eventTime `Prelude.seq`
        Prelude.rnf identifier `Prelude.seq`
          Prelude.rnf operation `Prelude.seq`
            Prelude.rnf operationStatus `Prelude.seq`
              Prelude.rnf requestToken `Prelude.seq`
                Prelude.rnf resourceModel `Prelude.seq`
                  Prelude.rnf retryAfter `Prelude.seq`
                    Prelude.rnf statusMessage `Prelude.seq`
                      Prelude.rnf typeName
