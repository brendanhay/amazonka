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
-- Module      : Network.AWS.Lightsail.Types.Operation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Operation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.OperationStatus
import Network.AWS.Lightsail.Types.OperationType
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType

-- | Describes the API operation.
--
-- /See:/ 'newOperation' smart constructor.
data Operation = Operation'
  { -- | Details about the operation (e.g., @Debian-1GB-Ohio-1@).
    operationDetails :: Core.Maybe Core.Text,
    -- | The status of the operation.
    status :: Core.Maybe OperationStatus,
    -- | A Boolean value indicating whether the operation is terminal.
    isTerminal :: Core.Maybe Core.Bool,
    -- | The timestamp when the operation was initialized (e.g.,
    -- @1479816991.349@).
    createdAt :: Core.Maybe Core.POSIX,
    -- | The ID of the operation.
    id :: Core.Maybe Core.Text,
    -- | The resource type.
    resourceType :: Core.Maybe ResourceType,
    -- | The timestamp when the status was changed (e.g., @1479816991.349@).
    statusChangedAt :: Core.Maybe Core.POSIX,
    -- | The AWS Region and Availability Zone.
    location :: Core.Maybe ResourceLocation,
    -- | The resource name.
    resourceName :: Core.Maybe Core.Text,
    -- | The type of operation.
    operationType :: Core.Maybe OperationType,
    -- | The error code.
    errorCode :: Core.Maybe Core.Text,
    -- | The error details.
    errorDetails :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Operation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationDetails', 'operation_operationDetails' - Details about the operation (e.g., @Debian-1GB-Ohio-1@).
--
-- 'status', 'operation_status' - The status of the operation.
--
-- 'isTerminal', 'operation_isTerminal' - A Boolean value indicating whether the operation is terminal.
--
-- 'createdAt', 'operation_createdAt' - The timestamp when the operation was initialized (e.g.,
-- @1479816991.349@).
--
-- 'id', 'operation_id' - The ID of the operation.
--
-- 'resourceType', 'operation_resourceType' - The resource type.
--
-- 'statusChangedAt', 'operation_statusChangedAt' - The timestamp when the status was changed (e.g., @1479816991.349@).
--
-- 'location', 'operation_location' - The AWS Region and Availability Zone.
--
-- 'resourceName', 'operation_resourceName' - The resource name.
--
-- 'operationType', 'operation_operationType' - The type of operation.
--
-- 'errorCode', 'operation_errorCode' - The error code.
--
-- 'errorDetails', 'operation_errorDetails' - The error details.
newOperation ::
  Operation
newOperation =
  Operation'
    { operationDetails = Core.Nothing,
      status = Core.Nothing,
      isTerminal = Core.Nothing,
      createdAt = Core.Nothing,
      id = Core.Nothing,
      resourceType = Core.Nothing,
      statusChangedAt = Core.Nothing,
      location = Core.Nothing,
      resourceName = Core.Nothing,
      operationType = Core.Nothing,
      errorCode = Core.Nothing,
      errorDetails = Core.Nothing
    }

-- | Details about the operation (e.g., @Debian-1GB-Ohio-1@).
operation_operationDetails :: Lens.Lens' Operation (Core.Maybe Core.Text)
operation_operationDetails = Lens.lens (\Operation' {operationDetails} -> operationDetails) (\s@Operation' {} a -> s {operationDetails = a} :: Operation)

-- | The status of the operation.
operation_status :: Lens.Lens' Operation (Core.Maybe OperationStatus)
operation_status = Lens.lens (\Operation' {status} -> status) (\s@Operation' {} a -> s {status = a} :: Operation)

-- | A Boolean value indicating whether the operation is terminal.
operation_isTerminal :: Lens.Lens' Operation (Core.Maybe Core.Bool)
operation_isTerminal = Lens.lens (\Operation' {isTerminal} -> isTerminal) (\s@Operation' {} a -> s {isTerminal = a} :: Operation)

-- | The timestamp when the operation was initialized (e.g.,
-- @1479816991.349@).
operation_createdAt :: Lens.Lens' Operation (Core.Maybe Core.UTCTime)
operation_createdAt = Lens.lens (\Operation' {createdAt} -> createdAt) (\s@Operation' {} a -> s {createdAt = a} :: Operation) Core.. Lens.mapping Core._Time

-- | The ID of the operation.
operation_id :: Lens.Lens' Operation (Core.Maybe Core.Text)
operation_id = Lens.lens (\Operation' {id} -> id) (\s@Operation' {} a -> s {id = a} :: Operation)

-- | The resource type.
operation_resourceType :: Lens.Lens' Operation (Core.Maybe ResourceType)
operation_resourceType = Lens.lens (\Operation' {resourceType} -> resourceType) (\s@Operation' {} a -> s {resourceType = a} :: Operation)

-- | The timestamp when the status was changed (e.g., @1479816991.349@).
operation_statusChangedAt :: Lens.Lens' Operation (Core.Maybe Core.UTCTime)
operation_statusChangedAt = Lens.lens (\Operation' {statusChangedAt} -> statusChangedAt) (\s@Operation' {} a -> s {statusChangedAt = a} :: Operation) Core.. Lens.mapping Core._Time

-- | The AWS Region and Availability Zone.
operation_location :: Lens.Lens' Operation (Core.Maybe ResourceLocation)
operation_location = Lens.lens (\Operation' {location} -> location) (\s@Operation' {} a -> s {location = a} :: Operation)

-- | The resource name.
operation_resourceName :: Lens.Lens' Operation (Core.Maybe Core.Text)
operation_resourceName = Lens.lens (\Operation' {resourceName} -> resourceName) (\s@Operation' {} a -> s {resourceName = a} :: Operation)

-- | The type of operation.
operation_operationType :: Lens.Lens' Operation (Core.Maybe OperationType)
operation_operationType = Lens.lens (\Operation' {operationType} -> operationType) (\s@Operation' {} a -> s {operationType = a} :: Operation)

-- | The error code.
operation_errorCode :: Lens.Lens' Operation (Core.Maybe Core.Text)
operation_errorCode = Lens.lens (\Operation' {errorCode} -> errorCode) (\s@Operation' {} a -> s {errorCode = a} :: Operation)

-- | The error details.
operation_errorDetails :: Lens.Lens' Operation (Core.Maybe Core.Text)
operation_errorDetails = Lens.lens (\Operation' {errorDetails} -> errorDetails) (\s@Operation' {} a -> s {errorDetails = a} :: Operation)

instance Core.FromJSON Operation where
  parseJSON =
    Core.withObject
      "Operation"
      ( \x ->
          Operation'
            Core.<$> (x Core..:? "operationDetails")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "isTerminal")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "statusChangedAt")
            Core.<*> (x Core..:? "location")
            Core.<*> (x Core..:? "resourceName")
            Core.<*> (x Core..:? "operationType")
            Core.<*> (x Core..:? "errorCode")
            Core.<*> (x Core..:? "errorDetails")
      )

instance Core.Hashable Operation

instance Core.NFData Operation
