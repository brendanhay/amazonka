{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.OperationStatus
import Network.AWS.Lightsail.Types.OperationType
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import qualified Network.AWS.Prelude as Prelude

-- | Describes the API operation.
--
-- /See:/ 'newOperation' smart constructor.
data Operation = Operation'
  { -- | Details about the operation (e.g., @Debian-1GB-Ohio-1@).
    operationDetails :: Prelude.Maybe Prelude.Text,
    -- | The status of the operation.
    status :: Prelude.Maybe OperationStatus,
    -- | A Boolean value indicating whether the operation is terminal.
    isTerminal :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp when the operation was initialized (e.g.,
    -- @1479816991.349@).
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the operation.
    id :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The timestamp when the status was changed (e.g., @1479816991.349@).
    statusChangedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The AWS Region and Availability Zone.
    location :: Prelude.Maybe ResourceLocation,
    -- | The resource name.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The type of operation.
    operationType :: Prelude.Maybe OperationType,
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error details.
    errorDetails :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { operationDetails = Prelude.Nothing,
      status = Prelude.Nothing,
      isTerminal = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      statusChangedAt = Prelude.Nothing,
      location = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      operationType = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorDetails = Prelude.Nothing
    }

-- | Details about the operation (e.g., @Debian-1GB-Ohio-1@).
operation_operationDetails :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_operationDetails = Lens.lens (\Operation' {operationDetails} -> operationDetails) (\s@Operation' {} a -> s {operationDetails = a} :: Operation)

-- | The status of the operation.
operation_status :: Lens.Lens' Operation (Prelude.Maybe OperationStatus)
operation_status = Lens.lens (\Operation' {status} -> status) (\s@Operation' {} a -> s {status = a} :: Operation)

-- | A Boolean value indicating whether the operation is terminal.
operation_isTerminal :: Lens.Lens' Operation (Prelude.Maybe Prelude.Bool)
operation_isTerminal = Lens.lens (\Operation' {isTerminal} -> isTerminal) (\s@Operation' {} a -> s {isTerminal = a} :: Operation)

-- | The timestamp when the operation was initialized (e.g.,
-- @1479816991.349@).
operation_createdAt :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_createdAt = Lens.lens (\Operation' {createdAt} -> createdAt) (\s@Operation' {} a -> s {createdAt = a} :: Operation) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the operation.
operation_id :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_id = Lens.lens (\Operation' {id} -> id) (\s@Operation' {} a -> s {id = a} :: Operation)

-- | The resource type.
operation_resourceType :: Lens.Lens' Operation (Prelude.Maybe ResourceType)
operation_resourceType = Lens.lens (\Operation' {resourceType} -> resourceType) (\s@Operation' {} a -> s {resourceType = a} :: Operation)

-- | The timestamp when the status was changed (e.g., @1479816991.349@).
operation_statusChangedAt :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_statusChangedAt = Lens.lens (\Operation' {statusChangedAt} -> statusChangedAt) (\s@Operation' {} a -> s {statusChangedAt = a} :: Operation) Prelude.. Lens.mapping Prelude._Time

-- | The AWS Region and Availability Zone.
operation_location :: Lens.Lens' Operation (Prelude.Maybe ResourceLocation)
operation_location = Lens.lens (\Operation' {location} -> location) (\s@Operation' {} a -> s {location = a} :: Operation)

-- | The resource name.
operation_resourceName :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_resourceName = Lens.lens (\Operation' {resourceName} -> resourceName) (\s@Operation' {} a -> s {resourceName = a} :: Operation)

-- | The type of operation.
operation_operationType :: Lens.Lens' Operation (Prelude.Maybe OperationType)
operation_operationType = Lens.lens (\Operation' {operationType} -> operationType) (\s@Operation' {} a -> s {operationType = a} :: Operation)

-- | The error code.
operation_errorCode :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_errorCode = Lens.lens (\Operation' {errorCode} -> errorCode) (\s@Operation' {} a -> s {errorCode = a} :: Operation)

-- | The error details.
operation_errorDetails :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_errorDetails = Lens.lens (\Operation' {errorDetails} -> errorDetails) (\s@Operation' {} a -> s {errorDetails = a} :: Operation)

instance Prelude.FromJSON Operation where
  parseJSON =
    Prelude.withObject
      "Operation"
      ( \x ->
          Operation'
            Prelude.<$> (x Prelude..:? "operationDetails")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "isTerminal")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "statusChangedAt")
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..:? "resourceName")
            Prelude.<*> (x Prelude..:? "operationType")
            Prelude.<*> (x Prelude..:? "errorCode")
            Prelude.<*> (x Prelude..:? "errorDetails")
      )

instance Prelude.Hashable Operation

instance Prelude.NFData Operation
