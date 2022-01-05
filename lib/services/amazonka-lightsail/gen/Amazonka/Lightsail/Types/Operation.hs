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
-- Module      : Amazonka.Lightsail.Types.Operation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Operation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.OperationStatus
import Amazonka.Lightsail.Types.OperationType
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes the API operation.
--
-- /See:/ 'newOperation' smart constructor.
data Operation = Operation'
  { -- | The status of the operation.
    status :: Prelude.Maybe OperationStatus,
    -- | Details about the operation (e.g., @Debian-1GB-Ohio-1@).
    operationDetails :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The timestamp when the operation was initialized (e.g.,
    -- @1479816991.349@).
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The resource name.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region and Availability Zone.
    location :: Prelude.Maybe ResourceLocation,
    -- | The timestamp when the status was changed (e.g., @1479816991.349@).
    statusChangedAt :: Prelude.Maybe Core.POSIX,
    -- | The error details.
    errorDetails :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The ID of the operation.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of operation.
    operationType :: Prelude.Maybe OperationType,
    -- | A Boolean value indicating whether the operation is terminal.
    isTerminal :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Operation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'operation_status' - The status of the operation.
--
-- 'operationDetails', 'operation_operationDetails' - Details about the operation (e.g., @Debian-1GB-Ohio-1@).
--
-- 'resourceType', 'operation_resourceType' - The resource type.
--
-- 'createdAt', 'operation_createdAt' - The timestamp when the operation was initialized (e.g.,
-- @1479816991.349@).
--
-- 'resourceName', 'operation_resourceName' - The resource name.
--
-- 'location', 'operation_location' - The AWS Region and Availability Zone.
--
-- 'statusChangedAt', 'operation_statusChangedAt' - The timestamp when the status was changed (e.g., @1479816991.349@).
--
-- 'errorDetails', 'operation_errorDetails' - The error details.
--
-- 'errorCode', 'operation_errorCode' - The error code.
--
-- 'id', 'operation_id' - The ID of the operation.
--
-- 'operationType', 'operation_operationType' - The type of operation.
--
-- 'isTerminal', 'operation_isTerminal' - A Boolean value indicating whether the operation is terminal.
newOperation ::
  Operation
newOperation =
  Operation'
    { status = Prelude.Nothing,
      operationDetails = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      location = Prelude.Nothing,
      statusChangedAt = Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      id = Prelude.Nothing,
      operationType = Prelude.Nothing,
      isTerminal = Prelude.Nothing
    }

-- | The status of the operation.
operation_status :: Lens.Lens' Operation (Prelude.Maybe OperationStatus)
operation_status = Lens.lens (\Operation' {status} -> status) (\s@Operation' {} a -> s {status = a} :: Operation)

-- | Details about the operation (e.g., @Debian-1GB-Ohio-1@).
operation_operationDetails :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_operationDetails = Lens.lens (\Operation' {operationDetails} -> operationDetails) (\s@Operation' {} a -> s {operationDetails = a} :: Operation)

-- | The resource type.
operation_resourceType :: Lens.Lens' Operation (Prelude.Maybe ResourceType)
operation_resourceType = Lens.lens (\Operation' {resourceType} -> resourceType) (\s@Operation' {} a -> s {resourceType = a} :: Operation)

-- | The timestamp when the operation was initialized (e.g.,
-- @1479816991.349@).
operation_createdAt :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_createdAt = Lens.lens (\Operation' {createdAt} -> createdAt) (\s@Operation' {} a -> s {createdAt = a} :: Operation) Prelude.. Lens.mapping Core._Time

-- | The resource name.
operation_resourceName :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_resourceName = Lens.lens (\Operation' {resourceName} -> resourceName) (\s@Operation' {} a -> s {resourceName = a} :: Operation)

-- | The AWS Region and Availability Zone.
operation_location :: Lens.Lens' Operation (Prelude.Maybe ResourceLocation)
operation_location = Lens.lens (\Operation' {location} -> location) (\s@Operation' {} a -> s {location = a} :: Operation)

-- | The timestamp when the status was changed (e.g., @1479816991.349@).
operation_statusChangedAt :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_statusChangedAt = Lens.lens (\Operation' {statusChangedAt} -> statusChangedAt) (\s@Operation' {} a -> s {statusChangedAt = a} :: Operation) Prelude.. Lens.mapping Core._Time

-- | The error details.
operation_errorDetails :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_errorDetails = Lens.lens (\Operation' {errorDetails} -> errorDetails) (\s@Operation' {} a -> s {errorDetails = a} :: Operation)

-- | The error code.
operation_errorCode :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_errorCode = Lens.lens (\Operation' {errorCode} -> errorCode) (\s@Operation' {} a -> s {errorCode = a} :: Operation)

-- | The ID of the operation.
operation_id :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_id = Lens.lens (\Operation' {id} -> id) (\s@Operation' {} a -> s {id = a} :: Operation)

-- | The type of operation.
operation_operationType :: Lens.Lens' Operation (Prelude.Maybe OperationType)
operation_operationType = Lens.lens (\Operation' {operationType} -> operationType) (\s@Operation' {} a -> s {operationType = a} :: Operation)

-- | A Boolean value indicating whether the operation is terminal.
operation_isTerminal :: Lens.Lens' Operation (Prelude.Maybe Prelude.Bool)
operation_isTerminal = Lens.lens (\Operation' {isTerminal} -> isTerminal) (\s@Operation' {} a -> s {isTerminal = a} :: Operation)

instance Core.FromJSON Operation where
  parseJSON =
    Core.withObject
      "Operation"
      ( \x ->
          Operation'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "operationDetails")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "resourceName")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "statusChangedAt")
            Prelude.<*> (x Core..:? "errorDetails")
            Prelude.<*> (x Core..:? "errorCode")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "operationType")
            Prelude.<*> (x Core..:? "isTerminal")
      )

instance Prelude.Hashable Operation where
  hashWithSalt _salt Operation' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` operationDetails
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` statusChangedAt
      `Prelude.hashWithSalt` errorDetails
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` operationType
      `Prelude.hashWithSalt` isTerminal

instance Prelude.NFData Operation where
  rnf Operation' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf operationDetails
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf statusChangedAt
      `Prelude.seq` Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf operationType
      `Prelude.seq` Prelude.rnf isTerminal
