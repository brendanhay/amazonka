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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Operation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types.OperationStatus
import Amazonka.Lightsail.Types.OperationType
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes the API operation.
--
-- /See:/ 'newOperation' smart constructor.
data Operation = Operation'
  { -- | The resource type.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The timestamp when the status was changed (e.g., @1479816991.349@).
    statusChangedAt :: Prelude.Maybe Core.POSIX,
    -- | A Boolean value indicating whether the operation is terminal.
    isTerminal :: Prelude.Maybe Prelude.Bool,
    -- | The resource name.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The type of operation.
    operationType :: Prelude.Maybe OperationType,
    -- | The error details.
    errorDetails :: Prelude.Maybe Prelude.Text,
    -- | The status of the operation.
    status :: Prelude.Maybe OperationStatus,
    -- | The ID of the operation.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region and Availability Zone.
    location :: Prelude.Maybe ResourceLocation,
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | Details about the operation (e.g., @Debian-1GB-Ohio-1@).
    operationDetails :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the operation was initialized (e.g.,
    -- @1479816991.349@).
    createdAt :: Prelude.Maybe Core.POSIX
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
-- 'resourceType', 'operation_resourceType' - The resource type.
--
-- 'statusChangedAt', 'operation_statusChangedAt' - The timestamp when the status was changed (e.g., @1479816991.349@).
--
-- 'isTerminal', 'operation_isTerminal' - A Boolean value indicating whether the operation is terminal.
--
-- 'resourceName', 'operation_resourceName' - The resource name.
--
-- 'operationType', 'operation_operationType' - The type of operation.
--
-- 'errorDetails', 'operation_errorDetails' - The error details.
--
-- 'status', 'operation_status' - The status of the operation.
--
-- 'id', 'operation_id' - The ID of the operation.
--
-- 'location', 'operation_location' - The Amazon Web Services Region and Availability Zone.
--
-- 'errorCode', 'operation_errorCode' - The error code.
--
-- 'operationDetails', 'operation_operationDetails' - Details about the operation (e.g., @Debian-1GB-Ohio-1@).
--
-- 'createdAt', 'operation_createdAt' - The timestamp when the operation was initialized (e.g.,
-- @1479816991.349@).
newOperation ::
  Operation
newOperation =
  Operation'
    { resourceType = Prelude.Nothing,
      statusChangedAt = Prelude.Nothing,
      isTerminal = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      operationType = Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      location = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      operationDetails = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The resource type.
operation_resourceType :: Lens.Lens' Operation (Prelude.Maybe ResourceType)
operation_resourceType = Lens.lens (\Operation' {resourceType} -> resourceType) (\s@Operation' {} a -> s {resourceType = a} :: Operation)

-- | The timestamp when the status was changed (e.g., @1479816991.349@).
operation_statusChangedAt :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_statusChangedAt = Lens.lens (\Operation' {statusChangedAt} -> statusChangedAt) (\s@Operation' {} a -> s {statusChangedAt = a} :: Operation) Prelude.. Lens.mapping Core._Time

-- | A Boolean value indicating whether the operation is terminal.
operation_isTerminal :: Lens.Lens' Operation (Prelude.Maybe Prelude.Bool)
operation_isTerminal = Lens.lens (\Operation' {isTerminal} -> isTerminal) (\s@Operation' {} a -> s {isTerminal = a} :: Operation)

-- | The resource name.
operation_resourceName :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_resourceName = Lens.lens (\Operation' {resourceName} -> resourceName) (\s@Operation' {} a -> s {resourceName = a} :: Operation)

-- | The type of operation.
operation_operationType :: Lens.Lens' Operation (Prelude.Maybe OperationType)
operation_operationType = Lens.lens (\Operation' {operationType} -> operationType) (\s@Operation' {} a -> s {operationType = a} :: Operation)

-- | The error details.
operation_errorDetails :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_errorDetails = Lens.lens (\Operation' {errorDetails} -> errorDetails) (\s@Operation' {} a -> s {errorDetails = a} :: Operation)

-- | The status of the operation.
operation_status :: Lens.Lens' Operation (Prelude.Maybe OperationStatus)
operation_status = Lens.lens (\Operation' {status} -> status) (\s@Operation' {} a -> s {status = a} :: Operation)

-- | The ID of the operation.
operation_id :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_id = Lens.lens (\Operation' {id} -> id) (\s@Operation' {} a -> s {id = a} :: Operation)

-- | The Amazon Web Services Region and Availability Zone.
operation_location :: Lens.Lens' Operation (Prelude.Maybe ResourceLocation)
operation_location = Lens.lens (\Operation' {location} -> location) (\s@Operation' {} a -> s {location = a} :: Operation)

-- | The error code.
operation_errorCode :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_errorCode = Lens.lens (\Operation' {errorCode} -> errorCode) (\s@Operation' {} a -> s {errorCode = a} :: Operation)

-- | Details about the operation (e.g., @Debian-1GB-Ohio-1@).
operation_operationDetails :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_operationDetails = Lens.lens (\Operation' {operationDetails} -> operationDetails) (\s@Operation' {} a -> s {operationDetails = a} :: Operation)

-- | The timestamp when the operation was initialized (e.g.,
-- @1479816991.349@).
operation_createdAt :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_createdAt = Lens.lens (\Operation' {createdAt} -> createdAt) (\s@Operation' {} a -> s {createdAt = a} :: Operation) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Operation where
  parseJSON =
    Core.withObject
      "Operation"
      ( \x ->
          Operation'
            Prelude.<$> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "statusChangedAt")
            Prelude.<*> (x Core..:? "isTerminal")
            Prelude.<*> (x Core..:? "resourceName")
            Prelude.<*> (x Core..:? "operationType")
            Prelude.<*> (x Core..:? "errorDetails")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "errorCode")
            Prelude.<*> (x Core..:? "operationDetails")
            Prelude.<*> (x Core..:? "createdAt")
      )

instance Prelude.Hashable Operation where
  hashWithSalt _salt Operation' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` statusChangedAt
      `Prelude.hashWithSalt` isTerminal
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` operationType
      `Prelude.hashWithSalt` errorDetails
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` operationDetails
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData Operation where
  rnf Operation' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf statusChangedAt
      `Prelude.seq` Prelude.rnf isTerminal
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf operationType
      `Prelude.seq` Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf operationDetails
      `Prelude.seq` Prelude.rnf createdAt
