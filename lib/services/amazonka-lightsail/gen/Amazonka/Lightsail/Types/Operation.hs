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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Operation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.OperationStatus
import Amazonka.Lightsail.Types.OperationType
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes the API operation.
--
-- /See:/ 'newOperation' smart constructor.
data Operation = Operation'
  { -- | The timestamp when the operation was initialized (e.g.,
    -- @1479816991.349@).
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error details.
    errorDetails :: Prelude.Maybe Prelude.Text,
    -- | The ID of the operation.
    id :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the operation is terminal.
    isTerminal :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services Region and Availability Zone.
    location :: Prelude.Maybe ResourceLocation,
    -- | Details about the operation (e.g., @Debian-1GB-Ohio-1@).
    operationDetails :: Prelude.Maybe Prelude.Text,
    -- | The type of operation.
    operationType :: Prelude.Maybe OperationType,
    -- | The resource name.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The status of the operation.
    status :: Prelude.Maybe OperationStatus,
    -- | The timestamp when the status was changed (e.g., @1479816991.349@).
    statusChangedAt :: Prelude.Maybe Data.POSIX
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
-- 'createdAt', 'operation_createdAt' - The timestamp when the operation was initialized (e.g.,
-- @1479816991.349@).
--
-- 'errorCode', 'operation_errorCode' - The error code.
--
-- 'errorDetails', 'operation_errorDetails' - The error details.
--
-- 'id', 'operation_id' - The ID of the operation.
--
-- 'isTerminal', 'operation_isTerminal' - A Boolean value indicating whether the operation is terminal.
--
-- 'location', 'operation_location' - The Amazon Web Services Region and Availability Zone.
--
-- 'operationDetails', 'operation_operationDetails' - Details about the operation (e.g., @Debian-1GB-Ohio-1@).
--
-- 'operationType', 'operation_operationType' - The type of operation.
--
-- 'resourceName', 'operation_resourceName' - The resource name.
--
-- 'resourceType', 'operation_resourceType' - The resource type.
--
-- 'status', 'operation_status' - The status of the operation.
--
-- 'statusChangedAt', 'operation_statusChangedAt' - The timestamp when the status was changed (e.g., @1479816991.349@).
newOperation ::
  Operation
newOperation =
  Operation'
    { createdAt = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      id = Prelude.Nothing,
      isTerminal = Prelude.Nothing,
      location = Prelude.Nothing,
      operationDetails = Prelude.Nothing,
      operationType = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      status = Prelude.Nothing,
      statusChangedAt = Prelude.Nothing
    }

-- | The timestamp when the operation was initialized (e.g.,
-- @1479816991.349@).
operation_createdAt :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_createdAt = Lens.lens (\Operation' {createdAt} -> createdAt) (\s@Operation' {} a -> s {createdAt = a} :: Operation) Prelude.. Lens.mapping Data._Time

-- | The error code.
operation_errorCode :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_errorCode = Lens.lens (\Operation' {errorCode} -> errorCode) (\s@Operation' {} a -> s {errorCode = a} :: Operation)

-- | The error details.
operation_errorDetails :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_errorDetails = Lens.lens (\Operation' {errorDetails} -> errorDetails) (\s@Operation' {} a -> s {errorDetails = a} :: Operation)

-- | The ID of the operation.
operation_id :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_id = Lens.lens (\Operation' {id} -> id) (\s@Operation' {} a -> s {id = a} :: Operation)

-- | A Boolean value indicating whether the operation is terminal.
operation_isTerminal :: Lens.Lens' Operation (Prelude.Maybe Prelude.Bool)
operation_isTerminal = Lens.lens (\Operation' {isTerminal} -> isTerminal) (\s@Operation' {} a -> s {isTerminal = a} :: Operation)

-- | The Amazon Web Services Region and Availability Zone.
operation_location :: Lens.Lens' Operation (Prelude.Maybe ResourceLocation)
operation_location = Lens.lens (\Operation' {location} -> location) (\s@Operation' {} a -> s {location = a} :: Operation)

-- | Details about the operation (e.g., @Debian-1GB-Ohio-1@).
operation_operationDetails :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_operationDetails = Lens.lens (\Operation' {operationDetails} -> operationDetails) (\s@Operation' {} a -> s {operationDetails = a} :: Operation)

-- | The type of operation.
operation_operationType :: Lens.Lens' Operation (Prelude.Maybe OperationType)
operation_operationType = Lens.lens (\Operation' {operationType} -> operationType) (\s@Operation' {} a -> s {operationType = a} :: Operation)

-- | The resource name.
operation_resourceName :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_resourceName = Lens.lens (\Operation' {resourceName} -> resourceName) (\s@Operation' {} a -> s {resourceName = a} :: Operation)

-- | The resource type.
operation_resourceType :: Lens.Lens' Operation (Prelude.Maybe ResourceType)
operation_resourceType = Lens.lens (\Operation' {resourceType} -> resourceType) (\s@Operation' {} a -> s {resourceType = a} :: Operation)

-- | The status of the operation.
operation_status :: Lens.Lens' Operation (Prelude.Maybe OperationStatus)
operation_status = Lens.lens (\Operation' {status} -> status) (\s@Operation' {} a -> s {status = a} :: Operation)

-- | The timestamp when the status was changed (e.g., @1479816991.349@).
operation_statusChangedAt :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_statusChangedAt = Lens.lens (\Operation' {statusChangedAt} -> statusChangedAt) (\s@Operation' {} a -> s {statusChangedAt = a} :: Operation) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Operation where
  parseJSON =
    Data.withObject
      "Operation"
      ( \x ->
          Operation'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorDetails")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "isTerminal")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "operationDetails")
            Prelude.<*> (x Data..:? "operationType")
            Prelude.<*> (x Data..:? "resourceName")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusChangedAt")
      )

instance Prelude.Hashable Operation where
  hashWithSalt _salt Operation' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorDetails
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` isTerminal
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` operationDetails
      `Prelude.hashWithSalt` operationType
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusChangedAt

instance Prelude.NFData Operation where
  rnf Operation' {..} =
    Prelude.rnf createdAt `Prelude.seq`
      Prelude.rnf errorCode `Prelude.seq`
        Prelude.rnf errorDetails `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf isTerminal `Prelude.seq`
              Prelude.rnf location `Prelude.seq`
                Prelude.rnf operationDetails `Prelude.seq`
                  Prelude.rnf operationType `Prelude.seq`
                    Prelude.rnf resourceName `Prelude.seq`
                      Prelude.rnf resourceType `Prelude.seq`
                        Prelude.rnf status `Prelude.seq`
                          Prelude.rnf statusChangedAt
