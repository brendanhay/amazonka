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
-- Module      : Amazonka.AppFlow.Types.DestinationFieldProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DestinationFieldProperties where

import Amazonka.AppFlow.Types.WriteOperationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that can be applied to a field when connector is being
-- used as a destination.
--
-- /See:/ 'newDestinationFieldProperties' smart constructor.
data DestinationFieldProperties = DestinationFieldProperties'
  { -- | Specifies whether the field can be updated during an @UPDATE@ or
    -- @UPSERT@ write operation.
    isUpdatable :: Prelude.Maybe Prelude.Bool,
    -- | A list of supported write operations. For each write operation listed,
    -- this field can be used in @idFieldNames@ when that write operation is
    -- present as a destination option.
    supportedWriteOperations :: Prelude.Maybe [WriteOperationType],
    -- | Specifies if the destination field can have a null value.
    isNullable :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the field can use the default value during a Create
    -- operation.
    isDefaultedOnCreate :: Prelude.Maybe Prelude.Bool,
    -- | Specifies if the flow run can either insert new rows in the destination
    -- field if they do not already exist, or update them if they do.
    isUpsertable :: Prelude.Maybe Prelude.Bool,
    -- | Specifies if the destination field can be created by the current user.
    isCreatable :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationFieldProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isUpdatable', 'destinationFieldProperties_isUpdatable' - Specifies whether the field can be updated during an @UPDATE@ or
-- @UPSERT@ write operation.
--
-- 'supportedWriteOperations', 'destinationFieldProperties_supportedWriteOperations' - A list of supported write operations. For each write operation listed,
-- this field can be used in @idFieldNames@ when that write operation is
-- present as a destination option.
--
-- 'isNullable', 'destinationFieldProperties_isNullable' - Specifies if the destination field can have a null value.
--
-- 'isDefaultedOnCreate', 'destinationFieldProperties_isDefaultedOnCreate' - Specifies whether the field can use the default value during a Create
-- operation.
--
-- 'isUpsertable', 'destinationFieldProperties_isUpsertable' - Specifies if the flow run can either insert new rows in the destination
-- field if they do not already exist, or update them if they do.
--
-- 'isCreatable', 'destinationFieldProperties_isCreatable' - Specifies if the destination field can be created by the current user.
newDestinationFieldProperties ::
  DestinationFieldProperties
newDestinationFieldProperties =
  DestinationFieldProperties'
    { isUpdatable =
        Prelude.Nothing,
      supportedWriteOperations = Prelude.Nothing,
      isNullable = Prelude.Nothing,
      isDefaultedOnCreate = Prelude.Nothing,
      isUpsertable = Prelude.Nothing,
      isCreatable = Prelude.Nothing
    }

-- | Specifies whether the field can be updated during an @UPDATE@ or
-- @UPSERT@ write operation.
destinationFieldProperties_isUpdatable :: Lens.Lens' DestinationFieldProperties (Prelude.Maybe Prelude.Bool)
destinationFieldProperties_isUpdatable = Lens.lens (\DestinationFieldProperties' {isUpdatable} -> isUpdatable) (\s@DestinationFieldProperties' {} a -> s {isUpdatable = a} :: DestinationFieldProperties)

-- | A list of supported write operations. For each write operation listed,
-- this field can be used in @idFieldNames@ when that write operation is
-- present as a destination option.
destinationFieldProperties_supportedWriteOperations :: Lens.Lens' DestinationFieldProperties (Prelude.Maybe [WriteOperationType])
destinationFieldProperties_supportedWriteOperations = Lens.lens (\DestinationFieldProperties' {supportedWriteOperations} -> supportedWriteOperations) (\s@DestinationFieldProperties' {} a -> s {supportedWriteOperations = a} :: DestinationFieldProperties) Prelude.. Lens.mapping Lens.coerced

-- | Specifies if the destination field can have a null value.
destinationFieldProperties_isNullable :: Lens.Lens' DestinationFieldProperties (Prelude.Maybe Prelude.Bool)
destinationFieldProperties_isNullable = Lens.lens (\DestinationFieldProperties' {isNullable} -> isNullable) (\s@DestinationFieldProperties' {} a -> s {isNullable = a} :: DestinationFieldProperties)

-- | Specifies whether the field can use the default value during a Create
-- operation.
destinationFieldProperties_isDefaultedOnCreate :: Lens.Lens' DestinationFieldProperties (Prelude.Maybe Prelude.Bool)
destinationFieldProperties_isDefaultedOnCreate = Lens.lens (\DestinationFieldProperties' {isDefaultedOnCreate} -> isDefaultedOnCreate) (\s@DestinationFieldProperties' {} a -> s {isDefaultedOnCreate = a} :: DestinationFieldProperties)

-- | Specifies if the flow run can either insert new rows in the destination
-- field if they do not already exist, or update them if they do.
destinationFieldProperties_isUpsertable :: Lens.Lens' DestinationFieldProperties (Prelude.Maybe Prelude.Bool)
destinationFieldProperties_isUpsertable = Lens.lens (\DestinationFieldProperties' {isUpsertable} -> isUpsertable) (\s@DestinationFieldProperties' {} a -> s {isUpsertable = a} :: DestinationFieldProperties)

-- | Specifies if the destination field can be created by the current user.
destinationFieldProperties_isCreatable :: Lens.Lens' DestinationFieldProperties (Prelude.Maybe Prelude.Bool)
destinationFieldProperties_isCreatable = Lens.lens (\DestinationFieldProperties' {isCreatable} -> isCreatable) (\s@DestinationFieldProperties' {} a -> s {isCreatable = a} :: DestinationFieldProperties)

instance Data.FromJSON DestinationFieldProperties where
  parseJSON =
    Data.withObject
      "DestinationFieldProperties"
      ( \x ->
          DestinationFieldProperties'
            Prelude.<$> (x Data..:? "isUpdatable")
            Prelude.<*> ( x Data..:? "supportedWriteOperations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "isNullable")
            Prelude.<*> (x Data..:? "isDefaultedOnCreate")
            Prelude.<*> (x Data..:? "isUpsertable")
            Prelude.<*> (x Data..:? "isCreatable")
      )

instance Prelude.Hashable DestinationFieldProperties where
  hashWithSalt _salt DestinationFieldProperties' {..} =
    _salt `Prelude.hashWithSalt` isUpdatable
      `Prelude.hashWithSalt` supportedWriteOperations
      `Prelude.hashWithSalt` isNullable
      `Prelude.hashWithSalt` isDefaultedOnCreate
      `Prelude.hashWithSalt` isUpsertable
      `Prelude.hashWithSalt` isCreatable

instance Prelude.NFData DestinationFieldProperties where
  rnf DestinationFieldProperties' {..} =
    Prelude.rnf isUpdatable
      `Prelude.seq` Prelude.rnf supportedWriteOperations
      `Prelude.seq` Prelude.rnf isNullable
      `Prelude.seq` Prelude.rnf isDefaultedOnCreate
      `Prelude.seq` Prelude.rnf isUpsertable
      `Prelude.seq` Prelude.rnf isCreatable
