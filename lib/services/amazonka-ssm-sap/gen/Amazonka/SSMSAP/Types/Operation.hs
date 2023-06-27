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
-- Module      : Amazonka.SSMSAP.Types.Operation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.Operation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.OperationStatus

-- | The operations performed by AWS Systems Manager for SAP.
--
-- /See:/ 'newOperation' smart constructor.
data Operation = Operation'
  { -- | The end time of the operation.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the operation.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time at which the operation was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The properties of the operation.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the operation.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the operation.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type of the operation.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The start time of the operation.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the operation.
    status :: Prelude.Maybe OperationStatus,
    -- | The status message of the operation.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The type of the operation.
    type' :: Prelude.Maybe Prelude.Text
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
-- 'endTime', 'operation_endTime' - The end time of the operation.
--
-- 'id', 'operation_id' - The ID of the operation.
--
-- 'lastUpdatedTime', 'operation_lastUpdatedTime' - The time at which the operation was last updated.
--
-- 'properties', 'operation_properties' - The properties of the operation.
--
-- 'resourceArn', 'operation_resourceArn' - The Amazon Resource Name (ARN) of the operation.
--
-- 'resourceId', 'operation_resourceId' - The resource ID of the operation.
--
-- 'resourceType', 'operation_resourceType' - The resource type of the operation.
--
-- 'startTime', 'operation_startTime' - The start time of the operation.
--
-- 'status', 'operation_status' - The status of the operation.
--
-- 'statusMessage', 'operation_statusMessage' - The status message of the operation.
--
-- 'type'', 'operation_type' - The type of the operation.
newOperation ::
  Operation
newOperation =
  Operation'
    { endTime = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      properties = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The end time of the operation.
operation_endTime :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_endTime = Lens.lens (\Operation' {endTime} -> endTime) (\s@Operation' {} a -> s {endTime = a} :: Operation) Prelude.. Lens.mapping Data._Time

-- | The ID of the operation.
operation_id :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_id = Lens.lens (\Operation' {id} -> id) (\s@Operation' {} a -> s {id = a} :: Operation)

-- | The time at which the operation was last updated.
operation_lastUpdatedTime :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_lastUpdatedTime = Lens.lens (\Operation' {lastUpdatedTime} -> lastUpdatedTime) (\s@Operation' {} a -> s {lastUpdatedTime = a} :: Operation) Prelude.. Lens.mapping Data._Time

-- | The properties of the operation.
operation_properties :: Lens.Lens' Operation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
operation_properties = Lens.lens (\Operation' {properties} -> properties) (\s@Operation' {} a -> s {properties = a} :: Operation) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the operation.
operation_resourceArn :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_resourceArn = Lens.lens (\Operation' {resourceArn} -> resourceArn) (\s@Operation' {} a -> s {resourceArn = a} :: Operation)

-- | The resource ID of the operation.
operation_resourceId :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_resourceId = Lens.lens (\Operation' {resourceId} -> resourceId) (\s@Operation' {} a -> s {resourceId = a} :: Operation)

-- | The resource type of the operation.
operation_resourceType :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_resourceType = Lens.lens (\Operation' {resourceType} -> resourceType) (\s@Operation' {} a -> s {resourceType = a} :: Operation)

-- | The start time of the operation.
operation_startTime :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_startTime = Lens.lens (\Operation' {startTime} -> startTime) (\s@Operation' {} a -> s {startTime = a} :: Operation) Prelude.. Lens.mapping Data._Time

-- | The status of the operation.
operation_status :: Lens.Lens' Operation (Prelude.Maybe OperationStatus)
operation_status = Lens.lens (\Operation' {status} -> status) (\s@Operation' {} a -> s {status = a} :: Operation)

-- | The status message of the operation.
operation_statusMessage :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_statusMessage = Lens.lens (\Operation' {statusMessage} -> statusMessage) (\s@Operation' {} a -> s {statusMessage = a} :: Operation)

-- | The type of the operation.
operation_type :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_type = Lens.lens (\Operation' {type'} -> type') (\s@Operation' {} a -> s {type' = a} :: Operation)

instance Data.FromJSON Operation where
  parseJSON =
    Data.withObject
      "Operation"
      ( \x ->
          Operation'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Properties" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Operation where
  hashWithSalt _salt Operation' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Operation where
  rnf Operation' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf type'
