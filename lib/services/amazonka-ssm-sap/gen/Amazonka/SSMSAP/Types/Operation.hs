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

-- |
--
-- /See:/ 'newOperation' smart constructor.
data Operation = Operation'
  { endTime :: Prelude.Maybe Data.POSIX,
    id :: Prelude.Maybe Prelude.Text,
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    resourceArn :: Prelude.Maybe Prelude.Text,
    resourceId :: Prelude.Maybe Prelude.Text,
    resourceType :: Prelude.Maybe Prelude.Text,
    startTime :: Prelude.Maybe Data.POSIX,
    status :: Prelude.Maybe OperationStatus,
    statusMessage :: Prelude.Maybe Prelude.Text,
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
-- 'endTime', 'operation_endTime' -
--
-- 'id', 'operation_id' -
--
-- 'lastUpdatedTime', 'operation_lastUpdatedTime' -
--
-- 'properties', 'operation_properties' -
--
-- 'resourceArn', 'operation_resourceArn' -
--
-- 'resourceId', 'operation_resourceId' -
--
-- 'resourceType', 'operation_resourceType' -
--
-- 'startTime', 'operation_startTime' -
--
-- 'status', 'operation_status' -
--
-- 'statusMessage', 'operation_statusMessage' -
--
-- 'type'', 'operation_type' -
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

operation_endTime :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_endTime = Lens.lens (\Operation' {endTime} -> endTime) (\s@Operation' {} a -> s {endTime = a} :: Operation) Prelude.. Lens.mapping Data._Time

operation_id :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_id = Lens.lens (\Operation' {id} -> id) (\s@Operation' {} a -> s {id = a} :: Operation)

operation_lastUpdatedTime :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_lastUpdatedTime = Lens.lens (\Operation' {lastUpdatedTime} -> lastUpdatedTime) (\s@Operation' {} a -> s {lastUpdatedTime = a} :: Operation) Prelude.. Lens.mapping Data._Time

operation_properties :: Lens.Lens' Operation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
operation_properties = Lens.lens (\Operation' {properties} -> properties) (\s@Operation' {} a -> s {properties = a} :: Operation) Prelude.. Lens.mapping Lens.coerced

operation_resourceArn :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_resourceArn = Lens.lens (\Operation' {resourceArn} -> resourceArn) (\s@Operation' {} a -> s {resourceArn = a} :: Operation)

operation_resourceId :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_resourceId = Lens.lens (\Operation' {resourceId} -> resourceId) (\s@Operation' {} a -> s {resourceId = a} :: Operation)

operation_resourceType :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_resourceType = Lens.lens (\Operation' {resourceType} -> resourceType) (\s@Operation' {} a -> s {resourceType = a} :: Operation)

operation_startTime :: Lens.Lens' Operation (Prelude.Maybe Prelude.UTCTime)
operation_startTime = Lens.lens (\Operation' {startTime} -> startTime) (\s@Operation' {} a -> s {startTime = a} :: Operation) Prelude.. Lens.mapping Data._Time

operation_status :: Lens.Lens' Operation (Prelude.Maybe OperationStatus)
operation_status = Lens.lens (\Operation' {status} -> status) (\s@Operation' {} a -> s {status = a} :: Operation)

operation_statusMessage :: Lens.Lens' Operation (Prelude.Maybe Prelude.Text)
operation_statusMessage = Lens.lens (\Operation' {statusMessage} -> statusMessage) (\s@Operation' {} a -> s {statusMessage = a} :: Operation)

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
