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
-- Module      : Amazonka.MQ.Types.DataReplicationMetadataOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.DataReplicationMetadataOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types.DataReplicationCounterpart
import qualified Amazonka.Prelude as Prelude

-- | The replication details of the data replication-enabled broker. Only
-- returned if dataReplicationMode or pendingDataReplicationMode is set to
-- CRDR.
--
-- /See:/ 'newDataReplicationMetadataOutput' smart constructor.
data DataReplicationMetadataOutput = DataReplicationMetadataOutput'
  { -- | Describes the replica\/primary broker. Only returned if this broker is
    -- currently set as a primary or replica in the broker\'s
    -- dataReplicationRole property.
    dataReplicationCounterpart :: Prelude.Maybe DataReplicationCounterpart,
    -- | Defines the role of this broker in a data replication pair. When a
    -- replica broker is promoted to primary, this role is interchanged.
    dataReplicationRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataReplicationMetadataOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataReplicationCounterpart', 'dataReplicationMetadataOutput_dataReplicationCounterpart' - Describes the replica\/primary broker. Only returned if this broker is
-- currently set as a primary or replica in the broker\'s
-- dataReplicationRole property.
--
-- 'dataReplicationRole', 'dataReplicationMetadataOutput_dataReplicationRole' - Defines the role of this broker in a data replication pair. When a
-- replica broker is promoted to primary, this role is interchanged.
newDataReplicationMetadataOutput ::
  -- | 'dataReplicationRole'
  Prelude.Text ->
  DataReplicationMetadataOutput
newDataReplicationMetadataOutput
  pDataReplicationRole_ =
    DataReplicationMetadataOutput'
      { dataReplicationCounterpart =
          Prelude.Nothing,
        dataReplicationRole = pDataReplicationRole_
      }

-- | Describes the replica\/primary broker. Only returned if this broker is
-- currently set as a primary or replica in the broker\'s
-- dataReplicationRole property.
dataReplicationMetadataOutput_dataReplicationCounterpart :: Lens.Lens' DataReplicationMetadataOutput (Prelude.Maybe DataReplicationCounterpart)
dataReplicationMetadataOutput_dataReplicationCounterpart = Lens.lens (\DataReplicationMetadataOutput' {dataReplicationCounterpart} -> dataReplicationCounterpart) (\s@DataReplicationMetadataOutput' {} a -> s {dataReplicationCounterpart = a} :: DataReplicationMetadataOutput)

-- | Defines the role of this broker in a data replication pair. When a
-- replica broker is promoted to primary, this role is interchanged.
dataReplicationMetadataOutput_dataReplicationRole :: Lens.Lens' DataReplicationMetadataOutput Prelude.Text
dataReplicationMetadataOutput_dataReplicationRole = Lens.lens (\DataReplicationMetadataOutput' {dataReplicationRole} -> dataReplicationRole) (\s@DataReplicationMetadataOutput' {} a -> s {dataReplicationRole = a} :: DataReplicationMetadataOutput)

instance Data.FromJSON DataReplicationMetadataOutput where
  parseJSON =
    Data.withObject
      "DataReplicationMetadataOutput"
      ( \x ->
          DataReplicationMetadataOutput'
            Prelude.<$> (x Data..:? "dataReplicationCounterpart")
            Prelude.<*> (x Data..: "dataReplicationRole")
      )

instance
  Prelude.Hashable
    DataReplicationMetadataOutput
  where
  hashWithSalt _salt DataReplicationMetadataOutput' {..} =
    _salt
      `Prelude.hashWithSalt` dataReplicationCounterpart
      `Prelude.hashWithSalt` dataReplicationRole

instance Prelude.NFData DataReplicationMetadataOutput where
  rnf DataReplicationMetadataOutput' {..} =
    Prelude.rnf dataReplicationCounterpart
      `Prelude.seq` Prelude.rnf dataReplicationRole
