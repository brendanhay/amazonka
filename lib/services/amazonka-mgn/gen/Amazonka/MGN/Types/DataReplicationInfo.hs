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
-- Module      : Amazonka.MGN.Types.DataReplicationInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.DataReplicationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types.DataReplicationError
import Amazonka.MGN.Types.DataReplicationInfoReplicatedDisk
import Amazonka.MGN.Types.DataReplicationInitiation
import Amazonka.MGN.Types.DataReplicationState
import qualified Amazonka.Prelude as Prelude

-- | Request data replication info.
--
-- /See:/ 'newDataReplicationInfo' smart constructor.
data DataReplicationInfo = DataReplicationInfo'
  { -- | Error in obtaining data replication info.
    dataReplicationError :: Prelude.Maybe DataReplicationError,
    -- | Request to query data replication lag duration.
    lagDuration :: Prelude.Maybe Prelude.Text,
    -- | Request to query whether data replication has been initiated.
    dataReplicationInitiation :: Prelude.Maybe DataReplicationInitiation,
    -- | Request to query disks replicated.
    replicatedDisks :: Prelude.Maybe [DataReplicationInfoReplicatedDisk],
    -- | Request to query data replication last snapshot time.
    lastSnapshotDateTime :: Prelude.Maybe Prelude.Text,
    -- | Request to query the data replication state.
    dataReplicationState :: Prelude.Maybe DataReplicationState,
    -- | Request to query the time when data replication will be complete.
    etaDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataReplicationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataReplicationError', 'dataReplicationInfo_dataReplicationError' - Error in obtaining data replication info.
--
-- 'lagDuration', 'dataReplicationInfo_lagDuration' - Request to query data replication lag duration.
--
-- 'dataReplicationInitiation', 'dataReplicationInfo_dataReplicationInitiation' - Request to query whether data replication has been initiated.
--
-- 'replicatedDisks', 'dataReplicationInfo_replicatedDisks' - Request to query disks replicated.
--
-- 'lastSnapshotDateTime', 'dataReplicationInfo_lastSnapshotDateTime' - Request to query data replication last snapshot time.
--
-- 'dataReplicationState', 'dataReplicationInfo_dataReplicationState' - Request to query the data replication state.
--
-- 'etaDateTime', 'dataReplicationInfo_etaDateTime' - Request to query the time when data replication will be complete.
newDataReplicationInfo ::
  DataReplicationInfo
newDataReplicationInfo =
  DataReplicationInfo'
    { dataReplicationError =
        Prelude.Nothing,
      lagDuration = Prelude.Nothing,
      dataReplicationInitiation = Prelude.Nothing,
      replicatedDisks = Prelude.Nothing,
      lastSnapshotDateTime = Prelude.Nothing,
      dataReplicationState = Prelude.Nothing,
      etaDateTime = Prelude.Nothing
    }

-- | Error in obtaining data replication info.
dataReplicationInfo_dataReplicationError :: Lens.Lens' DataReplicationInfo (Prelude.Maybe DataReplicationError)
dataReplicationInfo_dataReplicationError = Lens.lens (\DataReplicationInfo' {dataReplicationError} -> dataReplicationError) (\s@DataReplicationInfo' {} a -> s {dataReplicationError = a} :: DataReplicationInfo)

-- | Request to query data replication lag duration.
dataReplicationInfo_lagDuration :: Lens.Lens' DataReplicationInfo (Prelude.Maybe Prelude.Text)
dataReplicationInfo_lagDuration = Lens.lens (\DataReplicationInfo' {lagDuration} -> lagDuration) (\s@DataReplicationInfo' {} a -> s {lagDuration = a} :: DataReplicationInfo)

-- | Request to query whether data replication has been initiated.
dataReplicationInfo_dataReplicationInitiation :: Lens.Lens' DataReplicationInfo (Prelude.Maybe DataReplicationInitiation)
dataReplicationInfo_dataReplicationInitiation = Lens.lens (\DataReplicationInfo' {dataReplicationInitiation} -> dataReplicationInitiation) (\s@DataReplicationInfo' {} a -> s {dataReplicationInitiation = a} :: DataReplicationInfo)

-- | Request to query disks replicated.
dataReplicationInfo_replicatedDisks :: Lens.Lens' DataReplicationInfo (Prelude.Maybe [DataReplicationInfoReplicatedDisk])
dataReplicationInfo_replicatedDisks = Lens.lens (\DataReplicationInfo' {replicatedDisks} -> replicatedDisks) (\s@DataReplicationInfo' {} a -> s {replicatedDisks = a} :: DataReplicationInfo) Prelude.. Lens.mapping Lens.coerced

-- | Request to query data replication last snapshot time.
dataReplicationInfo_lastSnapshotDateTime :: Lens.Lens' DataReplicationInfo (Prelude.Maybe Prelude.Text)
dataReplicationInfo_lastSnapshotDateTime = Lens.lens (\DataReplicationInfo' {lastSnapshotDateTime} -> lastSnapshotDateTime) (\s@DataReplicationInfo' {} a -> s {lastSnapshotDateTime = a} :: DataReplicationInfo)

-- | Request to query the data replication state.
dataReplicationInfo_dataReplicationState :: Lens.Lens' DataReplicationInfo (Prelude.Maybe DataReplicationState)
dataReplicationInfo_dataReplicationState = Lens.lens (\DataReplicationInfo' {dataReplicationState} -> dataReplicationState) (\s@DataReplicationInfo' {} a -> s {dataReplicationState = a} :: DataReplicationInfo)

-- | Request to query the time when data replication will be complete.
dataReplicationInfo_etaDateTime :: Lens.Lens' DataReplicationInfo (Prelude.Maybe Prelude.Text)
dataReplicationInfo_etaDateTime = Lens.lens (\DataReplicationInfo' {etaDateTime} -> etaDateTime) (\s@DataReplicationInfo' {} a -> s {etaDateTime = a} :: DataReplicationInfo)

instance Core.FromJSON DataReplicationInfo where
  parseJSON =
    Core.withObject
      "DataReplicationInfo"
      ( \x ->
          DataReplicationInfo'
            Prelude.<$> (x Core..:? "dataReplicationError")
            Prelude.<*> (x Core..:? "lagDuration")
            Prelude.<*> (x Core..:? "dataReplicationInitiation")
            Prelude.<*> ( x Core..:? "replicatedDisks"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "lastSnapshotDateTime")
            Prelude.<*> (x Core..:? "dataReplicationState")
            Prelude.<*> (x Core..:? "etaDateTime")
      )

instance Prelude.Hashable DataReplicationInfo where
  hashWithSalt _salt DataReplicationInfo' {..} =
    _salt `Prelude.hashWithSalt` dataReplicationError
      `Prelude.hashWithSalt` lagDuration
      `Prelude.hashWithSalt` dataReplicationInitiation
      `Prelude.hashWithSalt` replicatedDisks
      `Prelude.hashWithSalt` lastSnapshotDateTime
      `Prelude.hashWithSalt` dataReplicationState
      `Prelude.hashWithSalt` etaDateTime

instance Prelude.NFData DataReplicationInfo where
  rnf DataReplicationInfo' {..} =
    Prelude.rnf dataReplicationError
      `Prelude.seq` Prelude.rnf lagDuration
      `Prelude.seq` Prelude.rnf dataReplicationInitiation
      `Prelude.seq` Prelude.rnf replicatedDisks
      `Prelude.seq` Prelude.rnf lastSnapshotDateTime
      `Prelude.seq` Prelude.rnf dataReplicationState
      `Prelude.seq` Prelude.rnf etaDateTime
