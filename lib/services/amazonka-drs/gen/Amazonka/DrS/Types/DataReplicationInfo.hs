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
-- Module      : Amazonka.DrS.Types.DataReplicationInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.DataReplicationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.DataReplicationError
import Amazonka.DrS.Types.DataReplicationInfoReplicatedDisk
import Amazonka.DrS.Types.DataReplicationInitiation
import Amazonka.DrS.Types.DataReplicationState
import qualified Amazonka.Prelude as Prelude

-- | Information about Data Replication
--
-- /See:/ 'newDataReplicationInfo' smart constructor.
data DataReplicationInfo = DataReplicationInfo'
  { -- | Error in data replication.
    dataReplicationError :: Prelude.Maybe DataReplicationError,
    -- | Data replication lag duration.
    lagDuration :: Prelude.Maybe Prelude.Text,
    -- | Information about whether the data replication has been initiated.
    dataReplicationInitiation :: Prelude.Maybe DataReplicationInitiation,
    -- | The disks that should be replicated.
    replicatedDisks :: Prelude.Maybe [DataReplicationInfoReplicatedDisk],
    -- | The state of the data replication.
    dataReplicationState :: Prelude.Maybe DataReplicationState,
    -- | An estimate of when the data replication will be completed.
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
-- 'dataReplicationError', 'dataReplicationInfo_dataReplicationError' - Error in data replication.
--
-- 'lagDuration', 'dataReplicationInfo_lagDuration' - Data replication lag duration.
--
-- 'dataReplicationInitiation', 'dataReplicationInfo_dataReplicationInitiation' - Information about whether the data replication has been initiated.
--
-- 'replicatedDisks', 'dataReplicationInfo_replicatedDisks' - The disks that should be replicated.
--
-- 'dataReplicationState', 'dataReplicationInfo_dataReplicationState' - The state of the data replication.
--
-- 'etaDateTime', 'dataReplicationInfo_etaDateTime' - An estimate of when the data replication will be completed.
newDataReplicationInfo ::
  DataReplicationInfo
newDataReplicationInfo =
  DataReplicationInfo'
    { dataReplicationError =
        Prelude.Nothing,
      lagDuration = Prelude.Nothing,
      dataReplicationInitiation = Prelude.Nothing,
      replicatedDisks = Prelude.Nothing,
      dataReplicationState = Prelude.Nothing,
      etaDateTime = Prelude.Nothing
    }

-- | Error in data replication.
dataReplicationInfo_dataReplicationError :: Lens.Lens' DataReplicationInfo (Prelude.Maybe DataReplicationError)
dataReplicationInfo_dataReplicationError = Lens.lens (\DataReplicationInfo' {dataReplicationError} -> dataReplicationError) (\s@DataReplicationInfo' {} a -> s {dataReplicationError = a} :: DataReplicationInfo)

-- | Data replication lag duration.
dataReplicationInfo_lagDuration :: Lens.Lens' DataReplicationInfo (Prelude.Maybe Prelude.Text)
dataReplicationInfo_lagDuration = Lens.lens (\DataReplicationInfo' {lagDuration} -> lagDuration) (\s@DataReplicationInfo' {} a -> s {lagDuration = a} :: DataReplicationInfo)

-- | Information about whether the data replication has been initiated.
dataReplicationInfo_dataReplicationInitiation :: Lens.Lens' DataReplicationInfo (Prelude.Maybe DataReplicationInitiation)
dataReplicationInfo_dataReplicationInitiation = Lens.lens (\DataReplicationInfo' {dataReplicationInitiation} -> dataReplicationInitiation) (\s@DataReplicationInfo' {} a -> s {dataReplicationInitiation = a} :: DataReplicationInfo)

-- | The disks that should be replicated.
dataReplicationInfo_replicatedDisks :: Lens.Lens' DataReplicationInfo (Prelude.Maybe [DataReplicationInfoReplicatedDisk])
dataReplicationInfo_replicatedDisks = Lens.lens (\DataReplicationInfo' {replicatedDisks} -> replicatedDisks) (\s@DataReplicationInfo' {} a -> s {replicatedDisks = a} :: DataReplicationInfo) Prelude.. Lens.mapping Lens.coerced

-- | The state of the data replication.
dataReplicationInfo_dataReplicationState :: Lens.Lens' DataReplicationInfo (Prelude.Maybe DataReplicationState)
dataReplicationInfo_dataReplicationState = Lens.lens (\DataReplicationInfo' {dataReplicationState} -> dataReplicationState) (\s@DataReplicationInfo' {} a -> s {dataReplicationState = a} :: DataReplicationInfo)

-- | An estimate of when the data replication will be completed.
dataReplicationInfo_etaDateTime :: Lens.Lens' DataReplicationInfo (Prelude.Maybe Prelude.Text)
dataReplicationInfo_etaDateTime = Lens.lens (\DataReplicationInfo' {etaDateTime} -> etaDateTime) (\s@DataReplicationInfo' {} a -> s {etaDateTime = a} :: DataReplicationInfo)

instance Data.FromJSON DataReplicationInfo where
  parseJSON =
    Data.withObject
      "DataReplicationInfo"
      ( \x ->
          DataReplicationInfo'
            Prelude.<$> (x Data..:? "dataReplicationError")
            Prelude.<*> (x Data..:? "lagDuration")
            Prelude.<*> (x Data..:? "dataReplicationInitiation")
            Prelude.<*> ( x Data..:? "replicatedDisks"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "dataReplicationState")
            Prelude.<*> (x Data..:? "etaDateTime")
      )

instance Prelude.Hashable DataReplicationInfo where
  hashWithSalt _salt DataReplicationInfo' {..} =
    _salt `Prelude.hashWithSalt` dataReplicationError
      `Prelude.hashWithSalt` lagDuration
      `Prelude.hashWithSalt` dataReplicationInitiation
      `Prelude.hashWithSalt` replicatedDisks
      `Prelude.hashWithSalt` dataReplicationState
      `Prelude.hashWithSalt` etaDateTime

instance Prelude.NFData DataReplicationInfo where
  rnf DataReplicationInfo' {..} =
    Prelude.rnf dataReplicationError
      `Prelude.seq` Prelude.rnf lagDuration
      `Prelude.seq` Prelude.rnf dataReplicationInitiation
      `Prelude.seq` Prelude.rnf replicatedDisks
      `Prelude.seq` Prelude.rnf dataReplicationState
      `Prelude.seq` Prelude.rnf etaDateTime
