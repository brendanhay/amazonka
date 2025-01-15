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
-- Module      : Amazonka.DrS.Types.RecoveryInstanceDataReplicationInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryInstanceDataReplicationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationError
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInfoReplicatedDisk
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiation
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationState
import qualified Amazonka.Prelude as Prelude

-- | Information about Data Replication
--
-- /See:/ 'newRecoveryInstanceDataReplicationInfo' smart constructor.
data RecoveryInstanceDataReplicationInfo = RecoveryInstanceDataReplicationInfo'
  { -- | Information about Data Replication
    dataReplicationError :: Prelude.Maybe RecoveryInstanceDataReplicationError,
    -- | Information about whether the data replication has been initiated.
    dataReplicationInitiation :: Prelude.Maybe RecoveryInstanceDataReplicationInitiation,
    -- | The state of the data replication.
    dataReplicationState :: Prelude.Maybe RecoveryInstanceDataReplicationState,
    -- | An estimate of when the data replication will be completed.
    etaDateTime :: Prelude.Maybe Prelude.Text,
    -- | Data replication lag duration.
    lagDuration :: Prelude.Maybe Prelude.Text,
    -- | The disks that should be replicated.
    replicatedDisks :: Prelude.Maybe [RecoveryInstanceDataReplicationInfoReplicatedDisk]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryInstanceDataReplicationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataReplicationError', 'recoveryInstanceDataReplicationInfo_dataReplicationError' - Information about Data Replication
--
-- 'dataReplicationInitiation', 'recoveryInstanceDataReplicationInfo_dataReplicationInitiation' - Information about whether the data replication has been initiated.
--
-- 'dataReplicationState', 'recoveryInstanceDataReplicationInfo_dataReplicationState' - The state of the data replication.
--
-- 'etaDateTime', 'recoveryInstanceDataReplicationInfo_etaDateTime' - An estimate of when the data replication will be completed.
--
-- 'lagDuration', 'recoveryInstanceDataReplicationInfo_lagDuration' - Data replication lag duration.
--
-- 'replicatedDisks', 'recoveryInstanceDataReplicationInfo_replicatedDisks' - The disks that should be replicated.
newRecoveryInstanceDataReplicationInfo ::
  RecoveryInstanceDataReplicationInfo
newRecoveryInstanceDataReplicationInfo =
  RecoveryInstanceDataReplicationInfo'
    { dataReplicationError =
        Prelude.Nothing,
      dataReplicationInitiation =
        Prelude.Nothing,
      dataReplicationState = Prelude.Nothing,
      etaDateTime = Prelude.Nothing,
      lagDuration = Prelude.Nothing,
      replicatedDisks = Prelude.Nothing
    }

-- | Information about Data Replication
recoveryInstanceDataReplicationInfo_dataReplicationError :: Lens.Lens' RecoveryInstanceDataReplicationInfo (Prelude.Maybe RecoveryInstanceDataReplicationError)
recoveryInstanceDataReplicationInfo_dataReplicationError = Lens.lens (\RecoveryInstanceDataReplicationInfo' {dataReplicationError} -> dataReplicationError) (\s@RecoveryInstanceDataReplicationInfo' {} a -> s {dataReplicationError = a} :: RecoveryInstanceDataReplicationInfo)

-- | Information about whether the data replication has been initiated.
recoveryInstanceDataReplicationInfo_dataReplicationInitiation :: Lens.Lens' RecoveryInstanceDataReplicationInfo (Prelude.Maybe RecoveryInstanceDataReplicationInitiation)
recoveryInstanceDataReplicationInfo_dataReplicationInitiation = Lens.lens (\RecoveryInstanceDataReplicationInfo' {dataReplicationInitiation} -> dataReplicationInitiation) (\s@RecoveryInstanceDataReplicationInfo' {} a -> s {dataReplicationInitiation = a} :: RecoveryInstanceDataReplicationInfo)

-- | The state of the data replication.
recoveryInstanceDataReplicationInfo_dataReplicationState :: Lens.Lens' RecoveryInstanceDataReplicationInfo (Prelude.Maybe RecoveryInstanceDataReplicationState)
recoveryInstanceDataReplicationInfo_dataReplicationState = Lens.lens (\RecoveryInstanceDataReplicationInfo' {dataReplicationState} -> dataReplicationState) (\s@RecoveryInstanceDataReplicationInfo' {} a -> s {dataReplicationState = a} :: RecoveryInstanceDataReplicationInfo)

-- | An estimate of when the data replication will be completed.
recoveryInstanceDataReplicationInfo_etaDateTime :: Lens.Lens' RecoveryInstanceDataReplicationInfo (Prelude.Maybe Prelude.Text)
recoveryInstanceDataReplicationInfo_etaDateTime = Lens.lens (\RecoveryInstanceDataReplicationInfo' {etaDateTime} -> etaDateTime) (\s@RecoveryInstanceDataReplicationInfo' {} a -> s {etaDateTime = a} :: RecoveryInstanceDataReplicationInfo)

-- | Data replication lag duration.
recoveryInstanceDataReplicationInfo_lagDuration :: Lens.Lens' RecoveryInstanceDataReplicationInfo (Prelude.Maybe Prelude.Text)
recoveryInstanceDataReplicationInfo_lagDuration = Lens.lens (\RecoveryInstanceDataReplicationInfo' {lagDuration} -> lagDuration) (\s@RecoveryInstanceDataReplicationInfo' {} a -> s {lagDuration = a} :: RecoveryInstanceDataReplicationInfo)

-- | The disks that should be replicated.
recoveryInstanceDataReplicationInfo_replicatedDisks :: Lens.Lens' RecoveryInstanceDataReplicationInfo (Prelude.Maybe [RecoveryInstanceDataReplicationInfoReplicatedDisk])
recoveryInstanceDataReplicationInfo_replicatedDisks = Lens.lens (\RecoveryInstanceDataReplicationInfo' {replicatedDisks} -> replicatedDisks) (\s@RecoveryInstanceDataReplicationInfo' {} a -> s {replicatedDisks = a} :: RecoveryInstanceDataReplicationInfo) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    RecoveryInstanceDataReplicationInfo
  where
  parseJSON =
    Data.withObject
      "RecoveryInstanceDataReplicationInfo"
      ( \x ->
          RecoveryInstanceDataReplicationInfo'
            Prelude.<$> (x Data..:? "dataReplicationError")
            Prelude.<*> (x Data..:? "dataReplicationInitiation")
            Prelude.<*> (x Data..:? "dataReplicationState")
            Prelude.<*> (x Data..:? "etaDateTime")
            Prelude.<*> (x Data..:? "lagDuration")
            Prelude.<*> ( x
                            Data..:? "replicatedDisks"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    RecoveryInstanceDataReplicationInfo
  where
  hashWithSalt
    _salt
    RecoveryInstanceDataReplicationInfo' {..} =
      _salt
        `Prelude.hashWithSalt` dataReplicationError
        `Prelude.hashWithSalt` dataReplicationInitiation
        `Prelude.hashWithSalt` dataReplicationState
        `Prelude.hashWithSalt` etaDateTime
        `Prelude.hashWithSalt` lagDuration
        `Prelude.hashWithSalt` replicatedDisks

instance
  Prelude.NFData
    RecoveryInstanceDataReplicationInfo
  where
  rnf RecoveryInstanceDataReplicationInfo' {..} =
    Prelude.rnf dataReplicationError `Prelude.seq`
      Prelude.rnf dataReplicationInitiation `Prelude.seq`
        Prelude.rnf dataReplicationState `Prelude.seq`
          Prelude.rnf etaDateTime `Prelude.seq`
            Prelude.rnf lagDuration `Prelude.seq`
              Prelude.rnf replicatedDisks
