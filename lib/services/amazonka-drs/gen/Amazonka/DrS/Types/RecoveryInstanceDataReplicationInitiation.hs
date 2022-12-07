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
-- Module      : Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStep
import qualified Amazonka.Prelude as Prelude

-- | Data replication initiation.
--
-- /See:/ 'newRecoveryInstanceDataReplicationInitiation' smart constructor.
data RecoveryInstanceDataReplicationInitiation = RecoveryInstanceDataReplicationInitiation'
  { -- | The date and time of the current attempt to initiate data replication.
    startDateTime :: Prelude.Maybe Prelude.Text,
    -- | The steps of the current attempt to initiate data replication.
    steps :: Prelude.Maybe [RecoveryInstanceDataReplicationInitiationStep]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryInstanceDataReplicationInitiation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startDateTime', 'recoveryInstanceDataReplicationInitiation_startDateTime' - The date and time of the current attempt to initiate data replication.
--
-- 'steps', 'recoveryInstanceDataReplicationInitiation_steps' - The steps of the current attempt to initiate data replication.
newRecoveryInstanceDataReplicationInitiation ::
  RecoveryInstanceDataReplicationInitiation
newRecoveryInstanceDataReplicationInitiation =
  RecoveryInstanceDataReplicationInitiation'
    { startDateTime =
        Prelude.Nothing,
      steps = Prelude.Nothing
    }

-- | The date and time of the current attempt to initiate data replication.
recoveryInstanceDataReplicationInitiation_startDateTime :: Lens.Lens' RecoveryInstanceDataReplicationInitiation (Prelude.Maybe Prelude.Text)
recoveryInstanceDataReplicationInitiation_startDateTime = Lens.lens (\RecoveryInstanceDataReplicationInitiation' {startDateTime} -> startDateTime) (\s@RecoveryInstanceDataReplicationInitiation' {} a -> s {startDateTime = a} :: RecoveryInstanceDataReplicationInitiation)

-- | The steps of the current attempt to initiate data replication.
recoveryInstanceDataReplicationInitiation_steps :: Lens.Lens' RecoveryInstanceDataReplicationInitiation (Prelude.Maybe [RecoveryInstanceDataReplicationInitiationStep])
recoveryInstanceDataReplicationInitiation_steps = Lens.lens (\RecoveryInstanceDataReplicationInitiation' {steps} -> steps) (\s@RecoveryInstanceDataReplicationInitiation' {} a -> s {steps = a} :: RecoveryInstanceDataReplicationInitiation) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    RecoveryInstanceDataReplicationInitiation
  where
  parseJSON =
    Data.withObject
      "RecoveryInstanceDataReplicationInitiation"
      ( \x ->
          RecoveryInstanceDataReplicationInitiation'
            Prelude.<$> (x Data..:? "startDateTime")
              Prelude.<*> (x Data..:? "steps" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    RecoveryInstanceDataReplicationInitiation
  where
  hashWithSalt
    _salt
    RecoveryInstanceDataReplicationInitiation' {..} =
      _salt `Prelude.hashWithSalt` startDateTime
        `Prelude.hashWithSalt` steps

instance
  Prelude.NFData
    RecoveryInstanceDataReplicationInitiation
  where
  rnf RecoveryInstanceDataReplicationInitiation' {..} =
    Prelude.rnf startDateTime
      `Prelude.seq` Prelude.rnf steps
