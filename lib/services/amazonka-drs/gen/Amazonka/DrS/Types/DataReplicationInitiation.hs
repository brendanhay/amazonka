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
-- Module      : Amazonka.DrS.Types.DataReplicationInitiation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.DataReplicationInitiation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.DataReplicationInitiationStep
import qualified Amazonka.Prelude as Prelude

-- | Data replication initiation.
--
-- /See:/ 'newDataReplicationInitiation' smart constructor.
data DataReplicationInitiation = DataReplicationInitiation'
  { -- | The date and time of the next attempt to initiate data replication.
    nextAttemptDateTime :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the current attempt to initiate data replication.
    startDateTime :: Prelude.Maybe Prelude.Text,
    -- | The steps of the current attempt to initiate data replication.
    steps :: Prelude.Maybe [DataReplicationInitiationStep]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataReplicationInitiation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextAttemptDateTime', 'dataReplicationInitiation_nextAttemptDateTime' - The date and time of the next attempt to initiate data replication.
--
-- 'startDateTime', 'dataReplicationInitiation_startDateTime' - The date and time of the current attempt to initiate data replication.
--
-- 'steps', 'dataReplicationInitiation_steps' - The steps of the current attempt to initiate data replication.
newDataReplicationInitiation ::
  DataReplicationInitiation
newDataReplicationInitiation =
  DataReplicationInitiation'
    { nextAttemptDateTime =
        Prelude.Nothing,
      startDateTime = Prelude.Nothing,
      steps = Prelude.Nothing
    }

-- | The date and time of the next attempt to initiate data replication.
dataReplicationInitiation_nextAttemptDateTime :: Lens.Lens' DataReplicationInitiation (Prelude.Maybe Prelude.Text)
dataReplicationInitiation_nextAttemptDateTime = Lens.lens (\DataReplicationInitiation' {nextAttemptDateTime} -> nextAttemptDateTime) (\s@DataReplicationInitiation' {} a -> s {nextAttemptDateTime = a} :: DataReplicationInitiation)

-- | The date and time of the current attempt to initiate data replication.
dataReplicationInitiation_startDateTime :: Lens.Lens' DataReplicationInitiation (Prelude.Maybe Prelude.Text)
dataReplicationInitiation_startDateTime = Lens.lens (\DataReplicationInitiation' {startDateTime} -> startDateTime) (\s@DataReplicationInitiation' {} a -> s {startDateTime = a} :: DataReplicationInitiation)

-- | The steps of the current attempt to initiate data replication.
dataReplicationInitiation_steps :: Lens.Lens' DataReplicationInitiation (Prelude.Maybe [DataReplicationInitiationStep])
dataReplicationInitiation_steps = Lens.lens (\DataReplicationInitiation' {steps} -> steps) (\s@DataReplicationInitiation' {} a -> s {steps = a} :: DataReplicationInitiation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DataReplicationInitiation where
  parseJSON =
    Data.withObject
      "DataReplicationInitiation"
      ( \x ->
          DataReplicationInitiation'
            Prelude.<$> (x Data..:? "nextAttemptDateTime")
            Prelude.<*> (x Data..:? "startDateTime")
            Prelude.<*> (x Data..:? "steps" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DataReplicationInitiation where
  hashWithSalt _salt DataReplicationInitiation' {..} =
    _salt `Prelude.hashWithSalt` nextAttemptDateTime
      `Prelude.hashWithSalt` startDateTime
      `Prelude.hashWithSalt` steps

instance Prelude.NFData DataReplicationInitiation where
  rnf DataReplicationInitiation' {..} =
    Prelude.rnf nextAttemptDateTime
      `Prelude.seq` Prelude.rnf startDateTime
      `Prelude.seq` Prelude.rnf steps
