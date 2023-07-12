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
-- Module      : Amazonka.MGN.Types.DataReplicationInitiation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.DataReplicationInitiation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.DataReplicationInitiationStep
import qualified Amazonka.Prelude as Prelude

-- | Data replication initiation.
--
-- /See:/ 'newDataReplicationInitiation' smart constructor.
data DataReplicationInitiation = DataReplicationInitiation'
  { -- | Request to query next data initiation date and time.
    nextAttemptDateTime :: Prelude.Maybe Prelude.Text,
    -- | Request to query data initiation start date and time.
    startDateTime :: Prelude.Maybe Prelude.Text,
    -- | Request to query data initiation steps.
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
-- 'nextAttemptDateTime', 'dataReplicationInitiation_nextAttemptDateTime' - Request to query next data initiation date and time.
--
-- 'startDateTime', 'dataReplicationInitiation_startDateTime' - Request to query data initiation start date and time.
--
-- 'steps', 'dataReplicationInitiation_steps' - Request to query data initiation steps.
newDataReplicationInitiation ::
  DataReplicationInitiation
newDataReplicationInitiation =
  DataReplicationInitiation'
    { nextAttemptDateTime =
        Prelude.Nothing,
      startDateTime = Prelude.Nothing,
      steps = Prelude.Nothing
    }

-- | Request to query next data initiation date and time.
dataReplicationInitiation_nextAttemptDateTime :: Lens.Lens' DataReplicationInitiation (Prelude.Maybe Prelude.Text)
dataReplicationInitiation_nextAttemptDateTime = Lens.lens (\DataReplicationInitiation' {nextAttemptDateTime} -> nextAttemptDateTime) (\s@DataReplicationInitiation' {} a -> s {nextAttemptDateTime = a} :: DataReplicationInitiation)

-- | Request to query data initiation start date and time.
dataReplicationInitiation_startDateTime :: Lens.Lens' DataReplicationInitiation (Prelude.Maybe Prelude.Text)
dataReplicationInitiation_startDateTime = Lens.lens (\DataReplicationInitiation' {startDateTime} -> startDateTime) (\s@DataReplicationInitiation' {} a -> s {startDateTime = a} :: DataReplicationInitiation)

-- | Request to query data initiation steps.
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
    _salt
      `Prelude.hashWithSalt` nextAttemptDateTime
      `Prelude.hashWithSalt` startDateTime
      `Prelude.hashWithSalt` steps

instance Prelude.NFData DataReplicationInitiation where
  rnf DataReplicationInitiation' {..} =
    Prelude.rnf nextAttemptDateTime
      `Prelude.seq` Prelude.rnf startDateTime
      `Prelude.seq` Prelude.rnf steps
