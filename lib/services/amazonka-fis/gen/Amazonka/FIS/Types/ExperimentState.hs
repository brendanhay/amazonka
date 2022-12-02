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
-- Module      : Amazonka.FIS.Types.ExperimentState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ExperimentStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of an experiment.
--
-- /See:/ 'newExperimentState' smart constructor.
data ExperimentState = ExperimentState'
  { -- | The state of the experiment.
    status :: Prelude.Maybe ExperimentStatus,
    -- | The reason for the state.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'experimentState_status' - The state of the experiment.
--
-- 'reason', 'experimentState_reason' - The reason for the state.
newExperimentState ::
  ExperimentState
newExperimentState =
  ExperimentState'
    { status = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The state of the experiment.
experimentState_status :: Lens.Lens' ExperimentState (Prelude.Maybe ExperimentStatus)
experimentState_status = Lens.lens (\ExperimentState' {status} -> status) (\s@ExperimentState' {} a -> s {status = a} :: ExperimentState)

-- | The reason for the state.
experimentState_reason :: Lens.Lens' ExperimentState (Prelude.Maybe Prelude.Text)
experimentState_reason = Lens.lens (\ExperimentState' {reason} -> reason) (\s@ExperimentState' {} a -> s {reason = a} :: ExperimentState)

instance Data.FromJSON ExperimentState where
  parseJSON =
    Data.withObject
      "ExperimentState"
      ( \x ->
          ExperimentState'
            Prelude.<$> (x Data..:? "status")
            Prelude.<*> (x Data..:? "reason")
      )

instance Prelude.Hashable ExperimentState where
  hashWithSalt _salt ExperimentState' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` reason

instance Prelude.NFData ExperimentState where
  rnf ExperimentState' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf reason
