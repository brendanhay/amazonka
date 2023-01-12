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
-- Module      : Amazonka.IoTEventsData.Types.DetectorState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.DetectorState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types.Timer
import Amazonka.IoTEventsData.Types.Variable
import qualified Amazonka.Prelude as Prelude

-- | Information about the current state of the detector instance.
--
-- /See:/ 'newDetectorState' smart constructor.
data DetectorState = DetectorState'
  { -- | The name of the state.
    stateName :: Prelude.Text,
    -- | The current values of the detector\'s variables.
    variables :: [Variable],
    -- | The current state of the detector\'s timers.
    timers :: [Timer]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateName', 'detectorState_stateName' - The name of the state.
--
-- 'variables', 'detectorState_variables' - The current values of the detector\'s variables.
--
-- 'timers', 'detectorState_timers' - The current state of the detector\'s timers.
newDetectorState ::
  -- | 'stateName'
  Prelude.Text ->
  DetectorState
newDetectorState pStateName_ =
  DetectorState'
    { stateName = pStateName_,
      variables = Prelude.mempty,
      timers = Prelude.mempty
    }

-- | The name of the state.
detectorState_stateName :: Lens.Lens' DetectorState Prelude.Text
detectorState_stateName = Lens.lens (\DetectorState' {stateName} -> stateName) (\s@DetectorState' {} a -> s {stateName = a} :: DetectorState)

-- | The current values of the detector\'s variables.
detectorState_variables :: Lens.Lens' DetectorState [Variable]
detectorState_variables = Lens.lens (\DetectorState' {variables} -> variables) (\s@DetectorState' {} a -> s {variables = a} :: DetectorState) Prelude.. Lens.coerced

-- | The current state of the detector\'s timers.
detectorState_timers :: Lens.Lens' DetectorState [Timer]
detectorState_timers = Lens.lens (\DetectorState' {timers} -> timers) (\s@DetectorState' {} a -> s {timers = a} :: DetectorState) Prelude.. Lens.coerced

instance Data.FromJSON DetectorState where
  parseJSON =
    Data.withObject
      "DetectorState"
      ( \x ->
          DetectorState'
            Prelude.<$> (x Data..: "stateName")
            Prelude.<*> (x Data..:? "variables" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "timers" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DetectorState where
  hashWithSalt _salt DetectorState' {..} =
    _salt `Prelude.hashWithSalt` stateName
      `Prelude.hashWithSalt` variables
      `Prelude.hashWithSalt` timers

instance Prelude.NFData DetectorState where
  rnf DetectorState' {..} =
    Prelude.rnf stateName
      `Prelude.seq` Prelude.rnf variables
      `Prelude.seq` Prelude.rnf timers
