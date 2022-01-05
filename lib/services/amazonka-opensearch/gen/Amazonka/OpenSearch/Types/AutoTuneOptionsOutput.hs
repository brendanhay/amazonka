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
-- Module      : Amazonka.OpenSearch.Types.AutoTuneOptionsOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AutoTuneOptionsOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.AutoTuneState
import qualified Amazonka.Prelude as Prelude

-- | The Auto-Tune options: the Auto-Tune desired state for the domain and
-- list of maintenance schedules.
--
-- /See:/ 'newAutoTuneOptionsOutput' smart constructor.
data AutoTuneOptionsOutput = AutoTuneOptionsOutput'
  { -- | The @AutoTuneState@ for the domain.
    state :: Prelude.Maybe AutoTuneState,
    -- | The error message while enabling or disabling Auto-Tune.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneOptionsOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'autoTuneOptionsOutput_state' - The @AutoTuneState@ for the domain.
--
-- 'errorMessage', 'autoTuneOptionsOutput_errorMessage' - The error message while enabling or disabling Auto-Tune.
newAutoTuneOptionsOutput ::
  AutoTuneOptionsOutput
newAutoTuneOptionsOutput =
  AutoTuneOptionsOutput'
    { state = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The @AutoTuneState@ for the domain.
autoTuneOptionsOutput_state :: Lens.Lens' AutoTuneOptionsOutput (Prelude.Maybe AutoTuneState)
autoTuneOptionsOutput_state = Lens.lens (\AutoTuneOptionsOutput' {state} -> state) (\s@AutoTuneOptionsOutput' {} a -> s {state = a} :: AutoTuneOptionsOutput)

-- | The error message while enabling or disabling Auto-Tune.
autoTuneOptionsOutput_errorMessage :: Lens.Lens' AutoTuneOptionsOutput (Prelude.Maybe Prelude.Text)
autoTuneOptionsOutput_errorMessage = Lens.lens (\AutoTuneOptionsOutput' {errorMessage} -> errorMessage) (\s@AutoTuneOptionsOutput' {} a -> s {errorMessage = a} :: AutoTuneOptionsOutput)

instance Core.FromJSON AutoTuneOptionsOutput where
  parseJSON =
    Core.withObject
      "AutoTuneOptionsOutput"
      ( \x ->
          AutoTuneOptionsOutput'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "ErrorMessage")
      )

instance Prelude.Hashable AutoTuneOptionsOutput where
  hashWithSalt _salt AutoTuneOptionsOutput' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData AutoTuneOptionsOutput where
  rnf AutoTuneOptionsOutput' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf errorMessage
