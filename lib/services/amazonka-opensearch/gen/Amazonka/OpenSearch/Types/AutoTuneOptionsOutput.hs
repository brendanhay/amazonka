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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AutoTuneOptionsOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.AutoTuneState
import qualified Amazonka.Prelude as Prelude

-- | The Auto-Tune settings for a domain, displayed when enabling or
-- disabling Auto-Tune.
--
-- /See:/ 'newAutoTuneOptionsOutput' smart constructor.
data AutoTuneOptionsOutput = AutoTuneOptionsOutput'
  { -- | Any errors that occurred while enabling or disabling Auto-Tune.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The current state of Auto-Tune on the domain.
    state :: Prelude.Maybe AutoTuneState,
    -- | Whether the domain\'s off-peak window will be used to deploy Auto-Tune
    -- changes rather than a maintenance schedule.
    useOffPeakWindow :: Prelude.Maybe Prelude.Bool
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
-- 'errorMessage', 'autoTuneOptionsOutput_errorMessage' - Any errors that occurred while enabling or disabling Auto-Tune.
--
-- 'state', 'autoTuneOptionsOutput_state' - The current state of Auto-Tune on the domain.
--
-- 'useOffPeakWindow', 'autoTuneOptionsOutput_useOffPeakWindow' - Whether the domain\'s off-peak window will be used to deploy Auto-Tune
-- changes rather than a maintenance schedule.
newAutoTuneOptionsOutput ::
  AutoTuneOptionsOutput
newAutoTuneOptionsOutput =
  AutoTuneOptionsOutput'
    { errorMessage =
        Prelude.Nothing,
      state = Prelude.Nothing,
      useOffPeakWindow = Prelude.Nothing
    }

-- | Any errors that occurred while enabling or disabling Auto-Tune.
autoTuneOptionsOutput_errorMessage :: Lens.Lens' AutoTuneOptionsOutput (Prelude.Maybe Prelude.Text)
autoTuneOptionsOutput_errorMessage = Lens.lens (\AutoTuneOptionsOutput' {errorMessage} -> errorMessage) (\s@AutoTuneOptionsOutput' {} a -> s {errorMessage = a} :: AutoTuneOptionsOutput)

-- | The current state of Auto-Tune on the domain.
autoTuneOptionsOutput_state :: Lens.Lens' AutoTuneOptionsOutput (Prelude.Maybe AutoTuneState)
autoTuneOptionsOutput_state = Lens.lens (\AutoTuneOptionsOutput' {state} -> state) (\s@AutoTuneOptionsOutput' {} a -> s {state = a} :: AutoTuneOptionsOutput)

-- | Whether the domain\'s off-peak window will be used to deploy Auto-Tune
-- changes rather than a maintenance schedule.
autoTuneOptionsOutput_useOffPeakWindow :: Lens.Lens' AutoTuneOptionsOutput (Prelude.Maybe Prelude.Bool)
autoTuneOptionsOutput_useOffPeakWindow = Lens.lens (\AutoTuneOptionsOutput' {useOffPeakWindow} -> useOffPeakWindow) (\s@AutoTuneOptionsOutput' {} a -> s {useOffPeakWindow = a} :: AutoTuneOptionsOutput)

instance Data.FromJSON AutoTuneOptionsOutput where
  parseJSON =
    Data.withObject
      "AutoTuneOptionsOutput"
      ( \x ->
          AutoTuneOptionsOutput'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "UseOffPeakWindow")
      )

instance Prelude.Hashable AutoTuneOptionsOutput where
  hashWithSalt _salt AutoTuneOptionsOutput' {..} =
    _salt
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` useOffPeakWindow

instance Prelude.NFData AutoTuneOptionsOutput where
  rnf AutoTuneOptionsOutput' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf useOffPeakWindow
