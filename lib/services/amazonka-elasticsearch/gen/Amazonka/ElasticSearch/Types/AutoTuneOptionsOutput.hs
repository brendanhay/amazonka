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
-- Module      : Amazonka.ElasticSearch.Types.AutoTuneOptionsOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.AutoTuneOptionsOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.AutoTuneState
import qualified Amazonka.Prelude as Prelude

-- | Specifies the Auto-Tune options: the Auto-Tune desired state for the
-- domain and list of maintenance schedules.
--
-- /See:/ 'newAutoTuneOptionsOutput' smart constructor.
data AutoTuneOptionsOutput = AutoTuneOptionsOutput'
  { -- | Specifies the error message while enabling or disabling the Auto-Tune.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @AutoTuneState@ for the Elasticsearch domain.
    state :: Prelude.Maybe AutoTuneState
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
-- 'errorMessage', 'autoTuneOptionsOutput_errorMessage' - Specifies the error message while enabling or disabling the Auto-Tune.
--
-- 'state', 'autoTuneOptionsOutput_state' - Specifies the @AutoTuneState@ for the Elasticsearch domain.
newAutoTuneOptionsOutput ::
  AutoTuneOptionsOutput
newAutoTuneOptionsOutput =
  AutoTuneOptionsOutput'
    { errorMessage =
        Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | Specifies the error message while enabling or disabling the Auto-Tune.
autoTuneOptionsOutput_errorMessage :: Lens.Lens' AutoTuneOptionsOutput (Prelude.Maybe Prelude.Text)
autoTuneOptionsOutput_errorMessage = Lens.lens (\AutoTuneOptionsOutput' {errorMessage} -> errorMessage) (\s@AutoTuneOptionsOutput' {} a -> s {errorMessage = a} :: AutoTuneOptionsOutput)

-- | Specifies the @AutoTuneState@ for the Elasticsearch domain.
autoTuneOptionsOutput_state :: Lens.Lens' AutoTuneOptionsOutput (Prelude.Maybe AutoTuneState)
autoTuneOptionsOutput_state = Lens.lens (\AutoTuneOptionsOutput' {state} -> state) (\s@AutoTuneOptionsOutput' {} a -> s {state = a} :: AutoTuneOptionsOutput)

instance Data.FromJSON AutoTuneOptionsOutput where
  parseJSON =
    Data.withObject
      "AutoTuneOptionsOutput"
      ( \x ->
          AutoTuneOptionsOutput'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable AutoTuneOptionsOutput where
  hashWithSalt _salt AutoTuneOptionsOutput' {..} =
    _salt
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` state

instance Prelude.NFData AutoTuneOptionsOutput where
  rnf AutoTuneOptionsOutput' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf state
