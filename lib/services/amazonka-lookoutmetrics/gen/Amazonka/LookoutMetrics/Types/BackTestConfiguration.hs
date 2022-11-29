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
-- Module      : Amazonka.LookoutMetrics.Types.BackTestConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.BackTestConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings for backtest mode.
--
-- /See:/ 'newBackTestConfiguration' smart constructor.
data BackTestConfiguration = BackTestConfiguration'
  { -- | Run a backtest instead of monitoring new data.
    runBackTestMode :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackTestConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runBackTestMode', 'backTestConfiguration_runBackTestMode' - Run a backtest instead of monitoring new data.
newBackTestConfiguration ::
  -- | 'runBackTestMode'
  Prelude.Bool ->
  BackTestConfiguration
newBackTestConfiguration pRunBackTestMode_ =
  BackTestConfiguration'
    { runBackTestMode =
        pRunBackTestMode_
    }

-- | Run a backtest instead of monitoring new data.
backTestConfiguration_runBackTestMode :: Lens.Lens' BackTestConfiguration Prelude.Bool
backTestConfiguration_runBackTestMode = Lens.lens (\BackTestConfiguration' {runBackTestMode} -> runBackTestMode) (\s@BackTestConfiguration' {} a -> s {runBackTestMode = a} :: BackTestConfiguration)

instance Core.FromJSON BackTestConfiguration where
  parseJSON =
    Core.withObject
      "BackTestConfiguration"
      ( \x ->
          BackTestConfiguration'
            Prelude.<$> (x Core..: "RunBackTestMode")
      )

instance Prelude.Hashable BackTestConfiguration where
  hashWithSalt _salt BackTestConfiguration' {..} =
    _salt `Prelude.hashWithSalt` runBackTestMode

instance Prelude.NFData BackTestConfiguration where
  rnf BackTestConfiguration' {..} =
    Prelude.rnf runBackTestMode

instance Core.ToJSON BackTestConfiguration where
  toJSON BackTestConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RunBackTestMode" Core..= runBackTestMode)
          ]
      )
