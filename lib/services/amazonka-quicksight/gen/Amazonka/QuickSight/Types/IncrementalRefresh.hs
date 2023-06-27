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
-- Module      : Amazonka.QuickSight.Types.IncrementalRefresh
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IncrementalRefresh where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LookbackWindow

-- | The incremental refresh configuration for a dataset.
--
-- /See:/ 'newIncrementalRefresh' smart constructor.
data IncrementalRefresh = IncrementalRefresh'
  { -- | The lookback window setup for an incremental refresh configuration.
    lookbackWindow :: LookbackWindow
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncrementalRefresh' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lookbackWindow', 'incrementalRefresh_lookbackWindow' - The lookback window setup for an incremental refresh configuration.
newIncrementalRefresh ::
  -- | 'lookbackWindow'
  LookbackWindow ->
  IncrementalRefresh
newIncrementalRefresh pLookbackWindow_ =
  IncrementalRefresh'
    { lookbackWindow =
        pLookbackWindow_
    }

-- | The lookback window setup for an incremental refresh configuration.
incrementalRefresh_lookbackWindow :: Lens.Lens' IncrementalRefresh LookbackWindow
incrementalRefresh_lookbackWindow = Lens.lens (\IncrementalRefresh' {lookbackWindow} -> lookbackWindow) (\s@IncrementalRefresh' {} a -> s {lookbackWindow = a} :: IncrementalRefresh)

instance Data.FromJSON IncrementalRefresh where
  parseJSON =
    Data.withObject
      "IncrementalRefresh"
      ( \x ->
          IncrementalRefresh'
            Prelude.<$> (x Data..: "LookbackWindow")
      )

instance Prelude.Hashable IncrementalRefresh where
  hashWithSalt _salt IncrementalRefresh' {..} =
    _salt `Prelude.hashWithSalt` lookbackWindow

instance Prelude.NFData IncrementalRefresh where
  rnf IncrementalRefresh' {..} =
    Prelude.rnf lookbackWindow

instance Data.ToJSON IncrementalRefresh where
  toJSON IncrementalRefresh' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LookbackWindow" Data..= lookbackWindow)
          ]
      )
