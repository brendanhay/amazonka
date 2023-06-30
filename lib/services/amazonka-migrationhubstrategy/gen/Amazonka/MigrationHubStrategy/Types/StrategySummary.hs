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
-- Module      : Amazonka.MigrationHubStrategy.Types.StrategySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.StrategySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.Strategy
import qualified Amazonka.Prelude as Prelude

-- | Object containing the summary of the strategy recommendations.
--
-- /See:/ 'newStrategySummary' smart constructor.
data StrategySummary = StrategySummary'
  { -- | The count of recommendations per strategy.
    count :: Prelude.Maybe Prelude.Int,
    -- | The name of recommended strategy.
    strategy :: Prelude.Maybe Strategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StrategySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'strategySummary_count' - The count of recommendations per strategy.
--
-- 'strategy', 'strategySummary_strategy' - The name of recommended strategy.
newStrategySummary ::
  StrategySummary
newStrategySummary =
  StrategySummary'
    { count = Prelude.Nothing,
      strategy = Prelude.Nothing
    }

-- | The count of recommendations per strategy.
strategySummary_count :: Lens.Lens' StrategySummary (Prelude.Maybe Prelude.Int)
strategySummary_count = Lens.lens (\StrategySummary' {count} -> count) (\s@StrategySummary' {} a -> s {count = a} :: StrategySummary)

-- | The name of recommended strategy.
strategySummary_strategy :: Lens.Lens' StrategySummary (Prelude.Maybe Strategy)
strategySummary_strategy = Lens.lens (\StrategySummary' {strategy} -> strategy) (\s@StrategySummary' {} a -> s {strategy = a} :: StrategySummary)

instance Data.FromJSON StrategySummary where
  parseJSON =
    Data.withObject
      "StrategySummary"
      ( \x ->
          StrategySummary'
            Prelude.<$> (x Data..:? "count")
            Prelude.<*> (x Data..:? "strategy")
      )

instance Prelude.Hashable StrategySummary where
  hashWithSalt _salt StrategySummary' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` strategy

instance Prelude.NFData StrategySummary where
  rnf StrategySummary' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf strategy
