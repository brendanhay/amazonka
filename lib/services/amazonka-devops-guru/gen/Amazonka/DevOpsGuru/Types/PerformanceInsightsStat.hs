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
-- Module      : Amazonka.DevOpsGuru.Types.PerformanceInsightsStat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.PerformanceInsightsStat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A statistic in a Performance Insights collection.
--
-- /See:/ 'newPerformanceInsightsStat' smart constructor.
data PerformanceInsightsStat = PerformanceInsightsStat'
  { -- | The statistic type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The value of the statistic.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PerformanceInsightsStat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'performanceInsightsStat_type' - The statistic type.
--
-- 'value', 'performanceInsightsStat_value' - The value of the statistic.
newPerformanceInsightsStat ::
  PerformanceInsightsStat
newPerformanceInsightsStat =
  PerformanceInsightsStat'
    { type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The statistic type.
performanceInsightsStat_type :: Lens.Lens' PerformanceInsightsStat (Prelude.Maybe Prelude.Text)
performanceInsightsStat_type = Lens.lens (\PerformanceInsightsStat' {type'} -> type') (\s@PerformanceInsightsStat' {} a -> s {type' = a} :: PerformanceInsightsStat)

-- | The value of the statistic.
performanceInsightsStat_value :: Lens.Lens' PerformanceInsightsStat (Prelude.Maybe Prelude.Double)
performanceInsightsStat_value = Lens.lens (\PerformanceInsightsStat' {value} -> value) (\s@PerformanceInsightsStat' {} a -> s {value = a} :: PerformanceInsightsStat)

instance Data.FromJSON PerformanceInsightsStat where
  parseJSON =
    Data.withObject
      "PerformanceInsightsStat"
      ( \x ->
          PerformanceInsightsStat'
            Prelude.<$> (x Data..:? "Type") Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable PerformanceInsightsStat where
  hashWithSalt _salt PerformanceInsightsStat' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData PerformanceInsightsStat where
  rnf PerformanceInsightsStat' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value
