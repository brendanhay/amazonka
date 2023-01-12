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
-- Module      : Amazonka.GuardDuty.Types.FindingStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.FindingStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about finding statistics.
--
-- /See:/ 'newFindingStatistics' smart constructor.
data FindingStatistics = FindingStatistics'
  { -- | Represents a map of severity to count statistics for a set of findings.
    countBySeverity :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countBySeverity', 'findingStatistics_countBySeverity' - Represents a map of severity to count statistics for a set of findings.
newFindingStatistics ::
  FindingStatistics
newFindingStatistics =
  FindingStatistics'
    { countBySeverity =
        Prelude.Nothing
    }

-- | Represents a map of severity to count statistics for a set of findings.
findingStatistics_countBySeverity :: Lens.Lens' FindingStatistics (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int))
findingStatistics_countBySeverity = Lens.lens (\FindingStatistics' {countBySeverity} -> countBySeverity) (\s@FindingStatistics' {} a -> s {countBySeverity = a} :: FindingStatistics) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FindingStatistics where
  parseJSON =
    Data.withObject
      "FindingStatistics"
      ( \x ->
          FindingStatistics'
            Prelude.<$> ( x Data..:? "countBySeverity"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable FindingStatistics where
  hashWithSalt _salt FindingStatistics' {..} =
    _salt `Prelude.hashWithSalt` countBySeverity

instance Prelude.NFData FindingStatistics where
  rnf FindingStatistics' {..} =
    Prelude.rnf countBySeverity
