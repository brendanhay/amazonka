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
-- Module      : Network.AWS.GuardDuty.Types.FindingStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FindingStatistics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about finding statistics.
--
-- /See:/ 'newFindingStatistics' smart constructor.
data FindingStatistics = FindingStatistics'
  { -- | Represents a map of severity to count statistics for a set of findings.
    countBySeverity :: Core.Maybe (Core.HashMap Core.Text Core.Int)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  FindingStatistics' {countBySeverity = Core.Nothing}

-- | Represents a map of severity to count statistics for a set of findings.
findingStatistics_countBySeverity :: Lens.Lens' FindingStatistics (Core.Maybe (Core.HashMap Core.Text Core.Int))
findingStatistics_countBySeverity = Lens.lens (\FindingStatistics' {countBySeverity} -> countBySeverity) (\s@FindingStatistics' {} a -> s {countBySeverity = a} :: FindingStatistics) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON FindingStatistics where
  parseJSON =
    Core.withObject
      "FindingStatistics"
      ( \x ->
          FindingStatistics'
            Core.<$> (x Core..:? "countBySeverity" Core..!= Core.mempty)
      )

instance Core.Hashable FindingStatistics

instance Core.NFData FindingStatistics
