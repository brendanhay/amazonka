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
-- Module      : Network.AWS.SWF.Types.ExecutionTimeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ExecutionTimeFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Used to filter the workflow executions in visibility APIs by various
-- time-based rules. Each parameter, if specified, defines a rule that must
-- be satisfied by each returned query result. The parameter values are in
-- the <https://en.wikipedia.org/wiki/Unix_time Unix Time format>. For
-- example: @\"oldestDate\": 1325376070.@
--
-- /See:/ 'newExecutionTimeFilter' smart constructor.
data ExecutionTimeFilter = ExecutionTimeFilter'
  { -- | Specifies the latest start or close date and time to return.
    latestDate :: Core.Maybe Core.POSIX,
    -- | Specifies the oldest start or close date and time to return.
    oldestDate :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExecutionTimeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestDate', 'executionTimeFilter_latestDate' - Specifies the latest start or close date and time to return.
--
-- 'oldestDate', 'executionTimeFilter_oldestDate' - Specifies the oldest start or close date and time to return.
newExecutionTimeFilter ::
  -- | 'oldestDate'
  Core.UTCTime ->
  ExecutionTimeFilter
newExecutionTimeFilter pOldestDate_ =
  ExecutionTimeFilter'
    { latestDate = Core.Nothing,
      oldestDate = Core._Time Lens.# pOldestDate_
    }

-- | Specifies the latest start or close date and time to return.
executionTimeFilter_latestDate :: Lens.Lens' ExecutionTimeFilter (Core.Maybe Core.UTCTime)
executionTimeFilter_latestDate = Lens.lens (\ExecutionTimeFilter' {latestDate} -> latestDate) (\s@ExecutionTimeFilter' {} a -> s {latestDate = a} :: ExecutionTimeFilter) Core.. Lens.mapping Core._Time

-- | Specifies the oldest start or close date and time to return.
executionTimeFilter_oldestDate :: Lens.Lens' ExecutionTimeFilter Core.UTCTime
executionTimeFilter_oldestDate = Lens.lens (\ExecutionTimeFilter' {oldestDate} -> oldestDate) (\s@ExecutionTimeFilter' {} a -> s {oldestDate = a} :: ExecutionTimeFilter) Core.. Core._Time

instance Core.Hashable ExecutionTimeFilter

instance Core.NFData ExecutionTimeFilter

instance Core.ToJSON ExecutionTimeFilter where
  toJSON ExecutionTimeFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("latestDate" Core..=) Core.<$> latestDate,
            Core.Just ("oldestDate" Core..= oldestDate)
          ]
      )
