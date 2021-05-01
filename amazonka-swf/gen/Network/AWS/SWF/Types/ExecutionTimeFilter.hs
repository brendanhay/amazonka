{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used to filter the workflow executions in visibility APIs by various
-- time-based rules. Each parameter, if specified, defines a rule that must
-- be satisfied by each returned query result. The parameter values are in
-- the <https://en.wikipedia.org/wiki/Unix_time Unix Time format>. For
-- example: @\"oldestDate\": 1325376070.@
--
-- /See:/ 'newExecutionTimeFilter' smart constructor.
data ExecutionTimeFilter = ExecutionTimeFilter'
  { -- | Specifies the latest start or close date and time to return.
    latestDate :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies the oldest start or close date and time to return.
    oldestDate :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.UTCTime ->
  ExecutionTimeFilter
newExecutionTimeFilter pOldestDate_ =
  ExecutionTimeFilter'
    { latestDate = Prelude.Nothing,
      oldestDate = Prelude._Time Lens.# pOldestDate_
    }

-- | Specifies the latest start or close date and time to return.
executionTimeFilter_latestDate :: Lens.Lens' ExecutionTimeFilter (Prelude.Maybe Prelude.UTCTime)
executionTimeFilter_latestDate = Lens.lens (\ExecutionTimeFilter' {latestDate} -> latestDate) (\s@ExecutionTimeFilter' {} a -> s {latestDate = a} :: ExecutionTimeFilter) Prelude.. Lens.mapping Prelude._Time

-- | Specifies the oldest start or close date and time to return.
executionTimeFilter_oldestDate :: Lens.Lens' ExecutionTimeFilter Prelude.UTCTime
executionTimeFilter_oldestDate = Lens.lens (\ExecutionTimeFilter' {oldestDate} -> oldestDate) (\s@ExecutionTimeFilter' {} a -> s {oldestDate = a} :: ExecutionTimeFilter) Prelude.. Prelude._Time

instance Prelude.Hashable ExecutionTimeFilter

instance Prelude.NFData ExecutionTimeFilter

instance Prelude.ToJSON ExecutionTimeFilter where
  toJSON ExecutionTimeFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("latestDate" Prelude..=) Prelude.<$> latestDate,
            Prelude.Just ("oldestDate" Prelude..= oldestDate)
          ]
      )
