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
-- Module      : Amazonka.SWF.Types.ExecutionTimeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ExecutionTimeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used to filter the workflow executions in visibility APIs by various
-- time-based rules. Each parameter, if specified, defines a rule that must
-- be satisfied by each returned query result. The parameter values are in
-- the <https://en.wikipedia.org/wiki/Unix_time Unix Time format>. For
-- example: @\"oldestDate\": 1325376070.@
--
-- /See:/ 'newExecutionTimeFilter' smart constructor.
data ExecutionTimeFilter = ExecutionTimeFilter'
  { -- | Specifies the latest start or close date and time to return.
    latestDate :: Prelude.Maybe Data.POSIX,
    -- | Specifies the oldest start or close date and time to return.
    oldestDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
      oldestDate = Data._Time Lens.# pOldestDate_
    }

-- | Specifies the latest start or close date and time to return.
executionTimeFilter_latestDate :: Lens.Lens' ExecutionTimeFilter (Prelude.Maybe Prelude.UTCTime)
executionTimeFilter_latestDate = Lens.lens (\ExecutionTimeFilter' {latestDate} -> latestDate) (\s@ExecutionTimeFilter' {} a -> s {latestDate = a} :: ExecutionTimeFilter) Prelude.. Lens.mapping Data._Time

-- | Specifies the oldest start or close date and time to return.
executionTimeFilter_oldestDate :: Lens.Lens' ExecutionTimeFilter Prelude.UTCTime
executionTimeFilter_oldestDate = Lens.lens (\ExecutionTimeFilter' {oldestDate} -> oldestDate) (\s@ExecutionTimeFilter' {} a -> s {oldestDate = a} :: ExecutionTimeFilter) Prelude.. Data._Time

instance Prelude.Hashable ExecutionTimeFilter where
  hashWithSalt _salt ExecutionTimeFilter' {..} =
    _salt
      `Prelude.hashWithSalt` latestDate
      `Prelude.hashWithSalt` oldestDate

instance Prelude.NFData ExecutionTimeFilter where
  rnf ExecutionTimeFilter' {..} =
    Prelude.rnf latestDate
      `Prelude.seq` Prelude.rnf oldestDate

instance Data.ToJSON ExecutionTimeFilter where
  toJSON ExecutionTimeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("latestDate" Data..=) Prelude.<$> latestDate,
            Prelude.Just ("oldestDate" Data..= oldestDate)
          ]
      )
