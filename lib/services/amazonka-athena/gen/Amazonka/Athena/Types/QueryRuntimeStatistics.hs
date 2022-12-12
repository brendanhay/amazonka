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
-- Module      : Amazonka.Athena.Types.QueryRuntimeStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.QueryRuntimeStatistics where

import Amazonka.Athena.Types.QueryRuntimeStatisticsRows
import Amazonka.Athena.Types.QueryRuntimeStatisticsTimeline
import Amazonka.Athena.Types.QueryStage
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The query execution timeline, statistics on input and output rows and
-- bytes, and the different query stages that form the query execution
-- plan.
--
-- /See:/ 'newQueryRuntimeStatistics' smart constructor.
data QueryRuntimeStatistics = QueryRuntimeStatistics'
  { -- | Stage statistics such as input and output rows and bytes, execution
    -- time, and stage state. This information also includes substages and the
    -- query stage plan.
    outputStage :: Prelude.Maybe QueryStage,
    rows :: Prelude.Maybe QueryRuntimeStatisticsRows,
    timeline :: Prelude.Maybe QueryRuntimeStatisticsTimeline
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryRuntimeStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputStage', 'queryRuntimeStatistics_outputStage' - Stage statistics such as input and output rows and bytes, execution
-- time, and stage state. This information also includes substages and the
-- query stage plan.
--
-- 'rows', 'queryRuntimeStatistics_rows' - Undocumented member.
--
-- 'timeline', 'queryRuntimeStatistics_timeline' - Undocumented member.
newQueryRuntimeStatistics ::
  QueryRuntimeStatistics
newQueryRuntimeStatistics =
  QueryRuntimeStatistics'
    { outputStage =
        Prelude.Nothing,
      rows = Prelude.Nothing,
      timeline = Prelude.Nothing
    }

-- | Stage statistics such as input and output rows and bytes, execution
-- time, and stage state. This information also includes substages and the
-- query stage plan.
queryRuntimeStatistics_outputStage :: Lens.Lens' QueryRuntimeStatistics (Prelude.Maybe QueryStage)
queryRuntimeStatistics_outputStage = Lens.lens (\QueryRuntimeStatistics' {outputStage} -> outputStage) (\s@QueryRuntimeStatistics' {} a -> s {outputStage = a} :: QueryRuntimeStatistics)

-- | Undocumented member.
queryRuntimeStatistics_rows :: Lens.Lens' QueryRuntimeStatistics (Prelude.Maybe QueryRuntimeStatisticsRows)
queryRuntimeStatistics_rows = Lens.lens (\QueryRuntimeStatistics' {rows} -> rows) (\s@QueryRuntimeStatistics' {} a -> s {rows = a} :: QueryRuntimeStatistics)

-- | Undocumented member.
queryRuntimeStatistics_timeline :: Lens.Lens' QueryRuntimeStatistics (Prelude.Maybe QueryRuntimeStatisticsTimeline)
queryRuntimeStatistics_timeline = Lens.lens (\QueryRuntimeStatistics' {timeline} -> timeline) (\s@QueryRuntimeStatistics' {} a -> s {timeline = a} :: QueryRuntimeStatistics)

instance Data.FromJSON QueryRuntimeStatistics where
  parseJSON =
    Data.withObject
      "QueryRuntimeStatistics"
      ( \x ->
          QueryRuntimeStatistics'
            Prelude.<$> (x Data..:? "OutputStage")
            Prelude.<*> (x Data..:? "Rows")
            Prelude.<*> (x Data..:? "Timeline")
      )

instance Prelude.Hashable QueryRuntimeStatistics where
  hashWithSalt _salt QueryRuntimeStatistics' {..} =
    _salt `Prelude.hashWithSalt` outputStage
      `Prelude.hashWithSalt` rows
      `Prelude.hashWithSalt` timeline

instance Prelude.NFData QueryRuntimeStatistics where
  rnf QueryRuntimeStatistics' {..} =
    Prelude.rnf outputStage
      `Prelude.seq` Prelude.rnf rows
      `Prelude.seq` Prelude.rnf timeline
