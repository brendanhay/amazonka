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
-- Module      : Amazonka.DevOpsGuru.Types.InsightTimeRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.InsightTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A time ranged that specifies when the observed behavior in an insight
-- started and ended.
--
-- /See:/ 'newInsightTimeRange' smart constructor.
data InsightTimeRange = InsightTimeRange'
  { -- | The time when the behavior described in an insight ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The time when the behavior described in an insight started.
    startTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'insightTimeRange_endTime' - The time when the behavior described in an insight ended.
--
-- 'startTime', 'insightTimeRange_startTime' - The time when the behavior described in an insight started.
newInsightTimeRange ::
  -- | 'startTime'
  Prelude.UTCTime ->
  InsightTimeRange
newInsightTimeRange pStartTime_ =
  InsightTimeRange'
    { endTime = Prelude.Nothing,
      startTime = Data._Time Lens.# pStartTime_
    }

-- | The time when the behavior described in an insight ended.
insightTimeRange_endTime :: Lens.Lens' InsightTimeRange (Prelude.Maybe Prelude.UTCTime)
insightTimeRange_endTime = Lens.lens (\InsightTimeRange' {endTime} -> endTime) (\s@InsightTimeRange' {} a -> s {endTime = a} :: InsightTimeRange) Prelude.. Lens.mapping Data._Time

-- | The time when the behavior described in an insight started.
insightTimeRange_startTime :: Lens.Lens' InsightTimeRange Prelude.UTCTime
insightTimeRange_startTime = Lens.lens (\InsightTimeRange' {startTime} -> startTime) (\s@InsightTimeRange' {} a -> s {startTime = a} :: InsightTimeRange) Prelude.. Data._Time

instance Data.FromJSON InsightTimeRange where
  parseJSON =
    Data.withObject
      "InsightTimeRange"
      ( \x ->
          InsightTimeRange'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..: "StartTime")
      )

instance Prelude.Hashable InsightTimeRange where
  hashWithSalt _salt InsightTimeRange' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData InsightTimeRange where
  rnf InsightTimeRange' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
